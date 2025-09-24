;;; zorg.el --- Zen + Org project notes with side panels -*- lexical-binding: t; -*-

;;; Commentary:
;; Zorg provides a "Zen mode" for Emacs that centers your editing buffer
;; by creating side panels, which you can fill with project-specific notes.
;; Notes can be either:
;;
;; - Paired notes: linked to a specific project file (supporting multiple).
;; - Loose notes: free-floating but scoped to the project.
;;
;; Commands:
;; - `M-x zorg-mode`                 : Toggle the zen-like layout.
;; - `M-x zorg-note-pair`            : Open/create paired note(s).
;; - `M-x zorg-create-new-pair-note` : Create an additional pair note.
;; - `M-x zorg-note-new`             : Create a loose note.
;; - `M-x zorg-find-project-notes`   : Pick from all notes in project.
;; - `M-x zorg-find-directory-notes` : Pick from notes in current directory.
;; - `M-x zorg-find-directory-notes-recursive` : Pick from notes under current directory.
;; - `M-x zorg-find-pair-note`       : Pick from pair notes for current file.

;;; Code:

(require 'project)
(require 'org)
(require 'subr-x)

(defgroup zorg nil
  "Zen + Org project notes with side panels."
  :group 'convenience)

(defcustom zorg-notes-dir ".zorg-notes"
  "Directory (relative to project root) where Zorg notes are stored."
  :type 'string
  :group 'zorg)

(defvar zorg--saved-config nil)
(defvar zorg--active-p nil)
(defvar zorg--left-window nil)
(defvar zorg--right-window nil)

(defun zorg--project-root ()
  (or (when-let ((proj (project-current))) (project-root proj))
      default-directory))

(defun zorg--notes-path ()
  (expand-file-name zorg-notes-dir (zorg--project-root)))

(defun zorg--ensure-dir (path)
  (make-directory (file-name-directory path) t))

(defun zorg--main-window ()
  "Return the central main window when Zorg mode is active.
Prefer a window explicitly marked as the main window; otherwise,
pick any live window that is not the recorded left/right window."
  (or
   ;; Prefer a window tagged as main
   (car (seq-filter (lambda (w)
                      (eq (window-parameter w 'zorg-role) 'main))
                    (window-list)))
   ;; Fallback: any window that isn't left or right
   (car (seq-filter (lambda (w)
                      (and (window-live-p w)
                           (not (eq w zorg--left-window))
                           (not (eq w zorg--right-window))))
                    (window-list)))
   ;; Last resort: the currently selected window
   (selected-window)))

(defun zorg--main-buffer ()
  "Return the buffer displayed in the main (center) window."
  (let ((w (zorg--main-window)))
    (when (window-live-p w)
      (window-buffer w))))

(defun zorg--main-file ()
  (with-current-buffer (zorg--main-buffer)
    buffer-file-name))

(defun zorg--in-side-window-p ()
  (cond
   ((and zorg--left-window (eq (selected-window) zorg--left-window)) 'left)
   ((and zorg--right-window (eq (selected-window) zorg--right-window)) 'right)
   (t nil)))

(defun zorg--select-side-window ()
  (let* ((choice (completing-read "Open in side: " '("left" "right") nil t)))
    (if (string= choice "left")
        zorg--left-window
      zorg--right-window)))

;; -----------------------------
;; Paired notes
;; -----------------------------

(defun zorg--pair-files (rel)
  "Return list of real pair notes for REL (file-relative path)."
  (let* ((real-dir (expand-file-name "paired" (zorg--notes-path)))
         (hash (md5 rel)))
    (when (file-directory-p real-dir)
      (directory-files real-dir t (concat "^" hash)))))

(defun zorg-create-new-pair-note (title)
  "Create an additional paired note for the current file."
  (interactive "sPair note title: ")
  (let ((target-file (zorg--main-file)))
    (unless target-file
      (user-error "No file open in the main buffer"))
    (let* ((rel (file-relative-name target-file (zorg--project-root)))
           (hash (md5 rel))
           (real-dir (expand-file-name "paired" (zorg--notes-path)))
           (slug (replace-regexp-in-string "[^a-zA-Z0-9_-]" "_" title))
           (note (expand-file-name (format "%s-%s.org" hash slug) real-dir)))
      (zorg--ensure-dir note)
      (find-file note)
      (when(zorg--in-side-window-p)
      (org-mode)))))

(defun zorg-find-pair-note ()
  "Pick one of the paired notes for the current file."
  (interactive)
  (let ((target-file (zorg--main-file)))
    (unless target-file
      (user-error "No file open in the main buffer"))
    (let* ((rel (file-relative-name target-file (zorg--project-root)))
           (matches (zorg--pair-files rel)))
      (cond
       ((null matches)
        (message "No pair notes yet, use `zorg-create-new-pair-note`."))
       ((= (length matches) 1)
        (find-file (car matches)))
       (t
        (let ((choice (completing-read "Choose pair note: " matches nil t)))
          (find-file choice)))))))

(defun zorg-note-pair ()
  "Open paired note(s) for the current main buffer file.

- If in a side buffer: pretend the command was run in the main buffer,
  but reuse that side window to show the note.
- If in the main buffer: prompt for which side window to use.
- If no notes exist: create the first one."
  (interactive)
  (let* ((side (zorg--in-side-window-p))
         (main-buf (zorg--main-buffer)))
    (unless main-buf
      (user-error "No main window/buffer found"))
    (let ((target-file (with-current-buffer main-buf buffer-file-name)))
      (unless target-file
        (user-error "No file open in the main buffer"))
      (let* ((rel (file-relative-name target-file (zorg--project-root)))
             (matches (zorg--pair-files rel))
             (note
              (cond
               ((null matches)
                (let* ((real-dir (expand-file-name "paired" (zorg--notes-path)))
                       (real (expand-file-name (concat (md5 rel) ".org") real-dir)))
                  (zorg--ensure-dir real)
                  (with-temp-file real
                    (insert (format "# Paired note for %s\n\n" rel)))
                  real))
               ((= (length matches) 1)
                (car matches))
               (t
                (completing-read "Choose pair note: " matches nil t)))))
        (cond
         ;; Already in side window → reuse that one
         (side
          (find-file note))
         ;; In main buffer → ask which side to use
         (t
          (let ((win (zorg--select-side-window)))
            (if (window-live-p win)
                (with-selected-window win
                  (find-file note))
              (user-error "Side window is not live")))))
        (when (zorg--in-side-window-p)
  (org-mode))))))

;; -----------------------------
;; Loose notes
;; -----------------------------

(defun zorg-note-new (title)
  "Create a loose note for this project."
  (interactive "sLoose note title: ")
  (let* ((slug (replace-regexp-in-string "[^a-zA-Z0-9_-]" "_" title))
         (note (expand-file-name (concat "loose-" slug ".org")
                                 (zorg--notes-path))))
    (zorg--ensure-dir note)
    (pcase (zorg--in-side-window-p)
      ('left (find-file note))
      ('right (find-file note))
      (_ (let ((win (zorg--select-side-window)))
           (with-selected-window win
             (find-file note)))))
    (when (zorg--in-side-window-p)
  (org-mode))))

;; -----------------------------
;; Views
;; -----------------------------

(defun zorg--choose-note-from (files prompt)
  (when files
    (let ((choice (completing-read prompt files nil t)))
      (find-file choice)
      (when (zorg--in-side-window-p)
  (org-mode)))))

(defun zorg-find-project-notes ()
  "Pick from all notes in the project."
  (interactive)
  (let* ((dir (zorg--notes-path))
         (files (when (file-directory-p dir)
                  (directory-files-recursively dir "\\.org$"))))
    (if files
        (zorg--choose-note-from files "Project note: ")
      (message "No notes for this project."))))

(defun zorg-find-directory-notes ()
  "Pick from notes in the same directory (non-recursive)."
  (interactive)
  (let* ((dir (file-name-directory (or (zorg--main-file) "")))
         (notes-dir (zorg--notes-path))
         (files (directory-files notes-dir t "\\.org$")))
    (setq files (cl-remove-if-not
                 (lambda (f) (string= (file-name-directory f) notes-dir))
                 files))
    (zorg--choose-note-from files "Directory note: ")))

(defun zorg-find-directory-notes-recursive ()
  "Pick from notes in the same directory and subdirectories."
  (interactive)
  (let* ((dir (file-name-directory (or (zorg--main-file) "")))
         (notes-dir (zorg--notes-path))
         (files (when (file-directory-p notes-dir)
                  (directory-files-recursively notes-dir "\\.org$"))))
    (zorg--choose-note-from files "Directory (recursive) note: ")))

;; -----------------------------
;; Zorg layout
;; -----------------------------

(defcustom zorg-side-window-width-left 40
  "Width of the left Zorg side panel in columns."
  :type 'integer
  :group 'zorg)

(defcustom zorg-side-window-width-right 40
  "Width of the right Zorg side panel in columns."
  :type 'integer
  :group 'zorg)

(defun zorg-save-side-window-widths ()
  "Save Zorg side widths to custom-file for persistence."
  (interactive)
  (when (window-live-p zorg--left-window)
    (customize-save-variable
     'zorg-side-window-width-left
     (window-total-width zorg--left-window)))
  (when (window-live-p zorg--right-window)
    (customize-save-variable
     'zorg-side-window-width-right
     (window-total-width zorg--right-window)))
  (message "Saved Zorg widths: left=%d, right=%d"
           zorg-side-window-width-left
           zorg-side-window-width-right))

(add-hook 'kill-emacs-hook #'zorg-save-side-window-widths)

(defun zorg--setup-layout ()
  "Create the Zorg three-window layout safely."
  (let* ((total (frame-width))
         (left (max 20 zorg-side-window-width-left))
         (right (max 20 zorg-side-window-width-right))
         (main-buf (current-buffer))
         (main (selected-window)))
    (delete-other-windows)
    ;; left panel
    (setq zorg--left-window (split-window main left 'left))
    (with-selected-window zorg--left-window
      (switch-to-buffer (get-buffer-create "*zorg-left*"))
      (when (zorg--in-side-window-p)
  (org-mode)))
    ;; back to main before right split
    (select-window main)
    (setq zorg--right-window (split-window main (- right) 'right))
    (with-selected-window zorg--right-window
      (switch-to-buffer (get-buffer-create "*zorg-right*"))
      (when (zorg--in-side-window-p)
  (org-mode)))

    ;; ---> Tag window roles so we can find them reliably later
    (set-window-parameter zorg--left-window  'zorg-role 'left)
    (set-window-parameter main               'zorg-role 'main)
    (set-window-parameter zorg--right-window 'zorg-role 'right)

    ;; restore main buffer in center
    (select-window main)
    (switch-to-buffer main-buf)

    ;; normalize/save actual widths
    (when (and (window-live-p zorg--left-window)
               (window-live-p zorg--right-window))
      (setq zorg-side-window-width-left  (window-total-width zorg--left-window)
            zorg-side-window-width-right (window-total-width zorg--right-window))
      (customize-save-variable 'zorg-side-window-width-left  zorg-side-window-width-left)
      (customize-save-variable 'zorg-side-window-width-right zorg-side-window-width-right))))

;;;###autoload
(defun zorg-mode ()
  "Toggle Zorg mode: center buffer with side windows."
  (interactive)
  (if zorg--active-p
      ;; --- turn off
      (progn
        (zorg-save-side-window-widths)
        (when zorg--saved-config
          (set-window-configuration zorg--saved-config))
        (setq zorg--saved-config nil
              zorg--active-p nil
              zorg--left-window nil
              zorg--right-window nil)
        (message "Zorg mode off"))
    ;; --- turn on
    (setq zorg--saved-config (current-window-configuration)
          zorg--active-p t)
    (zorg--setup-layout)
    (let ((main-buf (current-buffer)))
      (delete-other-windows)
      ;; left panel
      (setq zorg--left-window (split-window (selected-window)
                                            zorg-side-window-width-left
                                            'left))
      (with-selected-window zorg--left-window
        (switch-to-buffer (get-buffer-create "*zorg-left*"))
        (when (zorg--in-side-window-p)
  (org-mode)))
      ;; right panel
      (let ((center (selected-window)))
        (setq zorg--right-window (split-window center
                                               (- zorg-side-window-width-right)
                                               'right))
        (with-selected-window zorg--right-window
          (switch-to-buffer (get-buffer-create "*zorg-right*"))
          (when (zorg--in-side-window-p)
  (org-mode)))
        ;; restore main buffer in center
        (with-selected-window center
          (switch-to-buffer main-buf))))
    (message "Zorg mode on")))




;;-------------------
;; Zorg Linking Helpers
;;-------------------
(defun zorg-copy-link-to-here ()
  "Copy an Org link to the current location in the main buffer.
The link is placed on the kill ring / clipboard for easy pasting into a note."
  (interactive)
  (unless buffer-file-name
    (user-error "This buffer is not visiting a file"))
  (let* ((file (buffer-file-name))
         (line (line-number-at-pos (point)))
         (link (format "[[file:%s::%d][%s:%d]]"
                       (file-relative-name file (zorg--project-root))
                       line
                       (file-name-nondirectory file)
                       line)))
    (kill-new link)
    (message "Copied link: %s" link)))

(defun zorg-insert-note-link ()
  "Insert an org link to another zorg note."
  (interactive)
  (let* ((dir (zorg--notes-path))
         (files (directory-files-recursively dir "\\.org$"))
         (choice (completing-read "Link to note: " files nil t)))
    (insert (format "[[file:%s][%s]]"
                    (file-relative-name choice (zorg--project-root))
                    (file-name-base choice)))))

(provide 'zorg)

;;; zorg.el ends here

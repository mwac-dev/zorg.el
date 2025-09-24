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
;; - `M-x zorg-pair-note`            : Open/create paired note(s).
;; - `M-x zorg-create-new-pair-note` : Create an additional pair note.
;; - `M-x zorg-dir-note`            : Open/create paired note(s).
;; - `M-x zorg-create-new-dir-note` : Create an additional directory note.
;; - `M-x zorg-note-new`             : Create a loose note.
;; - `M-x zorg-find-project-notes`   : Pick from all notes in project.
;; - `M-x zorg-find-directory-notes` : Pick from notes in current directory.
;; - `M-x zorg-find-directory-notes-recursive` : Pick from notes under current directory.
;; - `M-x zorg-find-pair-notes`       : Pick from pair notes for current file.

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
;; Directory notes
;; -----------------------------

(defun zorg--dir-key (dir)
  "Return a stable key (hash) for DIR relative to project root."
  (md5 (file-relative-name dir (zorg--project-root))))

(defun zorg--make-dir-note (dir title)
  "Create a directory note for DIR with TITLE."
  (let* ((hash (zorg--dir-key dir))
         (real-dir (expand-file-name "dir" (zorg--notes-path)))
         (slug (replace-regexp-in-string "[^a-zA-Z0-9_-]" "_" title))
         (note (expand-file-name (format "%s-%s.org" hash slug) real-dir)))
    (zorg--ensure-dir note)
    (unless (file-exists-p note)
      (with-temp-file note
        (insert (format "# Directory note for %s\n\n" dir))))
    note))

(defun zorg-create-new-dir-note (title)
  "Always create a new directory note for the current buffer’s directory, asking for TITLE."
  (interactive "sDirectory note title: ")
  (let* ((main-file (zorg--main-file)))
    (unless main-file
      (user-error "No file open in the main buffer"))
    (let* ((dir (file-name-directory main-file))
           (note (zorg--make-dir-note dir title)))
      (find-file note)
      (when (zorg--in-side-window-p)
        (org-mode)))))
(defun zorg-dir-note ()
  "Open or create a note scoped to the current buffer’s directory."
  (interactive)
  (let* ((main-file (zorg--main-file)))
    (unless main-file
      (user-error "No file open in the main buffer"))
    (let* ((dir (file-name-directory main-file))
           (hash (zorg--dir-key dir))
           (real-dir (expand-file-name "dir" (zorg--notes-path)))
           (matches (when (file-directory-p real-dir)
                      (directory-files real-dir t (concat "^" hash))))
           (note (cond
                  ((null matches)
                   (zorg--make-dir-note dir (read-string "Title for first directory note: ")))
                  ((= (length matches) 1)
                   (car matches))
                  (t
                   (let* ((choices (mapcar (lambda (f)
                                             (cons (zorg--note-title f) f))
                                           matches))
                          (choice (completing-read "Choose directory note: " (mapcar #'car choices) nil t)))
                     (cdr (assoc choice choices)))))))
      (let ((win (or (zorg--in-side-window-p) (zorg--select-side-window))))
        (with-selected-window (if (windowp win) win (selected-window))
          (find-file note)
          (org-mode))))))

;; -----------------------------
;; Paired notes
;; -----------------------------

(defun zorg--pair-files (rel)
  "Return list of real pair notes for REL (file-relative path)."
  (let* ((real-dir (expand-file-name "paired" (zorg--notes-path)))
         (hash (md5 rel)))
    (when (file-directory-p real-dir)
      (directory-files real-dir t (concat "^" hash)))))


(defun zorg--make-pair-note (rel title)
  "Create a new titled pair note for REL with TITLE.
Returns the path to the created note."
  (let* ((hash (md5 rel))
         (real-dir (expand-file-name "paired" (zorg--notes-path)))
         (slug (replace-regexp-in-string "[^a-zA-Z0-9_-]" "_" title))
         (note (expand-file-name (format "%s-%s.org" hash slug) real-dir)))
    (zorg--ensure-dir note)
    (unless (file-exists-p note)
      (with-temp-file note
        (insert (format "# Paired note for %s\n\n" rel))))
    note))

(defun zorg-create-new-pair-note (title)
  "Create an additional paired note for the current file, asking for TITLE."
  (interactive "sPair note title: ")
  (let ((target-file (zorg--main-file)))
    (unless target-file
      (user-error "No file open in the main buffer"))
    (let* ((rel (file-relative-name target-file (zorg--project-root)))
           (note (zorg--make-pair-note rel title)))
      (find-file note)
      (when (zorg--in-side-window-p)
        (org-mode)))))

(defun zorg--note-title (file)
  "Extract the human-readable title (slug) from a pair FILE name."
  (let ((base (file-name-base file)))
    ;; Drop the leading md5 hash and dash
    (if (string-match "^[0-9a-f]\\{32\\}-\\(.*\\)$" base)
        (match-string 1 base)
      base)))

(defcustom zorg-global-dir (expand-file-name "~/projects/org/zorg/")
  "Directory where global Zorg notes live.

You can override this in your config to choose a different
location. For example:

  (setq zorg-global-dir \"~/Documents/zorg-notes/\")

This directory is independent of project-scoped notes, and is
intended for personal/global notes you want available everywhere."
  :type 'directory
  :group 'zorg)

(defun zorg-find-loose-notes ()
  "Pick from project-loose notes."
  (interactive)
  (let* ((dir (zorg--notes-path))
         (files (when (file-directory-p dir)
                  ;; Only notes in root of .zorg-notes, not paired
                  (directory-files dir t "\\.org$"))))
    (if files
        (zorg--choose-note-from files "Loose note: ")
      (message "No loose notes for this project."))))

(defun zorg-find-global-notes ()
  "Pick from global notes in `zorg-global-dir`."
  (interactive)
  (let* ((dir zorg-global-dir)
         (files (when (file-directory-p dir)
                  (directory-files dir t "\\.org$"))))
    (if files
        (zorg--choose-note-from files "Global note: ")
      (message "No global notes found."))))

(defun zorg-create-global-note (title)
  "Create a global note with TITLE in `zorg-global-dir`."
  (interactive "sGlobal note title: ")
  (let* ((slug (replace-regexp-in-string "[^a-zA-Z0-9_-]" "_" title))
         (note (expand-file-name (concat slug ".org") zorg-global-dir)))
    (zorg--ensure-dir note)
    (find-file note)
    (org-mode)))

(defun zorg-find-pair-notes ()
  "Pick one of the paired notes for the current file, showing only titles."
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
        (let* ((choices (mapcar (lambda (f)
                                  (cons (zorg--note-title f) f))
                                matches))
               (choice (completing-read "Choose pair note: " choices nil t)))
          (find-file (cdr (assoc choice choices)))))))))

(defun zorg-pair-note()
  "Open paired note(s) for the current main buffer file.

- If in a side buffer: reuse that window.
- If in the main buffer: ask which side to use.
- If no notes exist: create the first one with a title prompt."
  (interactive)
  (let* ((side (zorg--in-side-window-p))
         (main-buf (zorg--main-buffer)))
    (unless main-buf
      (user-error "No main window/buffer found"))
    (let ((target-file (with-current-buffer main-buf buffer-file-name)))
      (unless target-file
        (user-error "No file open in the main buffer"))
      (let* ((rel     (file-relative-name target-file (zorg--project-root)))
             (matches (zorg--pair-files rel))
             (note
              (cond
               ;; No pair notes yet → ask title and create
               ((null matches)
                (zorg--make-pair-note
                 rel (read-string "Title for first pair note: ")))
               ;; One existing → open it
               ((= (length matches) 1)
                (car matches))
               ;; Multiple → pick by title only
               (t
                (let* ((choices (mapcar (lambda (f)
                                          (cons (or (zorg--note-title f)
                                                    (file-name-base f))
                                                f))
                                        matches))
                       (title   (completing-read "Choose pair note: "
                                                 (mapcar #'car choices) nil t))
                       (path    (cdr (assoc title choices))))
                  path)))))
        (if side
            (progn
              (find-file note)
              (org-mode))  ;; only the side window gets org-mode
          (let ((win (zorg--select-side-window)))
            (if (window-live-p win)
                (with-selected-window win
                  (find-file note)
                  (org-mode))  ;; only the chosen side window gets org-mode
              (user-error "Side window is not live"))))))))

;; -----------------------------
;; Loose notes
;; -----------------------------

(defun zorg-note-new (title)
  "Create a loose note for this project."
  (interactive "sLoose note title: ")
  (let* ((slug (replace-regexp-in-string "[^a-zA-Z0-9_-]" "_" title))
         ;; used to add a loose- prefix but seems redundant - might revisit if theres any good reason
         (note (expand-file-name (concat slug ".org")
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
  "Pick from notes in the same directory as the main buffer file (non-recursive)."
  (interactive)
  (let* ((main-file (zorg--main-file)))
    (unless main-file
      (user-error "No file open in the main buffer"))
    (let* ((dir (file-name-directory main-file))
           (notes-dir (expand-file-name zorg-notes-dir (zorg--project-root)))
           (files (when (file-directory-p notes-dir)
                    (directory-files notes-dir t "\\.org$"))))
      (setq files (cl-remove-if-not
                   (lambda (f)
                     (string= (file-name-directory f)
                              (file-name-as-directory notes-dir)))
                   files))
      (if files
          (let ((choice (completing-read "Directory note: " files nil t)))
            (cond
             ((zorg--in-side-window-p)
              (find-file choice)
              (org-mode))
             (t
              (let ((win (zorg--select-side-window)))
                (with-selected-window win
                  (find-file choice)
                  (org-mode))))))
        (message "No notes found for this directory.")))))

(defun zorg-find-directory-notes-recursive ()
  "Pick from notes in the same directory and subdirectories as the main buffer file."
  (interactive)
  (let* ((main-file (zorg--main-file)))
    (unless main-file
      (user-error "No file open in the main buffer"))
    (let* ((dir (file-name-directory main-file))
           (notes-dir (expand-file-name zorg-notes-dir (zorg--project-root)))
           (files (when (file-directory-p notes-dir)
                    (directory-files-recursively notes-dir "\\.org$"))))
      (setq files (cl-remove-if-not
                   (lambda (f)
                     (string-prefix-p dir (file-name-directory f)))
                   files))
      (if files
          (let ((choice (completing-read "Directory (recursive) note: " files nil t)))
            (cond
             ((zorg--in-side-window-p)
              (find-file choice)
              (org-mode))
             (t
              (let ((win (zorg--select-side-window)))
                (with-selected-window win
                  (find-file choice)
                  (org-mode))))))
        (message "No notes found for this directory (recursive).")))))

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

(defun zorg--open-in-main-or-side (orig-fun file &optional arg)
  "Open links differently depending on scheme.
- `zorgmain:` links → always open in the main window, recentered at top.
- `file:` links → open normally (in the side window)."
  (if (string-prefix-p "zorgmain:" file)
      (let ((real-file (string-remove-prefix "zorgmain:" file)))
        (let ((win (zorg--main-window)))
          (if (window-live-p win)
              (with-selected-window win
                (funcall orig-fun real-file arg)
                ;; scroll target line to top
                (if (bound-and-true-p evil-mode)
                    (evil-scroll-line-to-top (line-number-at-pos))
                  (recenter 0)))
            (funcall orig-fun real-file arg))))
    ;; normal file link
    (funcall orig-fun file arg)))

(with-eval-after-load 'org
  (org-link-set-parameters
   "zorgmain"
   :follow (lambda (path)
             (let* ((parts (split-string path "::"))
                    (file  (car parts))
                    (line  (when (cadr parts) (string-to-number (cadr parts))))
                    (win   (zorg--main-window)))
               (if (window-live-p win)
                   (with-selected-window win
                     (find-file file)
                     (when line
                       (goto-char (point-min))
                       (forward-line (1- line))
                       (if (bound-and-true-p evil-mode)
                           ;; scroll *this* line to top
                           (evil-scroll-line-to-top (line-number-at-pos))
                         (recenter 0))))
                 (find-file file))))))


(defun zorg-copy-link-to-here ()
  "Copy an Org link to the current location in the main buffer.
If called from a side buffer, the link still targets the main
buffer’s location, but it is inserted at point in the side buffer
and also copied to the kill ring."
  (interactive)
  (let* ((side-buf (current-buffer)) ;; remember where you are
         (main-buf (zorg--main-buffer)))
    (unless main-buf
      (user-error "No main buffer found"))
    (with-current-buffer main-buf
      (unless buffer-file-name
        (user-error "Main buffer is not visiting a file"))
      (let* ((file (expand-file-name buffer-file-name))
             (line (line-number-at-pos (point)))
             (label (format "%s:%d" (file-name-nondirectory file) line))
             ;; add a marker so we know this is a “main-buffer jump”
             (link  (format "[[zorgmain:%s::%d][%s]]" file line label)))
        (kill-new link)
        (when (zorg--in-side-window-p)
          (with-current-buffer side-buf
            (insert link)))
        (message "Copied link: %s" link)))))


(defun zorg-insert-note-link ()
  "Insert an Org link to another Zorg note in the current project.
Skips linking to the current note itself. Inserts at point in the
calling buffer (side window)."
  (interactive)
  (let* ((main-buf (zorg--main-buffer)))
    (unless main-buf
      (user-error "No main buffer found to determine project"))
    (let* ((proj-root (with-current-buffer main-buf (zorg--project-root)))
           (notes-dir (expand-file-name zorg-notes-dir proj-root))
           (this-file (buffer-file-name (current-buffer)))
           (files (when (file-directory-p notes-dir)
                    (directory-files-recursively notes-dir "\\.org$"))))
      (if (not files)
          (user-error "No Zorg notes found in this project")
        ;; filter out current note
        (setq files (cl-remove-if (lambda (f)
                                    (and this-file (file-equal-p f this-file)))
                                  files))
        (let* ((choices (mapcar (lambda (f)
                                  (cons (zorg--note-title f)
                                        (expand-file-name f )))
                                files))
               (choice (completing-read "Link to note: " (mapcar #'car choices) nil t))
               (path (cdr (assoc choice choices))))
          (insert (format "[[file:%s][%s]]" path choice)))))))

(provide 'zorg)

;;; zorg.el ends here

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

;; Width state management
(defvar zorg--desired-left-width nil
  "The desired width for the left panel. When nil, uses `zorg-width-left`.")
(defvar zorg--desired-right-width nil
  "The desired width for the right panel. When nil, uses `zorg-width-right`.")
(defvar zorg--saved-left-width nil
  "Last non-zero width for the left panel, used when toggling back on.")
(defvar zorg--saved-right-width nil
  "Last non-zero width for the right panel, used when toggling back on.")

(defun zorg--get-current-left-width ()
  "Get the current effective left width (0 if disabled)."
  (or zorg--desired-left-width zorg-width-left))

(defun zorg--get-current-right-width ()
  "Get the current effective right width (0 if disabled)."
  (or zorg--desired-right-width zorg-width-right))

(defun zorg--set-left-width (width)
  "Set the desired left width, updating both desired and customization variables."
  (setq zorg--desired-left-width width
        zorg-width-left width))

(defun zorg--set-right-width (width)
  "Set the desired right width, updating both desired and customization variables."
  (setq zorg--desired-right-width width
        zorg-width-right width))

(defun zorg--initialize-saved-widths ()
  "Initialize saved width variables if they haven't been set yet."
  (unless zorg--saved-left-width
    (setq zorg--saved-left-width (if (> zorg-width-left 0) zorg-width-left 40)))
  (unless zorg--saved-right-width
    (setq zorg--saved-right-width (if (> zorg-width-right 0) zorg-width-right 40)))
  ;; Ensure saved widths are reasonable (between 20 and 100)
  (when (or (< zorg--saved-left-width 20) (> zorg--saved-left-width 100))
    (setq zorg--saved-left-width 40))
  (when (or (< zorg--saved-right-width 20) (> zorg--saved-right-width 100))
    (setq zorg--saved-right-width 40)))

(defun zorg-debug-widths ()
  "Show current width state for debugging."
  (interactive)
  (message "=== Zorg Width Debug ===")
  (message "Active: %s" zorg--active-p)
  (message "Customization vars: L=%d M=%d R=%d" zorg-width-left zorg-width-main zorg-width-right)
  (message "Desired widths: L=%s R=%s" zorg--desired-left-width zorg--desired-right-width)
  (message "Saved widths: L=%s R=%s" zorg--saved-left-width zorg--saved-right-width)
  (message "Current effective: L=%d R=%d" (zorg--get-current-left-width) (zorg--get-current-right-width))
  (when zorg--active-p
    (message "Actual window widths: L=%s R=%s"
             (if (window-live-p zorg--left-window) 
                 (window-total-width zorg--left-window) "none")
             (if (window-live-p zorg--right-window) 
                 (window-total-width zorg--right-window) "none"))))

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
      (zorg--open-note-in-side note))))

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
      (zorg--open-note-in-side note))))

;; -----------------------------
;; Paired notes
;; -----------------------------

(defun zorg--pair-files (rel)
  "Return list of pair notes for REL, following alias indirections."
  (let* ((real-dir (expand-file-name "paired" (zorg--notes-path)))
         (hash (md5 rel))
         (base (expand-file-name hash real-dir)))
    (when (file-directory-p real-dir)
      (let ((matches (directory-files real-dir t (concat "^" hash))))
        (if matches
            ;; If it’s an alias, resolve to stored path
            (mapcan (lambda (f)
                      (if (string-suffix-p ".alias" f)
                          (let ((target (string-trim (with-temp-buffer
                                                       (insert-file-contents f)
                                                       (buffer-string)))))
                            (when (file-exists-p target)
                              (list target)))
                        (list f)))
                    matches)
          nil)))))


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
      (zorg--open-note-in-side note))))

(defun zorg-set-pair-to-existing-note ()
  "Associate the current main buffer file with an existing pair note.

Prompts for an existing note under `.zorg-notes/paired/` and records
an alias hash file so that this file will now resolve to the chosen note.
Multiple files can point to the same note."
  (interactive)
  (let ((target-file (zorg--main-file)))
    (unless target-file
      (user-error "No file open in the main buffer"))
    (let* ((real-dir (expand-file-name "paired" (zorg--notes-path)))
           (all-notes (when (file-directory-p real-dir)
                        (directory-files real-dir t "\\.org$"))))
      (unless all-notes
        (user-error "No existing pair notes to choose from"))
      (let* ((choices (mapcar (lambda (f)
                                (cons (zorg--note-title f) f))
                              all-notes))
             (choice (completing-read "Associate with note: "
                                      (mapcar #'car choices) nil t))
             (note (cdr (assoc choice choices)))
             ;; compute alias name using this file’s hash
             (rel (file-relative-name target-file (zorg--project-root)))
             (alias (expand-file-name
                     (format "%s.alias"
                             (md5 rel))
                     real-dir)))
        ;; Write the path of the real note into the alias file
        (with-temp-file alias
          (insert note))
        (message "Associated %s with %s" rel note)))))


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

(defun zorg-create-new-global-note (title)
  "Create a global note with TITLE in `zorg-global-dir`."
  (interactive "sGlobal note title: ")
  (let* ((slug (replace-regexp-in-string "[^a-zA-Z0-9_-]" "_" title))
         (note (expand-file-name (concat slug ".org") zorg-global-dir)))
    (make-directory zorg-global-dir t) ;; ensure global dir exists
    (unless (file-exists-p note)
      (with-temp-file note
        (insert (format "# Global note: %s\n\n" title))))
    (zorg--open-note-in-side note)))

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
        (message "No pair notes yet for this source file, use `zorg-create-new-pair-note`."))
       ((= (length matches) 1)
        (zorg--open-note-in-side (car matches)))
       (t
        (let* ((choices (mapcar (lambda (f)
                                  (cons (zorg--note-title f) f))
                                matches))
               (choice (completing-read "Choose pair note: " (mapcar #'car choices) nil t)))
          (zorg--open-note-in-side (cdr (assoc choice choices)))))))))

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
         (note (expand-file-name (concat slug ".org")
                                 (zorg--notes-path))))
    (zorg--ensure-dir note)
    (zorg--open-note-in-side note)))
;; -----------------------------
;; Views
;; -----------------------------


(defun zorg--open-note-in-side (file)
  "Open FILE in the appropriate side window.
If called from a side window, reuse it. Otherwise prompt left/right."
  (let ((side (zorg--in-side-window-p)))
    (cond
     ;; Already in a side window → just open here
     (side
      (find-file file)
      (org-mode))
     ;; From main window → prompt for side
     (t
      (let ((win (zorg--select-side-window)))
        (if (window-live-p win)
            (with-selected-window win
              (find-file file)
              (org-mode))
          (user-error "Side window is not live")))))))

(defun zorg--choose-note-from (files prompt)
  (when files
    (let ((choice (completing-read prompt files nil t)))
      (zorg--open-note-in-side choice))))

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
            (zorg--open-note-in-side choice))
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
            (zorg--open-note-in-side choice))
        (message "No notes found for this directory (recursive).")))))

;; -----------------------------
;; Zorg layout
;; -----------------------------



(defcustom zorg-width-left 40
  "Width in columns of the left Zorg panel. 0 means disabled."
  :type 'integer
  :group 'zorg)

(defcustom zorg-width-main 80
  "Width in columns of the main Zorg buffer."
  :type 'integer
  :group 'zorg)

(defcustom zorg-width-right 40
  "Width in columns of the right Zorg panel. 0 means disabled."
  :type 'integer
  :group 'zorg)
(defun zorg-reset-custom-layout ()
  "Reset Zorg windows to my preferred widths."
  (interactive)
  ;; overwrite the saved + current widths
  (zorg--set-left-width 44)
  (zorg--set-right-width 40)
  (setq zorg-width-main 199
        zorg--saved-left-width 44
        zorg--saved-right-width 40)
  ;; if active, rebuild right away
  (if zorg--active-p
      (zorg--setup-layout)
    (message "Zorg custom widths set for next activation: L=44, M=199, R=40")))


(defun zorg--capture-widths ()
  "Save real current widths into width variables."
  (when zorg--active-p
    (let ((total (frame-width))
          (left-width (if (window-live-p zorg--left-window)
                          (window-total-width zorg--left-window) 0))
          (right-width (if (window-live-p zorg--right-window)
                           (window-total-width zorg--right-window) 0)))
      (zorg--set-left-width left-width)
      (zorg--set-right-width right-width)
      (setq zorg-width-main (- total left-width right-width)))))


(defun zorg-toggle-left ()
  "Toggle the left Zorg panel between hidden (0) and its saved width.
When turning off, shrink only the left window so the main window expands.
When turning on, restore the left width (default 40 if none saved)."
  (interactive)
  (zorg--initialize-saved-widths)
  (let ((current-width (zorg--get-current-left-width)))
    (if (zerop current-width)
        ;; Turn ON: restore from saved width
        (zorg--set-left-width (or zorg--saved-left-width 40))
      ;; Turn OFF: save current width and set to 0
      (setq zorg--saved-left-width current-width)
      (zorg--set-left-width 0)))
  (when zorg--active-p
    (zorg--setup-layout)
    ;; Force right panel to maintain its size
    (when (window-live-p zorg--right-window)
      (let ((target-right-width (zorg--get-current-right-width)))
        (window-resize zorg--right-window
                       (- target-right-width (window-total-width zorg--right-window))
                       t)))))

(defun zorg-toggle-right ()
  "Toggle the right Zorg panel between hidden (0) and its saved width.
When turning off, shrink only the right window so the main window expands.
When turning on, restore the right width (default 40 if none saved)."
  (interactive)
  (zorg--initialize-saved-widths)
  (let ((current-width (zorg--get-current-right-width)))
    (if (zerop current-width)
        ;; Turn ON: restore from saved width
        (zorg--set-right-width (or zorg--saved-right-width 40))
      ;; Turn OFF: save current width and set to 0
      (setq zorg--saved-right-width current-width)
      (zorg--set-right-width 0)))
  (when zorg--active-p
    (zorg--setup-layout)
    ;; Force left panel to maintain its size
    (when (window-live-p zorg--left-window)
      (let ((target-left-width (zorg--get-current-left-width)))
        (window-resize zorg--left-window
                       (- target-left-width (window-total-width zorg--left-window))
                       t)))))

(defun zorg-save-widths ()
  (interactive)
  (zorg--capture-widths)
  (customize-save-variable 'zorg-width-left zorg-width-left)
  (customize-save-variable 'zorg-width-main zorg-width-main)
  (customize-save-variable 'zorg-width-right zorg-width-right)
  (message "Saved Zorg widths: L=%d, M=%d, R=%d"
           zorg-width-left zorg-width-main zorg-width-right))

(add-hook 'kill-emacs-hook #'zorg-save-widths)

(defun zorg--setup-layout ()
  "Recreate Zorg layout with exact widths from current settings."
  (let ((main-buf (current-buffer))
        (left-width (zorg--get-current-left-width))
        (right-width (zorg--get-current-right-width)))
    (delete-other-windows)
    (let ((center (selected-window)))
      (set-window-parameter center 'zorg-role 'main)

      ;; Always split LEFT first
      (when (> left-width 0)
        (setq zorg--left-window (split-window center left-width 'left))
        (with-selected-window zorg--left-window
          (switch-to-buffer (get-buffer-create "*zorg-left*"))
          (org-mode))
        (set-window-parameter zorg--left-window 'zorg-role 'left))

      ;; Then split RIGHT from the (new) center
      (when (> right-width 0)
        (setq zorg--right-window (split-window center right-width 'right))
        (with-selected-window zorg--right-window
          (switch-to-buffer (get-buffer-create "*zorg-right*"))
          (org-mode))
        (set-window-parameter zorg--right-window 'zorg-role 'right))

      ;; Restore main buffer
      (with-selected-window center
        (switch-to-buffer main-buf))

      ;; Now force exact widths
      (when (window-live-p zorg--left-window)
        (window-resize zorg--left-window
                       (- left-width (window-total-width zorg--left-window))
                       t))
      (when (window-live-p zorg--right-window)
        (window-resize zorg--right-window
                       (- right-width (window-total-width zorg--right-window))
                       t))
      ;; center auto-adjusts
      (select-window center))))


;;;###autoload
(defun zorg-mode ()
  "Toggle Zorg mode: center buffer with side panels."
  (interactive)
  (if zorg--active-p
      ;; --- turn off
      (progn
        ;; Capture current widths before turning off
        (zorg--capture-widths)
        (let ((main-buf (zorg--main-buffer)))
          (delete-other-windows)
          (when (buffer-live-p main-buf)
            (switch-to-buffer main-buf)))
        (setq zorg--saved-config nil
              zorg--active-p nil
              zorg--left-window nil
              zorg--right-window nil)
        (message "Zorg mode off"))
    ;; --- turn on
    (progn
      ;; Initialize saved widths if needed
      (zorg--initialize-saved-widths)
      ;; Reset desired widths to match customization variables if they're unset
      (unless zorg--desired-left-width
        (setq zorg--desired-left-width zorg-width-left))
      (unless zorg--desired-right-width
        (setq zorg--desired-right-width zorg-width-right))
      (setq zorg--saved-config (current-window-configuration)
            zorg--active-p t)
      (zorg--setup-layout)
      (message "Zorg mode on"))))


(defun zorg--maybe-restore-layout (&rest _)
  "If Zorg mode is active, restore its layout after project/workspace switch."
  (when zorg--active-p
    (zorg--setup-layout)))
;; Adapter for hooks that pass args we don't care about.
(defun zorg--save-widths-hook (&rest _)
  (when zorg--active-p
    (zorg-save-widths)))

;;saving and then restoring on project switching
;;might add project-specific layouts but for now its global
;; Doom workspaces (persp-mode)
(with-eval-after-load 'persp-mode
  (add-hook 'persp-before-switch-functions #'zorg--save-widths-hook)
  (add-hook 'persp-activated-functions      #'zorg--maybe-restore-layout))

;; Projectile project switching
(with-eval-after-load 'projectile
  (add-hook 'projectile-before-switch-project-hook #'zorg--save-widths-hook)
  (add-hook 'projectile-after-switch-project-hook  #'zorg--maybe-restore-layout))

;; Vanilla Emacs tab-bar
(with-eval-after-load 'tab-bar
  (add-hook 'tab-bar-tab-pre-close-functions #'zorg--save-widths-hook)
  (add-hook 'tab-bar-switch-hook #'zorg--save-widths-hook)
  (add-hook 'tab-bar-switch-hook             #'zorg--maybe-restore-layout))


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

;;-------------------
;; AI Tools Integration
;;-------------------

(defvar zorg--ai-state nil
  "Current AI tool state: nil, 'gptel, or 'copilot.")

(defvar zorg--right-saved-buffer nil
  "Buffer that was in the right window before AI tools.")

(defvar zorg--right-was-closed nil
  "Whether the right panel was closed (width 0) before AI activation.")

(defun zorg--save-right-state ()
  "Save the current state of the right panel before AI activation."
  (setq zorg--right-was-closed (zerop (zorg--get-current-right-width)))
  (when (and (not zorg--right-was-closed) 
             (window-live-p zorg--right-window))
    (setq zorg--right-saved-buffer (window-buffer zorg--right-window))))

(defun zorg--restore-right-state ()
  "Restore the right panel to its pre-AI state."
  (cond
   ;; If right panel was closed, close it again
   (zorg--right-was-closed
    (let ((current-width (zorg--get-current-right-width)))
      (when (> current-width 0)
        (setq zorg--saved-right-width current-width))
      (zorg--set-right-width 0))
    (when zorg--active-p
      (zorg--setup-layout)
      ;; Force left panel to keep its size
      (when (window-live-p zorg--left-window)
        (let ((target-left-width (zorg--get-current-left-width)))
          (window-resize zorg--left-window
                         (- target-left-width (window-total-width zorg--left-window))
                         t)))))
   ;; If there was a saved buffer, restore it
   ((and zorg--right-saved-buffer 
         (buffer-live-p zorg--right-saved-buffer)
         (window-live-p zorg--right-window))
    (with-selected-window zorg--right-window
      (switch-to-buffer zorg--right-saved-buffer)))
   ;; Otherwise, show the default right buffer
   ((window-live-p zorg--right-window)
    (with-selected-window zorg--right-window
      (switch-to-buffer (get-buffer-create "*zorg-right*"))
      (org-mode))))
  (setq zorg--ai-state nil
        zorg--right-saved-buffer nil
        zorg--right-was-closed nil))

(defun zorg--ensure-right-panel ()
  "Ensure the right panel is open, opening at default width if closed."
  (when (zerop (zorg--get-current-right-width))
    (zorg--initialize-saved-widths)
    (zorg--set-right-width (or zorg--saved-right-width 40))
    (when zorg--active-p
      (zorg--setup-layout))))

(defun zorg-ai-gptel ()
  "Toggle gptel in the right buffer, preserving panel state."
  (interactive)
  (unless zorg--active-p
    (user-error "Zorg mode is not active"))

  (cond
   ;; If gptel is already active, restore previous state
   ((eq zorg--ai-state 'gptel)
    (zorg--restore-right-state))

   ;; If another AI tool is active, switch to gptel
   ((eq zorg--ai-state 'copilot)
    (zorg--ensure-right-panel)
    (when (window-live-p zorg--right-window)
      (with-selected-window zorg--right-window
        (switch-to-buffer (gptel "*zorg-gptel*")))) ;; <--- switch explicitly
    (setq zorg--ai-state 'gptel))

   ;; If no AI tool is active, activate gptel
   (t
    (zorg--save-right-state)
    (zorg--ensure-right-panel)
    (when (window-live-p zorg--right-window)
      (with-selected-window zorg--right-window
        (switch-to-buffer (gptel "*zorg-gptel*")))) ;; <--- switch explicitly
    (setq zorg--ai-state 'gptel))))

(defun zorg-ai-copilot ()
  "Toggle copilot CLI in the right buffer via vterm, preserving panel state."
  (interactive)
  (unless zorg--active-p
    (user-error "Zorg mode is not active"))
  
  (cond
   ;; If copilot is already active, restore previous state
   ((eq zorg--ai-state 'copilot)
    (zorg--restore-right-state))
   
   ;; If another AI tool is active, switch to copilot
   ((eq zorg--ai-state 'gptel)
    (zorg--ensure-right-panel)
    (when (window-live-p zorg--right-window)
      (with-selected-window zorg--right-window
        ;; Just switch to existing copilot buffer or create new one without running commands
        (if (get-buffer "*zorg-copilot*")
            (switch-to-buffer "*zorg-copilot*")
          (progn
            (vterm "*zorg-copilot*")
            ;; Only run copilot if this is a new buffer
            (let ((project-root (zorg--project-root)))
              (vterm-send-string (format "cd %s" (shell-quote-argument project-root)))
              (vterm-send-return)
              (vterm-send-string "copilot")
              (vterm-send-return))))))
    (setq zorg--ai-state 'copilot))
   
   ;; If no AI tool is active, activate copilot
   (t
    (zorg--save-right-state)
    (zorg--ensure-right-panel)
    (when (window-live-p zorg--right-window)
      (with-selected-window zorg--right-window
        ;; Check if copilot buffer already exists and has copilot running
        (if (and (get-buffer "*zorg-copilot*")
                 (with-current-buffer "*zorg-copilot*"
                   ;; Check if vterm process is alive and buffer has content
                   (and (get-buffer-process (current-buffer))
                        (> (buffer-size) 0))))
            ;; Just switch to existing buffer - don't run commands
            (switch-to-buffer "*zorg-copilot*")
          ;; Create new buffer and run copilot
          (progn
            (vterm "*zorg-copilot*")
            (let ((project-root (zorg--project-root)))
              (vterm-send-string (format "cd %s" (shell-quote-argument project-root)))
              (vterm-send-return)
              (vterm-send-string "copilot")
              (vterm-send-return))))))
    (setq zorg--ai-state 'copilot))))

(provide 'zorg)

;;; zorg.el ends here

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
;; - `M-x zorg-ai-gptel`              : Toggle gptel in the AI side panel.
;; - `M-x zorg-ai-agent`              : Toggle AI agent CLI in the AI side panel.
;; - `M-x zorg-set-ai-cli-command`    : Set the AI CLI command interactively.
;;
;; Configuration:
;; - `zorg-ai-side`: Set to 'left or 'right to choose which side panel AI tools use.
;; - `zorg-ai-cli-command`: Command to run for the AI CLI (default: "copilot").
;; - `zorg-ai-cli-restart-if-not-running`: Restart CLI if process is not running (default: t).
;; - `zorg-left-width-fraction`: Width of left panel as fraction (0.0-1.0).
;; - `zorg-right-width-fraction`: Width of right panel as fraction (0.0-1.0).

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

(defvar zorg--saved-config nil
  "Saved window configuration before Zorg mode was activated.")

(defvar zorg--active-p nil
  "Non-nil when Zorg mode is currently active.")

(defvar zorg--left-window nil
  "Reference to the left side window when Zorg mode is active.")

(defvar zorg--right-window nil
  "Reference to the right side window when Zorg mode is active.")

;; Side panel configuration using percentages for side window system
(defcustom zorg-left-width-fraction 0.25
  "Width of the left Zorg panel as a fraction of frame width (0.0 to 1.0).
Set to 0.0 to disable the left panel."
  :type 'float
  :group 'zorg)

(defcustom zorg-right-width-fraction 0.25
  "Width of the right Zorg panel as a fraction of frame width (0.0 to 1.0).
Set to 0.0 to disable the right panel."
  :type 'float
  :group 'zorg)

;; Internal state for side panels
(defvar zorg--left-enabled-p nil
  "Whether the left panel is currently enabled.")
(defvar zorg--right-enabled-p nil
  "Whether the right panel is currently enabled.")
(defvar zorg--saved-left-fraction nil
  "Last non-zero fraction for the left panel, used when toggling back on.")
(defvar zorg--saved-right-fraction nil
  "Last non-zero fraction for the right panel, used when toggling back on.")

;; AI tools configuration
(defcustom zorg-ai-side 'right
  "Which side panel to use for AI tools ('left or 'right)."
  :type '(choice (const :tag "Left side" left)
                 (const :tag "Right side" right))
  :group 'zorg)

(defcustom zorg-ai-cli-command "copilot"
  "Command to run for the AI CLI tool.
Can be any command like 'copilot', 'aider', 'cursor', etc."
  :type 'string
  :group 'zorg)

(defcustom zorg-ai-cli-restart-if-not-running t
  "Whether to restart the AI CLI if the process is not running.
When t, automatically detects if the AI CLI process has exited and restarts it.
When nil, simply switches to the existing buffer without checking process status."
  :type 'boolean
  :group 'zorg)

(defun zorg--initialize-saved-fractions ()
  "Initialize saved fraction variables if they haven't been set yet.
Ensures that saved fractions have reasonable default values between 0.1 and 0.5."
  (unless zorg--saved-left-fraction
    (setq zorg--saved-left-fraction (if (> zorg-left-width-fraction 0.0) 
                                        zorg-left-width-fraction 
                                      0.25)))
  (unless zorg--saved-right-fraction
    (setq zorg--saved-right-fraction (if (> zorg-right-width-fraction 0.0) 
                                         zorg-right-width-fraction 
                                       0.25)))
  ;; Ensure saved fractions are reasonable (between 0.1 and 0.5)
  (when (or (< zorg--saved-left-fraction 0.1) (> zorg--saved-left-fraction 0.5))
    (setq zorg--saved-left-fraction 0.25))
  (when (or (< zorg--saved-right-fraction 0.1) (> zorg--saved-right-fraction 0.5))
    (setq zorg--saved-right-fraction 0.25)))

(defun zorg--get-current-left-fraction ()
  "Get the current effective left fraction (0.0 if disabled)."
  (if zorg--left-enabled-p zorg-left-width-fraction 0.0))

(defun zorg--get-current-right-fraction ()
  "Get the current effective right fraction (0.0 if disabled)."
  (if zorg--right-enabled-p zorg-right-width-fraction 0.0))

(defun zorg--set-left-fraction (fraction)
  "Set the left width fraction and update customization variable.
FRACTION should be a float between 0.0 and 1.0."
  (setq zorg-left-width-fraction fraction)
  (setq zorg--left-enabled-p (> fraction 0.0)))

(defun zorg--set-right-fraction (fraction)
  "Set the right width fraction and update customization variable.
FRACTION should be a float between 0.0 and 1.0."
  (setq zorg-right-width-fraction fraction)
  (setq zorg--right-enabled-p (> fraction 0.0)))

(defun zorg-debug-widths ()
  "Show current width state for debugging."
  (interactive)
  (message "=== Zorg Width Debug ===")
  (message "Active: %s" zorg--active-p)
  (message "Frame width: %d" (frame-width))
  (message "Left panel: enabled=%s fraction=%.2f (%.0f cols)" 
           zorg--left-enabled-p zorg-left-width-fraction 
           (* (frame-width) zorg-left-width-fraction))
  (message "Right panel: enabled=%s fraction=%.2f (%.0f cols)" 
           zorg--right-enabled-p zorg-right-width-fraction
           (* (frame-width) zorg-right-width-fraction))
  (message "Saved fractions: L=%.2f R=%.2f" 
           (or zorg--saved-left-fraction 0.0) 
           (or zorg--saved-right-fraction 0.0))
  (message "AI side: %s, AI state: %s" zorg-ai-side zorg--ai-state)
  (when zorg--active-p
    (message "Actual window widths: L=%s R=%s M=%s"
             (if (window-live-p zorg--left-window) 
                 (window-total-width zorg--left-window) "none")
             (if (window-live-p zorg--right-window) 
                 (window-total-width zorg--right-window) "none")
             (if (zorg--main-window)
                 (window-total-width (zorg--main-window)) "none"))))

(defun zorg--project-root ()
  "Return the root directory of the current project.
Falls back to `default-directory' if no project is detected."
  (if-let ((proj (project-current)))
      (expand-file-name (project-root proj))
    default-directory))

(defun zorg--notes-path ()
  "Return the full path to the Zorg notes directory for the current project."
  (expand-file-name zorg-notes-dir (zorg--project-root)))

(defun zorg--ensure-dir (path)
  "Ensure that the directory containing PATH exists, creating it if necessary."
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
  "Return the file path of the file open in the main window.
Returns nil if the main buffer is not visiting a file."
  (with-current-buffer (zorg--main-buffer)
    buffer-file-name))

(defun zorg--in-side-window-p ()
  "Check if the current window is a Zorg side window.
Returns 'left if in the left side window, 'right if in the right side window,
or nil if in the main window or Zorg mode is not active."
  (cond
   ((and zorg--left-window (eq (selected-window) zorg--left-window)) 'left)
   ((and zorg--right-window (eq (selected-window) zorg--right-window)) 'right)
   (t nil)))

(defun zorg--select-side-window ()
  "Prompt the user to select which side window to use and return it.
Returns the left or right window object based on user choice."
  (let* ((choice (completing-read "Open in side: " '("left" "right") nil t)))
    (if (string= choice "left")
        zorg--left-window
      zorg--right-window)))

;; -----------------------------
;; Directory notes
;;
;; Directory notes are scoped to a specific directory and use a hash-based
;; naming system to ensure they persist across directory renames. Multiple
;; notes can be associated with the same directory.
;; -----------------------------

(defun zorg--dir-key (dir)
  "Return a stable key (hash) for DIR relative to project root."
  (md5 (file-relative-name dir (zorg--project-root))))

(defun zorg--make-dir-note (dir title)
  "Create a directory note for DIR with TITLE.
Returns the path to the created note file. The note file is named using
a hash of the directory path plus the provided title slug."
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
;;
;; Paired notes are associated with specific files using a hash-based system.
;; They support alias files to maintain associations even when files are moved
;; or renamed. Multiple notes can be paired with the same file.
;; -----------------------------

(defun zorg--pair-files (rel)
  "Return list of pair notes for REL, following alias indirections.
REL should be a relative file path from the project root.
Resolves any .alias files to their target notes and returns actual note files."
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
  "Extract the human-readable title (slug) from a pair FILE name.
Removes the MD5 hash prefix to get the meaningful part of the filename."
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
;;
;; Loose notes are project-scoped but not tied to any specific file or directory.
;; They provide a general scratchpad space within the project context.
;; -----------------------------

(defun zorg-note-new (title)
  "Create a loose note for this project.
TITLE is the human-readable title which gets converted to a filename-safe slug.
The note is created in the project's `.zorg-notes` directory."
  (interactive "sLoose note title: ")
  (let* ((slug (replace-regexp-in-string "[^a-zA-Z0-9_-]" "_" title))
         (note (expand-file-name (concat slug ".org")
                                 (zorg--notes-path))))
    (zorg--ensure-dir note)
    (zorg--open-note-in-side note)))
;; -----------------------------
;; Views and note browsing
;;
;; These functions handle opening notes in the appropriate side windows
;; and provide various ways to browse and select existing notes.
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
  "Present a completion interface to choose from FILES using PROMPT.
Opens the selected note in the appropriate side window."
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
;; Zorg layout management
;;
;; These functions handle the creation, configuration, and management of
;; the Zorg window layout with its characteristic left-main-right structure.
;; Uses Emacs' side window system with percentage-based sizing.
;; -----------------------------



(defun zorg-reset-layout ()
  "Reset Zorg panels to default fractions.
Sets both left and right panels to 25% width and enables them.
If Zorg mode is currently active, immediately applies the new layout."
  (interactive)
  ;; Reset to default fractions
  (zorg--set-left-fraction 0.25)
  (zorg--set-right-fraction 0.25)
  (setq zorg--saved-left-fraction 0.25
        zorg--saved-right-fraction 0.25
        zorg--left-enabled-p t
        zorg--right-enabled-p t)
  ;; if active, rebuild right away
  (if zorg--active-p
      (zorg--setup-layout)
    (message "Zorg reset to default layout: L=25%% R=25%%")))


(defun zorg--capture-fractions ()
  "Save real current fractions from actual window widths.
Updates the customization variables to reflect the current window layout
so they can be persisted or restored later."
  (when zorg--active-p
    (let ((total (float (frame-width))))
      (when (window-live-p zorg--left-window)
        (let ((left-fraction (/ (window-total-width zorg--left-window) total)))
          (setq zorg-left-width-fraction left-fraction)
          (setq zorg--saved-left-fraction left-fraction)))
      (when (window-live-p zorg--right-window)
        (let ((right-fraction (/ (window-total-width zorg--right-window) total)))
          (setq zorg-right-width-fraction right-fraction)
          (setq zorg--saved-right-fraction right-fraction))))))

(defun zorg--clean-old-custom-variables ()
  "Remove old column-based custom variables if they exist.
This function helps migrate from older versions of Zorg that used
column-based width settings instead of fraction-based ones."
  (when (boundp 'zorg-width-left)
    (makunbound 'zorg-width-left))
  (when (boundp 'zorg-width-main)
    (makunbound 'zorg-width-main))
  (when (boundp 'zorg-width-right)
    (makunbound 'zorg-width-right))
  ;; Remove from custom-set-variables if they exist
  (when (and (boundp 'custom-file) custom-file (file-exists-p custom-file))
    (with-temp-buffer
      (insert-file-contents custom-file)
      (when (re-search-forward "'(zorg-width-[^)]+)" nil t)
        (message "Found old zorg-width variables in custom.el - please remove them manually")))))

(defun zorg--cleanup-side-windows ()
  "Clean up zorg side windows properly.
Deletes both side windows and kills their associated buffers to ensure
a clean state when turning off Zorg mode."
  (when (window-live-p zorg--left-window)
    (delete-window zorg--left-window)
    (setq zorg--left-window nil))
  (when (window-live-p zorg--right-window)
    (delete-window zorg--right-window)
    (setq zorg--right-window nil))
  ;; Also clean up any side window buffers that might be lingering
  (let ((left-buf (get-buffer "*zorg-left*"))
        (right-buf (get-buffer "*zorg-right*")))
    (when left-buf (kill-buffer left-buf))
    (when right-buf (kill-buffer right-buf))))


(defun zorg-toggle-left ()
  "Toggle the left Zorg panel between hidden and its saved fraction.
When turning off, saves the current fraction and disables the panel.
When turning on, restores the panel using the previously saved fraction."
  (interactive)
  (zorg--initialize-saved-fractions)
  (if zorg--left-enabled-p
      ;; Turn OFF: save current fraction and disable
      (progn
        (when (window-live-p zorg--left-window)
          (let ((current-fraction (/ (float (window-total-width zorg--left-window)) (frame-width))))
            (setq zorg--saved-left-fraction current-fraction)
            (setq zorg-left-width-fraction 0.0)))
        (setq zorg--left-enabled-p nil))
    ;; Turn ON: restore from saved fraction
    (progn
      (zorg--set-left-fraction (or zorg--saved-left-fraction 0.25))
      (setq zorg--left-enabled-p t)))
  (when zorg--active-p
    (zorg--setup-layout)))

(defun zorg-toggle-right ()
  "Toggle the right Zorg panel between hidden and its saved fraction.
When turning off, saves the current fraction and disables the panel.
When turning on, restores the panel using the previously saved fraction."
  (interactive)
  (zorg--initialize-saved-fractions)
  (if zorg--right-enabled-p
      ;; Turn OFF: save current fraction and disable
      (progn
        (when (window-live-p zorg--right-window)
          (let ((current-fraction (/ (float (window-total-width zorg--right-window)) (frame-width))))
            (setq zorg--saved-right-fraction current-fraction)
            (setq zorg-right-width-fraction 0.0)))
        (setq zorg--right-enabled-p nil))
    ;; Turn ON: restore from saved fraction
    (progn
      (zorg--set-right-fraction (or zorg--saved-right-fraction 0.25))
      (setq zorg--right-enabled-p t)))
  (when zorg--active-p
    (zorg--setup-layout)))

(defun zorg-save-fractions ()
  "Save current side panel fractions to custom.el.
Captures the actual window widths and saves them as customization variables
for persistence across Emacs sessions."
  (interactive)
  (zorg--capture-fractions)
  (zorg--clean-old-custom-variables)
  (customize-save-variable 'zorg-left-width-fraction zorg-left-width-fraction)
  (customize-save-variable 'zorg-right-width-fraction zorg-right-width-fraction)
  (message "Saved Zorg fractions: L=%.2f R=%.2f"
           zorg-left-width-fraction zorg-right-width-fraction))

(add-hook 'kill-emacs-hook #'zorg-save-fractions)

(defun zorg--setup-layout ()
  "Recreate Zorg layout using Emacs side window system with percentage-based widths.
Creates side windows based on the current fraction settings and ensures
proper window parameters are set for Zorg's window management."
  (let ((main-buf (current-buffer)))
    
    ;; Clean up any existing side windows first
    (zorg--cleanup-side-windows)
    
    ;; Ensure we start with a clean slate
    (delete-other-windows)
    (let ((center (selected-window)))
      (set-window-parameter center 'zorg-role 'main)
      
      ;; Create left side window if enabled
      (when zorg--left-enabled-p
        (let* ((left-fraction zorg-left-width-fraction)
               (left-buffer (get-buffer-create "*zorg-left*")))
          (with-current-buffer left-buffer (org-mode))
          (setq zorg--left-window
                (display-buffer-in-side-window 
                 left-buffer
                 `((side . left)
                   (slot . 0)
                   (window-width . ,left-fraction)
                   (window-parameters . ((zorg-role . left)
                                        (no-delete-other-windows . t))))))
          (when (window-live-p zorg--left-window)
            (set-window-parameter zorg--left-window 'zorg-role 'left))))
      
      ;; Create right side window if enabled
      (when zorg--right-enabled-p
        (let* ((right-fraction zorg-right-width-fraction)
               (right-buffer (get-buffer-create "*zorg-right*")))
          (with-current-buffer right-buffer (org-mode))
          (setq zorg--right-window
                (display-buffer-in-side-window 
                 right-buffer
                 `((side . right)
                   (slot . 0)
                   (window-width . ,right-fraction)
                   (window-parameters . ((zorg-role . right)
                                        (no-delete-other-windows . t))))))
          (when (window-live-p zorg--right-window)
            (set-window-parameter zorg--right-window 'zorg-role 'right))))
      
      ;; Restore main buffer and ensure it's selected
      (with-selected-window center
        (switch-to-buffer main-buf))
      (select-window center))))


;;;###autoload
(defun zorg-mode ()
  "Toggle Zorg mode: center buffer with side panels."
  (interactive)
  (if zorg--active-p
      ;; --- turn off
      (progn
        ;; Capture current fractions before turning off
        (zorg--capture-fractions)
        (let ((main-buf (zorg--main-buffer)))
          (zorg--cleanup-side-windows)
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
      ;; Initialize saved fractions and enable panels based on current fractions
      (zorg--initialize-saved-fractions)
      (setq zorg--left-enabled-p (> zorg-left-width-fraction 0.0))
      (setq zorg--right-enabled-p (> zorg-right-width-fraction 0.0))
      (setq zorg--saved-config (current-window-configuration)
            zorg--active-p t)
      (zorg--setup-layout)
      (message "Zorg mode on"))))


(defun zorg--maybe-restore-layout (&rest _)
  "If Zorg mode is active, restore its layout after project/workspace switch.
This function is designed to be used as a hook function and ignores
any arguments passed to it."
  (when zorg--active-p
    (zorg--setup-layout)))
;; Adapter for hooks that pass args we don't care about.
(defun zorg--save-fractions-hook (&rest _)
  "Save current fractions when Zorg mode is active.
This function is designed to be used as a hook function and ignores
any arguments passed to it."
  (when zorg--active-p
    (zorg-save-fractions)))

;;saving and then restoring on project switching
;;might add project-specific layouts but for now its global

;; Integration with various workspace/project switching systems
;; These hooks ensure that Zorg layouts are preserved and restored
;; when switching between projects or workspaces.

;; Doom workspaces (persp-mode)
(with-eval-after-load 'persp-mode
  (add-hook 'persp-before-switch-functions #'zorg--save-fractions-hook)
  (add-hook 'persp-activated-functions      #'zorg--maybe-restore-layout))

;; Projectile project switching
(with-eval-after-load 'projectile
  (add-hook 'projectile-before-switch-project-hook #'zorg--save-fractions-hook)
  (add-hook 'projectile-after-switch-project-hook  #'zorg--maybe-restore-layout))

;; Vanilla Emacs tab-bar
(with-eval-after-load 'tab-bar
  (add-hook 'tab-bar-tab-pre-close-functions #'zorg--save-fractions-hook)
  (add-hook 'tab-bar-switch-hook #'zorg--save-fractions-hook)
  (add-hook 'tab-bar-switch-hook             #'zorg--maybe-restore-layout))


;; Zorg Linking Helpers
;;
;; These functions provide specialized linking capabilities for Zorg notes:
;; - `zorgmain:` links that always open in the main window
;; - Helper functions to copy links to current location
;; - Functions to insert links between notes

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

;; AI Tools Integration
;;
;; The following variables and functions provide seamless integration
;; with AI tools like gptel and copilot CLI. AI tools can be toggled
;; into one of the side panels, and the system preserves the state
;; of whatever was in that panel before the AI tool was activated.

(defvar zorg--ai-state nil
  "Current AI tool state: nil, 'gptel, or 'agent.")

(defvar zorg--ai-saved-buffer nil
  "Buffer that was in the AI side panel before AI tools.")

(defvar zorg--ai-side-was-closed nil
  "Whether the AI side panel was disabled before AI activation.")

(defun zorg--save-right-state ()
  "Save the current state of the right panel before AI activation.
This is used to restore the panel to its previous state when AI tools are toggled off."
  (setq zorg--right-was-closed (not zorg--right-enabled-p))
  (when (and zorg--right-enabled-p 
             (window-live-p zorg--right-window))
    (setq zorg--right-saved-buffer (window-buffer zorg--right-window))))

(defun zorg--restore-right-state ()
  "Restore the right panel to its pre-AI state.
Handles three cases: panel was closed, had a specific buffer, or gets default buffer."
  (cond
   ;; If right panel was closed, close it again
   (zorg--right-was-closed
    (setq zorg--right-enabled-p nil)
    (when zorg--active-p
      (zorg--setup-layout)))
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
  "Ensure the right panel is open, opening at default fraction if closed.
Used by AI tools that need the right panel to be available."
  (unless zorg--right-enabled-p
    (zorg--initialize-saved-fractions)
    (zorg--set-right-fraction (or zorg--saved-right-fraction 0.25))
    (setq zorg--right-enabled-p t)
    (when zorg--active-p
      (zorg--setup-layout))))

(defun zorg--save-ai-state ()
  "Save the current state of the AI side panel before AI activation.
Works with either left or right panel depending on `zorg-ai-side' configuration."
  (if (eq zorg-ai-side 'left)
      (progn
        (setq zorg--ai-side-was-closed (not zorg--left-enabled-p))
        (when (and zorg--left-enabled-p 
                   (window-live-p zorg--left-window))
          (setq zorg--ai-saved-buffer (window-buffer zorg--left-window))))
    (progn
      (setq zorg--ai-side-was-closed (not zorg--right-enabled-p))
      (when (and zorg--right-enabled-p 
                 (window-live-p zorg--right-window))
        (setq zorg--ai-saved-buffer (window-buffer zorg--right-window))))))

(defun zorg--restore-ai-state ()
  "Restore the AI side panel to its pre-AI state.
Handles restoring either left or right panel depending on `zorg-ai-side' configuration."
  (if (eq zorg-ai-side 'left)
      (cond
       ;; If left panel was closed, close it again
       (zorg--ai-side-was-closed
        (setq zorg--left-enabled-p nil)
        (when zorg--active-p
          (zorg--setup-layout)))
       ;; If there was a saved buffer, restore it
       ((and zorg--ai-saved-buffer 
             (buffer-live-p zorg--ai-saved-buffer)
             (window-live-p zorg--left-window))
        (with-selected-window zorg--left-window
          (switch-to-buffer zorg--ai-saved-buffer)))
       ;; Otherwise, show the default left buffer
       ((window-live-p zorg--left-window)
        (with-selected-window zorg--left-window
          (switch-to-buffer (get-buffer-create "*zorg-left*"))
          (org-mode))))
    (cond
     ;; If right panel was closed, close it again
     (zorg--ai-side-was-closed
      (setq zorg--right-enabled-p nil)
      (when zorg--active-p
        (zorg--setup-layout)))
     ;; If there was a saved buffer, restore it
     ((and zorg--ai-saved-buffer 
           (buffer-live-p zorg--ai-saved-buffer)
           (window-live-p zorg--right-window))
      (with-selected-window zorg--right-window
        (switch-to-buffer zorg--ai-saved-buffer)))
     ;; Otherwise, show the default right buffer
     ((window-live-p zorg--right-window)
      (with-selected-window zorg--right-window
        (switch-to-buffer (get-buffer-create "*zorg-right*"))
        (org-mode)))))
  (setq zorg--ai-state nil
        zorg--ai-saved-buffer nil
        zorg--ai-side-was-closed nil))

(defun zorg--ensure-ai-panel ()
  "Ensure the AI side panel is open, opening at default fraction if closed.
Works with either left or right panel depending on `zorg-ai-side' configuration."
  (if (eq zorg-ai-side 'left)
      (unless zorg--left-enabled-p
        (zorg--initialize-saved-fractions)
        (zorg--set-left-fraction (or zorg--saved-left-fraction 0.25))
        (setq zorg--left-enabled-p t)
        (when zorg--active-p
          (zorg--setup-layout)))
    (unless zorg--right-enabled-p
      (zorg--initialize-saved-fractions)
      (zorg--set-right-fraction (or zorg--saved-right-fraction 0.25))
      (setq zorg--right-enabled-p t)
      (when zorg--active-p
        (zorg--setup-layout)))))

(defun zorg--get-ai-window ()
  "Get the AI side window based on zorg-ai-side configuration.
Returns either the left or right window depending on user preference."
  (if (eq zorg-ai-side 'left)
      zorg--left-window
    zorg--right-window))

(defun zorg--ai-cli-process-running-p ()
  "Check if the AI CLI process is running in the *zorg-agent* buffer.
Returns t if the process is alive and running, nil otherwise."
  (let ((buf (get-buffer "*zorg-agent*")))
    (and buf
         (buffer-live-p buf)
         (with-current-buffer buf
           (let ((proc (get-buffer-process buf)))
             (and proc (process-live-p proc)))))))

(defun zorg--start-ai-cli ()
  "Start the AI CLI in the *zorg-agent* buffer.
Uses the command specified in `zorg-ai-cli-command` and sets the working directory
to the project root."
  ;; Kill existing buffer if it exists to ensure clean start
  (when (get-buffer "*zorg-agent*")
    (kill-buffer "*zorg-agent*"))
  ;; Create vterm buffer and ensure it opens in current window (the AI side panel)
  (let* ((project-root (zorg--project-root))
         (default-directory project-root)
         (buf (get-buffer-create "*zorg-agent*")))
    ;; Switch to the buffer in current window (should be AI side panel)
    (switch-to-buffer buf)
    ;; Initialize vterm in this buffer
    (vterm-mode)
    ;; Send the CLI command
    (vterm-send-string zorg-ai-cli-command)
    (vterm-send-return)))

(defun zorg-set-ai-cli-command (command)
  "Set the AI CLI command interactively.
COMMAND is the shell command to run for the AI CLI (e.g., 'copilot', 'aider', 'cursor')."
  (interactive "sAI CLI command: ")
  (setq zorg-ai-cli-command command)
  (customize-save-variable 'zorg-ai-cli-command command)
  (message "AI CLI command set to: %s" command))

(defun zorg-ai-gptel ()
  "Toggle gptel in the configured AI side panel, preserving panel state.
Activates gptel if no AI tool is active, switches to gptel if another AI tool
is active, or restores previous panel state if gptel is already active."
  (interactive)
  (unless zorg--active-p
    (user-error "Zorg mode is not active"))

  (cond
   ;; If gptel is already active, restore previous state
   ((eq zorg--ai-state 'gptel)
    (zorg--restore-ai-state))

   ;; If another AI tool is active, switch to gptel
   ((eq zorg--ai-state 'agent)
    (zorg--ensure-ai-panel)
    (when (window-live-p (zorg--get-ai-window))
      (with-selected-window (zorg--get-ai-window)
        (switch-to-buffer (gptel "*zorg-gptel*"))))
    (setq zorg--ai-state 'gptel))

   ;; If no AI tool is active, activate gptel
   (t
    (zorg--save-ai-state)
    (zorg--ensure-ai-panel)
    (when (window-live-p (zorg--get-ai-window))
      (with-selected-window (zorg--get-ai-window)
        (switch-to-buffer (gptel "*zorg-gptel*"))))
    (setq zorg--ai-state 'gptel))))

(defun zorg-ai-agent ()
  "Toggle AI agent CLI in the configured AI side panel via vterm, preserving panel state.
Starts the CLI specified in `zorg-ai-cli-command` in a vterm session within the AI 
side panel. Manages session persistence and automatically sets up the working 
directory to the project root. If `zorg-ai-cli-restart-if-not-running` is t, 
will restart the CLI process if it has exited."
  (interactive)
  (unless zorg--active-p
    (user-error "Zorg mode is not active"))
  
  (cond
   ;; If agent is already active, restore previous state
   ((eq zorg--ai-state 'agent)
    (zorg--restore-ai-state))
   
   ;; If another AI tool is active, switch to agent
   ((eq zorg--ai-state 'gptel)
    (zorg--ensure-ai-panel)
    (when (window-live-p (zorg--get-ai-window))
      (with-selected-window (zorg--get-ai-window)
        ;; Check if we should just switch to existing buffer or restart
        ;; Switch to existing buffer if:
        ;; - Buffer exists AND
        ;; - (restart is disabled OR process is still running)
        ;; Otherwise, start/restart the AI CLI
        (if (and (get-buffer "*zorg-agent*")
                 (or (not zorg-ai-cli-restart-if-not-running)
                     (zorg--ai-cli-process-running-p)))
            (switch-to-buffer "*zorg-agent*")
          (zorg--start-ai-cli))))
    (setq zorg--ai-state 'agent))
   
   ;; If no AI tool is active, activate agent
   (t
    (zorg--save-ai-state)
    (zorg--ensure-ai-panel)
    (when (window-live-p (zorg--get-ai-window))
      (with-selected-window (zorg--get-ai-window)
        ;; Check if we should just switch to existing buffer or restart
        ;; Switch to existing buffer if:
        ;; - Buffer exists AND
        ;; - (restart is disabled OR process is still running)
        ;; Otherwise, start/restart the AI CLI
        (if (and (get-buffer "*zorg-agent*")
                 (or (not zorg-ai-cli-restart-if-not-running)
                     (zorg--ai-cli-process-running-p)))
            ;; Just switch to existing buffer
            (switch-to-buffer "*zorg-agent*")
          ;; Start/restart the AI CLI
          (zorg--start-ai-cli))))
    (setq zorg--ai-state 'agent))))

;; Compatibility alias for existing users
(defalias 'zorg-ai-copilot 'zorg-ai-agent
  "Compatibility alias for zorg-ai-agent. Use zorg-ai-agent instead.")

(provide 'zorg)

;;; zorg.el ends here

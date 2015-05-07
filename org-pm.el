
(defun org-pm-menu ()
  "Top level menu of common org-pm commands."
  (interactive)
  (command-execute
   (intern
    (concat
     "org-pm-"
     (replace-regexp-in-string
      " " "-"
      (grizzl-completing-read
       " === SELECT ACTION: === "
       (grizzl-make-index
        (reverse '(
                   "publish file"
                   "publish subtree"
                   "dired root directory"
                   "dired source directory"
                   "dired target directory"
                   "republish entire project"
                   "open last published file"
                   "make project")))))))))

(defun org-pm-publish-file (&optional project)
  (interactive)
  (org-pm-publish-file-or-subtree (or project (org-pm-choose-project))))

(defun org-pm-publish-subtree (&optional project)
  (interactive)
  (org-pm-publish-file-or-subtree (or project (org-pm-choose-project)) t))

(defun org-pm-choose-project (&optional project subtree-p)
  "Choose a project from menu.
- Present vertical menu with grizzl.
- If no project exists, then create one.
- If name of project entered is not in list of exising projects, then create
  that project.
- Offer last chosen project for file or section in current org-mode buffer
  as default.
- List already exported projects for file or section marked with *, and
  present them next to the default."
  (let* ((projects (org-pm-list-projects))
         (defaults (org-pm-default-project-list subtree-p))
         (separator (if (and projects defaults) '("===================") ()))
         (chosen-project
          (if projects
              (grizzl-completing-read
               "=== CHOOSE PROJECT: ==="
               (grizzl-make-index
                (reverse (delete-dups (append defaults separator projects)))))
            (org-pm-make-project)))))
  (if chosen-project
      (setq org-pm-last-chosen-project chosen-project))
  chosen-project)

(defun org-pm-default-project-list (&optional subtree-p)
  "Present list of default projects for user to choose from.
If current buffer is in org-mode, then list projects that this file or subtree
has already been exported in.
Else list the last project that has been exported to."
  (if (eq (buffer-local-value 'major-mode (current-buffer)) 'org-mode)
      ())
  (if (list org-pm-last-chosen-project))
  )

;; (or
;;  (car (memq org-pm-last-chosen-project projects))
;;  (car projects))

(defun org-pm-make-project (&optional project)
  "Create project folder if it does not exist.
If no project name is given, then prompt user for one.
If project with that name exists, then post a message about it.
If project does not exist, then ask user to confirm before creating it."
  (setq project
        (or project
            (read-from-minibuffer "Enter name for new project: "  "project1")))
  (setq project (replace-regexp-in-string "[^[:alnum:]]+" "-" project))
  (let* ((path (org-pm-get-project-dir project)))
    (if (file-exists-p path)
        (message "Project '%s' already exists." project)
      (if (y-or-n-p (format "Really create project named '%s'?" project))
          (mkdir path t)
        (setq project nil)))
    project))

;; variables, path construction

(defvar org-pm-root-dir
  (expand-file-name "~/org-pm-projects")
  "Path to directory containing project definitions.
Project definitions are placed in subdirectories of this directory")

(defvar org-pm-target-subdir
  ""
  "Subdirectory of project dir where publish exports files to.
Per default, it is empty, meaning the target dir is the root of the project dir.
May be changed to save different types of exports in different subdirs. ")

(defvar org-pm-subdir-property-name
  "subdirectory"
  "The name of the property for storing the subdirectory where a file
or a subtree should be exported in the target directory.")

(defvar org-pm-last-chosen-project "project1"
  "Name of last chosen project.
Used as default for menu in org-pm-choose-project.")

(defun org-pm-get-project-dir (project-name)
  "Return the root directory for project named PROJECT-NAME."
  (expand-file-name (concat org-pm-root-dir "/" project-name)))

(defun org-pm-get-config-dir (project-name)
  "Return the config directory for project named PROJECT-NAME."
  (concat (org-pm-get-project-dir project-name) "/CONFIG"))

(defun org-pm-get-source-dir (project-name)
  "Return the org source file directory for project named PROJECT-NAME.
Create directory if needed."
  (let ((dir
         (expand-file-name
          (concat org-pm-root-dir "/" project-name org-pm-source-dir))))
    (unless (file-exists-p dir) (mkdir dir t))
    dir))

(defun org-pm-get-target-dir (project-name)
  "Return the export file directory for project named PROJECT-NAME.
Create directory if needed."
  (let ((dir (concat (org-pm-get-project-dir project-name) org-pm-target-subdir)))
    (unless (file-exists-p dir) (mkdir dir t))
    dir))

(defun org-pm-get-includes-dir (project-name)
  "Return source directory for includes for project named PROJECT-NAME."
  (concat  (org-pm-get-target-dir project-name) "/includes"))

(defun org-pm-last-exported-file-path ()
  (if org-pm-last-saved-source-path
      (org-pm-make-export-path-from-source-path org-pm-last-saved-source-path)
    nil))

(defun org-pm-make-export-path-from-source-path (source-path)
  (concat
   (file-name-sans-extension
    (replace-regexp-in-string org-pm-source-dir org-pm-target-subdir source-path))
   ".html"))

(defvar org-pm-registered-projects-property "PM_PROJECTS")

(defvar org-pm-source-dir "/SOURCE")

(defvar org-pm-last-saved-source-path ""
  "org-pm-last-exported-file-path is computed from this variable.")

(defun org-pm-get-source-file-path (project-name subtree-p)
  (concat
   (org-pm-get-source-file-dir project-name subtree-p)
   "/"
   (org-pm-make-source-file-name project-name subtree-p)))

(defun org-pm-get-source-file-dir (project-name subtree-p)
  (let* ((maindir (org-pm-get-source-dir project-name))
         (subdir (org-pm-get-subdir project-name subtree-p))
         (full-dir (if subdir (concat maindir "/" subdir) maindir)))
    (unless (file-exists-p full-dir) (mkdir full-dir t))
    full-dir))

(defun org-pm-get-subdir (project-name subtree-p)
  (org-pm-get-project-attribute
   project-name org-pm-subdir-property-name subtree-p))

(defun org-pm-make-source-file-name (project-name subtree-p)
  (or (org-pm-get-project-attribute project-name "filename" subtree-p)
      (if subtree-p
          (let (fname (concat
                       (replace-regexp-in-string
                        "[^[:alnum:]]+" "-" (org-pm-get-subtree-headline))
                       ".org")))
        (file-name-nondirectory (buffer-file-name)))))

(defun org-pm-get-target-file-path (project-name subtree-p &optional file-type)
  "Get full path where file/subtree will be exported.
Used to open that file for viewing (on browser etc)."
  (concat
   (org-pm-get-target-file-dir (project-name subtree-p))
   (concat
    (file-name-sans-extension (org-pm-get-make-source-file-name))
    (and file-type ".html"))))

(defun org-pm-get-target-file-dir (project-name subtree-p)
  (let* ((maindir (org-pm-get-target-dir project-name))
         (subdir (org-pm-get-subdir project-name subtree-p))
         (full-dir (if subdir (concat maindir "/" subdir) maindir)))
    (unless (file-exists-p full-dir) (mkdir full-dir t))
    full-dir))

(defun org-pm-select-project-then-action ()
  (interactive)
  (let ((project (org-pm-choose-project))
        (action (org-pm-select-action)))
    (funcall action project)))

(defun org-pm-select-action-then-project ()
  "Project selection is provided by the action, if needed."
  (interactive)
  (funcall (org-pm-select-action)))

(global-set-key (kbd "H-m") 'org-pm-select-action-then-project)
(global-set-key (kbd "H-M") 'org-pm-select-project-then-action)

(defun org-pm-select-action ()
  (intern
   (concat
    "org-pm-"
    (replace-regexp-in-string
     " " "-"
     (grizzl-completing-read
      " === SELECT ACTION: === "
      (grizzl-make-index
       '("make project"
         "dired root directory"
         "dired source directory"
         "dired target directory"
         "publish subtree"
         "publish file"
         "republish entire project"
         "open last published file")))))))


(defun org-pm-republish-entire-project (&optional project)
  "Republish entire source of PROJECT."
  (interactive)
  (org-pm-publish (or project (org-pm-choose-project)) t))

(defun org-pm-dired-root-directory (&optional dummy)
  (interactive)
  (dired org-pm-root-dir))

(defun org-pm-dired-source-directory (&optional project)
  (interactive)
  (dired (org-pm-get-source-dir (or project (org-pm-choose-project)))))

(defun org-pm-dired-target-directory (&optional project)
  (interactive)
  (dired (org-pm-get-target-dir (or project (org-pm-choose-project)))))

(defun org-pm-open-last-published-file (&optional dummy)
  (interactive)
  (let ((path (org-pm-last-exported-file-path)))
    (if (and path (file-exists-p path))
        (shell-command (concat "open " path))
      (message "No file found to open: %s" path))))

;;; Main function

(defun org-pm-publish-file-or-subtree (project &optional subtree-p)
  "Publish current file or subtree to a project chosen from template folder."
  (org-add-option-or-property
   org-pm-registered-projects-property project subtree-p)
  (org-pm-save-org-source project subtree-p)
  (org-pm-publish project nil))

(defun org-pm-publish (project force)
  "Publish PROJECT, forcing re-publish of all files if FORCE."
  (let ((org-publish-project-alist (org-pm-create-project-plist project))
        (org-export-before-parsing-hook '(org-pm-insert-headers))
        (project-name project))
    (org-publish project force)))

(defun org-pm-list-projects ()
  (mapcar
   (lambda (p) (file-name-nondirectory (file-name-sans-extension p)))
   (file-expand-wildcards (concat org-pm-root-dir "/*"))))

(defun org-pm-create-project-plist (project-name)
  "Create org-publish-project-alist with project from template folder.
PROJECT-NAME is the name of the project, and is the same as the folder
that contains the project template."
  (list
   ;; TODO: Merge this list with options read from config.org
   (list
    project-name
    :base-directory (org-pm-get-source-dir project-name)
    :publishing-directory (org-pm-get-target-dir project-name)
    :base-extension "org"
    :recursive t
    :publishing-function 'org-html-publish-to-html
    ;; :headline-levels 4
    ;; :auto-preamble t
    )))

(defun org-pm-save-org-source (project-name subtree-p)
  (save-buffer)
  (let* ((contents-buffer (current-buffer))
         (contents-path (or (buffer-file-name) (buffer-name)))
         (source-file-path (org-pm-get-source-file-path project-name subtree-p))
         (source-file-dir (file-name-directory source-file-path)))
    (if subtree-p (org-copy-subtree))
    (unless (file-exists-p source-file-dir) (mkdir source-file-dir t))
    (find-file source-file-path)
    (erase-buffer)
    (insert "#+EXPORT_DATE: "
            (format-time-string "%A %d %B %Y %T %Z\n")
            "#+SOURCE: "
            contents-path
            "\n")
    ;; If excerpting from subtree, then
    ;; subfolder must be stored in file now, to be used later
    ;; by org-export-before-parsing hook function org-pm-insert-headers
    ;; (if wnole-file, then any subdir spec will already be in place).
    (if subtree-p
        (let* ((pname
                (org-pm-compose-project-attribute-name
                 project-name org-pm-subdir-property-name))
               (subdir (org-entry-get (point) pname)))
          (if subdir (insert "#+" pname " " subdir "\n"))
          (org-paste-subtree 1)))
    (insert-buffer contents-buffer)
    (save-buffer)
    (kill-buffer)
    (setq org-pm-last-saved-source-path source-file-path)))

(defun org-pm-insert-headers (backend)
  "Insert org-publish headers to current buffer before publishing.

This function is called by org-publish through org-export-before-parsing-hook.
The BACKEND is therefore passed by org-publish function and is not used here.

The value of PROJECT-NAME is inherited from the 'let' binding in org-pm-publish.

The headers are created by scanning the CONFIG and includes folders
of project folder corresponding to PROJECT_NAME."
  (insert (org-pm-make-includes-headers project-name)))

(defun org-pm-make-includes-headers (project-name)
  "Make HTML_HEAD_EXTRA lines with links for each css and js file in includes.
Copy all css and js files from template directory to export directory.
For each of these files, construct a HTML_HEAD_EXTRA string,
to be added to the top of the org source file for publishing."
  (let* ((subdir
          (concat
           org-pm-target-subdir
           (or (org-get-option (org-pm-make-subdir-option project-name)) "")))
         (includes-path (org-pm-get-includes-dir project-name))
         (includes-string "")
         (relative-path "includes/"))
    (when (file-exists-p includes-path)
      (setq
       includes-string
       (concat includes-string
               (org-pm-make-options includes-path)
               (org-pm-make-html-head includes-path "HTML_HEAD")
               (org-pm-make-html-head includes-path "HTML_HEAD_EXTRA")))
      (if (> (length subdir) 0)
          (setq relative-path
                (concat
                 (mapconcat (lambda (x) "../") (split-string subdir "/") "")
                 relative-path)))
      (dolist (path (file-expand-wildcards (concat includes-path "/*.css")))
        (setq includes-string
              (concat
               includes-string
               "#+HTML_HEAD_EXTRA: <link rel=\"stylesheet\" href=\""
               relative-path
               (file-name-nondirectory path)
               "\"/>\n")))
      (dolist (path (file-expand-wildcards (concat includes-path "/*.js")))
        (setq includes-string
              (concat
               includes-string
               "#+HTML_HEAD_EXTRA: <script src=\""
               relative-path
               (file-name-nondirectory path)
               "\"></script>\n"))))
    includes-string))

(defun org-pm-make-options (path)
  "Create string from OPTIONS file"
  (let ((file-name (file-truename (concat path "/OPTIONS.org"))))
    (if (file-exists-p file-name)
        (with-temp-buffer
          (insert-file-contents file-name)
          (buffer-string))
      "")))

(defun org-pm-make-html-head (path head-type)
  "Create string with one HTML_HEAD(_EXTRA) per line from file in template folder."
  (let ((file-name (file-truename (concat path "/" head-type ".html"))))
    (if (file-exists-p file-name)
        (with-temp-buffer
          (insert-file-contents file-name)
          (goto-char (point-min))
          (while (re-search-forward "^" nil t)
            (replace-match (concat "#+" head-type ": ")))
          (buffer-string))
      "")))

(defun org-pm-get-subtree-headline () (nth 4 (org-headline-components)))

(defun org-pm-get-file-and-subtree-projects ()
  (delete-dups
   (append (org-pm-get-file-projects) (org-pm-get-subtree-projects))))

(defun org-pm-get-file-projects ()
  (-select (lambda (x) (> (length x) 0))
           (split-string
            (or (org-get-option org-pm-registered-projects-property) "") " ")))

(defun org-pm-get-subtree-projects ()
  (-select (lambda (x) (> (length x) 0))
           (split-string
            (or (org-entry-get (point) org-pm-registered-projects-property) "") " ")))

(defun org-pm-get-project-attribute (project-name property &optional subtree-p)
  (let ((property-name ;; use function for DRY when setting/getting
         (org-pm-compose-project-attribute-name project-name property)))
   (if subtree-p
       (org-entry-get (point) property-name)
     (org-get-option property-name))))

(defun org-pm-compose-project-attribute-name (project-name property)
  "Compose property or option name from PROJECT-NAME and PROPERTY.
The code of this function is shorter than its name, but this function
ensures that the attrubute name is always constructed in the same way."
  (concat "_" project-name "-" property))

(defun org-pm-make-subdir-option (project-name)
  (org-pm-compose-project-attribute-name
   project-name org-pm-subdir-property-name))

(defun org-get-option (option)
  (org-with-wide-buffer
   (goto-char (point-min))
   (let ((found
           (re-search-forward (org-make-options-regexp (list option)) nil t)))
     (if found (match-string-no-properties 2) nil))))

(defun org-add-option-or-property (option value &optional subtree-p)
  "Add option or property value in buffer.
This is a special case: Option or property items must be separated by spaces.
See also org-set-option-or-property."
  (if subtree-p
      (org-add-property option value)
    (org-add-option option value)))

(defun org-add-option (option value)
    (org-with-wide-buffer
     (goto-char (point-min))
     (let* ((found
             (re-search-forward (org-make-options-regexp (list option)) nil t))
            (found-string (if found (match-string 2) "")))
       (if found
           (kill-whole-line)
         (goto-char (point-min)))
       (insert
        (concat
         "#+"
         option
         ": "
         (add-word-to-string-set value found-string)
         "\n")))))

(defun org-add-property (property value)
  (org-entry-put
   (point)
   property
   (add-word-to-string-set value (or (org-entry-get (point) property) ""))))

(defun add-word-to-string-set (word string)
  (mapconcat
   (lambda (x) x)
   (delete-dups (cons word (split-string (or string "") " ")))
   " "))

(provide 'org-pm)

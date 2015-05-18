
(require 'grizzl)
(require 'dash)

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

(defvar org-pm-template-dirs
  (list (concat (file-name-directory (or load-file-name (buffer-file-name))) "TEMPLATES/*"))
  "List of directories containing templates")

(defun org-pm-get-project-dir (project-name)
  "Return the root directory for project named PROJECT-NAME."
  (expand-file-name (concat org-pm-root-dir "/" project-name)))

(defun org-pm-get-options-path (project-name)
  "Return the path of the options file for project named PROJECT-NAME."
  (concat (org-pm-get-config-dir project-name) "/options.org"))

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
      (replace-regexp-in-string
       "/SOURCE/" ;; dangerous: if many /SOURCE/ are present ...
       "/"
       (concat
        (file-name-sans-extension org-pm-last-saved-source-path) ".html"))
    nil))

(defvar org-pm-registered-projects-property "PM_PROJECTS")

(defvar org-pm-source-dir "/SOURCE")

(defvar org-pm-last-saved-source-path ""
  "org-pm-last-exported-file-path is computed from this variable.")

(global-set-key (kbd "H-e m") 'org-pm-menu)
(global-set-key (kbd "<f14> m") 'org-pm-menu)
(global-set-key (kbd "<f14> <f14>") 'org-pm-menu)
(global-set-key (kbd "H-e f") 'org-pm-publish-file)
(global-set-key (kbd "<f14> f") 'org-pm-publish-file)
(global-set-key (kbd "H-e s") 'org-pm-publish-subtree)
(global-set-key (kbd "<f14> s") 'org-pm-publish-subtree)

(defun org-pm-menu (deploy-p)
  "Top level menu of common org-pm commands."
  (interactive "P")
  (call-interactively
   (intern
    (concat
     "org-pm-"
     (replace-regexp-in-string
      " " "-"
      (grizzl-completing-read
       " === SELECT ACTION: === "
       (grizzl-make-index
        (reverse '("publish file"
                   "publish subtree"
                   "dired root directory"
                   "dired source directory"
                   "dired target directory"
                   "republish entire project"
                   "open last published file"
                   "make project")))))))))

(defun org-pm-publish-file (&optional deploy-p project)
  (interactive "P")
  (org-pm-publish-file-or-subtree deploy-p (or project (org-pm-choose-project))))

(defun org-pm-publish-subtree (&optional deploy-p project)
  (interactive "P")
  (org-pm-publish-file-or-subtree deploy-p (or project (org-pm-choose-project t)) t))

(defun org-pm-make-project (&optional project-name)
  (interactive)
  (unless project-name
    (let ((project-index (grizzl-make-index (org-pm-list-projects))))
      (setq project-name
            (grizzl-completing-read
             "Enter project name or select a project to update its template"
             project-index))))
  (setq project-name
        (replace-regexp-in-string "[^[:alnum:]]+" "-" project-name))
  (let (template-index
        template-dir
        (project-dir (org-pm-get-project-dir project-name)))
    (if (file-exists-p project-dir)
        (unless
            (yes-or-no-p
             (format "Project '%s' exists. Overwrite its template?" project-name))
          (error (format "Cancelled creation of project '%s'" project-name)))
      (mkdir project-dir t))
    (setq template-index
          (grizzl-make-index
           (apply 'append
                  (mapcar 'file-expand-wildcards org-pm-template-dirs))))
    (setq template-dir
          (grizzl-completing-read "Select a template: " template-index))
     (if (file-exists-p template-dir)
         (progn
          (copy-directory
           template-dir
           (concat project-dir "/includes")
           nil t t)
          (message (format "Updated template for project %s" project-name)))
       (error (format "Template not found: %s" template-dir)))))

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

(defun org-pm-republish-entire-project (&optional deploy-p project)
  "Republish entire source of PROJECT."
  (interactive "P")
  (org-pm-publish (or project (org-pm-choose-project)) deploy-p t))

(defun org-pm-choose-project (&optional subtree-p)
  "Choose a project from menu.
  - Present vertical menu with grizzl.
  - If no project exists, then create one.
  - If name of project entered is not in list of exising projects, then create
    that project.
  - Offer last chosen project for file or section in current org-mode buffer
    as default.
  - Append list already exported projects for file or section, distinguishing it
  through a separator line."
  (let* ((existing-projects
          (reverse (append '("====== Existing projects: ======") (org-pm-list-projects))))
         (default-projects
           (if (eq (buffer-local-value 'major-mode (current-buffer)) 'org-mode)
               (if subtree-p
                   (org-pm-get-subtree-projects)
                 (org-pm-get-file-projects))
             nil))
         (default-project-menu
           (if default-projects
               (reverse
                (cons
                 (format
                  "====== Projects which current %s already exports to are: ======"
                  (if subtree-p "section" "file"))
                 default-projects))
             (reverse (cons "====== Default project: ======="
                            (list (or org-pm-last-chosen-project "project1"))))))
         (chosen-project
          (strip-nonalpha
           (grizzl-completing-read
            "=== CHOOSE EXISTING PROJECT ==="
            (grizzl-make-index
             (reverse
              (delete-dups
               (append '("CREATE NEW PROJECT")
                       default-project-menu existing-projects)))))))
         (project-path (org-pm-get-project-dir chosen-project)))
    (if (equal chosen-project "CREATE-NEW-PROJECT")
        (setq chosen-project
              (strip-nonalpha
               (read-from-minibuffer
                "Enter name of new project:" "my-new-project"))))
    (unless (file-exists-p project-path)
      (if (y-or-n-p (format "Really create project named '%s'?" chosen-project))
          (mkdir project-path t)
        (error
         (format "Did not create project %s. Publishing cancelled."
                 chosen-project))))
    chosen-project))

(defun org-pm-list-projects ()
  (mapcar
   (lambda (p) (file-name-nondirectory (file-name-sans-extension p)))
   (file-expand-wildcards (concat org-pm-root-dir "/*"))))

  ;;; INCOMPLETE!:
(defun org-pm-default-project-list (&optional subtree-p)
  "Present list of default projects for user to choose from.
  If current buffer is in org-mode, then list projects that this file or subtree
  has already been exported in.
  Else list the last project that has been exported to."
  (let ((exported-projects
         ))
    (if exported-projects
        exported-projects
      (if org-pm-last-chosen-project
          (list org-pm-last-chosen-project)
        nil))))

(defun org-pm-publish-file-or-subtree (deploy-p project &optional subtree-p)
  "Publish current file or subtree to a project chosen from template folder."
  (when project
    (org-pm-save-org-source project subtree-p)
    (org-pm-publish project deploy-p nil)))

(defun org-pm-save-org-source (project-name subtree-p)
  (org-add-option-or-property
   org-pm-registered-projects-property project-name subtree-p)
  (save-buffer)
  (let* ((contents-buffer (current-buffer))
         (contents-path (or (buffer-file-name) (buffer-name)))
         (source-file-path (org-pm-query-source-file-path project-name subtree-p))
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
          (if subdir (insert "#+" pname " " subdir "\n")) ;; TODO: review this
          (org-paste-subtree 1))
      (insert-buffer-substring contents-buffer))
    (save-buffer)
    (kill-buffer)
    (setq org-pm-last-saved-source-path source-file-path)))

;;; Main function

(defun org-pm-publish (project deploy-p force)
  "Publish PROJECT, forcing re-publish of all files if FORCE."
  (let ((org-publish-project-alist (org-pm-create-project-plist project deploy-p))
        (org-export-before-parsing-hook '(org-pm-insert-headers))
        (project-name project))
    (org-publish project force)))

(defun org-pm-create-project-plist (project-name deploy-p)
  "Create org-publish-project-alist with project from template folder.
PROJECT-NAME is the name of the project, and is the same as the folder
that contains the project."
  (list
   (org-pm-make-project-base-plist project-name deploy-p)
   ;; (append
   ;;  (org-pm-make-project-base-plist project-name)
   ;;  (org-pm-get-project-options project-name))
   ))

(defun org-pm-make-project-base-plist (project-name deploy-p)
  (let ((the-list
         (list
          project-name
          :base-directory (org-pm-get-source-dir project-name)
          :publishing-directory (org-pm-get-target-dir project-name)
          :base-extension "org"
          :recursive t
          :publishing-function 'org-html-publish-to-html
          )))
    (if deploy-p
        (setq
         the-list
         (append
          the-list
          '(:completion-function
            (lambda ()
              (org-pm-deploy
               (plist-get
                (cdr (assoc project-name org-publish-project-alist))
                :publishing-directory)))))))
    the-list))

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
For each js or css files in includes directory, construct a HTML_HEAD_EXTRA
string and add it to the top of the org source file for publishing."
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
                ;;
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
  ;; TODO: Also convert relative paths here as in org-pm-make-html-head
  ;; For options such as #+LINK_HOME, #+LINK_UP
  "Create string from OPTIONS file"
  (let ((file-name (file-truename (concat path "/OPTIONS.org"))))
    (if (file-exists-p file-name)
        (with-temp-buffer
          (insert-file-contents file-name)
          (buffer-string))
      "")))

(defun org-pm-make-html-head (path head-type)
  ;; TODO: insert replace-regexp-in-string before last return,
  ;; to convert relative links to root for source files in subfolders.
  ;; Project root can be indicated by {.}
  ;; need extra argument relative-path to replace project root placeholder.
  "Create string with one HTML_HEAD(_EXTRA) per line from file in template folder."
  (let ((file-name (file-truename (concat path "/" head-type ".html"))))
    (if (file-exists-p file-name)
        (with-temp-buffer
          (insert-file-contents file-name)
          (goto-char (point-min))
          (while (re-search-forward "^" nil t)
            (replace-match (concat "#+" head-type ": ")))
          (concat (buffer-string) "\n"))
      "")))

(defun org-pm-deploy (publishing-directory)
  (let ((script
         (car (file-expand-wildcards
               (concat publishing-directory "/DEPLOY/*.sh")))))
    (when script
      (message "Deploying project to site with script:\n%s\n" script)
      (start-process "deploy" "*deploy*" script publishing-directory)
      (message
       "Deployed project to site with script:\n%s\n=====COMPLETED!======\n"
       script))))

(defun org-pm-get-source-file-path (project-name subtree-p)
  "Create path for saving file or subtree to SOURCE dir for export.
  1. Get source file directory based on project name.  Use subfolder
     if that is specified as property for that project in file or subtree.

  2. Get source file name for this project and file or subtree. For this:
     Ask the user to for a file name.
     As default, provide the file name stored for this project in
     this file's or subtree's properties, or if the corresponding property
     is not set, then create a default from the name of the file or section.

  3. Concatenate directory and file name.
  "
  (concat
   (org-pm-get-source-file-dir project-name subtree-p)
   "/"
   (org-pm-make-source-file-name project-name subtree-p)))

(defun org-pm-query-source-file-path (project-name subtree-p)
  ;; TODO: Save new path in property
  (let* ((filename
          (file-name-sans-extension
           (strip-nonalpha
            (read-from-minibuffer
             (format "Save %s as: " (if subtree-p "section" "file"))
             (file-name-sans-extension
              (org-pm-make-source-file-name project-name subtree-p))))))
         (dir (org-pm-get-source-file-dir project-name subtree-p)))
    (org-add-option-or-property
     ;; TODO: construct option name here!
     filename
     subtree-p)
    (concat dir "/" filename ".org")))

(defun strip-nonalpha (input-string)
  (replace-regexp-in-string "[^[:alnum:]]+" "-" input-string))

(defun org-pm-get-source-file-dir (project-name subtree-p)
  "Get the directory to store this file or subtree for export.
  Compute MAINDIR from the root project directory + PROJECT-NAME.
  If a subdirectory property is set for this project and this file
  or subtree, then add it to MAINDIR."
  (let* ((maindir (org-pm-get-source-dir project-name))
         (subdir (org-pm-get-subdir project-name subtree-p))
         (full-dir (if subdir (concat maindir "/" subdir) maindir)))
    (unless (file-exists-p full-dir) (mkdir full-dir t))
    full-dir))

(defun org-pm-get-subdir (project-name subtree-p)
  "Return subdirectory string if present as property for this project."
  (org-pm-get-project-attribute
   project-name org-pm-subdir-property-name subtree-p))

(defun org-pm-make-source-file-name (project-name subtree-p)
  "Return string for saving this file or subtree.
  If corresponding property of file or subtree has a value, return it.
  Otherwise construct string from file name or subtree heading."
  (strip-nonalpha
   (or (org-pm-get-project-attribute project-name "filename" subtree-p)
       (if subtree-p
           (concat
            (strip-nonalpha (org-pm-get-subtree-headline))
            ".org")
         (file-name-nondirectory (buffer-file-name))))))

(defun org-pm-get-target-file-path (project-name subtree-p &optional file-type)
  "Get full path where file/subtree will be exported.
  Used to open that file for viewing (on browser etc)."
  (concat
   (org-pm-get-target-file-dir project-name subtree-p)
   (concat
    (file-name-sans-extension
     (org-pm-make-source-file-name project-name subtree-p))
    (and file-type ".html"))))

(defun org-pm-get-target-file-dir (project-name subtree-p)
  (let* ((maindir (org-pm-get-target-dir project-name))
         (subdir (org-pm-get-subdir project-name subtree-p))
         (full-dir (if subdir (concat maindir "/" subdir) maindir)))
    (unless (file-exists-p full-dir) (mkdir full-dir t))

    ))

(defun org-pm-compose-project-attribute-name (project-name property)
  "Compose property or option name from PROJECT-NAME and PROPERTY."
  (concat "_" project-name "-" property))

(defun org-pm-make-filename-property-name (project)
  (org-pm-compose-project-attribute-name project "filename"))

(defun org-pm-make-subdirectory-property-name (project)
  (org-pm-compose-project-attribute-name project "subdirectory"))

;; The source file name is stored in property _<project-name>-filename
;; The subdirectory is stored in property _<project-name>-subdirectory

(defun org-pm-get-project-attribute (project-name property &optional subtree-p)
  (let ((property-name
         (org-pm-compose-project-attribute-name project-name property)))
    (if subtree-p
        (org-entry-get (point) property-name)
      (org-get-option property-name))))

(defun org-pm-store-source-filename (project-name filename subtree-p)
  (org-add-option-or-property
   (org-pm-make-filename-property-name project-name)
   filename
   subtree-p))

(defun org-pm-store-source-subdirectory (project-name subdir subtree-p)
  (org-add-option-or-property
   (org-pm-make-subdirectory-property-name project-name)
   subdir
   subtree-p))

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

(defun org-pm-get-subtree-headline () (nth 4 (org-heading-components)))

(defun org-get-option (option)
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (let ((found
             (re-search-forward (org-make-options-regexp (list option)) nil t)))
        (if found (match-string-no-properties 2) nil)))))

(defun org-add-option-or-property (option value &optional subtree-p)
  "Add option or property value in buffer.
This is a special case: Option or property items must be separated by spaces.
See also org-set-option-or-property."
  (if subtree-p
      (org-add-property option value)
    (org-add-option option value)))

(defun org-add-option (option value)
  (save-excursion
    (save-restriction
      (widen)
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
          "\n"))))))

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

(defun org-pm-menu-local ()
  "Present org-pm-menu using current file's folder + EXPORTS as project root."
  (interactive)
  (let ((org-pm-root-dir
         (concat
          (file-truename
           (file-name-directory (buffer-file-name)))
          "/EXPORTS")))
    (unless (file-exists-p org-pm-root-dir)
      (message "WILL NOW CREATE %s" org-pm-root-dir)
      (mkdir org-pm-root-dir t))
    (call-interactively 'org-pm-menu)))

(global-set-key (kbd "H-e H-m") 'org-pm-menu-local)

(provide 'org-pm)

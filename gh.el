;;; gh.el ---                                        -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Qiqi Jin

;; Author: Qiqi Jin <ginqi7@gmail.com>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'yaml)
(require 'cl-seq)
(require 'eieio)
(require 'keymap)
(require 'transient)

;;; Define Custom Variables:

(defcustom gh-command (executable-find "gh")
  "The command path for gh.")

(defcustom gh-json-headers
  '("repo-list"
    ("nameWithOwner" "name" "description" "updatedAt" "visibility" "isFork")
    "repo-view"
    ("nameWithOwner" "description" "homepageUrl" "updatedAt" "visibility" "isFork")
    "issue-list"
    ("url" "number" "title" "labels" "updatedAt")
    "my-owned-issue-list"
    ("url" "repository" "number" "title" "labels" "updatedAt")
    "workflow-list"
    ("id" "name" "state")
    "workflow-tasks"
    ("url" "status" "conclusion" "displayTitle" "event"))
  "JSON headers for various commands.")

(defcustom gh-jq-exps
  '("issue-list"
    "[.[] | {url: .url, number: .number, labels: .labels[0].name, title: .title, updatedAt: .updatedAt}]"
    "my-owned-issue-list"
    "[.[] | {url: .url, repository: .repository.nameWithOwner, number: .number, labels: .labels[0].name, title: .title, updatedAt: .updatedAt}]")
  "JQ expressions for various commands.")

(defcustom gh-default-limit
  30
  "The default maximum size of the list.")

(defcustom gh-clone-url-format
  "git@github.com:%s.git"
  "Format string for cloning a repository from GitHub.
You can choose between the SSH format ('git@github.com:%s.git')
and the HTTPS format ('https://github.com/%s.git'). The %s will
be replaced with the repository name."
  :type '(choice
          (const :tag "SSH Format" "git@github.com:%s.git")
          (const :tag "HTTPS Format" "https://github.com/%s.git")))

(defcustom gh-debug nil
  "If logging some intermediate logs.")

(defcustom gh-issue-labels
  '("bug" "documentation" "duplicate" "enhancement" "good first issue" "help wanted" "invalid" "question" "wontfix")
  "Variable for issue labels.")

;;; Define Inner Variables:
(defvar gh--temp-output-file nil "Template file as the command output.")

(defvar-local gh--buffer-comand-name nil "A buffer-local variable for command name.")

(defvar gh--process-success-p t "The success of the GH process")

;;; Define Inner functions:

(defun gh--notify (name data)
  (if (not (featurep 'knockknock))
      (print (format "%s: %s" name data))
    (knockknock-notify :title name
                       :icon (if gh--process-success-p
                                 "cod-check"
                               "cod-error")
                       :message data)
    (unless gh--process-success-p
      (print (format "%s: %s" name data)))))

(defun gh--list-get-value (list-name column-name)
  "Get the cell value by LIST-NAME and COLUMN-NAME in GH List."
  (aref (tabulated-list-get-entry)
        (cl-position
         column-name
         (cdr (plist-get gh-json-headers list-name #'equal))
         :test #'equal)))

(defun gh--hash-render (name table)
  "Render the NAME and hash TABLE."
  (with-current-buffer (get-buffer-create (format "*%s*" name))
    (setq-local gh--buffer-comand-name name)
    (yaml-mode)
    (erase-buffer)
    (mapc (lambda (key)
            (insert (format "%s: %s\n" key (gethash key table))))
          (plist-get gh-json-headers name #'equal))
    (switch-to-buffer (current-buffer))))

(defun gh--text-render (name text)
  "Render the NAME and TEXT."
  (with-current-buffer (get-buffer-create (format "*%s*" name))
    (setq-local gh--buffer-comand-name name)
    (markdown-mode)
    (erase-buffer)
    (insert text)
    (switch-to-buffer (current-buffer))))

(defun gh--completing-read (name data key callback)
  "Completing read by the NAME, DATA and KEY.
Run CALLBACK when finished."
  (let* ((collection (append data nil))
         (selected (completing-read
                    (format "[%s]: " name)
                    (mapcar (lambda (table) (gethash key table))
                            collection))))
    (funcall callback
             (cl-find-if
              (lambda (item) (string= selected (gethash key item)))
              collection))))

(defun gh--list-render (name data)
  "Render the NAME and hash DATA as tabulated-list."
  (let ((project-default-directory default-directory))
    (if (and data
             (not (zerop (length data))))
        (with-current-buffer (get-buffer-create "*gh-list*")
          (setq default-directory project-default-directory)
          (let* ((header (plist-get gh-json-headers name #'equal))
                 (header-widths (gh--header-widths header data)))
            (gh-list-mode)
            (setq tabulated-list-format
                  (vconcat (mapcar
                            (lambda (item)
                              (list item (gethash item header-widths) t))
                            (cdr header))))
            (tabulated-list-init-header)
            (setq tabulated-list-entries
                  (mapcar (lambda (row)
                            (list (gethash (car header)  row)
                                  (vconcat (mapcar
                                            (lambda (key)
                                              (format "%s" (gethash key row)))
                                            (cdr header)))))
                          data))
            (tabulated-list-print)
            (setq-local gh--buffer-comand-name name) ;; Tabulated mode may reset the buffer-local variables, so place this expression at the end.
            (switch-to-buffer (current-buffer))))
      (gh--notify name
                  (format "The data is empty." name)))))

(defun gh--repo-default-name ()
  "Get git repo default name."
  (when default-directory
    (car (last (string-split default-directory "/") 2))))

(defun gh--command-format (name args)
  "Combine the command string with the NAME and ARGS."
  (format "gh %s %s %s &> %s"
          (string-join args " ")
          (gh--json-header-format name)
          (gh--jq-exp-format name)
          gh--temp-output-file))

(defun gh--jq-exp-format (name)
  "Combine the command string with the NAME."
  (let ((jq-exp (plist-get gh-jq-exps name #'equal)))
    (if jq-exp
        (format "--jq '%s'" jq-exp)
      "")))

(defun gh--json-header-format (name)
  "Combine the JSON argument with the NAME."
  (let ((header (plist-get gh-json-headers name #'equal)))
    (if header
        (format "--json %s" (string-join header ","))
      "")))

(defun gh--run (name args callback)
  "Run gh command.
NAME: The command name (string).
ARGS: The command arguments (list).
CALLBACK: The function that executes after the command has completed.
CALLBACK has two parameters:
   NAME: The command name (string).
   DATA: The command output data."
  (unless gh--temp-output-file
    (setq gh--temp-output-file (make-temp-file "gh-output-")))
  (let* ((cmd (gh--command-format name args))
         (log (when gh-debug (print cmd)))
         (proc (start-process-shell-command name nil cmd)))
    (set-process-sentinel proc #'gh--process-sentinel)
    (process-put proc 'callback callback)))

(defun gh--clear-file-contents (file-path)
  "Clear the contents of the file at FILE-PATH."
  (let ((inhibit-message t))  ; Temporarily inhibit messages
    (with-temp-buffer
      (write-region "" nil file-path))))

(defun gh--process-sentinel (process event)
  "The sentinel about gh process.
PROCESS: The gh process.
EVENT: describing a change in the state of the `process`"
  (when (member (process-status process) '(exit))
    (setq gh--process-success-p (string= "finished" (string-trim event)))
    (when-let* ((callback (process-get process 'callback))
                (name (process-name process))
                (data (gh--read-output-file name gh--temp-output-file)))
      (gh--clear-file-contents gh--temp-output-file)
      (funcall callback name data))))

(defun gh--read-output-file (name file)
  "Read the contents of FILE.
NAME is the process name."
  (with-temp-buffer
    (insert-file-contents file)
    (if (plist-get gh-json-headers name #'equal)
        (json-parse-buffer
         :false-object "false")
      (buffer-string))))

(defun gh--header-widths (header data)
  "Calculate the widths of tabulated list headers.
HEADER: Table Header.
DATA: Table data."
  (let ((hash-widths (make-hash-table)))
    (mapc
     (lambda (row)
       (mapc
        (lambda (key)
          (unless (gethash key hash-widths)
            (puthash key (max
                          (length key)
                          (length (format "%s" (gethash key row))))
                     hash-widths))
          (puthash key (max
                        (length (format "%s" (gethash key row)))
                        (gethash key hash-widths))
                   hash-widths))
        header))
     data)
    hash-widths))

(defun gh--repo-create (&optional args)
  "Create a repo by the ARGS."
  (interactive (list (transient-args 'gh-repo-create)))
  (let ((name (read-string "Intput the repo name: "
                           (gh--repo-default-name))))
    (gh--run "repo-create"
             (append (list "repo" "create" name) args)
             #'gh--notify)))

;;; Define Class:

(defclass gh--transient:choices (transient-variable)
  ((choices     :initarg :choices)
   (fallback    :initarg :fallback    :initform nil)
   (default     :initarg :default     :initform nil)))

(cl-defmethod transient-infix-read ((obj gh--transient:choices))
  "Get the next choices value from OBJ."
  (let ((choices (oref obj choices))
        (value (or (oref obj value)
                   (oref obj default))))
    (or (cadr (member value choices))
        (car choices))))

(cl-defmethod transient-infix-value ((obj gh--transient:choices))
  "Get the current choices value from OBJ."
  (let ((default (oref obj default))
        (value (oref obj value)))
    (concat (oref obj argument) (or value default))))

(cl-defmethod transient-format-value ((obj gh--transient:choices))
  "Format the choices from OBJ."
  (let ((value (or (oref obj value)
                   (oref obj default)))
        (choices (oref obj choices)))
    (concat
     (propertize "[" 'face 'transient-inactive-value)
     (mapconcat (lambda (choice)
                  (propertize choice 'face (if (equal choice value)
                                               (if (member choice choices)
                                                   'transient-value
                                                 'font-lock-warning-face)
                                             'transient-inactive-value)))
                choices
                (propertize "|" 'face 'transient-inactive-value))
     (propertize "]" 'face 'transient-inactive-value))))

;;; Define Transient Infix:
(transient-define-infix gh--transient-visibility-option ()
  :description "Visibility of the repository"
  :class 'gh--transient:choices
  :shortarg "-v"
  :argument " "
  :choices '("--public" "--private" "--internal")
  :default "--public")

;;; Define Interactive Functions:
(defun gh-repo-list ()
  "Open the repo list."
  (interactive)
  (gh--run "repo-list" '("repo" "list") #'gh--list-render))

(transient-define-prefix gh-repo-create ()
  "Create a GitHub repository."
  ["Arguments"
   ("v" "Visibility of the repository" gh--transient-visibility-option)
   ("i" "Disable Issue in the new repository" "--disable-issues")
   ("w" "Disable wiki in the new repository" "--disable-wiki")
   ("d" "Description of the repository" "--description "
    :class transient-option
    :reader
    (lambda (prompt initial-input history)
      (format "'%s'" (read-string prompt initial-input history))))
   ("h" "Repository home page URL" "--homepage " :class transient-option)]
  [["Actions"
    ("RET" "Create" gh--repo-create)]])

(defun gh-repo-view (&optional name)
  "View a Github repository by NAME."
  (interactive (list (read-string "repo name: ")))
  (gh--run "repo-view"
           (list "repo" "view" name)
           #'gh--hash-render))

(defun gh-issue-view (&optional number)
  "View a Github issue by NUMBER."
  (interactive (list (read-string "Issue number: ")))
  (gh--run "issue-view"
           (list "issue" "view" number)
           #'gh--text-render))

(defun gh-repo-edit ()
  "Edit and Update the Github repository."
  (interactive)
  (let* ((data (yaml-parse-string
                (buffer-substring-no-properties
                 (point-min)
                 (point-max))
                :null-object nil))
         (nameWithOwner (gethash 'nameWithOwner data))
         (description (gethash 'description data))
         (homepageUrl (gethash 'homepageUrl data))
         (args))
    (when nameWithOwner
      (push nameWithOwner args))
    (when description
      (push (format "--description '%s'" description) args))
    (when homepageUrl
      (push (format "--homepage '%s'" homepageUrl) args))
    (gh--run "repo edit"
             (append (list "repo" "edit")
                     (reverse args))
             #'gh--notify)))

(defun gh-workflow-run ()
  "Run a workflow"
  (interactive)
  (gh--run "workflow-run"
           (list "workflow" "run" (int-to-string (tabulated-list-get-id)))
           #'gh--notify))

(defun gh-repo-list-get-view ()
  "View a repository in the gh list."
  (interactive)
  (gh-repo-view (tabulated-list-get-id)))

(defun gh-issue-list-close ()
  "Close a issue in the gh list."
  (interactive)
  (let ((url (tabulated-list-get-id))
        (title (gh--list-get-value gh--buffer-comand-name "title")))
    (gh-issue-close title url)))

(defun gh-repo-list-clone ()
  "Clone a repo in the gh list."
  (interactive)
  (let ((name (tabulated-list-get-id)))
    (when (yes-or-no-p
           (format "Are you sure you want to Clone the repo: [%s] to [%s]?" name default-directory))
      (gh--run "repo-clone"
               (list "repo" "clone" name)
               #'gh--notify))))

(defun gh-repo-list-delete ()
  "Delete a repo in the gh list."
  (interactive)
  (let ((name (tabulated-list-get-id)))
    (when (yes-or-no-p
           (format "Are you sure you want to delete the repo: [%s]?" name))
      (gh--run "repo-delete"
               (list "repo" "delete" name "--yes")
               #'gh--notify))))

(defun gh-issue-list-delete (command-name)
  "Close a issue in the gh list."
  (interactive)
  (let ((url (tabulated-list-get-id))
        (title (gh--list-get-value command-name "title")))
    (when (yes-or-no-p
           (format "Are you sure you want to delete the issue: [%s: %s]?" title url))
      (gh--run "issue-delete"
               (list "issue" "delete" url "--yes")
               #'gh--notify))))

(defun gh-issue-list-get-view ()
  "View a issue in the gh list."
  (interactive)
  (gh-issue-view (tabulated-list-get-id)))

(defun gh-workflow-list-get-tasks ()
  "View tasks of a workflow in the GitHub list."
  (interactive)
  (gh--run "workflow-tasks"
           (list "run" "list" "--workflow" (int-to-string (tabulated-list-get-id)))
           #'gh--list-render))

(defun gh-repo-list-copy-url ()
  "Copy a repository URL in the gh list."
  (interactive)
  (let ((url (format gh-clone-url-format (tabulated-list-get-id))))
    (gh--notify gh--buffer-comand-name
                (format "Copy %s" url))
    (kill-new url)))

(defun gh-issue-list-copy-url ()
  "Copy a issue URL in the gh list."
  (interactive)
  (let ((url (tabulated-list-get-id)))
    (gh--notify gh--buffer-comand-name
                (format "Copy %s" url))
    (kill-new url)))

(defun gh-repo-list-browse-url ()
  "Browse a repository URL in the gh list."
  (interactive)
  (let ((url (format "https://github.com/%s" (tabulated-list-get-id))))
    (browse-url url)))

(defun gh-issue-list-browse-url ()
  "Browse a issue URL in the gh list."
  (interactive)
  (let ((url (tabulated-list-get-id)))
    (browse-url url)))

(transient-define-prefix gh-issue-create ()
  ""
  ["Arguments"
   ("l" "Add label" "label=" :reader (lambda (prompt initial-input history)
                                       (completing-read prompt gh-issue-labels nil t initial-input history)))
   ("m" "Assignee Me" "assignee=\"@me\"")]
  [("RET" "Create" gh--issue-create)])

(defun gh--parse-transient (args key)
  (when-let ((arg (find-if (lambda (arg) (string-prefix-p (concat key "=") arg)) args)))
    (substring arg (1+ (length key)))))

(defun gh--issue-create (&optional args)
  "Create a Github issue buffer."
  (interactive (list (transient-args 'gh-issue-create)))
  (let ((assignee (gh--parse-transient args "assignee"))
        (label (gh--parse-transient args "label"))
        (project-default-directory default-directory))
    (with-current-buffer (get-buffer-create "*gh-issue-create*")
      (erase-buffer)
      (markdown-mode)
      (gh-edit-mode)
      ;; Setq-local after the mode is set, as the major mode may reset local variables.
      (setq-local default-directory project-default-directory)
      (setq-local gh--buffer-comand-name "issue-create")
      (insert "---\n")
      (insert "title:\n")
      (insert (format "label: %s\n" (or label "")))
      (insert (format "assignee: %s\n" (or assignee "")))
      (insert "milestone:\n")
      (insert "---\n")
      (switch-to-buffer (current-buffer)))))

(defun gh-submit()
  "Submit"
  (interactive)
  (pcase gh--buffer-comand-name
    ("issue-create" (gh-issue-create-submit))))

(defun gh-issue-create-submit ()
  "Submit the create Github issue buffer."
  (interactive)
  (with-current-buffer (get-buffer-create "*gh-issue-create*")
    (goto-char (point-min))
    (let* ((begin (search-forward-regexp markdown-regex-yaml-metadata-border nil t))
           (end (search-forward-regexp (markdown-get-yaml-metadata-end-border nil) nil t))
           (metadata)
           (body)
           (args))
      (when (and begin end)
        (setq metadata
              (yaml-parse-string
               (buffer-substring-no-properties (1+ begin) (- end 3))
               :null-object nil))
        (setq body
              (buffer-substring-no-properties (1+ end) (point-max)))
        (when body
          (puthash "body" body metadata))
        (mapc (lambda (key)
                (when (gethash key metadata)
                  (push (format "--%s '%s'" key (gethash key metadata)) args)))
              (hash-table-keys metadata))
        (gh--run "issue-create"
                 (append '("issue" "create")
                         args)
                 #'gh--notify)))))

(defun gh-issue-list ()
  "Show the Github issue list."
  (interactive)
  (gh--run "issue-list"
           '("issue" "list")
           #'gh--list-render))

(defun gh-issue-list-completing-read (&optional callback)
  "Completing read Github issue."
  (unless callback
    (setq callback #'print))
  (gh--run "issue-list"
           '("issue" "list")
           (lambda (name data)
             (gh--completing-read name data "title" callback))))

(defun gh-issue-close (title url)
  "Close Github issue by TITLE and URL."
  (when (yes-or-no-p
         (format "Do you close the issue? [%s](%s)" title url))
    (gh--run "issue-close"
             (list "issue" "close" url)
             #'gh--notify)))

(defun gh-issue-close-interactive ()
  "Close Github issue interactively."
  (interactive)
  (gh-issue-list-completing-read
   (lambda (data)
     (gh-issue-close
      (gethash "title" data)
      (gethash "url" data)))))

(defun gh-issue-my-owned-list ()
  "Show the my owned Github issue list."
  (interactive)
  (gh--run "my-owned-issue-list"
           '("search" "issues" "--assignee" "@me" "--state" "open")
           #'gh--list-render))

(defun gh-workflow-list ()
  "Show the Github workflow list."
  (interactive)
  (gh--run "workflow-list"
           '("workflow" "list")
           #'gh--list-render))

(transient-define-prefix gh ()
  "A global menu collects all GitHub comments."
  [["Repo"
    ("rc" "Create" gh-repo-create)
    ("rl" "List" gh-repo-list)]
   ["Issue"
    ("ic" "Create" gh-issue-create)
    ("il" "List" gh-issue-list)]])

(transient-define-prefix gh-workflow-list-actions ()
  "Actions in workflow-list"
  ["Actions in workflow-list"
   ("t" "Tasks" gh-workflow-list-get-tasks)
   ("r" "Run workflow" gh-workflow-run)])

(transient-define-prefix gh-issue-list-actions ()
  "Actions in issue-list"
  ["Actions in issue-list"
   ("b" "Browse URL" gh-issue-list-browse-url)
   ("c" "Copy URL" gh-issue-list-copy-url)
   ("C" "Close Issue" gh-issue-list-close)
   ("d" "Delete Issue" gh-issue-list-delete)
   ("v" "View Issue" gh-issue-list-get-view)])

(transient-define-prefix gh-repo-list-actions ()
  "Actions in repo-list"
  ["Actions in repo-list"
   ("b" "Browse URL" gh-repo-list-browse-url)
   ("c" "Copy URL" gh-repo-list-copy-url)
   ("C" "Clone Repo" gh-repo-list-clone)
   ("d" "Delete Repo" gh-repo-list-delete)
   ("v" "View Repo" gh-repo-list-get-view)])

(defun gh-list-actions ()
  "Actions in gh list."
  (interactive)
  (pcase gh--buffer-comand-name
    ("repo-list" (gh-repo-list-actions))
    ((or "issue-list" "my-owned-issue-list") (gh-issue-list-actions))
    ("workflow-list" (gh-workflow-list-actions))))

;;; Define mode:
(define-derived-mode gh-list-mode tabulated-list-mode "gh-list"
  "A minor list mode about the gh."
  (keymap-set gh-list-mode-map "RET" 'gh-list-actions))

(define-minor-mode gh-edit-mode
  "gh Edit mode."
  :init-value nil
  :lighter "gh E"
  :global nil
  :keymap
  (let ((map (make-sparse-keymap)))
    (keymap-set map "C-c C-c" 'gh-submit)
    map))

(provide 'gh)
;;; gh.el ends here

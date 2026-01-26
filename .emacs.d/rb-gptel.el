;;; rb-gptel.el --- RB's gptel extensions -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Helper utilities for gptel.

;;; Code:

(require 'gptel)

(require 'rb-clojure)
(require 'rb-elisp)
(require 'rb-bash)
(require 'rb-ts)
(require 'rb-file)
(require 'rb-io)
(require 'rb-rg)
(require 'rb-fd)
(require 'rb-object-store)

(defun rb-gptel-mark-user-response-regions (_info)
  "In current prompt buffer, mark regions between @rb/@ai lines for gptel.

Everything from an @rb line (inclusive of its following text) up to the
next @ai is \"user\" (no gptel response prop).  Everything from an @ai
line up to the next @rb is \"assistant\" (gets the `gptel response'
prop).  Markers are stripped before sending."
  (save-excursion
    ;; Clear old props
    (remove-text-properties (point-min) (point-max)
                            '(gptel nil front-sticky nil))
    (goto-char (point-min))
    (let (segments)
      ;; Collect markers (pos . role)
      (while (re-search-forward "^@\\(rb\\|ai\\)\\s-+" nil t)
        (let ((role (match-string 1))
              (pos (match-beginning 0)))
          (push (cons pos role) segments)))
      (setq segments (nreverse segments))
      ;; Walk segments, apply props to spans
      (while segments
        (pcase-let* ((`(,start-pos . ,role) (pop segments))
                     (end-pos (or (car (car segments)) (point-max))))
          (when (string= role "ai")
            (add-text-properties
             start-pos end-pos
             '(gptel response front-sticky (gptel)))))))))

(defun rb-gptel-read-file (path &optional with-line-numbers)
  "Read PATH from disk and return as a string.
If WITH-LINE-NUMBERS is true, prefix each line with its number."
  (let ((content (rb-io-read-file path)))
    (if with-line-numbers
        (rb-tools-with-line-numbers content)
      content)))

(defun rb-gptel-sanitize-arg (arg)
  "Return sanitized ARG of gptel tool call."
  (pcase arg
    ((or :null :json-false) nil)
    ((or (pred listp)
         (pred vectorp)) (mapcar #'rb-gptel-sanitize-arg arg))
    (x x)))

(defun rb-gptel-make-tool (&rest params)
  "Invoke `gptel-make-tool' with PARAMS and a :function which sanitizes its args.

The sanitizer recursively converts all :null and :json-false values in
actual tool arguments to nil."
  (let ((f (plist-get params :function)))
    (plist-put params :function (lambda (&rest args)
                                  (apply f (mapcar #'rb-gptel-sanitize-arg args))))
    (apply #'gptel-make-tool params)))

;;; Tools -----------------------------------------------------------------

(rb-gptel-make-tool
 :name "clojure_eval"
 :category "rb"
 :description (documentation 'rb-clojure-eval)
 :function #'rb-clojure-eval
 :args
 '(( :name "input"
     :type string
     :description "The form to evaluate in the context of the connected Clojure/ClojureScript app.")
   ( :name "ns"
     :type string
     :description "The Clojure/ClojureScript namespace in which the form should be evaluated."))
 :confirm t
 :include t)

(rb-gptel-make-tool
 :name "elisp_eval"
 :category "rb"
 :description (documentation 'rb-elisp-eval)
 :function #'rb-elisp-eval
 :args
 '(( :name "input"
     :type string
     :description "The Emacs Lisp expression to evaluate inside the connected Emacs instance."))
 :confirm t
 :include t)

(rb-gptel-make-tool
 :name "bash_eval"
 :category "rb"
 :description (documentation 'rb-bash-eval)
 :function #'rb-bash-eval
 :args
 '(( :name "script"
     :type string
     :description "The Bash script to evaluate via `bash -c`."))
 :confirm t
 :include t)

(rb-gptel-make-tool
 :name "tree_sitter_list_nodes"
 :category "rb"
 :description (documentation 'rb-ts-list-nodes)
 :function #'rb-ts-list-nodes
 :args
 '(( :name "path"
     :type string
     :description "Path to the source code file.")
   ( :name "preview_lines"
     :type integer
     :description "Number of lines to include in the preview (defaults to 1)."
     :optional t))
 :include t)

(rb-gptel-make-tool
 :name "tree_sitter_get_nodes"
 :category "rb"
 :description (documentation 'rb-ts-get-nodes)
 :function #'rb-ts-get-nodes
 :args
 '(( :name "path"
     :type string
     :description "Path to the source code file.")
   ( :name "line_numbers"
     :type array
     :items (:type integer)
     :description "List of line numbers referencing the start of each target node.")
   )
 :include t)

(rb-gptel-make-tool
 :name "tree_sitter_update_nodes"
 :category "rb"
 :description (documentation 'rb-ts-update-nodes)
 :function #'rb-ts-update-nodes
 :args
 '(( :name "path"
     :type string
     :description "Path to the source code file.")
   ( :name "nodes"
     :type array
     :items ( :type object
              :properties ( :index (:type integer)
                            :kind (:type string)
                            :line (:type integer)
                            :hash (:type string)
                            :new_text (:type string)))
     :description "List of nodes to update.")
   ( :name "skip_format"
     :type boolean
     :description "If true, do not format/reindent the new node."
     :optional t)
   ( :name "dry_run"
     :type boolean
     :description "If true, do not modify the file, just report what would be done."
     :optional t))
 :confirm t
 :include t)

(rb-gptel-make-tool
 :name "tree_sitter_insert_before_node"
 :category "rb"
 :description (documentation 'rb-ts-insert-before-node)
 :function #'rb-ts-insert-before-node
 :args
 '(( :name "path"
     :type string
     :description "Path to the source code file.")
   ( :name "selector"
     :type object
     :properties ( :line (:type integer :description "Starting line of the node." :optional t)
                   :hash (:type string :description "MD5 hash of the node text." :optional t))
     :description "Node selector by :line and/or :hash.")
   ( :name "new_text"
     :type string
     :description "Text to insert before the node.")
   ( :name "skip_format"
     :type boolean
     :description "If true, do not format/reindent the inserted text."
     :optional t))
 :confirm t)

(rb-gptel-make-tool
 :name "tree_sitter_insert_after_node"
 :category "rb"
 :description (documentation 'rb-ts-insert-after-node)
 :function #'rb-ts-insert-after-node
 :args
 '(( :name "path"
     :type string
     :description "Path to the source code file.")
   ( :name "selector"
     :type object
     :properties ( :line (:type integer :description "Starting line of the node." :optional t)
                   :hash (:type string :description "MD5 hash of the node text." :optional t))
     :description "Node selector by :line and/or :hash.")
   ( :name "new_text"
     :type string
     :description "Text to insert after the node.")
   ( :name "skip_format"
     :type boolean
     :description "If true, do not format/reindent the inserted text."
     :optional t))
 :confirm t)

(rb-gptel-make-tool
 :name "insert_before_regex"
 :category "rb"
 :description (documentation 'rb-file-insert-before-regex)
 :function #'rb-file-insert-before-regex
 :args
 '(( :name "path"
     :type string
     :description "Path to the source code file.")
   ( :name "regex"
     :type string
     :description "Regex marker to search for using ripgrep.")
   ( :name "new_text"
     :type string
     :description "Text to insert before the matching line.")
   ( :name "line"
     :type integer
     :description "Anchor line when multiple matches exist."
     :optional t)
   ( :name "validate"
     :type boolean
     :description "When true, re-parse the buffer with Tree-sitter before writing."
     :optional t))
 :confirm t)

(rb-gptel-make-tool
 :name "insert_after_regex"
 :category "rb"
 :description (documentation 'rb-file-insert-after-regex)
 :function #'rb-file-insert-after-regex
 :args
 '(( :name "path"
     :type string
     :description "Path to the source code file.")
   ( :name "regex"
     :type string
     :description "Regex marker to search for using ripgrep.")
   ( :name "new_text"
     :type string
     :description "Text to insert after the matching line.")
   ( :name "line"
     :type integer
     :description "Anchor line when multiple matches exist."
     :optional t)
   ( :name "validate"
     :type boolean
     :description "When true, re-parse the buffer with Tree-sitter before writing."
     :optional t))
 :confirm t)

(rb-gptel-make-tool
 :name "get_line_ranges"
 :category "rb"
 :description (documentation 'rb-file-get-line-ranges)
 :function #'rb-file-get-line-ranges
 :args
 '(( :name "path"
     :type string
     :description "Path to the source code file.")
   ( :name "ranges"
     :type array
     :items ( :type object
              :properties ( :start (:type integer)
                            :end (:type integer)))
     :description "List of line ranges to retrieve (1-based, inclusive)."))
 :include t)

(rb-gptel-make-tool
 :name "update_line_ranges"
 :category "rb"
 :description (documentation 'rb-file-update-line-ranges)
 :function #'rb-file-update-line-ranges
 :args
 '(( :name "path"
     :type string
     :description "Path to the source code file.")
   ( :name "ranges"
     :type array
     :items ( :type object
              :properties ( :start (:type integer)
                            :end (:type integer)
                            :hash (:type string :description "MD5 hash of current text block between :start and :end.")
                            :new_text (:type string :description "Replacement text.")))
     :description "List of line ranges to replace with verification."))
 :confirm t)

(rb-gptel-make-tool
 :name "read_file"
 :category "rb"
 :description (documentation 'rb-gptel-read-file)
 :function #'rb-gptel-read-file
 :args
 '(( :name "path"
     :type string
     :description "Path of the file to read.")
   ( :name "with_line_numbers"
     :type boolean
     :description "If true, include line numbers."
     :optional t))
 :confirm t
 :include t)

(rb-gptel-make-tool
 :name "write_file"
 :category "rb"
 :description (documentation 'rb-io-write-file)
 :function #'rb-io-write-file
 :args
 '(( :name "path"
     :type string
     :description "Path of the file to write.")
   ( :name "content"
     :type string
     :description "New content of the file."))
 :confirm t)

(rb-gptel-make-tool
 :name "rg"
 :category "rb"
 :description (documentation 'rb-rg)
 :function #'rb-rg
 :args
 '(( :name "pattern"
     :type string
     :description "Regex pattern to search for.")
   ( :name "directory"
     :type string
     :description "Directory to search (defaults to current directory)."
     :optional t)
   ( :name "extra_args"
     :type string
     :description "Additional args to pass to rg, e.g. \"-g *.el --hidden\"."
     :optional t))
 :include t)

(rb-gptel-make-tool
 :name "fd"
 :category "rb"
 :description (documentation 'rb-fd)
 :function #'rb-fd
 :args
 '(( :name "query"
     :type string
     :description "Pattern to search for.")
   ( :name "directory"
     :type string
     :description "Directory to search (defaults to current directory)."
     :optional t)
   ( :name "extra_args"
     :type string
     :description "Additional args to pass to fd, e.g. \"--type f --max-depth 3\"."
     :optional t))
 :include t)

(rb-gptel-make-tool
 :name "get_json_schema_for_class"
 :category "rb"
 :description (documentation 'rb-object-store-get-json-schema-for-class)
 :function #'rb-object-store-get-json-schema-for-class
 :args
 '(( :name "class"
     :type string
     :description "Name of the class, an uppercase string."))
 :include t)

(rb-gptel-make-tool
 :name "insert_object"
 :category "rb"
 :description (documentation 'rb-object-store-insert-object)
 :function #'rb-object-store-insert-object
 :args '(( :name "class"
           :type string
           :description "Object class, uppercase string.")
         ( :name "id"
           :type string
           :description "Object identifier, unique within a class.")
         ( :name "properties"
           :type array
           :items ( :type object
                    :properties ( :name (:type string)
                                  :value (:type string)))
           :description "Additional properties as name/value pairs."
           :optional t))
 :include t)

(rb-gptel-make-tool
 :name "get_object"
 :category "rb"
 :description (documentation 'rb-object-store-get-object)
 :function #'rb-object-store-get-object
 :args '(( :name "class"
           :type string
           :description "Object class, uppercase string.")
         ( :name "id"
           :type string
           :description "Object identifier, unique within a class.")
         ( :name "properties"
           :type array
           :items (:type string)
           :description "Optional list of property names to return; if omitted, all properties are returned."
           :optional t))
 :include t)

(rb-gptel-make-tool
 :name "update_object"
 :category "rb"
 :description (documentation 'rb-object-store-update-object)
 :function #'rb-object-store-update-object
 :args '(( :name "class"
           :type string
           :description "Object class, uppercase string.")
         ( :name "id"
           :type string
           :description "Object identifier, unique within a class.")
         ( :name "properties"
           :type array
           :items ( :type object
                    :properties ( :name (:type string)
                                  :value (:type string)))
           :description "Properties to update as name/value pairs."))
 :include t)

(rb-gptel-make-tool
 :name "find_objects"
 :category "rb"
 :description (documentation 'rb-object-store-find-objects)
 :function #'rb-object-store-find-objects
 :args '(( :name "class"
           :type string
           :description "Object class, uppercase string.")
         ( :name "id"
           :type string
           :description "Optional object identifier regex to restrict search."
           :optional t)
         ( :name "properties"
           :type array
           :items ( :type object
                    :properties ( :name (:type string)
                                  :value (:type string)))
           :description "List of property name/regex pairs to match content."
           :optional t))
 :include t)

;;; Presets ---------------------------------------------------------------

(defun rb-gptel--all-active-major-modes ()
  "Return a list of all major modes currently used by any live buffer."
  (let (modes)
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (push major-mode modes)))
    (delete-dups modes)))

(defun rb-gptel--preset-tools-dev ()
  "Build tools for the @dev preset."
  (let ((tools '("elisp_eval" "bash_eval"))
        (modes (rb-gptel--all-active-major-modes)))
    (when (memq 'clojure-mode modes)
      (push "clojure_eval" tools))
    tools))

(gptel-make-preset 'dev
  :description "Development preset - only used as a base for others."
  :system "You play the role of an agent who helps me develop software.

You are inside a project folder and we are conversing via gptel in Emacs.

_Some notes regarding the nature of our collaboration_

I am a dreamer by nature and I easily get consumed by exciting
ideas. You should be like a father figure who provides the necessary
contrast to this dreamer, who curbs the exuberance if necessary, such
that the project under development has a better chance to succeed.

_General instructions_

1. If something is not clear, ask for clarification.

2. Never touch the =project.org= file, that is used to store our
   conversations.

3. The tools which you are using have been mostly developed by me. They
   may be buggy. If a tool does work as advertised, stop immediately and
   report the problem so that I have a chance to fix it.

_Development guidelines_

1. Follow the philosophy of `functional core, imperative shell'

2. When it is feasible, write tests first.

"
  :tools '(:eval (rb-gptel--preset-tools-dev))
  :temperature 0.1
  :prompt-transform-functions '(:prepend (rb-gptel-mark-user-response-regions)))

(gptel-make-preset 'repo-reader
  :description "Grants read access to all files in the Git repository."
  :tools '(:append ("rg" "fd" "read_file" "get_line_ranges"))
  :system '(:append "To explore the repository, use the following tools:

- =fd= for getting a recursive listing of all files in the project
- =rg= for finding files with lines matching a certain regex
- =read_file= to slurp an entire file into context
- =get_line_ranges= to load parts of a file into context

"))

(gptel-make-preset 'repo-editor
  :description "Grants write access to all files in the Git repository."
  :parents '(repo-reader)
  :tools '(:append ("write_file" "update_line_ranges"))
  :system '(:append "To update a range of lines in a text file:

1. If it is a source code file, use the tree_sitter tools.

2. If it is a plain text file, use =get_line_ranges= to fetch the
content and text hash for the ranges you want to update, then call
=update_line_ranges= with the hashes and the replacements.

"))

(gptel-make-preset 'store-reader
  :description "Grants read access to the object store."
  :tools '(:append ("get_json_schema_for_class" "get_object" "find_objects"))
  :system '(:append "You have access to an object store where we store objects of classes
like STORY or TASK.

Currently the following classes are available:

- STORY: user stories
- TASK: implementation tasks

Before you do anything with objects of a certain class, invoke
=get_json_schema_for_class= to get a description of the class and the
JSON schema for its objects.

"))

(gptel-make-preset 'store-editor
  :description "Grants write access to the object store."
  :parents '(store-reader)
  :tools '(:append ("insert_object" "update_object"))
  :system '(:append "Occasionally I will ask you to create an object of some class.

When this happens, I may or may not explicitly state the object id you
should use. If I do not supply you with an explicit object id, use the
=find_objects= tool to list all objects of the class, determine the
highest id, increment by one and add a slug which feels adequate.

"))

(gptel-make-preset 'code-reader
  :description "Grants read access to source code files."
  :tools '(:append ("tree_sitter_list_nodes" "tree_sitter_get_nodes" "get_line_ranges" "read_file"))
  :system '(:append "_Guidelines on analyzing source code files_

*If you want to analyze a source code file, use the following tools:*

1. tree_sitter_list_nodes: Gives you the name, type signature and
   starting line number of all function and variable definitions in the
   file.

2. tree_sitter_get_nodes: If you have starting line numbers, this tool
   translates them to the complete text of the AST nodes which start on
   those lines

3. get_line_ranges: Loads line ranges of a file into the context.

4. read_file: Slurps the whole file into the context, with optional line
   numbers. Use this as a last resort as it may consume a lot of
   context.

"))

(gptel-make-preset 'code-editor
  :description "Grants write access to source code files."
  :parents '(code-reader)
  :tools '(:append ("tree_sitter_update_nodes"
                    "tree_sitter_insert_before_node"
                    "tree_sitter_insert_after_node"
                    "write_file"))
  :system '(:append "_Guidelines on changing source code files_

*If you want to change something in a source code file:*

1. Use =tree_sitter_list_nodes= and =tree_sitter_get_nodes= to get the
   current state.

2a. Use =tree_sitter_update_nodes= to update or remove desired AST nodes.

2b. Use =tree_sitter_insert_before_node= or
    =tree_sitter_insert_after_node= to insert new AST nodes.

"))

(gptel-make-preset 'plan
  :description "Used to plan stories and write tasks."
  :parents '(dev repo-editor code-reader store-editor)
  :system '(:append "Your job is to help me plan a STORY and write its TASKS.

_Description of the planning workflow_

We will enter into a dialogue about a new feature or idea I have in
mind. Your task is to help me flesh out the details. When I feel that we
are ready, I will ask you to write a STORY and break it down to TASKs.

The STORY and TASKs you create will be used for implementation so they
should be sharp and crystal clear. If you feel that you do not have
enough information to guarantee this, stop and ask for clarification. We
shall continue this feedback loop until you are satisfied.

In the current stage, writing the code is out of scope: your only job is
to prepare the story and its tasks. Implementation will be done by a
coding agent in a later stage.

The planning workflow has the following outcomes:

- STORY created, description and implementation_plan fleshed out
- TASKS created, linked to parent STORY, descriptions filled

TASKS break down the implementation plan into individual steps. Each
task should be implementable by an independent agent who has access to
both the TASK and its STORY.

All planning must be done upfront. There are no planning TASKS.

"))

(gptel-make-preset 'code
  :description "Used to implement tasks."
  :parents '(dev repo-editor code-editor store-editor)
  :system '(:append "Your job is to implement a TASK.

_Description of the implementation workflow_

1. I provide you with the id of the TASK which should be implemented.

2. You retrieve the TASK and the corresponding STORY from the object
   store, analyze them, understand what to do.  If something is not
   clear, you stop and ask for clarification.

3. You implement the TASK.

4. You populate the =review= field of the TASK object with information
   about the result: what went well, what had to be changed, what we
   learnt, what reality taught us.

"))

(gptel-make-preset 'review
  :description "Used to review stories."
  :parents '(dev repo-editor code-reader store-editor)
  :system '(:append "Your job is to review a story based on the individual reviews of its tasks.

_Description of the review workflow_

1. I give you a STORY id.
2. You collect all TASKS of the STORY, extract their =review= fields.
3. Analyze the individual reviews and write a summary into the =review= field of the STORY.

"))

(provide 'rb-gptel)
;;; rb-gptel.el ends here

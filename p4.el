;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; p4.el --- Simple description
;;
;; Copyright (C) 2011-2013, Yang, Ying-chao
;;
;; Author:        Yang, Ying-chao <yangyingchao@gmail.com>
;;
;; This file is NOT part of GNU Emacs.
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
;;
;; Commentary:
;;
;; Wrapper of P4 command line client.
;; Some variables should be set before using functions provided by
;; this file, including:
;;    P4USER, P4PASSWD, P4CLIENT --- refer to p4 help for more description.
;;    P4OMITS --- some special tokens, if one of these tokens appeared in a
;;                sync path, then this path will be overwrite if it is not
;;                clobbered. Should separated by ":".
;;
;; This is written to use git for managing files, so lots of p4 commands are removed.
;;
;; Lots of code is merged from https://github.com/fujii/p4el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; -*- emacs-lisp -*- -*- coding: utf-8; -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; $Id: p4.el, 04-16-2012

 ;; Initiate of this package.

(require 'magit )

(defgroup p4 nil
  "p4 interface for Emacs."
  :group 'tools)

(defgroup p4-faces nil
  "psvn faces."
  :group 'p4)

(defcustom p4user nil
  "Username."
   :group 'p4)

(defcustom p4passwd nil
  "Password."
   :group 'p4)

(defcustom p4client nil
  "Client work space."
  :group 'p4)

(defcustom p4host nil
  "Host of p4 client."
  :group 'p4
  )
(defcustom p4name nil
  "Host of p4 client."
  :group 'p4)

(defcustom p4port nil
  "Host of p4 client."
  :group 'p4)

(defconst p4-command-prefix
  (format "source %s && "
          (expand-file-name "~/.emacs.d/site-lisp/version-control/p4/p4utils.sh"))
  "Prefix of p4-command.")

(defcustom p4-r-match-branch-name nil
  "Regular expression which should be provided by user.
Example: (rx (or \"main\" (: \"feature_\" (= 6 digit))))"
  :group 'p4)

(defcustom p4-max-search-depth 6
  "Max depth of searching path, used to optimize search when sync-all"
  :group 'p4)

(defvar p4-root nil
  "Host of p4 client.")

;; TODO: Hide following consts using (let ...)
(defconst p4-r-match-client-name
  (rx "Client name:" (* blank) (group (+ nonl)))
  "Regular expression to match client name")

(defconst p4-r-match-client-root
  (rx "Client root:" (* blank) (group (+ nonl)))
  "Regular expression to match client root")

(defconst p4-buffer (get-buffer-create "*P4-Progress*"))

(defvar p4-cmd-counter 0 "Counter of how many commands have been executed.")

(defvar p4-cmd-executing nil
  "Flag to indicate if p4-cmd-processor is running")

(defvar p4-pending-cmds nil  "List of commands that need to be processed")
(defvar p4-confilict-detected nil "status")
(defvar p4-r-match-max-depth nil "nil")
(defvar p4-cmd-starttime 0 "Start timestamp of a command")
(defvar p4-logged-in nil "Status flag of whether user logged in.")

(defvar p4-running-emacs nil
  "If the current Emacs is not XEmacs, then, this is non-nil.")
(defvar p4-running-xemacs nil
  "If the current Emacs is XEmacs/Lucid, then, this is non-nil.")
(if (string-match "XEmacs\\|Lucid" emacs-version)
    (setq p4-running-xemacs t)
  (setq p4-running-emacs t))

(defvar p4-window-config-stack nil
  "Stack of saved window configurations.")

(defvar p4-cl-template nil
  "Template used to create change list.
This variable get set when p4-get-changeList is first called.")

(defvar p4-last-output "" "Last output string.")
(defvar p4-error-counter 0 "Error counter.")

(defcustom p4-window-config-stack-size 20 "Maximum stack size
for saved window configurations."
  :type 'integer
  :group 'p4)

(defconst p4-r-match-unknown-cl
  (rx bol "Change" (+ blank) (+ digit) (+ blank) "unknown." eol))

(defconst p4-r-match-none-local-file
  (rx (group (+? ascii)) " - file(s) not on client."))

(defconst p4-r-match-depot-file (rx  "//depot" (group (+? ascii)) (? "#" (+ digit)) eol))

(defconst p4-r-match-changes (rx "Change" (+ blank) (group (+ digit)) (+ blank) "on" (+ nonl) (repeat 2 "
") (group (+? ascii))  (repeat 2 "
")))

(defconst p4-r-match-affected-file
  (rx bol "..." (+ blank)
      (group "//" (+ nonl) "#" digit) blank
      (group (or "add" "edit" "delete" "move/add" "move/delete" "branch" "integrate"))
      (* blank) eol))


(defun p4-make-derived-map (base-map)
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map base-map)
    map))


(defvar p4-basic-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map t)
    (define-key map [mouse-2] 'p4-buffer-mouse-clicked)
    (define-key map [mouse-3] 'p4-buffer-mouse-clicked-3)
    (define-key map "\t" 'p4-forward-active-link)
    (define-key map "\e\t" 'p4-backward-active-link)
    (define-key map [(shift tab)] 'p4-backward-active-link)
    (define-key map "\C-m" 'p4-buffer-commands)
    (define-key map "q"	 'p4-quit-current-buffer)
    (define-key map "n"	 'next-line)
    (define-key map "p"	 'previous-line)
    (define-key map [backspace] 'p4-scroll-down-1-window)
    (define-key map " "	 'p4-scroll-up-1-window)
    (define-key map "<"	 'p4-top-of-buffer)
    (define-key map ">"	 'p4-bottom-of-buffer)
    (define-key map "="	 'p4-delete-other-windows)
    map))

(defvar p4-log-mode-map
  (let ((map (p4-make-derived-map p4-basic-mode-map)))
    (define-key map "d"	 'p4-diff2)
    (define-key map "E"	 'p4-ediff2)
    (define-key map "s"	 'p4-log-short-format)
    (define-key map "l"	 'p4-log-long-format)
    (define-key map "k"	 'p4-scroll-down-1-line-other-w)
    (define-key map "j"	 'p4-scroll-up-1-line-other-w)
    (define-key map "b"	 'p4-scroll-down-1-window-other-w)
    (define-key map [backspace] 'p4-scroll-down-1-window-other-w)
    (define-key map " "	 'p4-scroll-up-1-window-other-w)
    (define-key map "<"	 'p4-top-of-buffer-other-w)
    (define-key map ">"	 'p4-bottom-of-buffer-other-w)
    (define-key map "="	 'p4-delete-other-windows)
    (define-key map "n"	 'p4-goto-next-change)
    (define-key map "p"	 'p4-goto-prev-change)
    (define-key map "N" (lookup-key map "p"))
    map)
  "The key map to use for selecting filelog properties.")

(defvar p4-diff-mode-map
  (let ((map (p4-make-derived-map p4-basic-mode-map)))
    (define-key map "n"	 'p4-goto-next-diff)
    (define-key map "p"	 'p4-goto-prev-diff)
    (define-key map "N" (lookup-key map "p"))
    (define-key map "d"	 'p4-next-depot-diff)
    (define-key map "u"	 'p4-prev-depot-diff)
    map))

(defvar p4-print-rev-mode-map
  (let ((map (p4-make-derived-map p4-basic-mode-map)))
    (define-key map "n"	 'p4-next-change-rev-line)
    (define-key map "p"	 'p4-prev-change-rev-line)
    (define-key map "N" (lookup-key map "p"))
    (define-key map "l"	 'p4-toggle-line-wrap)
    map)
  "The key map to use for browsing print-revs buffers.")

(defvar p4-changeList-table (make-hash-table :test 'equal) "Hash table of change lists.")

(define-derived-mode p4-basic-mode nil "P4 Basic")
(define-derived-mode p4-diff-mode p4-basic-mode "P4 Diff")
(define-derived-mode p4-log-mode p4-basic-mode "P4 File Log")
(define-derived-mode p4-print-rev-mode p4-basic-mode "P4 Print Rev")


;;Private functions.

(eval-and-compile
  (defvar p4-include-help-to-command-docstring
    (let (val)
      (eval-when (compile) (setq val t))
      val))

  (defun p4-help-text (cmd text)
    (concat text
	    (with-temp-buffer
	      (if (and p4-include-help-to-command-docstring
		       (p4-get-executable)
		       (zerop (call-process (p4-get-executable) nil t nil "help" cmd)))
		  (buffer-substring (point-min) (point-max))
		"")))))

(defun p4-concat-string-array (array &optional sep)
  "Concat string array"
  (when (listp array)
    (mapconcat (lambda (x) (identity x)) array sep)))

(defun p4-normalize-path (path)
  "Normalize path, replace '\\' with '/'"
  (let ((newpath path))
    (when (and path
               (or (string= system-type "windows-nt")
                   (string= system-type "ms-dos")))
      (let ((subpath (split-string path "\\\\")))
        (setq newpath (p4-concat-string-array subpath "/"))))
    newpath))

(defun p4-denormalize-path (path)
  "DeNormalize path, replace '/' with '\\' if on windows"
  (let ((newpath path))
    (when (and path
               (or (string= system-type "windows-nt")
                   (string= system-type "ms-dos")))
      (let ((subpath (split-string path "/")))
        (setq newpath (p4-concat-string-array subpath "\\"))))
    newpath))

(defmacro defp4cmd (fkn &rest all-args)
  (let ((args (car all-args))
	(help-cmd (cadr all-args))
	(help-txt (eval (cadr (cdr all-args))))
	(body (cdr (cddr all-args))))
    `(defalias ',fkn
       ,(append (list 'lambda args
		      (p4-help-text help-cmd help-txt))
		body))))

(defun p4-make-output-buffer (buffer-name &optional mode &optional initString)
  "Make read only buffer and return the buffer."
  (let ((dir default-directory)
	(inhibit-read-only t))
    (with-current-buffer (get-buffer-create buffer-name)
      (erase-buffer)
      (funcall (or mode 'p4-basic-mode))
      (if initString  (insert initString))
      (setq buffer-read-only t)
      (setq buffer-undo-list t)
      (cd dir)
      (current-buffer))))

(defun p4-set-extent-properties (start end prop-list)
  (let ((ext (make-overlay start end)))
    (while prop-list
      (overlay-put ext (caar prop-list) (cdar prop-list))
      (setq prop-list (cdr prop-list)))))

(defun p4-create-active-link (start end prop-list)
  (p4-set-extent-properties start end
			    (append (list (cons 'face 'bold)
					  (cons 'mouse-face 'highlight))
				    prop-list)))

(defun p4-forward-active-link ()
  (interactive)
  (while (and (not (eobp))
	      (goto-char (next-overlay-change (point)))
	      (not (get-char-property (point) 'face)))))

(defun p4-backward-active-link ()
  (interactive)
  (while (and (not (bobp))
	      (goto-char (previous-overlay-change (point)))
	      (not (get-char-property (point) 'face)))))

(defun p4-move-buffer-point-to-top (buf-name)
  (if (get-buffer-window buf-name)
      (save-selected-window
	(select-window (get-buffer-window buf-name))
	(goto-char (point-min)))))

;; Scan specified region for references to change numbers
;; and make the change numbers clickable.
(defun p4-find-change-numbers (buffer start end)
  (save-excursion
    (set-buffer buffer)
    (goto-char start)
    (while (re-search-forward "\\(changes?\\|submit\\|p4\\)[:#]?[ \t\n]+" end t)
      (while (looking-at
              (concat "\\([#@]\\|number\\|no\\.\\|\\)[ \t\n]*"
                      "\\([0-9]+\\)[-, \t\n]*"
                      "\\(and/or\\|and\\|&\\|or\\|\\)[ \t\n]*"))
        (let ((ch-start (match-beginning 2))
              (ch-end (match-end 2))
              (ch-str (match-string 2))
              (next (match-end 0)))
          (set-text-properties 0 (length ch-str) nil ch-str)
          (p4-create-active-link ch-start ch-end (list (cons 'change ch-str)))
          (goto-char next))))))


(defun p4-activate-file-change-log-buffer (buffer)
  (with-current-buffer buffer
    (let ((p4-r-match-log-entry (concat
				 "^\\(\\.\\.\\. #\\([0-9]+\\) \\)?[Cc]hange "
				 "\\([0-9]+\\) \\([a-z]+\\)?.*on.*by "
				 "\\([^ @]+\\)@\\([^ \n]+\\).*\n"
				 "\\(\\(\\([ \t].*\\)?\n\\)*\\)"))
          p4-cur-rev p4-cur-change p4-cur-action
          p4-cur-user p4-cur-client)
      (p4-mark-print-buffer buffer nil)
      (goto-char (point-min))
      (while (re-search-forward p4-r-match-log-entry nil t)
	(let ((rev-match 2)
	      (ch-match 3)
	      (act-match 4)
	      (user-match 5)
	      (cl-match 6)
	      (desc-match 7))
	  (setq p4-cur-rev (match-string rev-match))
	  (setq p4-cur-change (match-string ch-match))
	  (setq p4-cur-action (match-string act-match))
	  (setq p4-cur-user (match-string user-match))
	  (setq p4-cur-client (match-string cl-match))

	  (if (match-beginning rev-match)
	      (p4-create-active-link (match-beginning rev-match)
				     (match-end rev-match)
				     (list (cons 'rev p4-cur-rev))))
	  (p4-create-active-link (match-beginning ch-match)
				 (match-end ch-match)
				 (list (cons 'change p4-cur-change)))
	  (if (match-beginning act-match)
	      (p4-create-active-link (match-beginning act-match)
				     (match-end act-match)
				     (list (cons 'action p4-cur-action)
					   (cons 'rev p4-cur-rev))))
	  (p4-create-active-link (match-beginning user-match)
				 (match-end user-match)
				 (list (cons 'user p4-cur-user)))
	  (p4-create-active-link (match-beginning cl-match)
				 (match-end cl-match)
				 (list (cons 'client p4-cur-client)))
	  (p4-set-extent-properties (match-beginning desc-match)
				    (match-end desc-match)
				    (list (cons 'invisible t)
					  (cons 'isearch-open-invisible t)))))
      (p4-find-change-numbers buffer (point-min) (point-max))
      (setq buffer-invisibility-spec (list))
      (p4-move-buffer-point-to-top buffer))))

(defvar p4-basic-list-mode-map
  (let ((map (p4-make-derived-map p4-basic-mode-map)))
    (define-key map "n" 'next-line)
    (define-key map "p" 'previous-line)
    (define-key map "\C-m" 'p4-basic-list-activate)
    map)
  "The key map to use for selecting opened files.")

(defvar p4-basic-list-font-lock-keywords
  '(("^\\(//.*\\)#[0-9]+ - delete" 1 'p4-depot-deleted-face)
    ("^\\(//.*\\)#[0-9]+ - add" 1 'p4-depot-added-face)
    ("^\\(//.*\\)#[0-9]+ - branch" 1 'p4-depot-branched-face)))

(defun p4-basic-list-get-filename ()
  (save-excursion
    (beginning-of-line)
    (when (looking-at "^\\(//.*\\)#[0-9]+ - ")
      (match-string-no-properties 1))))

(defun p4-basic-list-activate ()
  (interactive)
  (let ((file (p4-basic-list-get-filename)))
    (when file
      (p4-find-file-or-print-other-window nil file))))

(define-derived-mode p4-basic-list-mode p4-basic-mode "P4 Basic List"
  (setq font-lock-defaults '(p4-basic-list-font-lock-keywords t)))

;; Return the file name associated with a buffer. If the real buffer file
;; name doesn't exist, try special filename tags set in some of the p4
;; buffers.
(defun p4-buffer-file-name-2 ()
  (cond ((p4-buffer-file-name))
	((get-char-property (point) 'link-client-name))
	((get-char-property (point) 'link-depot-name))
	((get-char-property (point) 'block-client-name))
	((get-char-property (point) 'block-depot-name))
	;; ((if (and (fboundp 'dired-get-filename)
	;; 	  (dired-get-filename nil t))
	;;      (p4-follow-link-name (dired-get-filename nil t))))
	((p4-basic-list-get-filename))))


(defun p4-buffer-file-name ()
  buffer-file-name
  ;; (cond (buffer-file-name
  ;;        (p4-follow-link-name buffer-file-name))
  ;;       (t nil))
  )


(defun p4-push-window-config ()
  "Push the current window configuration on the `p4-window-config-stack'
stack."
  (interactive)
  (setq p4-window-config-stack
	(cons (current-window-configuration)
	      p4-window-config-stack))
  (while (> (length p4-window-config-stack) p4-window-config-stack-size)
    (setq p4-window-config-stack
	  (nreverse (cdr (nreverse p4-window-config-stack))))))

(defun p4-pop-window-config (num)
  "Pop `num' elements from the `p4-window-config-stack' stack and use
the last popped element to restore the window configuration."
  (interactive "p")
  (when (> num 0)
    (setq p4-window-config-stack (nthcdr (1- num) p4-window-config-stack))
    (unless p4-window-config-stack
      (error "window config stack empty"))
    (set-window-configuration (car p4-window-config-stack))
    (setq p4-window-config-stack (cdr p4-window-config-stack))))

;; Kinds of functions to invoke p4 or other external commands.
(defun p4-call-command (cmd args buffer-name &optional mode callback)
  (let* ((buffer (p4-make-output-buffer
                  buffer-name mode
                  (concat "P4: " cmd " " (p4-concat-string-array args " ") "\n")))
	 (process nil))
    (with-current-buffer buffer
      (setq process (apply 'start-process "P4" buffer "p4" cmd args))
      (set-process-filter process 'p4-call-command-process-filter)
      (lexical-let ((callback callback))
        (set-process-sentinel process
                              (lambda (process message)
                                (p4-call-command-process-sentinel callback process message)))))
    (p4-push-window-config)
    (pop-to-buffer buffer)))


(defun p4-get-retcode (command &rest args)
  "Run command with args, and return its return code."
  (apply 'call-process command nil nil nil args))

(defun p4-call-command-sync (&rest args)
  "Execute a command synchronously"
  (apply 'call-process "p4" nil nil nil args))

(defun p4-call-command-sync-ex (command &rest args)
  "Execute a command synchronously"
  (apply 'call-process command nil nil nil args))

(defun p4-call-command-async (&rest args)
  "Execute a p4 command.
args should be a list, but to make caller's life easier, it can accept one atom
instead of a list."
  (add-to-list 'p4-pending-cmds (remq nil args) t)
  (display-buffer (get-buffer-create "*P4-Progress*"))
  (p4-trigger-commands))

(defun p4-command-output-to-buffer (command &rest args)
  "Call command synchronously and put output to buffer, Caller should be
responsible for delete this buffer. Return that buffer."
  (let ((output-buffer (p4-make-output-buffer "P4-output")))
    (if (not (eq 0 (apply 'call-process command nil output-buffer nil args)))
        (setq output-buffer nil))
    output-buffer))


(defun p4-command-output-to-string (cmd &rest args)
  "Execute a p4 command and return result as string.
args should be a list, but to make caller's life easier, it can accept one atom instead of a
  list."
  (let ((cmd-output (with-output-to-string
                      (with-current-buffer standard-output
                        (apply #'process-file
                               cmd
                               nil (list t t) nil
                               args)))))
    (ansi-color-apply cmd-output)))

(defalias 'command-output-to-string 'p4-command-output-to-string)

(defun p4-call-command-process-filter (proc string)
  "Process filter for `p4-call-command'. Keep point position if `bobp'."
  (let ((buffer (process-buffer proc)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
	(let ((inhibit-read-only t)
	      (moving (and (= (point) (process-mark proc))
			   (not (bobp)))))
	  (save-excursion
	    (goto-char (process-mark proc))
	    (insert string)
	    (set-marker (process-mark proc) (point)))
	  (if moving (goto-char (process-mark proc))))))))

(defun p4-call-command-process-sentinel (callback process message)
  (let ((inhibit-read-only t)
        (buffer (process-buffer process)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (when callback
          (funcall callback))
        (unless (string-equal message "finished\n")
          (save-excursion
            (goto-char (process-mark process))
            (insert "Process " (process-name process) " " message)
            (set-marker (process-mark process) (point)))
          (ding))
        (set-buffer-modified-p nil))))
  )

(defun p4-async-command-process-sentinel (callback process message)
  (let ((inhibit-read-only t)
	(buffer (process-buffer process)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
	(when callback
	  (funcall callback))
	(let ((moving (= (point) (process-mark process))))
	  (save-excursion
	    (goto-char (process-mark process))
	    (insert "Process " (process-name process) " " message)
	    (set-marker (process-mark process) (point)))
	  (if moving (goto-char (process-mark process))))
	(unless (string-equal message "finished\n")
	  (ding))
	(set-buffer-modified-p nil)))))

(defun p4-async-command (cmd args buffer-name &optional mode callback)
  (let* ((buffer (p4-make-output-buffer buffer-name mode))
	 (process (p4-start-p4 buffer (cons cmd args))))
    (lexical-let ((callback callback))
      (set-process-sentinel process
			    (lambda (process message)
			      (p4-async-command-process-sentinel callback process message))))
    (display-buffer buffer)
    process))


(defun p4-mark-print-buffer (buffer print-buffer)
  (with-current-buffer buffer
    (p4-mark-depot-list-buffer print-buffer)
    (let ((depot-regexp
           (if print-buffer
               "^\\(//[^/@# ][^/@#]*/\\)[^@#]+#[0-9]+ - "
             "^\\(//[^/@# ][^/@#]*/\\)")))
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward depot-regexp nil t)
          (let ((link-client-name (get-char-property (match-end 1)
        					     'link-client-name))
        	(link-depot-name (get-char-property (match-end 1)
        					    'link-depot-name))
        	(start (match-beginning 1))
        	(end (point-max)))
            (save-excursion
              (if (re-search-forward depot-regexp nil t)
                  (setq end (match-beginning 1))))
            (if link-client-name
                (p4-set-extent-properties start end
                			  (list (cons 'block-client-name
                				      link-client-name))))
            (if link-depot-name
                (p4-set-extent-properties start end
                			  (list (cons 'block-depot-name
                				      link-depot-name))))
            (p4-find-change-numbers buffer start
                		    (save-excursion
                		      (goto-char start)
                		      (line-end-position)))
            ))))))

(defun p4-activate-print-buffer (buffer-name print-buffer)
  (if print-buffer
      (p4-font-lock-buffer p4-output-buffer-name))
  (set-buffer p4-output-buffer-name)
  (get-buffer-create buffer-name) ;; We do these two lines
  (kill-buffer buffer-name)	  ;; to ensure no duplicates
  (set-buffer p4-output-buffer-name)
  (rename-buffer buffer-name t)
  (p4-mark-depot-list-buffer print-buffer)
  (setq buffer-read-only t)
  (p4-move-buffer-point-to-top buffer-name)
  (p4-mark-print-buffer buffer-name print-buffer))

(defconst p4-blame-change-regex
  (concat "^\\.\\.\\. #"     "\\([0-9]+\\)"   ;; revision
	  "\\s-+change\\s-+" "\\([0-9]+\\)"   ;; change
	  "\\s-+"            "\\([^ \t]+\\)"  ;; type
	  "\\s-+on\\s-+"     "\\([^ \t]+\\)"  ;; date
	  "\\s-+by\\s-+"     "\\([^ \t]+\\)"  ;; author
	  "@"))

(defconst p4-blame-branch-regex
  "^\\.\\.\\. \\.\\.\\. branch from \\(//[^#]*\\)#")

(defconst p4-blame-revision-regex
  (concat "^\\([0-9]+\\),?"
	  "\\([0-9]*\\)"
	  "\\([acd]\\)"
	  "\\([0-9]+\\),?"
	  "\\([0-9]*\\)"))

(defconst p4-blame-index-regex
  (concat " *\\([0-9]+\\)"               ;; change
	  " *\\([0-9]+\\)"               ;; revision
	  " *\\([0-9]+/[0-9]+/[0-9]+\\)" ;; date
	  "\\s-+\\([^:]*\\)"             ;; author
	  ":"))

(defconst P4-REV  0)
(defconst P4-DATE 1)
(defconst P4-AUTH 2)
(defconst P4-FILE 3)

(defun p4-blame ()
  "To Print a depot file with revision history to a buffer,
type \\[p4-blame]"
  (interactive)
  (let ((arg-string (p4-buffer-file-name-2))
	(rev (get-char-property (point) 'rev))
	(change (get-char-property (point) 'change)))
    (cond (rev
	   (setq arg-string (concat arg-string "#" rev)))
	  (change
	   (setq arg-string (concat arg-string "@" change))))
    (if (or current-prefix-arg (not arg-string))
	(setq arg-string (p4-read-arg-string "p4 print-revs: " arg-string)))
    (p4-blame-int arg-string)))

(defalias 'p4-print-with-rev-history 'p4-blame)

(defun p4-blame-int (file-spec)
  (let ((file-name file-spec)
	(buffer (p4-get-writable-output-buffer))
	head-name  ;; file spec of the head revision for this blame assignment
	branch-p   ;; have we tracked into a branch?
	cur-file   ;; file name of the current branch during blame assignment
	change ch-alist fullname head-rev headseen)

    ;; we asked for blame constrained by a change number
    (if (string-match "\\(.*\\)@\\([0-9]+\\)" file-spec)
	(progn
	  (setq file-name (match-string 1 file-spec))
	  (setq change (string-to-number (match-string 2 file-spec)))))

    ;; we asked for blame constrained by a revision
    (if (string-match "\\(.*\\)#\\([0-9]+\\)" file-spec)
	(progn
	  (setq file-name (match-string 1 file-spec))
	  (setq head-rev (string-to-number (match-string 2 file-spec)))))

    ;; make sure the filespec is unambiguous
    (with-temp-buffer
      (p4-exec-p4 t (list "files" file-name) t)
      (if (> (count-lines (point-min) (point-max)) 1)
	  (error "File pattern maps to more than one file.")))

    ;; get the file change history:
    (p4-exec-p4 buffer (list "filelog" "-i" file-spec) t)
    (setq fullname (p4-read-depot-output buffer)
	  cur-file  fullname
	  head-name fullname)

    ;; parse the history:
    (with-current-buffer  buffer
      (goto-char (point-min))
      (while (< (point) (point-max))

	;; record the current file name (and the head file name,
	;; if we have not yet seen one):
	(if (looking-at "^\\(//.*\\)$")
	    (setq cur-file (match-string 1)))

	;; a non-branch change:
	(if (looking-at p4-blame-change-regex)
	    (let ((rev (string-to-number (match-string 1)))
		  (ch (string-to-number (match-string 2)))
		  (op (match-string 3))
		  (date (match-string 4))
		  (author (match-string 5)))
	      (cond
	       ;; after the change constraint, OR
	       ;; after the revision constraint _for this file_
	       ;;   [remember, branches complicate this]:
	       ((or (and change   (< change ch))
		    (and head-rev (< head-rev rev)
			 (string= head-name cur-file))) nil)

	       ;; file has been deleted, can't assign blame:
	       ((string= op "delete")
		(if (not headseen) (goto-char (point-max))))

	       ;; OK, we actually want to look at this one:
	       (t
		(setq ch-alist
		      (cons
		       (cons ch (list rev date author cur-file)) ch-alist))
		(if (not head-rev) (setq head-rev rev))
		(setq headseen t)) ))

	  ;; not if we have entered a branch (this used to be used, isn't
	  ;; right now - maybe again later:
	  (if (and headseen (looking-at p4-blame-branch-regex))
	      (setq branch-p t)) )
	(forward-line)))

    (if (< (length ch-alist) 1)
	(error "Head revision not available"))

    (let ((base-ch (int-to-string (caar ch-alist)))
	  (ch-buffer (get-buffer-create "p4-ch-buf"))
	  (tmp-alst (copy-alist ch-alist)))
      (p4-exec-p4 ch-buffer
		  (list "print" "-q" (concat cur-file "@" base-ch)) t)
      (with-current-buffer ch-buffer
        (goto-char (point-min))
	(while (re-search-forward ".*\n" nil t)
	  (replace-match (concat base-ch "\n"))))
      (while (> (length tmp-alst) 1)
	(let ((ch-1 (car (car  tmp-alst)))
	      (ch-2 (car (cadr tmp-alst)))
	      (file1 (nth P4-FILE (cdr (car  tmp-alst))))
	      (file2 (nth P4-FILE (cdr (cadr tmp-alst))))
	      ins-string)
	  (setq ins-string (format "%d\n" ch-2))
	  (with-current-buffer buffer (erase-buffer))
	  (p4-exec-p4 buffer (list "diff2"
				   (format "%s@%d" file1 ch-1)
				   (format "%s@%d" file2 ch-2)) t)
          (with-current-buffer buffer
            (goto-char (point-max))
	    (while (re-search-backward p4-blame-revision-regex nil t)
	      (let ((la (string-to-number (match-string 1)))
		    (lb (string-to-number (match-string 2)))
		    (op (match-string 3))
		    (ra (string-to-number (match-string 4)))
		    (rb (string-to-number (match-string 5))))
		(if (= lb 0)
		    (setq lb la))
		(if (= rb 0)
		    (setq rb ra))
		(cond ((string= op "a")
		       (setq la (1+ la)))
		      ((string= op "d")
		       (setq ra (1+ ra))))
                (with-current-buffer ch-buffer
                  (goto-line la)
		  (let ((beg (point)))
		    (forward-line (1+ (- lb la)))
		    (delete-region beg (point)))
		  (while (<= ra rb)
		    (insert ins-string)
		    (setq ra (1+ ra)))))))

	  (setq tmp-alst (cdr tmp-alst))))
      (p4-noinput-buffer-action "print" nil t
				(list (format "%s#%d" fullname head-rev)))
      (p4-font-lock-buffer p4-output-buffer-name)
      (let (line cnum (old-cnum 0) change-data
                 xth-rev xth-date xth-auth xth-file)
        (with-current-buffer buffer
          (goto-line 2)
	  (move-to-column 0)
	  (p4-insert-no-properties "Change  Rev       Date  Author\n")
	  (while (setq line (p4-read-depot-output ch-buffer))
	    (setq cnum (string-to-number line))
	    (if (= cnum old-cnum)
		(p4-insert-no-properties (format "%29s : " ""))

	      ;; extract the change data from our alist: remember,
	      ;; `eq' works for integers so we can use assq here:
	      (setq change-data (cdr (assq cnum ch-alist))
		    xth-rev     (nth P4-REV  change-data)
		    xth-date    (nth P4-DATE change-data)
		    xth-auth    (nth P4-AUTH change-data)
		    xth-file    (nth P4-FILE change-data))

	      (p4-insert-no-properties
	       (format "%6d %4d %10s %7s: " cnum xth-rev xth-date xth-auth))
	      (move-to-column 0)
	      (if (looking-at p4-blame-index-regex)
		  (let ((nth-cnum (match-string 1))
			(nth-revn (match-string 2))
			(nth-user (match-string 4)))
		    (p4-create-active-link (match-beginning 1)
					   (match-end 1)
					   (list (cons 'change nth-cnum)))
		    ;; revision needs to be linked to a file now that we
		    ;; follow integrations (branches):
		    (p4-create-active-link (match-beginning 2)
					   (match-end 2)
					   (list (cons 'rev  nth-revn)
						 (cons 'link-depot-name xth-file)))
		    (p4-create-active-link (match-beginning 4)
					   (match-end 4)
					   (list (cons 'user nth-user)))
		    ;; truncate the user name:
		    (let ((start (+ (match-beginning 4) 7))
			  (end (match-end 4)))
		      (if (> end start)
			  (delete-region start end))))))
	    (setq old-cnum cnum)
	    (forward-line))))

      (kill-buffer ch-buffer))
    (let ((buffer-name (concat "*P4 print-revs " file-name "*")))
      (p4-activate-print-buffer buffer-name nil)
      (with-current-buffer buffer-name
        (setq truncate-lines t)
	(use-local-map p4-print-rev-mode-map)))))

(defun p4-visit-item (&optional other-window)
  "Visit current item.
With a prefix argument, visit in other window."
  (interactive "P")
  (message "p4-visit-item Called!")
  ;; (magit-section-action (item info "visit")
  ;;   ((untracked file)
  ;;    (call-interactively 'magit-visit-file-item))
  ;;   ((diff)
  ;;    (call-interactively 'magit-visit-file-item))
  ;;   ((diffstat)
  ;;    (call-interactively 'magit-visit-file-item))
  ;;   ((hunk)
  ;;    (call-interactively 'magit-visit-file-item))
  ;;   ((commit)
  ;;    (magit-show-commit info nil nil 'select))
  ;;   ((stash)
  ;;    (magit-show-stash info)
  ;;    (pop-to-buffer magit-stash-buffer-name))
  ;;   ((branch)
  ;;    (magit-checkout info))
  ;;   ((longer)
  ;;    (magit-log-show-more-entries ())))
  )

(defun p4-insert-mark (mark &optional pos)
  (if pos
      (goto-char pos))
  (insert mark))

(defun p4-get-mark-dot (color)
  (propertize "    "
              'help-echo 'svn-status-state-mark-tooltip
              'display
              `(image :type xpm
                      :data ,(format "/* XPM */
static char * data[] = {
\"18 13 3 1\",
\"  c None\",
\"+ c #000000\",
\". c %s\",
\"                  \",
\"       +++++      \",
\"      +.....+     \",
\"     +.......+    \",
\"    +.........+   \",
\"    +.........+   \",
\"    +.........+   \",
\"    +.........+   \",
\"    +.........+   \",
\"     +.......+    \",
\"      +.....+     \",
\"       +++++      \",
\"                  \"};"
                                     color)
                      :ascent center)))

(defun p4-get-mark-unfold (color)
  (propertize "    "
              'help-echo 'svn-status-state-mark-tooltip
              'display
              `(image :type xpm
                      :data  ,(format "/* XPM */
static char *unfold[] = {
/* columns rows colors chars-per-pixel */
\"24 12 2 1 \",
\"  c None\",
\"+ c %s\",
/* pixels */

\"                        \",
\"       ++++++++++       \",
\"       ++++++++++       \",
\"        ++++++++        \",
\"        ++++++++        \",
\"         ++++++         \",
\"         ++++++         \",
\"          ++++          \",
\"          ++++          \",
\"           ++           \",
\"           ++           \",
\"                        \"};" color)
                      :ascent center)))

(defun p4-get-mark-fold (color)
  (propertize "    "
              'help-echo 'svn-status-state-mark-tooltip
              'display
              `(image :type xpm
                      :data  ,(format "/* XPM */
static char *abc[] = {
/* columns rows colors chars-per-pixel */
\"18 16 2 1 \",
\"  c None\",
\"+ c %s\",
/* pixels */
\"                  \",
\"                  \",
\"    +             \",
\"    +++           \",
\"    ++++          \",
\"    ++++++        \",
\"    ++++++++      \",
\"    ++++++++++    \",
\"    ++++++++++    \",
\"    ++++++++      \",
\"    ++++++        \",
\"    ++++          \",
\"    +++           \",
\"    +             \",
\"                  \",
\"                  \",};" color)
                      :ascent center)))



(defun p4-file-directory-p (path)
  "Check if path is a directory or not (on server)"
  (let* ((remote-path (p4-make-depot-path path))
         (remote-parent-path
          (if remote-path
              (concat (file-name-directory remote-path) "*") nil))
         (remote-subdirs
          (if remote-parent-path
              (command-output-to-string (p4-get-executable) "dirs" "-C"
                                     remote-parent-path)
            nil)))
    (if (and  remote-subdirs
              (string-match (format ".*%s.*" remote-path) remote-subdirs))
        t
      nil)))

(defun p4-init-variables ( )
  "Initialize user customized variables."
  (if (or (not p4user)
          (not p4client)
          (not p4passwd)
          (and (not p4port)
               (not p4name))) ;; todo: should provide a way to input passwd interactively.
      (error "User, Password, Client and host(or port) must be set! Customize them first."))

  (setenv "P4USER" p4user)
  (setenv "P4CLIENT" p4client)
  (setenv "P4PASSWD" p4passwd)
  (if p4host
      (setenv "P4HOST" p4host)
    )
  (if p4port
      (setenv "P4PORT" p4port)
    )

  (if p4name
      (setenv "P4NAME" p4name)
    )

  ;; (p4-login)
  (let ((p4info (command-output-to-string "p4"  "info")))
    (if (string-match p4-r-match-client-root p4info)
        (setq p4-root (p4-normalize-path (match-string 1 p4info)))
      ))

  (setq p4-r-match-max-depth
        (format "\\(?:\\(?:\\(?:[[:alnum:]]\\|[[:space:]]\\)/\\)+\\)\\{%d,\\}"
                p4-max-search-depth))
  (message "Finished init variables for p4."))


(defun get-short-path (path)
  "Get short path"
  (if (string-match (concat p4-root "\\(.*$\\)") path)
      (match-string 1 path)
    (concat "/" path)))

(defun p4-make-depot-path (paths)
  "Return file or directory that can be recognized by P4."
  (let (outpaths)
    (if (listp paths)
        (dolist (path paths)
          (setq outpaths (cons (p4-make-depot-path path) outpaths)))
      (setq outpaths (let ((shortpath (get-short-path paths)))
                       (yc/strip-string (if shortpath
                                            (concat "//depot" (p4-normalize-path shortpath))
                                          paths)))))
    outpaths))


(defun p4-make-local-path (path)
  "Change path into local form, path maybe a depot path"
  (let ((short-path
         (if (string-match p4-r-match-depot-file path)
             (concat p4-root (match-string 1 path))
           path)))
    (p4-denormalize-path short-path)))

(defun p4-get-executable ()
  "return formated p4 command"
  (if (executable-find "p4")
    "p4"
    nil))


(defun p4-get-writable-files (output pos)
  "Get all writeable files from output message"
  (let ((p4-r-match-writeable (rx "Can't clobber writable file " (group (+ nonl))))
        result)
    (when (string-match p4-r-match-writeable output pos)
      (setq result (cons (match-string 1 output) (p4-get-writable-files output (match-end 0)))))
    result))


(defun p4-get-pending-cls ()
  "Get all pending change lists, returns (cons change_number description)"
  (let ((result-string (command-output-to-string "p4" "changes" "-s"  "pending"
                                                 "-l" "-u" p4user "-c" p4client))
        cls)
    (defun pending-cls-iter (pos)
      (let (res)
        (when (string-match p4-r-match-changes result-string pos)
          (setq pos (match-end 0))
          (setq res (cons (cons (match-string 1 result-string)
                                (strip-string (match-string 2 result-string) ))
                          (pending-cls-iter pos)))
          )
        res))
    (pending-cls-iter 0)))


(defun p4-get-files-in-cl (cl)
  "get affected files in specified change list"
  (if (not (stringp cl))
      (error "Argument CL should be a string!"))
  (let ((result-string (command-output-to-string "p4" "describe" "-s" cl))
        affected-files)
    (defun affected-files-iter (pos)
      (if (string-match p4-r-match-affected-file result-string pos)
          (cons (cons (match-string 1 result-string) (match-string 2 result-string))
                (affected-files-iter (match-end 0)))
        nil))
    (affected-files-iter 0)))

(defun p4-is-cl-empty (cl)
  "Return t if change list is empty"
  (not (p4-get-files-in-cl cl)))


(defun p4-report-error-and-quit (msg &optional marker)
  "Report error and quit"
  (with-current-buffer (get-buffer-create  "*P4-Progress*")
    (setq marker (if (stringp marker) (concat marker " :") "Unknown: "))
    (insert marker)
    (insert msg)
    (error msg)))

;; TODO: Clear empty change list!
(defun p4-process-sentinel (process event)
  "Sentinel to handle this process."
  (when (memq (process-status process) '(signal exit))
    (setq p4-cmd-executing nil)
    ;; (with-current-buffer (get-buffer-create  "*P4-Progress*")
    ;;   (goto-char (point-max))
    ;;   (insert (format  "\nRet: %d, counter: %d\n" (process-exit-status process) p4-error-counter)))
    (if (= 0 (process-exit-status process))
        ;; Succeeded, but there is some cases needs extra handling.
        (if (string-match p4-r-match-none-local-file p4-last-output)
            (let* ((remote-file (match-string 1 p4-last-output))
                   (local-file (p4-make-local-path remote-file)))
              (when (and local-file
                       (file-exists-p local-file))
                (rename-file local-file (make-temp-name local-file)))
              (p4-call-command-sync "sync" (concat remote-file "#head"))
              (p4-resume-processing))
          (with-current-buffer (get-buffer-create  "*P4-Progress*")
            (setq p4-last-output "")
            (setq p4-error-counter 0)
            (pop p4-pending-cmds)
            (goto-char (point-max))
            (insert (format "\nTask finished, cost: %.2f seconds\n-------------"
                            (float-time (time-since p4-cmd-starttime))))
            (if p4-pending-cmds
                (p4-trigger-commands)
              (with-current-buffer (get-buffer-create "*P4-Progress*")
                (goto-char (point-max))
                (when p4-confilict-detected
                  (insert (format "\nAuto resolving conflicts...\n%s\n"
                                  (p4-command-output-to-string "p4" "resolve" "-am")))
                  (p4-revert-cl (p4-get-changeList))
                  (setq p4-confilict-detected nil))
                (insert (format
"\n************************************************************************
** All commands have been processed at : %s **\n************************************************************************\n"
                                (format-time-string current-date-time-format (current-time)))))
              (display-buffer (get-buffer-create "*P4-Progress*"))
              (message "All commands have been processed."))))

      ;; Program terminated with error, try to handle it!
      (let* ((files (p4-get-writable-files p4-last-output 0))
             (need-stop t) cl)
        (setq p4-error-counter (1+ p4-error-counter))

        (with-current-buffer (get-buffer-create  "*P4-Progress*")
          (if (> p4-error-counter 10)
              (progn
                (goto-char (point-max))
                (insert (format  "\nRet: %d, counter: %d\n"
                                 (process-exit-status process) p4-error-counter))
                (p4-report-error-and-quit
                 (format "Command: %s failed, output string: %s, Should handle this!"
                         (p4-concat-string-array (car p4-pending-cmds) " ")
                         p4-last-output)
                 (format "Too many errors: %d" p4-error-counter)))

            (insert (format
                     "\nLast command failed, output is: %s, Trying to resolve it myself...\n"
                     (strip-string p4-last-output)))
            (when files
              (setq cl (p4-get-changeList))
              (if (not cl)
                  (error "Failed to create change list!"))
              (p4-co-multiple files cl t)
              (message "Change list %s needs to be handled!" cl)
              (setq need-stop nil
                    p4-confilict-detected t))
            (when (string-match p4-r-match-unknown-cl p4-last-output);;unknown cl, clear hash table
              (remhash (concat "Auto created by p4.el on: "
                               (format-time-string "%Y-%m-%d")) p4-changeList-table)
              (setq need-stop nil))
            (when (string-match (rx (or "- must resolve" " - is opened for edit")) p4-last-output) ;; Just forget about it!
              (setq need-stop nil))
            (if need-stop
                (p4-report-error-and-quit
                 (format "Command: %s failed, output string: %s, Should handle this!"
                         (p4-concat-string-array (car p4-pending-cmds) " ")
                         p4-last-output)
                 "Unhandled output")
              (p4-resume-processing))))))))

(defun p4-process-filter (process output)
  "Filter used for p4-prcesses"
  (with-current-buffer (get-buffer-create  "*P4-Progress*")
    (goto-char (point-max))
    ;; (insert output)
    (insert output)
    (setq p4-last-output (concat p4-last-output output))))

(defun p4-trigger-commands ()
  "Process pending commands"
  (if (and (not p4-cmd-executing)
           p4-pending-cmds)
      (let* ((cmd (p4-get-executable))
             (p4-buffer (get-buffer-create "*P4-Progress*"))
             (arglist (car p4-pending-cmds))
             (process nil))
        (setq p4-last-output "")
        (setq p4-cmd-starttime (current-time))
        (with-current-buffer (get-buffer-create "*P4-Progress*")
          (goto-char (point-max))
          (setq p4-cmd-counter (1+ p4-cmd-counter))
          (insert  (format "\nCommand(%d): %s\n" p4-cmd-counter
                           (p4-concat-string-array arglist " "))))
        (setq process
              (apply 'start-process "P4-Processor" p4-buffer "p4" (add-to-list 'arglist "-q")))
        (set-process-sentinel process 'p4-process-sentinel)
        (set-process-filter process 'p4-process-filter)
        (setq p4-cmd-executing t))))

(defun p4-sync-single-item (item &optional rev &optional force)
  "Sync single item"
  (if (and rev (not (stringp rev)))
      (error "Rev should be a string or nil!"))

  (let ((final-path (yc/strip-string item "/"))
        (format-func (if (file-exists-p item)
             'p4-make-local-path
           'p4-make-depot-path)))
    (when (or (file-directory-p item)
           (p4-file-directory-p item))
      (setq final-path (concat final-path "/...")))

    (setq final-path (funcall format-func  final-path))
    (if rev
        (setq final-path (concat final-path "#" rev))
      (setq final-path (concat final-path "#head")))
    (p4-call-command-async "sync" (if force "-f" ) final-path)))


(defun p4-delete-branch ()
  "Delete branch."
  (interactive)
  (message "Not implemented.")
  )

(defun p4-revert-cl (cl &optional revertAll sync)
  "Revert change list.
If reverAll is not provided, only revert files that are not changed."
  (let ((args (list "revert" "-c" cl))
        (func (if sync 'p4-call-command-sync 'p4-call-command-async)))
    (funcall func "revert" "-c" cl
             (if (and revertAll
                      (yes-or-no-p "Going to abort all changes and revert all files, continue?"))
                 (concat "//" p4client "/...")
               "-a"))))

(defun p4-co-single-item-sync (item &optional changeList)
  "Check out single item"
  (let ((final-path item))
    (if (file-exists-p item)
        (if (file-directory-p item)
            (setq final-path (concat (yc/strip-string item "/") "/...")))
      (if (p4-file-directory-p item)
          (setq final-path (concat (yc/strip-string item "/") "/..."))))

    (setq final-path (p4-make-depot-path  final-path))
    (p4-call-command-sync "edit" "-c" (if changeList changeList "default") final-path )))

(defun p4-co-single-item (item &optional changeList)
  "Check out single item"
  (let ((final-path item))
    (if (file-exists-p item)
        (if (file-directory-p item)
            (setq final-path (concat (yc/strip-string item "/") "/...")))
      (if (p4-file-directory-p item)
          (setq final-path (concat (yc/strip-string item "/") "/..."))))

    (setq final-path (p4-make-depot-path  final-path))
    (p4-call-command-async "edit" "-c" (if changeList changeList "default") final-path )))


(defun p4-co-multiple (files &optional changeList &optional front)
  "Check out a list of files."
  (if (not (listp files))
      (error "p4-co-multiple requires a list of files!")
    (add-to-list 'p4-pending-cmds (append (list "edit" "-c" (if changeList changeList "default"))
                                          (p4-make-depot-path files)) (not front))
    (p4-trigger-commands)))

(defun p4-add-multiple (files &optional changeList)
  "Add a list of files."
  (if (not (listp files))
      (error "p4-add-multiple requires a list of files!")
    (add-to-list 'p4-pending-cmds (append (list "add" "-c" (if changeList changeList "default"))
                                          (p4-make-depot-path files)) )
    (display-buffer (get-buffer-create "*P4-Progress*"))
    (p4-trigger-commands)))

(defun p4-delete-multiple (files &optional changeList)
  "Delete a list of files."
  (if (not (listp files))
      (error "p4-add-multiple requires a list of files!")
    (add-to-list 'p4-pending-cmds (append (list "delete" "-c" (if changeList changeList "default"))
                                          (p4-make-depot-path files)) t)
    (display-buffer (get-buffer-create "*P4-Progress*"))
    (p4-trigger-commands)))


(defun p4-get-current-item ()
  "Get target file or directory"
  (let ((item buffer-file-name))
    (when (and (not item)
               (y-or-n-p (format "No file provided, use current directory (%s)?"
                                 default-directory)))
      (setq item default-directory))
    item))

(defun p4-branch-iter (dirname rex &optional exclude)
  "Iterator a directory and return all directories that matching rex."
  (let* ((dirlist (directory-files dirname t r-match-dirname))
         (regular-exp (if rex rex p4-r-match-branch-name))
         (result nil))
    (dolist (dir dirlist)
      (when (and (file-directory-p dir)
                 (not (string-match ".*\.git.*"  dir)))
        (if (string-match regular-exp dir)
            (setq result (cons dir result))
          (if (string-match p4-r-match-max-depth dir)
              nil
            (let ((tmp (p4-branch-iter dir rex exclude)))
              (if tmp
                  (if (stringp tmp)
                      (setq result (cons tmp result))
                    (setq result (append tmp result)))))))))
    result))


(defconst p4-r-match-wash-raw-diff
  (rx ":" (+? (or blank "." hex)) (group (or "M" "D" "A" "U" "T"))
      (+ blank) (group (+ nonl)) eol))

(defun p4-wash-raw-diff (result)
  (defun p4-wash-raw-diff-iter (pos)
    (if (string-match p4-r-match-wash-raw-diff result pos)
        (let* ((status (cl-case (string-to-char (match-string-no-properties 1 result))
                         (?A 'new)
                         (?D 'deleted)
                         (?M 'modified)
                         (?U 'unmerged)
                         (?T 'typechange)
                         (t     nil)))
               (file (match-string-no-properties 2 result))
               (tmp (cons status file)))
          (cons tmp (p4-wash-raw-diff-iter (match-end 0))))
      nil))
  (p4-wash-raw-diff-iter 0))

(defun git-get-files-in-commits (start-commit &optional end-commit)
  "Get changed files in commits and return files as a cdr, description as car.
1. If end-commit is a valid commit, get all files changed between start and end.
2. If end-commit is t, then get all files changed since `start-commit`.
3. If end-commit is nil, get files changed in start-commit only.
"
  (if (or (not start-commit)
          (not (stringp start-commit))
          (not (eq 0 (p4-get-retcode "git" "cat-file" "commit" start-commit))))
      (error "Start commit is not valid!"))

  ;; Check if end-commit is set and is a valid commit.
  (if end-commit
      (if (stringp end-commit)
          (if (eq 0 (p4-get-retcode "git" "cat-file" "commit" end-commit))
              nil ;; TODO: Get files changed between start-commit and end-commit.
            (error "end-commit is provided but not valid!"))
        nil ;; TODO: Get all files changed between s&e commits
        )
    ;; Get files changed in start-commit.
    (let* ((rnumber (number-to-string (random 1000000)))
           (result-string
            (command-output-to-string "git" "show" "--raw" start-commit
                                      (concat "--format=%s%n%n%b" rnumber)
                                      ))
           (r-match-loger (format "\\([[:ascii:]]+?\\)%s\\([[:ascii:]]+\\)" rnumber))
           description  filelist
           modified-list deleted-list added-list)
      (when (string-match r-match-loger result-string)
        (setq filelist (match-string 2 result-string))
        (setq description (strip-string (match-string 1 result-string)))
        (dolist (ele (p4-wash-raw-diff filelist))
          (cl-case (car ele)
            ((unmerged)
             (error "Unmerged   %s" (cdr ele)))
            ((new)
             (add-to-list 'added-list (cdr ele)))
            ((deleted)
             (add-to-list 'deleted-list (cdr ele)))
            ((modified)
             (add-to-list 'modified-list (cdr ele)))
            ((typechange)
             (error "Typechange %s" (cdr ele)))
            (t
             (error "?          %s" (cdr ele))))))
      (list description (cons 'new added-list) (cons 'deleted deleted-list) (cons 'modified modified-list)))))

(defun p4-file-change-log (cmd file-list-spec)
  (p4-call-command cmd (cons "-l" file-list-spec)
		   (concat "*P4 " cmd ": " (p4-concat-string-array file-list-spec " ") "*")
		   'p4-log-mode
		   (lambda ()
		     (p4-activate-file-change-log-buffer (current-buffer)))))

(defun p4-get-file-rev (default-name rev)
  (if (string-match "^\\([0-9]+\\|none\\|head\\|have\\)$" rev)
      (setq rev (concat "#" rev)))
  (cond ((string-match "^[#@]" rev)
	 (concat default-name rev))
	((string= "" rev)
	 default-name)
	(t
	 rev)))


(defun p4-get-changeList (&optional description)
  "Create and return a new change list."
  (if (not description)
      (setq description (format "<Saved by Perforce at %s>" (format-time-string "%Y-%m-%d")))
    (if (not (stringp description))
        (error "Description should be a string or nil")))
  (let ((cl (gethash description p4-changeList-table))
        (modified-desc description)
        p4template p4-result)
    (when (not cl)
      (if (not p4-cl-template)
          (setq p4-cl-template (command-output-to-string "p4" "change" "-o")))
      (setq p4template p4-cl-template)
      (with-current-buffer (get-buffer-create "P4 New Change")
        (erase-buffer)
        (insert description)
        (goto-char (point-min))
        (while (search-forward-regexp "^" nil t)
          (replace-match "    "))
        (delete-trailing-whitespace)
        (setq modified-desc (buffer-string))
        (erase-buffer)

        (when (and modified-desc
                   (string-match (rx (group (+ anything)  "Description:"))  p4template))
          (setq p4template (concat (match-string 1 p4template)
                                   "\n" modified-desc)))
        (insert p4template)

        (setq p4-result (with-output-to-string
                          (call-process-region (point-min)
                                               (point-max) "p4"
                                               nil standard-output
                                               nil "change" "-i")))
        (when (string-match (rx (group (+ digit))) p4-result)
          (setq cl (match-string 1 p4-result))
          (puthash description cl p4-changeList-table))))
    cl))


(defun strip-string (src)
  "Strip tailing and leading white spaces and new-lines"
  (if (not (stringp src))
      (error "Source is not a string!"))
  (let ((sp 0) (ep (1- (length src))))
    (while (or (= (aref src sp) 9)
               (= (aref src sp) 10)
               (= (aref src sp) 32))
      (setq sp (1+ sp)))

    (while (or (= (aref src ep) 9)
               (= (aref src ep) 10)
               (= (aref src ep) 32))
      (setq ep (1- ep)))
    (substring src sp ep)))

(defun p4-get-git-commit ()
  "Get git commit"
  (let ((full-commit (buffer-substring-no-properties
                      (line-beginning-position) (line-end-position)))
        (parse-result nil)
        git-commit)
    (when (and full-commit
             (not (= 0 (length full-commit)))
             (string-match r-match-git-commit full-commit))
      (setq git-commit (match-string 1 full-commit)))
    (when (and (not git-commit)
             (if (yes-or-no-p "No git commit found, check changed files?")
                 (setq git-commit "CHANGED"))))
    git-commit))




;; Exposed functions (can be called interactively)

(defun p4-co ()
  "Checkout current file."
  (interactive)
  (let ((item (p4-get-current-item)))
    (if item
        (p4-co-single-item item)
      (error "No file/directory provided!"))))

(defalias 'p4-edit 'p4-co)

(defun p4-sync-item ()
  "Sync item"
  (interactive)
  (let ((item (p4-get-current-item)))
    (if item
        (progn
          (p4-sync-single-item item "head" ))
      (error "No file/directory provided!"))))


(defun p4-sync-multiple (files)
  "Sync multiple items"
  (if (not (listp files))
      (error "p4-sync-multiple requires a list of files!")
    (add-to-list 'p4-pending-cmds (append (list "sync")  (p4-make-depot-path files)) t)
    (display-buffer (get-buffer-create "*P4-Progress*"))
    (p4-trigger-commands)))

(defun p4-sync-regex ()
  "description"
  (interactive)
  (let ((regex (read-from-minibuffer "Regex: ")))
    (if (or (not p4-root)
            (not regex))
        (error "No root or branch name provided."))

    (let ((result (p4-branch-iter p4-root regex)))
      (if result
          (dolist (item result)
            (p4-sync-single-item item))
        (error (format "No branch found in %s, using depth: %d, reg: %s.
Maybe you need to tweak p4-max-search-depth or regular expression "
                       p4-root p4-max-search-depth regex))))))

(defconst r-match-git-commit
  (rx line-start (* blank) (group (+ hex))  (+ blank))
  "reg-expression to match a git comit."
  )


(defun p4-cbg ( )
  "Checkout files based on git commit."
  (interactive)
  (let ((commit (p4-get-git-commit))
        (parse-result nil))
    (if (not commit)
        (error "No avaiable content")

      ;; Parse and find changed files.
      (setq parse-result (git-get-files-in-commits
                          (match-string 1 commit)))
      (if parse-result
          (let* ((description (car parse-result))
                 (affected-files (cdr parse-result))
                 (change-list (p4-get-changeList description)))
            (if (not affected-files)
                (error "Failed to get list of changed files!"))
            (dolist (ele affected-files)
              (let ((files (cdr ele))
                    func)
                (cl-case (car ele)
                  ((new)
                   (setq func 'p4-add-multiple))
                  ((deleted)
                   (setq func 'p4-delete-multiple))
                  ((modified)
                   (setq func 'p4-co-multiple))
                  (t
                   (error "?          %s" (cdr ele))))
                (if (and func files)
                    (funcall func files change-list) )
                )
              )
            (p4-revert-cl change-list))))))

(defalias 'p4cbg 'p4-cbg)

(defconst r-match-dirname (rx (+ (or alnum whitespace) )))

(defun p4-sync-all ()
  "Sync all items"
  (interactive)
  (if (or (not p4-root)
         (not p4-r-match-branch-name))
      (error "No root or branch name provided."))
  (let ((result (p4-branch-iter p4-root p4-r-match-branch-name)))
    (if result
        (dolist (item result)
          (p4-sync-single-item item))
      (error (format "No branch found in %s, using depth: %d, reg: %s.
Maybe you need to tweak p4-max-search-depth p4-r-match-branch-name"
                     p4-root p4-max-search-depth p4-r-match-branch-name)))))


(defun p4-resume-processing ()
  "Resume to process pending commands"
  (interactive)
  (if (called-interactively-p 'any)
      (setq p4-error-counter 0))
  (if (= 0 (length p4-pending-cmds))
      (error "No commands are pending!")
    (p4-trigger-commands)))


(defun p4-co-all ()
  "Sync all items"
  (interactive)
  (if (or (not p4-root)
          (not p4-r-match-branch-name))
      (error "No root or branch name provided."))
  (let ((result (p4-branch-iter p4-root p4-r-match-branch-name)))
    (if result
        (dolist (item result)
          (p4-co-single-item item))
      (error (format "No branch found in %s, using depth: %d, reg: %s.
Maybe you need to tweak p4-max-search-depth p4-r-match-branch-name"
                     p4-root p4-max-search-depth p4-r-match-branch-name)))))

(defp4cmd p4sync-all (&rest args)
  "sync-all" "To sync all branches with server"
  (interactive)
  (p4-sync-all))



(defun p4-show-filelog ()
  "Show log of file."
  (interactive)
  (p4-log))

(defun p4-reset-status ()
  "Reset internal status of this elisp script"
  (interactive)
  (setq p4-cmd-executing nil
        p4-pending-cmds nil
        p4-cmd-counter 0
        p4-error-counter 0
        p4-confilict-detected nil)
  (clrhash p4-changeList-table)
  (with-current-buffer (get-buffer-create "*P4-Progress*")
    (erase-buffer)))

;; The p4 help command
(defp4cmd p4-help (&rest args)
  "help" "To print help message, type \\[p4-help].
Argument ARG command for which help is needed."
  (interactive (p4-read-args "p4 help: "))
  (p4-call-command "help" args "*P4 help*"))

;; The p4 info command
(defp4cmd p4-info ()
  "info" "To print out client/server information, type \\[p4-info].\n"
  (interactive)
  (p4-call-command "info" nil "*P4 info*"))

;; The p4 filelog command
(defp4cmd p4-log ()
  "filelog"
  "To view a history of the change made to the current file, type \\[p4-log].\n"
  (interactive)
  (let ((file-name (p4-buffer-file-name-2)))
    (if (or current-prefix-arg (not file-name))
	(setq file-name (p4-make-list-from-string
			 (p4-read-arg-string "p4 filelog: " file-name)))
      (setq file-name (list file-name)))
    (p4-file-change-log "filelog" file-name)))

(defp4cmd p4-diff2 (version1 version2)
  "diff2" "Display diff of two depot files.

When visiting a depot file, type \\[p4-diff2] and enter the versions.\n"
  (interactive
   (let ((rev (get-char-property (point) 'rev)))
     (if (and (not rev) (p4-buffer-file-name-2))
	 (let ((rev-num 0))
	   (setq rev (p4-is-vc nil (p4-buffer-file-name-2)))
	   (if rev
	       (setq rev-num (string-to-number rev)))
	   (if (> rev-num 1)
	       (setq rev (number-to-string (1- rev-num)))
	     (setq rev nil))))
     (list (p4-read-arg-string "First Depot File or Version# to diff: " rev)
	   (p4-read-arg-string "Second Depot File or Version# to diff: "))))
  (let (diff-version1
	diff-version2
	(diff-options (p4-make-list-from-string p4-default-diff-options)))
    (if current-prefix-arg
	(setq diff-options (p4-make-list-from-string
			    (p4-read-arg-string "Optional Args: "
						p4-default-diff-options))))
    ;; try to find out if this is a revision number, or a depot file
    (setq diff-version1 (p4-get-file-rev (p4-buffer-file-name-2) version1))
    (setq diff-version2 (p4-get-file-rev (p4-buffer-file-name-2) version2))

    (p4-call-command "diff2" (append diff-options
				     (list diff-version1
					   diff-version2))
		     "*P4 diff2*" 'p4-diff-mode 'p4-activate-diff-buffer)))

(defun p4-ediff ()
  "Use ediff to compare file with its original client version."
  (interactive)
  (if current-prefix-arg
      (call-interactively 'p4-ediff2)
    (let* ((local (current-buffer))
           (target-mode major-mode)
           (depot (p4-command-output-to-buffer
                   "p4" "print" "-q"
                   (concat (p4-buffer-file-name) "#head"))))
      (if (and target-mode
               (functionp target-mode))
          (funcall target-mode))
      (ediff-buffers depot
                     local
                     `((lambda ()
                         (make-local-variable 'ediff-cleanup-hook)
                         (setq ediff-cleanup-hook
                               (cons (lambda ()
                                       (kill-buffer ,depot)
                                       (p4-menu-add))
                                     ediff-cleanup-hook))))))))

(defp4cmd p4-ediff2 (version1 version2)
  "ediff2" "Use ediff to display two versions of a depot file.

When visiting a depot file, type \\[p4-ediff2] and enter the versions.\n"
  (interactive
   (let ((rev (get-char-property (point) 'rev)))
     (if (and (not rev) (p4-buffer-file-name-2))
	 (let ((rev-num 0))
	   (setq rev (p4-is-vc nil (p4-buffer-file-name-2)))
	   (if rev
	       (setq rev-num (string-to-number rev)))
	   (if (> rev-num 1)
	       (setq rev (number-to-string (1- rev-num)))
	     (setq rev nil))))
     (list (p4-read-arg-string "First Depot File or Version# to diff: " rev)
	   (p4-read-arg-string "Second Depot File or Version# to diff: "))))
  (let* ((file-name (p4-buffer-file-name-2))
         (basename (file-name-nondirectory file-name))
         (bufname1 (concat "*P4 ediff " basename "#" version1  "*"))
         (bufname2 (concat "*P4 ediff " basename "#" version2  "*"))
         (diff-version1 (p4-get-file-rev file-name version1))
         (diff-version2 (p4-get-file-rev file-name version2)))
    (set-buffer (p4-command-output-to-buffer "p4" "print" "-q" diff-version1))
    (rename-buffer bufname1 t)
    (set-buffer (p4-command-output-to-buffer "p4" "print" "-q" diff-version2))
    (rename-buffer bufname2 t)
    (let ((buffer-version-1 (p4-make-output-buffer bufname1))
          (buffer-version-2 (p4-make-output-buffer bufname2)))
      (ediff-buffers buffer-version-1
                     buffer-version-2
                     `((lambda ()
                         (make-local-variable 'ediff-cleanup-hook)
                         (setq ediff-cleanup-hook
                               (cons (lambda ()
                                       (kill-buffer ,buffer-version-1)
                                       (kill-buffer ,buffer-version-2)
                                       (p4-menu-add))
                                     ediff-cleanup-hook))))))))

(defp4cmd p4-login ()
  "login" "To login by obtaining a session ticket, type \\[p4-login].\n"
  (interactive)
  (let (args (pwbuffer (get-buffer-create "*p4passwd*")))
    (if current-prefix-arg
	(setq args (p4-make-list-from-string
		    (p4-read-arg-string "p4 login: "))))
    (if (or (not p4passwd)
            (eq 0 (length p4passwd)))
	(setq p4passwd (read-passwd "Enter perforce password: ")))
    (with-current-buffer pwbuffer
      (delete-region (point-min) (point-max))
      (insert p4passwd)
      (apply 'call-process-region (point-min) (point-max)
	     (p4-get-executable) t t nil "login" args)
      (message "%s" (buffer-substring (point-min) (1- (point-max)))))))

(defp4cmd p4-logout ()
  "logout" "To logout by removing a session ticket, type \\[p4-logout].\n"
  (interactive)
  (message (command-output-to-string "p4" "logout")))

;; The p4 have command
(defp4cmd p4-have (&rest args)
  "have" "To list revisions last gotten, type \\[p4-have].\n"
  (interactive (p4-read-args* "p4 have: " nil (p4-buffer-file-name-2)))
  (p4-call-command "have" args (concat "*P4 Have: (" (p4-current-client) ") " (car args) "*")
		   'p4-basic-list-mode))

;; The p4 changes command
(defp4cmd p4-changes (&rest args)
  "changes" "To list changes, type \\[p4-changes].\n"
  (interactive (p4-read-args* "p4 changes: " nil "-m" "200" "..."))
  (p4-file-change-log "changes" args))

(defp4cmd p4-get-specified-version ()
  "Get specified version" "Get specified version"
  (interactive)
  (let* ((files (directory-files "." nil))
         (filename (completing-read "File: " (push buffer-file-name files) nil t buffer-file-name))
         (version (completing-read "Version: " (list "head" ) nil t "head")))
    (p4-sync-single-item filename version t)))


(defun p4-discard-cl (cl)
  "Discard specified change list, it returns t if succeeded, or nil if failed."
  (if (and (p4-is-cl-empty cl)
           (= (p4-call-command-sync "change" "-d" cl) 0))
      (let (index)
        (maphash (lambda (key val) (if (string= cl val) (setq index key)))
                 p4-changeList-table)
        (if index (remhash index p4-changeList-table))
        t)
    nil))

(defun p4-cleanup-cls ()
  "Clean up empty change lists."
  (interactive)
  (let ((cls (p4-get-pending-cls))
        ccls)
    (when cls
      (dolist (cl cls)
        (when (p4-discard-cl (car cl))
          (setq ccls (cons (car cl) ccls)))))
    (if ccls
        (progn
          (when (get-buffer p4-manager-buffer-name)
            (p4-manager-refresh))
          (message (concat "Deleted empty changeslists: " (p4-list-to-string ccls))))
      (error "No empty ChangeList is found!"))))

(defun p4-reload-elisp ()
  "Debug function used to reload this elisp."
  (interactive)
  (load-file "~/.emacs.d/site-lisp/version-control/p4/p4.el"))

 ;; P4 Manager
(defconst p4-manager-buffer-name "*P4-Manager*" "Name of p4 manager." )
(defconst p4-manager-prefix-user   "User  :")
(defconst p4-manager-prefix-local  "Root  :")
(defconst p4-manager-prefix-client "Client:")

(defalias 'p4-manager-refresh 'p4-manager)

(defvar p4-manager-mode-map
  (let ((map (p4-make-derived-map p4-basic-mode-map)))
    (define-key map (kbd "1") (lambda () (interactive) (p4-manager-show-all-change-lists t)))
    (define-key map (kbd "2") (lambda () (interactive) (p4-manager-show-all-change-lists nil)))
    (define-key map (kbd "g") 'p4-manager-refresh)
    (define-key map (kbd "k") 'p4-manager-discard-cl)
    (define-key map (kbd "v") (lambda() (interactive) (p4-manager-revert-file)))
    (define-key map (kbd "U") (lambda() (interactive) (p4-manager-revert-file t)))
    map)
  "Map of P4-Manager mode")

(define-derived-mode p4-manager-mode p4-basic-mode "P4 Manager")


(defun p4-manager-revert-file (&optional force)
  "Revert file if it is unchanged or force is t"
  (interactive)
  (message "Not implemented!")
  (let ((changeList (get-char-property (point) 'changeList)))
    (print changeList)))

(defun p4-manager-discard-cl ()
  "Discard change this change list"
  (interactive)
  (let ((changeList (get-char-property (point) 'changeList)))
    (if changeList
        (if (p4-discard-cl changeList)
            (p4-manager-refresh)
          (error "Failed to delete change list: %s!" changeList))
      (error "No change list found at position!"))))

(defun p4-manager-show-all-change-lists (show)
  "Show or hide all changes lists!"
  (interactive)
  (message "Called with arg %s" (if show "t" "nil")))

(defun p4-get-manager-buffer ()
  "Create manager buffer for p4."
  (let* ((manager (get-buffer-create p4-manager-buffer-name)))
    (with-current-buffer manager
      (cd p4-root)
      (setq buffer-read-only nil)
      (erase-buffer)
      (insert (format "%s %s\n%s %s\n%s %s\n\n"
                      p4-manager-prefix-user p4user
                      p4-manager-prefix-client p4client
                      p4-manager-prefix-local p4-root))
      (insert "Change Lists:\n\n")
      (p4-buffer-set-face-property (format "^%s.*$" p4-manager-prefix-local) 'p4-root-dir-face)
      (p4-buffer-set-face-property (format "^%s" p4-manager-prefix-user) 'p4-header-title)
      (p4-buffer-set-face-property (format "^%s" p4-manager-prefix-client) 'p4-header-title)
      (p4-buffer-set-face-property (format "^%s" p4-manager-prefix-local) 'p4-header-title)
      (setq buffer-read-only t)
      (p4-manager-mode))
    manager))

(defun p4-manager ()
  "Open a P4 Manager to operate change list visually."
  (interactive)
  (let ((manager (p4-get-manager-buffer))
        (cls (p4-get-pending-cls))
        pos)
    (with-current-buffer manager
      (setq buffer-read-only nil)
      (dolist (cl cls)
        (setq pos (point))
        (p4-insert-mark (p4-get-mark-fold "red"))
        (insert (format "  %s --- %s\n" (car cl) (cdr cl)))
        (p4-set-extent-properties pos (1- (point-max))
                                  (list (cons 'changeList (car cl))
                                        (cons 'unfold t))))
      (setq buffer-read-only t)
      )
    (switch-to-buffer manager)
    )
  )

(defun p4-manager-toggle-changeList (cl)
  "Show or hide change list specified by cl."
  (save-excursion
    (setq buffer-read-only nil)
    (let* ((pos (point))
           (char-prop (get-char-property-and-overlay pos 'unfold))
           (unfold (car char-prop)))
      (if (cdr char-prop)
          (overlay-put (cdr char-prop) 'unfold nil)
        (overlay-put (make-overlay (line-beginning-position) (line-end-position)) 'unfold t))

      (if unfold ;; Unfold this cl.
          (let ((affected-files (p4-get-files-in-cl cl)) marker-color)
            (if affected-files
                ;; Add files into cl.
                (dolist (ele affected-files)
                  (cond
                   ((string-match "add" (cdr ele))
                    (setq marker-color "green"))
                   ((string-match "delete" (cdr ele))
                    (setq marker-color "red"))
                   ((string-match "edit" (cdr ele))
                    (setq marker-color "brown"))
                   (t (setq marker-color "yellow")))
                  (end-of-line)
                  (newline)
                  (insert "    ")
                  (insert (p4-get-mark-dot marker-color))
                  (insert (concat " " (car ele))))
              ;; No files in this cl.
              (end-of-line)
              (newline)
              (insert "    ")
              (insert (p4-get-mark-dot "yellow"))
              (insert " Empty Change List!")))
        ;; Code to hide files of this cl.
        (save-excursion
          (beginning-of-line 2)
          (let ((start (line-beginning-position)))
            (while (looking-at "\\(?:.+#[[:digit:]]+\\|[[:blank:]]+Empty Change List!\\)$")
              (beginning-of-line 2))
            (delete-region start (point))))
        )
      )
    )
  (setq buffer-read-only t))

 ;; TO BE CLEANED.
;; Find out what type of emacs we are running in. We will be using this
;; quite a few times in this program.

(defgroup p4 nil "Perforce VC System." :group 'tools)

;; This can be set to wherever 'p4' lies using p4-set-p4-executable

;; This is a string with default arguments to pass to "p4 diff",
;; "p4 diff2", "p4 describe", etc.
(defcustom p4-default-diff-options "-du"
  "Type of p4 diff output to be displayed. \(regular or context or
unified.\)"
  :type 'string
  :group 'p4)

(defcustom p4-default-depot-completion-prefix "//depot/"
  "Prefix to be used for completion prompt when prompting user for a depot
file."
  :type 'string
  :group 'p4)

;; Set this variable to nil to turn off colorized diff buffers.
(defcustom p4-colorized-diffs t
  "Set this to nil to disable colorized diffs."
  :type 'boolean
  :group 'p4)

;; Set whether P4CONFIG should be used exclusively for VC checking
(defcustom p4-use-p4config-exclusively nil
  "Whether P4 mode should use P4CONFIG exclusively to check whether a file
is under P4 version control. If set to nil, `p4-check-mode' is always
called; otherwise, it checks to see if the file named by P4CONFIG exists in
this or a parent directory, and if so, only then runs p4-check-mode.

This provides for a much faster `p4-find-file-hook'."
  :type 'boolean
  :group 'p4)

;; Auto-refresh?
(defcustom p4-auto-refresh t
  "Set this to automatically refresh p4 submitted files in buffers."
  :type 'boolean
  :group 'p4)

;; Check for empty diffs at submit time
(defcustom p4-check-empty-diffs nil
  "Set this to check for files with empty diffs before submitting."
  :type 'boolean
  :group 'p4)

(defcustom p4-verbose t
  "When set, p4 will pop up the output buffer with the result of the
command."
  :type 'boolean
  :group 'p4)

;; Follow Symlinks?
(defcustom p4-follow-symlinks nil
  "When set, p4 will call `file-truename' on all opened files."
  :type 'boolean
  :group 'p4)

(defcustom p4-mode-hook nil
  "Hook run by `p4-mode'."
  :type 'sexp
  :group 'p4)

(defvar p4-output-buffer-name "*P4 Output*" "P4 Output Buffer.")

;; Set this variable in .emacs if you want p4-set-client-name to complete
;; your client name for you.
(defvar p4-my-clients nil
  "This variable holds the alist of p4 clients that the function
`p4-set-client-name' can complete on.

Set this variable *only* if you don't want P4 to complete on all the clients
in the P4 server.

This is a alist, and should be set using the function
`p4-set-my-clients'. For example, in your .emacs:

\(load-library \"p4\"\)
\(p4-set-my-clients \'(client1 client2 client3)\)")

;; Set this variable in .emacs if you want to alter the completion
;; behavior of p4-set-client-name.

(defcustom p4-strict-complete t
  "Set this variable in .emacs \(or using `customize'\) if you want to alter
the completion behavior of `p4-set-client-name'.
"
  :type 'boolean
  :group 'p4)

(if (not (getenv "P4PORT"))
    (setenv "P4PORT" "perforce:1666"))

(defvar p4-notify-list (getenv "P4NOTIFY") "The P4 Notify List.")

(defcustom p4-sendmail-program (if (boundp 'sendmail-program)
                   sendmail-program
                 nil)
  "The sendmail program. To set this use `customize'."
  :type 'string
  :group 'p4)

(defcustom p4-user-email (if (boundp 'user-mail-address)
                 user-mail-address nil)
  "The e-mail address of the current user. This is used with the
notification system, and must be set if notification should take place. To
set this, use `customize'."
  :type 'string
  :group 'p4)

(defcustom p4-notify nil
  "If this is t then the users in the notification list set by
`p4-set-notify-list' will get a notification of any P4 change submitted from
within emacs."
  :type 'boolean
  :group 'p4)

;; This can be set with p4-toggle-vc-mode
(defcustom p4-do-find-file t
  "If non-nil, the `p4-find-file-hook' will run when opening files."
  :type 'boolean
  :group 'p4)

(defface p4-diff-file-face
  '((((class color) (background light)) (:background "gray90"))
    (((class color) (background dark)) (:background "gray10")))
  "Face used for file pathnames in difference buffers."
  :group 'p4-faces)

(defface p4-diff-head-face
  '((((class color) (background light)) (:background "gray95"))
    (((class color) (background dark)) (:background "gray5")))
  "Face used for ?"
  :group 'p4-faces)

(defface p4-diff-inserted-face
  '((((class color) (background light)) (:foreground "blue"))
    (((class color) (background dark)) (:foreground "cyan")))
  "Face used for new (inserted) text in difference buffers.
When the newer revision contains text not in the older revision, that text will
be marked with this face."
  :group 'p4-faces)

(defface p4-diff-deleted-face
  '((((class color) (background light)) (:foreground "red"))
    (((class color) (background dark)) (:foreground "pink")))
  "Face used for old (deleted) text in difference buffers.
When the older revision contains text not in the newer revision, that text will
be marked with this face."
  :group 'p4-faces)

(defface p4-diff-changed-face
  '((((class color) (background light)) (:foreground "dark green"))
    (((class color) (background dark)) (:foreground "light green")))
  "Face used for changed text in difference buffers.
When a section of text is in both the newer and older revision, but differs
between them, that text will be marked with this face."
  :group 'p4-faces)

(defface p4-depot-branched-face
  '((((class color) (background light)) (:foreground "blue4"))
    (((class color) (background dark)) (:foreground "sky blue")))
  "Face used for branched files."
  :group 'p4-faces)

(defface p4-depot-added-face
  '((((class color) (background light)) (:foreground "blue"))
    (((class color) (background dark)) (:foreground "cyan")))
  "Face used for files added to the depot."
  :group 'p4-faces)

(defface p4-depot-deleted-face
  '((((class color) (background light)) (:foreground "red"))
    (((class color) (background dark)) (:foreground "pink")))
  "Face used for files deleted from the depot."
  :group 'p4-faces)




(defgroup p4-faces nil
  "Customize the appearance of Magit."
  :prefix "p4-"
  :group 'faces
  :group 'magit)

(defface p4-header
  '((t :inherit header-line
       :weight bold))
  "Face for generic header lines.

Many Magit faces inherit from this one by default."
  :group 'p4-faces)

(defface p4-section-title
  '((t :inherit p4-header
       :underline t))
  "Face for section titles."
  :group 'p4-faces)

(defface p4-header-title
  '((t :inherit p4-header
       :underline t
       :foreground "sky blue"
       :background "brown"))
  "Face for section titles."
  :group 'p4-faces)

(defface p4-root-dir-face
  '((t :inherit font-lock-function-name-face))
  "Face for the current branch."
  :group 'p4-faces)

(defface p4-diff-file-header
  '((t :inherit diff-file-header))
  "Face for diff file header lines."
  :group 'p4-faces)

(defface p4-diff-hunk-header
  '((t :inherit diff-hunk-header))
  "Face for diff hunk header lines."
  :group 'p4-faces)

(defface p4-diff-add
  '((t :inherit diff-added))
  "Face for lines in a diff that have been added."
  :group 'p4-faces)

(defface p4-diff-merge-current
  '((t :inherit font-lock-preprocessor-face))
  "Face for merge conflict marker 'current' line."
  :group 'p4-faces)

(defface p4-diff-merge-separator
  '((t :inherit font-lock-preprocessor-face))
  "Face for merge conflict marker seperator."
  :group 'p4-faces)

(defface p4-diff-merge-diff3-separator
  '((t :inherit font-lock-preprocessor-face))
  "Face for merge conflict marker seperator."
  :group 'p4-faces)

(defface p4-diff-merge-proposed
  '((t :inherit font-lock-preprocessor-face))
  "Face for merge conflict marker 'proposed' line."
  :group 'p4-faces)

(defface p4-diff-none
  '((t :inherit diff-context))
  "Face for lines in a diff that are unchanged."
  :group 'p4-faces)

(defface p4-diff-del
  '((t :inherit diff-removed))
  "Face for lines in a diff that have been deleted."
  :group 'p4-faces)

(defface p4-log-graph
  '((((class color) (background light))
     :foreground "grey11")
    (((class color) (background dark))
     :foreground "grey80"))
  "Face for the graph element of the log output."
  :group 'p4-faces)

(defface p4-log-sha1
  '((((class color) (background light))
     :foreground "firebrick")
    (((class color) (background dark))
     :foreground "tomato"))
  "Face for the sha1 element of the log output."
  :group 'p4-faces)

(defface p4-log-author
  '((((class color) (background light))
     :foreground "firebrick")
    (((class color) (background dark))
     :foreground "tomato"))
  "Face for the author element of the log output."
  :group 'p4-faces)

(defface p4-log-author-date-cutoff
  '((t :inherit p4-log-author
       :bold t))
  "Face for the author element's cutoff mark."
  :group 'p4-faces)

(defface p4-log-date
  '((t))
  "Face for the date element of the log output."
  :group 'p4-faces)

(defface p4-log-message
  '((t))
  "Face for the message element of the log output."
  :group 'p4-faces)

(defface p4-item-highlight
  '((t :inherit highlight))
  "Face for highlighting the current item."
  :group 'p4-faces)

(defface p4-item-mark
  '((t :inherit secondary-selection))
  "Face for highlighting marked item."
  :group 'p4-faces)

(defface p4-log-head-label-bisect-good
  '((((class color) (background light))
     :box t
     :background "light green"
     :foreground "dark olive green")
    (((class color) (background dark))
     :box t
     :background "light green"
     :foreground "dark olive green"))
  "Face for good bisect refs."
  :group 'p4-faces)

(defface p4-log-head-label-bisect-bad
  '((((class color) (background light))
     :box t
     :background "IndianRed1"
     :foreground "IndianRed4")
    (((class color) (background dark))
     :box t
     :background "IndianRed1"
     :foreground "IndianRed4"))
  "Face for bad bisect refs."
  :group 'p4-faces)

(defface p4-log-head-label-remote
  '((((class color) (background light))
     :box t
     :background "Grey85"
     :foreground "OliveDrab4")
    (((class color) (background dark))
     :box t
     :background "Grey11"
     :foreground "DarkSeaGreen2"))
  "Face for remote branch head labels shown in log buffer."
  :group 'p4-faces)

(defface p4-log-head-label-tags
  '((((class color) (background light))
     :box t
     :background "LemonChiffon1"
     :foreground "goldenrod4")
    (((class color) (background dark))
     :box t
     :background "LemonChiffon1"
     :foreground "goldenrod4"))
  "Face for tag labels shown in log buffer."
  :group 'p4-faces)

(defface p4-log-head-label-patches
  '((((class color) (background light))
     :box t
     :background "IndianRed1"
     :foreground "IndianRed4")
    (((class color) (background dark))
     :box t
     :background "IndianRed1"
     :foreground "IndianRed4"))
  "Face for Stacked Git patches."
  :group 'p4-faces)

(defface p4-whitespace-warning-face
  '((t :inherit trailing-whitespace))
  "Face for highlighting whitespace errors in Magit diffs."
  :group 'p4-faces)

(defface p4-log-head-label-local
  '((((class color) (background light))
     :box t
     :background "Grey85"
     :foreground "LightSkyBlue4")
    (((class color) (background dark))
     :box t
     :background "Grey13"
     :foreground "LightSkyBlue1"))
  "Face for local branch head labels shown in log buffer."
  :group 'p4-faces)

(defface p4-log-head-label-default
  '((((class color) (background light))
     :box t
     :background "Grey50")
    (((class color) (background dark))
     :box t
     :background "Grey50"))
  "Face for unknown ref labels shown in log buffer."
  :group 'p4-faces)

(defface p4-valid-signature
  (if (require 'epa nil t)
      '((t :inherit epa-validity-high))
    '((t :weight bold :foreground "PaleTurquoise")))
  "Face for valid gpg signatures."
  :group 'p4-faces)



;; Now add a hook to find-file-hooks
;; (add-hook 'find-file-hooks 'p4-find-file-hook)
;; .. and one to kill-buffer-hook
(add-hook 'kill-buffer-hook 'p4-kill-buffer-hook)

;; Tell Emacs about this new kind of minor mode
(defvar p4-mode nil "Is this file under p4?")
(make-variable-buffer-local 'p4-mode)
(put 'p4-mode 'permanent-local t)

(defvar p4-offline-mode nil "Is this file under p4 but handled in offline mode?")
(make-variable-buffer-local 'p4-offline-mode)
(put 'p4-offline-mode 'permanent-local t)

(defvar p4-minor-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-x\C-q" 'p4-toggle-read-only)
    map)
  "Keymap for p4 minor mode")
(fset 'p4-minor-map p4-minor-map)
(or (assoc 'p4-mode minor-mode-alist)
    (setq minor-mode-alist (cons '(p4-mode p4-mode)
                 minor-mode-alist)))
(or (assoc 'p4-mode minor-mode-map-alist)
    (setq minor-mode-map-alist
      (cons '(p4-mode . p4-minor-map) minor-mode-map-alist)))
(or (assoc 'p4-offline-mode minor-mode-alist)
    (setq minor-mode-alist (cons '(p4-offline-mode p4-offline-mode)
                 minor-mode-alist)))
(or (assoc 'p4-offline-mode minor-mode-map-alist)
    (setq minor-mode-map-alist
      (cons '(p4-offline-mode . p4-minor-map) minor-mode-map-alist)))

(defvar p4-form-current-command nil)
(make-variable-buffer-local 'p4-form-current-command)
(put 'p4-form-current-command 'permanent-local t)
(set-default 'p4-form-current-command nil)

(defvar p4-form-current-args nil)
(make-variable-buffer-local 'p4-form-current-args)
(put 'p4-form-current-args 'permanent-local t)
(set-default 'p4-form-current-args nil)

;; To check if the current buffer's modeline and menu need to be altered
(defvar p4-vc-check nil)
(make-variable-buffer-local 'p4-vc-check)
(put 'p4-vc-check 'permanent-local t)
(set-default 'p4-vc-check nil)

(defvar p4-set-client-hooks nil
  "List of functions to be called after a p4 client is changed.
The buffer's local variables (if any) will have been processed before the
functions are called.")

(defvar p4-timer nil "Timer object that will be set to cleanup the caches
periodically.")

(defcustom p4-cleanup-time 600 "seconds after which `p4-cache-cleanup' will
check for dirty caches."
  :type 'integer
  :group 'p4)

(defcustom p4-cleanup-cache t "`p4-cache-cleanup' will cleanup the
branches/clients/dirs/labels caches once in a while if this is non-nil."
  :type 'boolean
  :group 'p4)

(defvar p4-all-buffer-files nil "An associated list of all buffers and
their files under p4 version control. This is to enable autorefreshing of
p4 submitted files being visited by the buffer.")

(defvar p4-file-refresh-timer nil "Timer object that will be set to refresh
the files in Emacs buffers that have been modified by a `p4-submit'.")

(defcustom p4-file-refresh-timer-time 60 "seconds after which
`p4-file-refresh' will check for modified files in Emacs buffers. Set this
variable to 0 to disable periodic refreshing."
  :type 'integer
  :group 'p4)

(defcustom p4-exec-arg-len-max 20000 "Maximum total length of all
arguments to p4 commands."
  :type 'integer
  :group 'p4)

(defface p4-form-comment-face
  '((t :inherit font-lock-comment-face))
  "Face for comment in P4 form mode."
  :group 'p4-faces)
(defvar p4-form-comment-face 'p4-form-comment-face)

(defface p4-form-keyword-face
  '((t :inherit font-lock-keyword-face))
  "Face for keyword in P4 form mode."
  :group 'p4-faces)
(defvar p4-form-keyword-face 'p4-form-keyword-face)

(defvar p4-form-font-lock-keywords
  '(("^#.*$" . p4-form-comment-face)
    ("^\\w+:" . p4-form-keyword-face)))

(define-derived-mode p4-form-mode indented-text-mode "P4 Form"
  "Major mode for P4 form derived from `indented-text-mode'"
  (setq fill-column 80
    indent-tabs-mode t
    font-lock-defaults '(p4-form-font-lock-keywords t)))

;;; All functions start here.

(defun p4-get-writable-output-buffer ()
  "Do not use this function. Old code assumes output buffer is writable."
  (let ((buffer (p4-make-output-buffer p4-output-buffer-name)))
    (with-current-buffer buffer
      (setq buffer-read-only nil))
    buffer))

;; A generic function that we use to execute p4 commands
;; If executing the p4 command fails with a "password invalid" error
;; and no-login is false, p4-login will be called to let the user
;; login. The failed command will then be retried.
(defun p4-exec-p4 (output-buffer args &optional dummy)
  "Internal function called by various p4 commands.
Executes p4 in the current buffer's current directory
with output to a dedicated output buffer.
If successful, adds the P4 menu to the current buffer.
Does auto re-highlight management (whatever that is)."
  (let ((result (apply 'call-process  (p4-get-executable) nil
               output-buffer
               nil        ; update display?
               args)))
    (p4-menu-add)
    result))

(defun p4-call-p4-here (&rest args)
  "Internal function called by various p4 commands.
Executes p4 in the current buffer (generally a temp)."
  (apply 'call-process  (p4-get-executable) nil
         t
         nil            ; update display?
         args))

(defun p4-start-p4 (buffer args)
  "Start P4 command asynchronously.
Return process object"
  (apply 'start-process "P4" buffer  (p4-get-executable) args))



;; The menu definition is in the XEmacs format. Emacs parses and converts
;; this definition to its own menu creation commands.

(defalias 'p4-toggle-vc-mode-off 'p4-toggle-vc-mode)
(defalias 'p4-toggle-vc-mode-on 'p4-toggle-vc-mode)

(defvar p4-menu-spec
  '(["Specify Arguments..." universal-argument t]
    ["--" nil nil]
    ["Add Current to P4" p4-add
     (and (p4-buffer-file-name) (not p4-mode))]
    ["Check out/Edit"    p4-edit
     (and (p4-buffer-file-name-2) (or (not p4-mode) buffer-read-only))]
    ["Re-open"           p4-reopen
     (and (p4-buffer-file-name-2) (or (not p4-mode) (not buffer-read-only)))]
    ["Revert File"  p4-revert
     (and (p4-buffer-file-name-2) (or (not p4-mode) (not buffer-read-only)))]
    ["Delete File from Depot"  p4-delete
     (and (p4-buffer-file-name-2) (or (not p4-mode) buffer-read-only))]
    ["Rename Depot File" p4-rename
     (and (p4-buffer-file-name-2) (or (not p4-mode) buffer-read-only))]
    ["Submit Changes"  p4-submit t]
    ["--" nil nil]
    ["Sync/Get Files from Depot" p4-get t]
    ["--" nil nil]
    ["Show Opened Files"    p4-opened t]
    ["Filelog" p4-log (p4-buffer-file-name-2)]
    ["Changes" p4-changes t]
    ["Describe Change" p4-describe t]
    ["--" nil nil]
    ["Diff 2 Versions" p4-diff2 (p4-buffer-file-name-2)]
    ["Diff Current" p4-diff t]
    ["Diff All Opened Files" p4-diff-all-opened t]
    ["Diff Current with Ediff"   p4-ediff
     (and (p4-buffer-file-name) (not buffer-read-only) p4-mode)]
    ["Diff 2 Versions with Ediff"   p4-ediff2 (p4-buffer-file-name-2)]
    ["--" nil nil]
    ["Schedule Integrations" p4-integ t]
    ["Resolve Conflicts" p4-resolve t]
    ["--" nil nil]
    ["Print" p4-print (p4-buffer-file-name-2)]
    ["Print with Revision History" p4-blame
     (p4-buffer-file-name-2)]
    ["Find File using Depot Spec" p4-depot-find-file
     p4-do-find-file]
    ["--" nil nil]
    ["Edit a Branch Specification" p4-branch t]
    ["Edit a Label Specification" p4-label t]
    ["Edit a Client Specification" p4-client t]
    ["Edit a User Specification" p4-user t]
    ["--" nil nil]
    ["Show Version" p4-emacs-version t]
    ["Disable P4 VC Check"  p4-toggle-vc-mode-off
     p4-do-find-file]
    ["Enable P4 VC Check"     p4-toggle-vc-mode-on
     (not p4-do-find-file)]
    ["--" nil nil]
    ["Set P4 Config"  p4-set-client-config p4-do-find-file]
    ["Get Current P4 Config"  p4-get-client-config
     p4-do-find-file]
    ["--" nil nil]
    ["Set P4 Client"  p4-set-client-name p4-do-find-file]
    ["Get Current P4 Client"  p4-get-client-name
     p4-do-find-file]
    ["--" nil nil]
    ["Set P4 Server/Port"     p4-set-p4-port p4-do-find-file]
    ["Get Current P4 Server/Port"     p4-get-p4-port
     p4-do-find-file]
    ["--" nil nil]
    ["Set P4 Notification List"  p4-set-notify-list
     p4-mode]
    ["Get P4 Notification List"  p4-get-notify-list p4-notify]
    )
  "The P4 menu definition")

(defun p4-mode-menu (modestr)
  (cons modestr p4-menu-spec))

(easy-menu-add-item nil '("tools")
            (easy-menu-create-menu "P4" p4-menu-spec)
            "PCL-CVS")

(defun p4-depot-output (command &optional args)
  "Executes p4 command inside a buffer.
Returns the buffer."
  (let ((buffer (p4-get-writable-output-buffer)))
    (p4-exec-p4 buffer (cons command args) t)
    buffer))

(defun p4-menu-add ()
  "To add the P4 menu bar button for files that are already not in
the P4 depot or in the current client view.."
  (interactive)
  (cond (p4-running-xemacs
         (if (not (boundp 'p4-mode))
             (setq p4-mode nil))
         (easy-menu-add (p4-mode-menu "P4"))))
  t)

;; The kill-buffer hook for p4.
(defun p4-kill-buffer-hook ()
  "To Remove a file and its associated buffer from our global list of P4
controlled files."
  (if p4-vc-check
      (p4-refresh-refresh-list (p4-buffer-file-name)
                   (buffer-name))))



(defun p4-noinput-buffer-action (cmd
                 dummy
                 show-output
                 &optional arguments)
  "Internal function called by various p4 commands."
  (p4-exec-p4 (p4-get-writable-output-buffer)
          (append (list cmd) arguments)
          t)
  (p4-partial-cache-cleanup cmd)
  (when show-output
    (p4-push-window-config)
    (display-buffer p4-output-buffer-name)))

(defun p4-simple-command (cmd args)
  (let* ((buffer (p4-make-output-buffer "*P4*"))
     (inhibit-read-only t)
     (ret (p4-exec-p4 buffer (cons cmd args))))
    (p4-partial-cache-cleanup cmd)
    (with-current-buffer buffer
      (if (and
       (zerop ret)
       (= (count-lines (point-min) (point-max)) 1)
       (not (save-excursion
          (goto-char (point-min))
          (looking-at "==== "))))
      (message (buffer-substring (point-min)
                     (save-excursion
                       (goto-char (point-min))
                       (end-of-line)
                       (point))))
    (p4-push-window-config)
    (display-buffer buffer)))
    (unless (zerop ret)
      (error "P4 exits abnormally"))))

(defun p4-simple-command-and-revert-buffer (cmd args)
  (p4-simple-command cmd args)
  (when (p4-buffer-file-name)
    (revert-buffer t t)))


;; The p4 add command
(defp4cmd p4-add ()
  "add" "To add the current file to the depot, type \\[p4-add].\n"
  (interactive)
  (let ((args (p4-buffer-file-name))
    refresh-after)
    (if (or current-prefix-arg (not args))
    (progn
      (setq args (if (p4-buffer-file-name-2)
             (p4-buffer-file-name-2)
               ""))
      (setq args (p4-make-list-from-string
              (p4-read-arg-string "p4 add: " (cons args 0))))
      (setq refresh-after t))
      (setq args (list args)))
    (p4-simple-command "add" args))
  (p4-update-opened-list))


;; The p4 delete command
(defp4cmd p4-delete ()
  "delete" "To delete the current file from the depot, type \\[p4-delete].\n"
  (interactive)
  (let ((args (p4-buffer-file-name)))
    (if (or current-prefix-arg (not args))
    (setq args (p4-make-list-from-string
            (p4-read-arg-string "p4 delete: "
                    (p4-buffer-file-name-2))))
      (setq args (list args)))
    (if (yes-or-no-p "Really delete from depot? ")
    (p4-simple-command "delete" args)))
  (p4-update-opened-list))


(defun p4-insert-no-properties (str)
  (let ((start (point))
    end)
    (insert str)
    (setq end (point))
    (set-text-properties start end nil)))

(defun p4-font-lock-buffer (buf-name)
  (save-excursion
    (let (file-name (first-line ""))
      (set-buffer buf-name)
      (goto-char (point-min))
      (if (looking-at "^//[^#@]+/\\([^/#@]+\\)")
      (progn
        (setq file-name (match-string 1))
        (forward-line 1)
        (setq first-line (buffer-substring (point-min) (point)))
        (delete-region (point-min) (point))))
      (setq buffer-file-name file-name)
      (set-auto-mode)
      (setq buffer-file-name nil)
      (condition-case nil
      (font-lock-fontify-buffer)
    (error nil))
      (fundamental-mode)
      (setq buffer-read-only nil)
      (goto-char (point-min))
      (p4-insert-no-properties first-line))))


(defun p4-scroll-down-1-line ()
  "Scroll down one line"
  (interactive)
  (scroll-down 1))

(defun p4-scroll-up-1-line ()
  "Scroll up one line"
  (interactive)
  (scroll-up 1))

(defun p4-scroll-down-1-window ()
  "Scroll down one window"
  (interactive)
  (scroll-down
   (- (window-height) next-screen-context-lines)))

(defun p4-scroll-up-1-window ()
  "Scroll up one window"
  (interactive)
  (scroll-up
   (- (window-height) next-screen-context-lines)))

(defun p4-top-of-buffer ()
  "Top of buffer"
  (interactive)
  (goto-char (point-min)))

(defun p4-bottom-of-buffer ()
  "Bottom of buffer"
  (interactive)
  (goto-char (point-max)))

(defun p4-delete-other-windows ()
  "Make buffer full height"
  (interactive)
  (delete-other-windows))

(defun p4-goto-next-diff ()
  "Next diff"
  (interactive)
  (goto-char (window-start))
  (if (= (point) (point-max))
      (error "At bottom"))
  (forward-line 1)
  (re-search-forward "^====" nil "")
  (beginning-of-line)
  (set-window-start (selected-window) (point)))

(defun p4-goto-prev-diff ()
  "Previous diff"
  (interactive)
  (if (= (point) (point-min))
      (error "At top"))
  (goto-char (window-start))
  (re-search-backward "^====" nil "")
  (set-window-start (selected-window) (point)))

(defun p4-next-depot-file ()
  "Next file"
  (interactive)
  (goto-char (window-start))
  (if (= (point) (point-max))
      (error "At bottom"))
  (forward-line 1)
  (re-search-forward "^//[^/@# ][^/@#]*/[^@#]+#[0-9]+ - " nil "")
  (beginning-of-line)
  (set-window-start (selected-window) (point)))

(defun p4-prev-depot-file ()
  "Previous file"
  (interactive)
  (if (= (point) (point-min))
      (error "At top"))
  (goto-char (window-start))
  (re-search-backward "^//[^/@# ][^/@#]*/[^@#]+#[0-9]+ - " nil "")
  (set-window-start (selected-window) (point)))


(defun p4-next-depot-diff ()
  "Next diff"
  (interactive)
  (goto-char (window-start))
  (if (= (point) (point-max))
      (error "At bottom"))
  (forward-line 1)
  (re-search-forward "^\\(@@\\|\\*\\*\\* \\|[0-9]+[,acd]\\)" nil "")
  (beginning-of-line)
  (set-window-start (selected-window) (point)))

(defun p4-prev-depot-diff ()
  "Previous diff"
  (interactive)
  (if (= (point) (point-min))
      (error "At top"))
  (goto-char (window-start))
  (re-search-backward "^\\(@@\\|\\*\\*\\* \\|[0-9]+[,acd]\\)" nil "")
  (set-window-start (selected-window) (point)))

(defun p4-moveto-print-rev-column (old-column)
  (let ((colon (save-excursion
         (move-to-column 0)
         (if (looking-at "[^:\n]*:")
             (progn
               (goto-char (match-end 0))
               (current-column))
           0))))
    (move-to-column old-column)
    (if (and (< (current-column) colon)
         (re-search-forward "[^ ][ :]" nil t))
    (goto-char (match-beginning 0)))))

(defun p4-next-change-rev-line ()
  "Next change/revision line"
  (interactive)
  (let ((c (current-column)))
    (move-to-column 1)
    (re-search-forward "^ *[0-9]+ +[0-9]+[^:]+:" nil "")
    (p4-moveto-print-rev-column c)))

(defun p4-prev-change-rev-line ()
  "Previous change/revision line"
  (interactive)
  (let ((c (current-column)))
    (forward-line -1)
    (move-to-column 32)
    (re-search-backward "^ *[0-9]+ +[0-9]+[^:]*:" nil "")
    (p4-moveto-print-rev-column c)))

(defun p4-toggle-line-wrap ()
  "Toggle line wrap mode"
  (interactive)
  (setq truncate-lines (not truncate-lines))
  (save-window-excursion
    (recenter)))

(defun p4-quit-current-buffer ()
  "Quit a buffer"
  (interactive)
  (bury-buffer)
  (p4-pop-window-config 1))

(defun p4-buffer-mouse-clicked (event)
  "Function to translate the mouse clicks in a P4 filelog buffer to
character events"
  (interactive "e")
  (let (win pnt)
    (cond (p4-running-xemacs
       (setq win (event-window event))
       (setq pnt (event-point event)))
      (p4-running-emacs
       (setq win (posn-window (event-end event)))
       (setq pnt (posn-point (event-start event)))))
    (select-window win)
    (goto-char pnt)
    (p4-buffer-commands pnt)))

(defun p4-buffer-mouse-clicked-3 (event)
  "Function to translate the mouse clicks in a P4 filelog buffer to
character events"
  (interactive "e")
  (let (win pnt)
    (cond (p4-running-xemacs
       (setq win (event-window event))
       (setq pnt (event-point event)))
      (p4-running-emacs
       (setq win (posn-window (event-end event)))
       (setq pnt (posn-point (event-start event)))))
    (select-window win)
    (goto-char pnt)
    (let ((link-name (or (get-char-property pnt 'link-client-name)
             (get-char-property pnt 'link-depot-name)))
      (rev (get-char-property pnt 'rev)))
      (cond (link-name
         (p4-diff))
        (rev
         (p4-diff2 rev "#head"))
        (t
         (error "No file to diff!"))))))

(defun p4-buffer-commands (pnt)
  "Function to get a given property and do the appropriate command on it"
  (interactive "d")
  (let ((changeList (get-char-property pnt 'changeList))
        (rev (get-char-property pnt 'rev))
        (change (get-char-property pnt 'change))
        (action (get-char-property pnt 'action))
        (user (get-char-property pnt 'user))
        (group (get-char-property pnt 'group))
        (client (get-char-property pnt 'client))
        (label (get-char-property pnt 'label))
        (branch (get-char-property pnt 'branch))
        (filename (p4-buffer-file-name-2)))

    (cond

     (changeList ;; Toggle change list.
      (p4-manager-toggle-changeList changeList))

     ((and (not action) rev)
      (let ((fn1 (concat filename "#" rev)))
        (p4-noinput-buffer-action "print" nil t (list fn1))
        (p4-activate-print-buffer "*P4 print*" t)))

     (action
      (let ((rev2 (int-to-string (1- (string-to-number rev)))))
        (if (> (string-to-number rev2) 0)
            (p4-diff2 (concat "#" rev2) (concat "#" rev))
          (error "There is no earlier revision to diff."))))

     (change (p4-describe-internal
              (append (p4-make-list-from-string p4-default-diff-options)
                      (list change))))
     ;; (user (p4-user user))
     ;; (group (p4-group group))
     ;; (client (p4-client client))
     ;; (label (p4-label (list label)))
     ;; (branch (p4-branch (list branch)))

     ;; Check if a "filename link" or an active "diff buffer area" was
     ;; selected.
     (t
      (let ((link-client-name (get-char-property pnt 'link-client-name))
            (link-depot-name (get-char-property pnt 'link-depot-name))
            (block-client-name (get-char-property pnt 'block-client-name))
            (block-depot-name (get-char-property pnt 'block-depot-name))
            (p4-history-for (get-char-property pnt 'history-for))
            (first-line (get-char-property pnt 'first-line))
            (start (get-char-property pnt 'start)))
        (cond
         (p4-history-for
          (p4-file-change-log "filelog" (list p4-history-for)))
         ((or link-client-name link-depot-name)
          (p4-find-file-or-print-other-window
           link-client-name link-depot-name))
         ((or block-client-name block-depot-name)
          (if first-line
              (let ((c (max 0 (- pnt
                                 (save-excursion
                                   (goto-char pnt)
                                   (beginning-of-line)
                                   (point))
                                 1)))
                    (r first-line))
                (save-excursion
                  (goto-char start)
                  (while (re-search-forward "^[ +>].*\n" pnt t)
                    (setq r (1+ r))))
                (p4-find-file-or-print-other-window
                 block-client-name block-depot-name)
                (goto-line r)
                (if (not block-client-name)
                    (forward-line 1))
                (beginning-of-line)
                (goto-char (+ (point) c)))
            (p4-find-file-or-print-other-window
             block-client-name block-depot-name)))
         (t
          (let* ((content (buffer-substring-no-properties
                          (line-beginning-position) (line-end-position)))
                 fn)
            (cond
             ((string-match p4-r-match-depot-file content)
              (find-file (p4-make-local-path content)))
             (t
              (error "There is no file at that cursor location!"))
             )
            ))
          ))))))

(defun p4-find-file-or-print-other-window (client-name depot-name)
  (if client-name
      (find-file-other-window client-name)
    (p4-depot-find-file depot-name)))

(defun p4-find-file-other-window ()
  "Open/print file"
  (interactive)
  (let ((link-client-name (get-char-property (point) 'link-client-name))
    (link-depot-name (get-char-property (point) 'link-depot-name))
    (block-client-name (get-char-property (point) 'block-client-name))
    (block-depot-name (get-char-property (point) 'block-depot-name)))
    (cond ((or link-client-name link-depot-name)
       (p4-find-file-or-print-other-window
        link-client-name link-depot-name)
       (other-window 1))
      ((or block-client-name block-depot-name)
       (p4-find-file-or-print-other-window
        block-client-name block-depot-name)
       (other-window 1)))))

(defun p4-log-short-format ()
  "Short format"
  (interactive)
  (setq buffer-invisibility-spec t)
  (redraw-display))

(defun p4-log-long-format ()
  "Long format"
  (interactive)
  (setq buffer-invisibility-spec (list))
  (redraw-display))

(defun p4-scroll-down-1-line-other-w ()
  "Scroll other window down one line"
  (interactive)
  (scroll-other-window -1))

(defun p4-scroll-up-1-line-other-w ()
  "Scroll other window up one line"
  (interactive)
  (scroll-other-window 1))

(defun p4-scroll-down-1-window-other-w ()
  "Scroll other window down one window"
  (interactive)
  (scroll-other-window
   (- next-screen-context-lines (window-height))))

(defun p4-scroll-up-1-window-other-w ()
  "Scroll other window up one window"
  (interactive)
  (scroll-other-window
   (- (window-height) next-screen-context-lines)))

(defun p4-top-of-buffer-other-w ()
  "Top of buffer, other window"
  (interactive)
  (other-window 1)
  (goto-char (point-min))
  (other-window -1))

(defun p4-bottom-of-buffer-other-w ()
  "Bottom of buffer, other window"
  (interactive)
  (other-window 1)
  (goto-char (point-max))
  (other-window -1))

(defun p4-goto-next-change ()
  "Next change"
  (interactive)
  (let ((c (current-column)))
    (forward-line 1)
    (while (get-char-property (point) 'invisible)
      (forward-line 1))
    (move-to-column c)))

(defun p4-goto-prev-change ()
  "Previous change"
  (interactive)
  (let ((c (current-column)))
    (forward-line -1)
    (while (get-char-property (point) 'invisible)
      (forward-line -1))
    (move-to-column c)))


(defun p4-buffer-set-face-property (regexp face-property)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward regexp nil t)
      (let ((start (match-beginning 0))
        (end (match-end 0)))
    (p4-set-extent-properties start end
                  (list (cons 'face face-property)))))))

(defun p4-mark-depot-list-buffer (&optional print-buffer)
  (when print-buffer
    (save-excursion
      (let ((depot-regexp
             (if print-buffer
                 "\\(^\\)\\(//[^/@# ][^/@#]*/[^@#]+\\)#[0-9]+ - "
               "^\\(\\.\\.\\. [^/\n]*\\|==== \\)?\\(//[^/@# ][^/@#]*/[^#\n]*\\)")))
        (goto-char (point-min))
        (while (re-search-forward depot-regexp nil t)
          (let ((p4-depot-file (match-string 2))
                (start (match-beginning 2))
                (end (match-end 2))
                (branching-op-p (and (match-string 1)
                                     (string-match "\\.\\.\\. \\.\\.\\..*"
                                                   (match-string 1))))
                prop-list)
            (setq prop-list (list (cons 'link-depot-name
                                        p4-depot-file)))
            ;; some kind of operation related to branching/integration
            (if branching-op-p
                (setq prop-list (append (list
                                         (cons 'history-for p4-depot-file)
                                         (cons 'face
                                               'p4-depot-branched-face))
                                        prop-list)))
            (p4-create-active-link start end prop-list)))))))

(defun p4-activate-diff-buffer ()
  (save-excursion
    (p4-mark-depot-list-buffer)
    (if p4-colorized-diffs
        (progn
          (p4-buffer-set-face-property "^=.*\n" 'p4-diff-file-face)
          (p4-buffer-set-face-property "^[@*].*" 'p4-diff-head-face)
          (p4-buffer-set-face-property "^\\([+>].*\n\\)+" 'p4-diff-inserted-face)
          (p4-buffer-set-face-property "^\\([-<].*\n\\)+" 'p4-diff-deleted-face)
          (p4-buffer-set-face-property "^\\(!.*\n\\)+" 'p4-diff-change-face)))

    (goto-char (point-min))
    (while (re-search-forward "^\\(==== //\\).*\n"
                      nil t)
      (let* ((link-depot-name (get-char-property (match-end 1) 'link-depot-name))
             (start (match-beginning 0))
             (end (save-excursion
                (if (re-search-forward "^==== " nil t)
                (match-beginning 0)
                  (point-max)))))
        (if link-depot-name
            (p4-set-extent-properties start end
                          (list (cons 'block-depot-name
                              link-depot-name))))))

    (goto-char (point-min))
    (while (re-search-forward
            (concat "^[@0-9].*\\([cad+]\\)\\([0-9]*\\).*\n"
                "\\(\\(\n\\|[^@0-9\n].*\n\\)*\\)") nil t)
      (let ((first-line (string-to-number (match-string 2)))
            (start (match-beginning 3))
            (end (match-end 3)))
        (p4-set-extent-properties start end
                      (list (cons 'first-line first-line)
                        (cons 'start start)))))

    (goto-char (point-min))
    (let ((stop
           (if (re-search-forward "^\\(\\.\\.\\.\\|====\\)" nil t)
               (match-beginning 0)
             (point-max))))
      (p4-find-change-numbers (current-buffer) (point-min) stop))

    (goto-char (point-min))
    (if (looking-at "^Change [0-9]+ by \\([^ @]+\\)@\\([^ \n]+\\)")
        (let ((user-match 1)
              (cl-match 2)
              cur-user cur-client)
          (setq cur-user (match-string user-match))
          (setq cur-client (match-string cl-match))
          (p4-create-active-link (match-beginning user-match)
                     (match-end user-match)
                     (list (cons 'user cur-user)))
          (p4-create-active-link (match-beginning cl-match)
                     (match-end cl-match)
                     (list (cons 'client cur-client)))))))


;; The p4 describe command
;; (defp4cmd p4-describe ()
;;   "describe" "To get a description for a change number, type \\[p4-describe].\n"
;;   (interactive)
;;   (let ((arg-string (p4-make-list-from-string
;;              (read-string "p4 describe: "
;;                   (concat p4-default-diff-options " ")))))
;;     (p4-describe-internal arg-string)))

;; Internal version of the p4 describe command
(defun p4-describe-internal (arg-string)
  (p4-call-command "describe" arg-string
           (concat "*P4 describe: " (p4-list-to-string arg-string) "*")
           'p4-diff-mode
           'p4-activate-diff-buffer))

;; The p4 opened command
;; (defp4cmd p4-opened (&rest args)
;;   "opened"
;;   "To display list of files opened for pending change, type \\[p4-opened].\n"
;;   (interactive (p4-read-args* "p4 opened: "))
;;   (p4-opened-internal args))

(defun p4-opened-internal (args)
  (let ((p4-client (p4-current-client)))
    (p4-call-command "opened" args (concat "*Opened Files: " p4-client "*")
             'p4-basic-list-mode)))

(defun p4-update-opened-list ()
  ;; (when (get-buffer-window (concat "*Opened Files: " (p4-current-client) "*"))
  ;;   (p4-opened-internal nil))
  )

(defun p4-regexp-create-links (buffer-name regexp property)
  (with-current-buffer buffer-name
    (setq buffer-read-only nil)
    (goto-char (point-min))
    (while (re-search-forward regexp nil t)
      (let ((start (match-beginning 1))
        (end (match-end 1))
        (str (match-string 1)))
    (p4-create-active-link start end (list (cons property str)))))
    (setq buffer-read-only t)))

;; The p4 users command
;; (defp4cmd p4-users (&rest args)
;;   "users" "To display list of known users, type \\[p4-users].\n"
;;   (interactive (p4-read-args* "p4 users: " "user"))
;;   (p4-call-command "users" args "*P4 users*" nil
;;            (lambda ()
;;              (p4-regexp-create-links "*P4 users*" "^\\([^ ]+\\).*\n" 'user))))

;; (defp4cmd p4-groups (&rest args)
;;   "groups" "To display list of known groups, type \\[p4-groups].\n"
;;   (interactive (p4-read-args* "p4 groups: " "group"))
;;   (p4-call-command "groups" args "*P4 groups*" nil
;;            (lambda ()
;;              (p4-regexp-create-links "*P4 groups*" "^\\(.*\\)\n" 'group))))

;; ;; The p4 jobs command
;; (defp4cmd p4-jobs (&rest args)
;;   "jobs" "To display list of jobs, type \\[p4-jobs].\n"
;;   (interactive (p4-read-args* "p4 jobs: "))
;;   (p4-call-command "jobs" args "*P4 jobs*"))

;; ;; The p4 fix command
;; (defp4cmd p4-fix (&rest args)
;;   "fix" "To mark jobs as being fixed by a changelist number, type \\[p4-fix].\n"
;;   (interactive (p4-read-args "p4 fix: " "job"))
;;   (p4-call-command "fix" args p4-output-buffer-name))

;; ;; The p4 fixes command
;; (defp4cmd p4-fixes (&rest args)
;;   "fixes" "To list what changelists fix what jobs, type \\[p4-fixes].\n"
;;   (interactive (p4-read-args* "p4 fixes: "))
;;   (p4-call-command "fixes" args "*P4 fixes*"))

;; ;; The p4 where command
;; (defp4cmd p4-where ()
;;   "where"
;;   "To show how local file names map into depot names, type \\[p4-where].\n"
;;   (interactive)
;;   (let (args)
;;     (if current-prefix-arg
;;     (setq args (p4-make-list-from-string
;;             (p4-read-arg-string "p4 where: "
;;                     (p4-buffer-file-name-2)))))
;;     (p4-simple-command "where" args)))


(defun p4-check-cmd-line-switch (args)
  (when (or (member "-i" args)
        (member "-o" args))
    (error "Do not specify -i or -o switches.")))


(defun p4-refresh-refresh-list (buffile bufname)
  "Refresh the list of files to be refreshed."
  (setq p4-all-buffer-files (delete (list buffile bufname)
                    p4-all-buffer-files))
  (if (not p4-all-buffer-files)
      (progn
    (if (and p4-running-emacs (timerp p4-file-refresh-timer))
        (cancel-timer p4-file-refresh-timer))
    (if (and p4-running-xemacs p4-file-refresh-timer)
        (disable-timeout p4-file-refresh-timer))
    (setq p4-file-refresh-timer nil))))

;; Set keymap. We use the C-x p Keymap for all perforce commands
(defvar p4-prefix-map
  (let ((map (make-sparse-keymap)))
    (define-key map "a" 'p4-add)
    (define-key map "b" 'p4-branches)
    (define-key map "B" 'p4-branch)
    (define-key map "c" 'p4-client)
    (define-key map "C" 'p4-changes)
    (define-key map "d" 'p4-diff2)
    (define-key map "D" 'p4-describe)
    (define-key map "e" 'p4-edit)
    (define-key map "E" 'p4-reopen)
    (define-key map "\C-f" 'p4-depot-find-file)
    (define-key map "f" 'p4-log)
    (define-key map "F" 'p4-files)
    (define-key map "g" 'p4-get-client-name)
    (define-key map "G" 'p4-get)
    (define-key map "h" 'p4-help)
    (define-key map "H" 'p4-have)
    (define-key map "i" 'p4-info)
    (define-key map "I" 'p4-integ)
    (define-key map "j" 'p4-job)
    (define-key map "J" 'p4-jobs)
    (define-key map "l" 'p4-label)
    (define-key map "L" 'p4-labels)
    (define-key map "\C-l" 'p4-labelsync)
    (define-key map "m" 'p4-rename)
    (define-key map "n" 'p4-notify)
    (define-key map "o" 'p4-opened)
    (define-key map "p" 'p4-print)
    (define-key map "P" 'p4-set-p4-port)
    (define-key map "q" 'p4-pop-window-config)
    (define-key map "r" 'p4-revert)
    (define-key map "R" 'p4-refresh)
    (define-key map "\C-r" 'p4-resolve)
    (define-key map "s" 'p4-set-client-name)
    (define-key map "S" 'p4-submit)
    (define-key map "t" 'p4-toggle-vc-mode)
    (define-key map "u" 'p4-user)
    (define-key map "U" 'p4-users)
    (define-key map "v" 'p4-emacs-version)
    (define-key map "V" 'p4-blame)
    (define-key map "w" 'p4-where)
    (define-key map "x" 'p4-delete)
    (define-key map "X" 'p4-fix)
    (define-key map "=" 'p4-diff)
    map)
  "The Prefix for P4 Library Commands.")

(fset 'p4-prefix-map p4-prefix-map)

;; For users interested in notifying a change, a notification list can be
;; set up using this function.
(defun p4-set-notify-list (p4-new-notify-list &optional p4-supress-stat)
  "To set the current value of P4NOTIFY, type \\[p4-set-notify-list].

This will change the current notify list from the existing list to the new
given value.

An empty string will disable notification.

Argument P4-NEW-NOTIFY-LIST is new value of the notification list.
Optional argument P4-SUPRESS-STAT when t will suppress display of the status
message. "

  (interactive (list (let
             ((symbol (read-string
                   "Change Notification List to: "
                   p4-notify-list)))
               (if (equal symbol "")
               nil
             symbol))))
  (let ((p4-old-notify-list p4-notify-list))
    (setenv "P4NOTIFY" p4-new-notify-list)
    (setq p4-notify-list p4-new-notify-list)
    (setq p4-notify (not (null p4-new-notify-list)))
    (if (not p4-supress-stat)
    (message "Notification list changed from '%s' to '%s'"
         p4-old-notify-list p4-notify-list))))

;; To get the current notification list.
(defun p4-get-notify-list ()
  "To get the current value of the environment variable P4NOTIFY,
type \\[p4-get-notify-list].

   This will be the current notification list that is in use for mailing
   change notifications through Emacs P4."

  (interactive)
  (message "P4NOTIFY is %s" p4-notify-list))

(defun p4-notify (users)
  "To notify a list of users of a change submission manually, type
\\[p4-notify].

To do auto-notification, set the notification list with `p4-set-notify-list'
and on each submission, the users in the list will be notified of the
change.

Since this uses the sendmail program, it is mandatory to set the correct
path to the sendmail program in the variable `p4-sendmail-program'.

Also, it is mandatory to set the user's email address in the variable
`p4-user-email'.

Argument USERS The users to notify to. The default value is the notification
list."
  (interactive (list (let
             ((symbol (read-string "Notify whom? "
                           p4-notify-list)))
               (if (equal symbol "")
               nil
             symbol))))
  (p4-set-notify-list users t)
  (if (and p4-sendmail-program p4-user-email)
      (p4-do-notify)
    (message "Please set p4-sendmail-program and p4-user-email variables.")))

(defun p4-do-notify ()
  "This is the internal notification function called by `p4-notify'."
  (save-excursion
    (if (and p4-notify-list (not (equal p4-notify-list "")))
    (save-excursion
      (set-buffer (p4-get-writable-output-buffer))
      (goto-char (point-min))
      (if (re-search-forward "[0-9]+.*submitted" (point-max) t)
          (let (p4-matched-change)
        (setq p4-matched-change (substring (match-string 0) 0 -10))
        (set-buffer (get-buffer-create "*P4 Notify*"))
        (delete-region (point-min) (point-max))
        (call-process-region (point-min) (point-max)
                     p4-get-executable
                     t t nil
                     "describe" "-s"
                     p4-matched-change)
        (switch-to-buffer "*P4 Notify*")
        (goto-char (point-min))
        (let (p4-chg-desc)
          (if (re-search-forward "^Change.*$" (point-max) t)
              (setq p4-chg-desc (match-string 0))
            (setq p4-chg-desc (concat
                       "Notification of Change "
                       p4-matched-change)))
          (goto-char (point-min))
          (insert
           "From: " p4-user-email "\n"
           "To: P4 Notification Recipients:;\n"
           "Subject: " p4-chg-desc "\n")
          (call-process-region (point-min) (point-max)
                       p4-sendmail-program t t nil
                       "-odi" "-oi" p4-notify-list)

          (kill-buffer nil)))
        (save-excursion
          (set-buffer (p4-get-writable-output-buffer))
          (goto-char (point-max))
          (insert "\np4-do-notify: No Change Submissions found."))))
      (save-excursion
    (set-buffer (p4-get-writable-output-buffer))
    (goto-char (point-max))
    (insert "\np4-do-notify: Notification list not set.")))))

;; Function to return the current version.
(defun p4-emacs-version ()
  "Return the current Emacs-P4 Integration version."
  (interactive)
  (message (concat (cond (p4-running-xemacs "X")) "Emacs-P4 Integration v%s")
       p4-emacs-version))

(defun p4-find-p4-config-file ()
  (let ((p4config (getenv "P4CONFIG"))
    (p4-cfg-dir (cond ((p4-buffer-file-name)
               (file-name-directory
                (file-truename (p4-buffer-file-name))))
              (t (file-truename default-directory)))))
    (if (not p4config)
    nil
      (let (found at-root)
    (while (not (or found at-root))
      (let ((parent-dir (file-name-directory
                 (directory-file-name
                  p4-cfg-dir))))
        (if (file-exists-p (concat p4-cfg-dir p4config))
        (setq found (concat p4-cfg-dir p4config)))
        (setq at-root (string-equal parent-dir p4-cfg-dir))
        (setq p4-cfg-dir parent-dir)))
    found))))



(defun p4-get-add-branch-files (&optional name-list)
  (let ((output-buffer (p4-depot-output "opened" name-list))
    files depot-map)
    (save-excursion
      (set-buffer output-buffer)
      (goto-char (point-min))
      (while (re-search-forward "^\\(//[^/@#]+/[^#\n]*\\)#[0-9]+ - add " nil t)
    (setq files (cons (cons (match-string 1) "Add")
              files)))
      (goto-char (point-min))
      (while (re-search-forward "^\\(//[^/@#]+/[^#\n]*\\)#[0-9]+ - branch " nil t)
    (setq files (cons (cons (match-string 1) "Branch")
              files))))
    (kill-buffer output-buffer)
    (setq depot-map (p4-map-depot-files (mapcar 'car files)))
    (mapcar (lambda (x) (cons (cdr (assoc (car x) depot-map))
                  (cdr x))) files)))

(defun p4-get-have-files (file-list)
  (let ((output-buffer (p4-depot-output "have" file-list))
    line files depot-map elt)
    (while (setq line (p4-read-depot-output output-buffer))
      (if (string-match "^\\(//[^/@#]+/[^#\n]*\\)#\\([0-9]+\\) - " line)
      (setq files (cons (cons (match-string 1 line)
                  (match-string 2 line))
                files))))
    (kill-buffer output-buffer)
    (setq depot-map (p4-map-depot-files (mapcar 'car files)))
    (setq files (mapcar (lambda (x) (cons (cdr (assoc (car x) depot-map))
                      (cdr x))) files))
    (while file-list
      (setq elt (car file-list))
      (setq file-list (cdr file-list))
      (if (not (assoc elt files))
      (setq files (cons (cons elt nil) files))))
    files))

;; A function to check if the file being opened is version controlled by p4.
(defun p4-is-vc (&optional file-mode-cache filename)
  "If a file is controlled by P4 then return version else return nil."
  (if (not filename)
      (setq filename (p4-buffer-file-name)))
  (let (version done)
    (let ((el (assoc filename file-mode-cache)))
      (setq done el)
      (setq version (cdr el)))
    (if (and (not done) filename)
    (let ((output-buffer (p4-depot-output "have" (list filename)))
          line)
      (setq line (p4-read-depot-output output-buffer))
      (kill-buffer output-buffer)
      (if (string-match "^//[^/@#]+/[^#\n]*#\\([0-9]+\\) - " line)
          (setq version (match-string 1 line)))
      (setq done version)))
    (if (and (not done) (not file-mode-cache))
    (progn
      (setq file-mode-cache
        (p4-get-add-branch-files (and filename (list filename))))
      (setq version (cdr (assoc filename file-mode-cache)))))
    version))

;; Force mode line updation for different Emacs versions
(defun p4-force-mode-line-update ()
  "To Force the mode line update for different flavors of Emacs."
  (cond (p4-running-xemacs
     (redraw-modeline))
    (p4-running-emacs
     (force-mode-line-update))))

;; In case, the P4 server is not available, or when operating off-line, the
;; p4-find-file-hook becomes a pain... this functions toggles the use of the
;; hook when opening files.

(defun p4-toggle-vc-mode ()
  "In case, the P4 server is not available, or when working off-line, toggle
the VC check on/off when opening files."
  (interactive)
  (setq p4-do-find-file (not p4-do-find-file))
  (message (concat "P4 mode check " (if p4-do-find-file
                    "enabled."
                      "disabled."))))

;; Wrap C-x C-q to allow p4-edit/revert and also to ensure that
;; we don't stomp on vc-toggle-read-only.

(defun p4-toggle-read-only (&optional arg)
  "If p4-mode is non-nil, \\[p4-toggle-read-only] toggles between `p4-edit'
and `p4-revert'. If ARG is non-nil, p4-offline-mode will be enabled for this
buffer before the toggling takes place. In p4-offline-mode, toggle between
making the file writable and write protected."
  (interactive "P")
  (if (and arg p4-mode)
      (setq p4-mode nil
        p4-offline-mode t))
  (cond
   (p4-mode
    (if buffer-read-only
    (p4-edit p4-verbose)
      (p4-revert p4-verbose)))
   (p4-offline-mode
    (toggle-read-only)
    (if buffer-file-name
    (let ((mode (file-modes buffer-file-name)))
      (if buffer-read-only
          (setq mode (logand mode (lognot 128)))
        (setq mode (logior mode 128)))
      (set-file-modes buffer-file-name mode))))))

;; Break up a string into a list of words
;; (p4-make-list-from-string "ab c de  f") -> ("ab" "c" "de" "f")
(defun p4-make-list-from-string (str)
  (let (lst)
    (while (or (string-match "^ *\"\\([^\"]*\\)\"" str)
           (string-match "^ *\'\\([^\']*\\)\'" str)
           (string-match "^ *\\([^ ]+\\)" str))
      (setq lst (append lst (list (match-string 1 str))))
      (setq str (substring str (match-end 0))))
    lst))

(defun p4-list-to-string (lst)
  (mapconcat (lambda (x) x) lst " "))

(defvar p4-depot-filespec-history nil
  "History for p4-depot filespecs.")

(defvar p4-depot-completion-cache nil
  "Cache for `p4-depot-completion'.
It is a list of lists whose car is a filespec and
cdr is the list of anwers")

(defvar p4-branches-history nil
  "History for p4 clients.")

(defvar p4-branches-completion-cache nil
  "Cache for `p4-depot-completion'.
It is a list of lists whose car is a client and
cdr is the list of answers??")

(defvar p4-clients-history nil
  "History for p4 clients.")

(defvar p4-clients-completion-cache nil
  "Cache for `p4-depot-completion'.
It is a list of lists whose car is a client and
cdr is the list of answers??")

(defvar p4-jobs-completion-cache nil
  "Cache for `p4-depot-completion'.
It is a list of lists whose car is a job and
cdr is the list of answers??")

(defvar p4-labels-history nil
  "History for p4 clients.")

(defvar p4-labels-completion-cache nil
  "Cache for `p4-depot-completion'.
It is a list of lists whose car is a label and
cdr is the list of answers??")

(defvar p4-users-completion-cache nil
  "Cache for `p4-depot-completion'.
It is a list of lists whose car is a user and
cdr is the list of answers??")

(defvar p4-groups-completion-cache nil
  "Cache for `p4-depot-completion'.
It is a list of lists whose car is a group and
cdr is the list of answers??")

(defvar p4-arg-string-history nil
  "History for p4 command arguments")

(defun p4-depot-completion-search (filespec cmd)
  "Look into `p4-depot-completion-cache' for filespec.
Filespec is the candidate for completion, so the
exact file specification is \"filespec*\".

If found in cache, return a list whose car is FILESPEC and cdr is the list
of matches.
If not found in cache, return nil.
So the 'no match' answer is different from 'not in cache'."
  (let ((l (cond
        ((equal cmd "branches") p4-branches-completion-cache)
        ((equal cmd "clients") p4-clients-completion-cache)
        ((equal cmd "dirs") p4-depot-completion-cache)
        ((equal cmd "jobs") p4-jobs-completion-cache)
        ((equal cmd "labels") p4-labels-completion-cache)
        ((equal cmd "users") p4-users-completion-cache)
        ((equal cmd "groups") p4-groups-completion-cache)))
    dir list)

    (if (and p4-cleanup-cache (not p4-timer))
    (setq p4-timer (cond (p4-running-emacs
                  (run-at-time p4-cleanup-time nil
                       'p4-cache-cleanup))
                 (p4-running-xemacs
                  (add-timeout p4-cleanup-time 'p4-cache-cleanup
                       nil nil)))))
    (while l
      (if (string-match (concat "^" (car (car l)) "[^/]*$") filespec)
      (progn
        ;; filespec is included in cache
        (if (string= (car (car l)) filespec)
        (setq list (cdr (car l)))
          (setq dir (cdr (car l)))
          (while dir
        (if (string-match (concat "^" filespec) (car dir))
            (setq list (cons (car dir) list)))
        (setq dir (cdr dir))))
        (setq l nil
          list (cons filespec list))))
      (setq l (cdr l)))
    list))

(defun p4-cache-cleanup (&optional arg)
  "Cleanup all the completion caches."
  (message "Cleaning up the p4 caches ...")
  (setq p4-branches-completion-cache nil)
  (setq p4-clients-completion-cache nil)
  (setq p4-depot-completion-cache nil)
  (setq p4-jobs-completion-cache nil)
  (setq p4-labels-completion-cache nil)
  (setq p4-users-completion-cache nil)
  (setq p4-groups-completion-cache nil)
  (if (and p4-running-emacs (timerp p4-timer)) (cancel-timer p4-timer))
  (if (and p4-running-xemacs p4-timer) (disable-timeout p4-timer))
  (setq p4-timer nil)
  (message "Cleaning up the p4 caches ... done."))

(defun p4-partial-cache-cleanup (type)
  "Cleanup a specific completion cache."
  (cond ((string= type "branch")
     (setq p4-branches-completion-cache nil))
    ((string= type "client")
     (setq p4-clients-completion-cache nil))
    ((or (string= type "submit") (string= type "change"))
     (setq p4-depot-completion-cache nil))
    ((string= type "job")
     (setq p4-jobs-completion-cache nil))
    ((string= type "label")
     (setq p4-labels-completion-cache nil))
    ((string= type "user")
     (setq p4-users-completion-cache nil))
    ((string= type "group")
     (setq p4-groups-completion-cache nil))))

(defun p4-read-depot-output (buffer &optional regexp)
  "Reads first line of BUFFER and returns it.
Read lines are deleted from buffer.

If optional REGEXP is passed in, return the substring of the first line that
matched the REGEXP."

  (save-excursion
    (set-buffer buffer)
    (goto-char (point-min))
    (forward-line)

    (let ((line (buffer-substring (point-min) (point))))
      (if (string= line "")
      nil
    (delete-region (point-min) (point))
    (if (and regexp (string-match regexp line))
        (setq line (substring line (match-beginning 1) (match-end 1))))

    ;; remove trailing newline
    (if (equal (substring line (1- (length line)) (length line)) "\n")
        (substring line 0 (1- (length line)))
      line)))))

(defun p4-completion-helper (filespec cmd var regexp)
  (let (output-buffer line list)
    (message "Making %s completion list..." cmd)
    (setq output-buffer (p4-depot-output cmd))
    (while (setq line (p4-read-depot-output
               output-buffer regexp))
      (if line
      (setq list (cons line list))))
    (kill-buffer output-buffer)
    (set var
     (cons (cons filespec list) (eval var)))
    list))

(defun p4-depot-completion-build (filespec cmd)
  "Ask Perforce for a list of files and directories beginning with FILESPEC."
  (let (output-buffer line list)
    (cond
     ((equal cmd "branches")
      (setq list (p4-completion-helper
          filespec cmd 'p4-branches-completion-cache
          "^Branch \\([^ \n]*\\) [0-9][0-9][0-9][0-9]/.*$")))
     ((equal cmd "clients")
      (setq list (p4-completion-helper
          filespec cmd 'p4-clients-completion-cache
          "^Client \\([^ \n]*\\) [0-9][0-9][0-9][0-9]/.*$")))

     ((equal cmd "dirs")
      (message "Making p4 completion list...")
      (setq output-buffer (p4-depot-output cmd
                       (list (concat filespec "*"))))
      (while (setq line (p4-read-depot-output output-buffer))
    (if (not (string-match "no such file" line))
        (setq list (cons (concat line "/") list))))
      (kill-buffer output-buffer)
      (setq output-buffer (p4-depot-output "files"
                       (list (concat filespec "*"))))
      (while (setq line (p4-read-depot-output output-buffer))
    (if (string-match "^\\(.+\\)#[0-9]+ - " line)
        (setq list (cons (match-string 1 line) list))))
      (kill-buffer output-buffer)
      (setq p4-depot-completion-cache
        (cons (cons filespec list) p4-depot-completion-cache)))

     ((equal cmd "jobs")
      (setq list (p4-completion-helper
          filespec cmd 'p4-jobs-completion-cache
          "\\([^ \n]*\\) on [0-9][0-9][0-9][0-9]/.*$")))
     ((equal cmd "labels")
      (setq list (p4-completion-helper
          filespec cmd 'p4-labels-completion-cache
          "^Label \\([^ \n]*\\) [0-9][0-9][0-9][0-9]/.*$")))
     ((equal cmd "users")
      (setq list (p4-completion-helper
          filespec cmd 'p4-users-completion-cache
          "^\\([^ ]+\\).*$")))
     ((equal cmd "groups")
      (setq list (p4-completion-helper
          filespec cmd 'p4-groups-completion-cache
          "^\\(.*\\)$"))))
    (message nil)
    (cons filespec list)))

(defun p4-completion-builder (type)
  `(lambda (string predicate action)
     ,(concat "Completion function for Perforce " type ".

Using the mouse in completion buffer on a client will select it
and exit, unlike standard selection. This is because
`choose-completion-string' (in simple.el) has a special code for
file name selection.")
     (let (list)
       ,(if (string= type "dirs")
        ;; when testing for an exact match, remove trailing /
        `(if (and (eq action 'lambda)
              (eq (aref string (1- (length string))) ?/))
         (setq string (substring string 0 (1- (length string))))))

       ;; First, look in cache
       (setq list (p4-depot-completion-search string ,type))

       ;; If not found in cache, build list.
       (if (not list)
       (setq list (p4-depot-completion-build string ,type)))

       (cond
    ;; try completion
    ((null action)
     (try-completion string (mapcar 'list (cdr list)) predicate))
    ;; all completions
    ((eq action t)
     (let ((lst
        (all-completions string (mapcar 'list (cdr list)) predicate)))
       ,(if (string= type "dirs")
        `(setq lst (mapcar (lambda (s)
                     (if (string-match ".*/\\(.+\\)" s)
                     (match-string 1 s)
                       s))
                   lst)))
       lst))
    ;; Test for an exact match
    (t
     (and (>= (length list) 2)
          (member (car list) (cdr list))))))))

(defalias 'p4-branches-completion (p4-completion-builder "branches"))
(defalias 'p4-clients-completion (p4-completion-builder "clients"))
(defalias 'p4-depot-completion (p4-completion-builder "dirs"))
(defalias 'p4-jobs-completion (p4-completion-builder "jobs"))
(defalias 'p4-labels-completion (p4-completion-builder "labels"))
(defalias 'p4-users-completion (p4-completion-builder "users"))
(defalias 'p4-groups-completion (p4-completion-builder "groups"))


(defun p4-read-arg-string (prompt &optional initial type)
  (let ((minibuffer-local-completion-map
     (copy-keymap minibuffer-local-completion-map)))
    (define-key minibuffer-local-completion-map " " 'self-insert-command)
    (completing-read prompt
             (cond ((not type)
                'p4-arg-string-completion)
               ((string= type "branch")
                'p4-branch-string-completion)
               ((string= type "client")
                'p4-client-string-completion)
               ((string= type "label")
                'p4-label-string-completion)
               ((string= type "job")
                'p4-job-string-completion)
               ((string= type "user")
                'p4-user-string-completion)
               ((string= type "group")
                'p4-group-string-completion))
             nil nil
             initial 'p4-arg-string-history)))

(defun p4-read-args (prompt &optional type &rest args)
  (p4-make-list-from-string
   (p4-read-arg-string prompt (p4-list-to-string args) type)))

(defun p4-read-args* (prompt &optional type &rest args)
  (if current-prefix-arg
      (apply 'p4-read-args prompt type args)
    args))

(defun p4-arg-string-completion (string predicate action)
  (let ((first-part "") completion)
    (if (string-match "^\\(.* +\\)\\(.*\\)" string)
    (progn
      (setq first-part (match-string 1 string))
      (setq string (match-string 2 string))))
    (cond ((string-match "-b +$" first-part)
       (setq completion (p4-branches-completion string predicate action)))
      ((string-match "-t +$" first-part)
       (setq completion (p4-list-completion
                 string (list "text " "xtext " "binary "
                      "xbinary " "symlink ")
                 predicate action)))
      ((string-match "-j +$" first-part)
       (setq completion (p4-jobs-completion string predicate action)))
      ((string-match "-l +$" first-part)
       (setq completion (p4-labels-completion string predicate action)))
      ((string-match "\\(.*status=\\)\\(.*\\)" string)
       (setq first-part (concat first-part (match-string 1 string)))
       (setq string (match-string 2 string))
       (setq completion (p4-list-completion
                 string (list "open " "closed " "suspended ")
                 predicate action)))
      ((or (string-match "\\(.*@.+,\\)\\(.*\\)" string)
           (string-match "\\(.*@\\)\\(.*\\)" string))
       (setq first-part (concat first-part (match-string 1 string)))
       (setq string (match-string 2 string))
       (setq completion (p4-labels-completion string predicate action)))
      ((string-match "\\(.*#\\)\\(.*\\)" string)
       (setq first-part (concat first-part (match-string 1 string)))
       (setq string (match-string 2 string))
       (setq completion (p4-list-completion
                 string (list "none" "head" "have")
                 predicate action)))
      ((string-match "^//" string)
       (setq completion (p4-depot-completion string predicate action)))
      ((string-match "\\(^-\\)\\(.*\\)" string)
       (setq first-part (concat first-part (match-string 1 string)))
       (setq string (match-string 2 string))
       (setq completion (p4-list-completion
                 string (list "a " "af " "am " "as " "at " "ay "
                      "b " "c " "d " "dc " "dn "
                      "ds " "du " "e " "f " "i " "j "
                      "l " "m " "n " "q " "r " "s " "sa "
                      "sd " "se " "sr " "t " "v ")
                 predicate action)))
      (t
       (setq completion (p4-file-name-completion string
                             predicate action))))
    (cond ((null action) ;; try-completion
       (if (stringp completion)
           (concat first-part completion)
         completion))
      ((eq action t) ;; all-completions
       completion)
      (t             ;; exact match
       completion))))

(defun p4-list-completion (string lst predicate action)
  (let ((collection (mapcar 'list lst)))
    (cond ((not action)
       (try-completion string collection predicate))
      ((eq action t)
       (all-completions string collection predicate))
      (t
       (eq (try-completion string collection predicate) t)))))

(defun p4-file-name-completion (string predicate action)
  (if (string-match "//\\(.*\\)" string)
      (setq string (concat "/" (match-string 1 string))))
  (setq string (substitute-in-file-name string))
  (setq string (p4-follow-link-name (expand-file-name string)))
  (let ((dir-path "") completion)
    (if (string-match "^\\(.*[/\\]\\)\\(.*\\)" string)
    (progn
      (setq dir-path (match-string 1 string))
      (setq string (match-string 2 string))))
    (cond ((not action)
       (setq completion (file-name-completion string dir-path))
       (if (stringp completion)
           (concat dir-path completion)
         completion))
      ((eq action t)
       (file-name-all-completions string dir-path))
      (t
       (eq (file-name-completion string dir-path) t)))))

(defun p4-string-completion-builder (completion-function)
  `(lambda (string predicate action)
     (let ((first-part "") completion)
       (if (string-match "^\\(.* +\\)\\(.*\\)" string)
       (progn
         (setq first-part (match-string 1 string))
         (setq string (match-string 2 string))))
       (cond ((string-match "^-" string)
          (setq completion nil))
         (t
          (setq completion
            (,completion-function string predicate action))))
       (cond ((null action);; try-completion
          (if (stringp completion)
          (concat first-part completion)
        completion))
         ((eq action t);; all-completions
          completion)
         (t;; exact match
          completion)))))

(defalias 'p4-branch-string-completion (p4-string-completion-builder
                    'p4-branches-completion))

(defalias 'p4-client-string-completion (p4-string-completion-builder
                    'p4-clients-completion))

(defalias 'p4-job-string-completion (p4-string-completion-builder
                     'p4-jobs-completion))

(defalias 'p4-label-string-completion (p4-string-completion-builder
                       'p4-labels-completion))

(defalias 'p4-user-string-completion (p4-string-completion-builder
                      'p4-users-completion))

(defalias 'p4-group-string-completion (p4-string-completion-builder
                                       'p4-groups-completion))

(defun p4-depot-find-file (file)
  (interactive (list (completing-read "Enter filespec: "
                      'p4-depot-completion
                      nil nil
                      p4-default-depot-completion-prefix
                      'p4-depot-filespec-history)))
  (let ((lfile (cdar (p4-map-depot-files (list file)))))
    (if lfile
    (find-file lfile)
      (if (get-file-buffer file)
      (switch-to-buffer-other-window file)
    (get-buffer-create file)
    (set-buffer file)
    (p4-noinput-buffer-action "print" nil t (list file))
    (p4-activate-print-buffer file t)))))


;; A function to get the current P4 client name
(defun p4-get-client-name ()
  "To get the current value of the environment variable P4CLIENT,
type \\[p4-get-client-name].

This will be the current client that is in use for access through
Emacs P4."
  (interactive)
  (let ((client (p4-current-client)))
    (message "P4CLIENT [local: %s], [global: %s]" client (getenv "P4CLIENT"))
    client))

(defun p4-get-config-info (file-name token)
  (let ((output-buffer (p4-get-writable-output-buffer))
    (data (getenv token)))
    (save-excursion
      (set-buffer output-buffer)
      (goto-char (point-min))
      (insert-file-contents file-name)
      (goto-char (point-min))
      (if (re-search-forward (concat "^" (regexp-quote token) "=\\(.*\\)")
                 nil t)
      (setq data (match-string 1))))
    (kill-buffer output-buffer)
    data))

(defun p4-current-client ()
  "Get the current local client, or the global client, if that."
  (let ((p4-config-file (p4-find-p4-config-file))
    cur-client pmin)
    (if (not p4-config-file)
    (setq cur-client (getenv "P4CLIENT"))
      (setq cur-client (p4-get-config-info p4-config-file "P4CLIENT")))
    (if (not cur-client)
    (save-excursion
      (p4-get-writable-output-buffer)
      (set-buffer p4-output-buffer-name)
      (goto-char (point-max))
      (setq pmin (point))
      (if (zerop (p4-call-p4-here "info"))
          (progn
        (goto-char pmin)
        (if (re-search-forward "^Client name:[ \t]+\\(.*\\)$" nil t)
            (setq cur-client (match-string 1)))
        (delete-region pmin (point-max))))))
    cur-client))

(defun p4-current-server-port ()
  "Get the current local server:port address, or the global server:port, if
that."
  (let ((p4-config-file (p4-find-p4-config-file)))
    (if (not p4-config-file)
    (getenv "P4PORT")
      (p4-get-config-info p4-config-file "P4PORT"))))

(defun p4-save-opened-files ()
  (let ((output-buffer (p4-depot-output "opened"))
    opened)
    (save-excursion
      (set-buffer output-buffer)
      (goto-char (point-min))
      (while (re-search-forward "^\\(.*\\)#[0-9]+ - " nil t)
    (setq opened (cons (match-string 1) opened))))
    (kill-buffer output-buffer)
    (setq opened (mapcar 'cdr (p4-map-depot-files opened)))
    (save-window-excursion
      (map-y-or-n-p
       (function
    (lambda (buffer)
      (and (buffer-modified-p buffer)
           (not (buffer-base-buffer buffer))
           (buffer-file-name buffer)
           (member (buffer-file-name buffer) opened)
           (format "Save file %s? "
               (buffer-file-name buffer)))))
       (function
    (lambda (buffer)
      (set-buffer buffer)
      (save-buffer)))
       (buffer-list)
       '("buffer" "buffers" "save")))))

(defun p4-empty-diff-p ()
  "Return t if there exists a file opened for edit with an empty diff"
  (let ((buffer (get-buffer-create "p4-edp-buf"))
    opened empty-diff)
    (p4-exec-p4 buffer (list "opened") t)
    (save-excursion
      (set-buffer buffer)
      (goto-char (point-min))
      (while (re-search-forward "^\\(.*\\)#[0-9]* - edit.*" nil t)
    (setq opened (cons (match-string 1) opened))))
    (if opened
    (progn
      (with-current-buffer buffer (erase-buffer))
      (p4-exec-p4 buffer (list "diff") t)
      (save-excursion
        (set-buffer buffer)
        (goto-char (point-max))
        (insert "====\n")
        (goto-char (point-min))
        (while (re-search-forward "^==== \\([^#\n]+\\)#.*\n====" nil t)
          (if (member (match-string 1) opened)
          (progn
            (setq empty-diff t)
            (goto-char (point-max))))))))
    (kill-buffer buffer)
    empty-diff))






;; Add some functions to key map.
(lazy-set-key
 (list
  (cons "C" 'p4-cbg))
 magit-reflog-mode-map
 )

(lazy-set-key
 (list
  (cons "C" 'p4-cbg))
 magit-log-mode-map
 )

(lazy-set-key
 (list
  (cons "C" 'p4-cbg))
 magit-status-mode-map
 )

(provide 'p4)
;;;;; p4.el ends here

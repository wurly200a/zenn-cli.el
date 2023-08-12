;;; zenn-cli.el -- functions for using Zenn CLI

;; Author: Wurly, July 2022
;; Version: 0.3.0

;;; Install:

;; Put this file into load-path'ed directory.
;; And put these expressions into your ~/.emacs.
;;
;;   ;---- zenn-cli ----
;;   (when (locate-library "zenn-cli")
;;     (require 'zenn-cli)
;;     (custom-set-variables
;;      '(zenn-cli-default-directory "/path/to/zenn-contents/"))
;;    )

;;; Code:
;(setq debug-on-error t)

(provide 'zenn-cli)

(defconst zenn-cli-process-name "zenn-cli-command-process")
(defconst zenn-cli-output-buffer-name "*zenn-cli-output*")
(defconst zenn-cli-list-buffer-name "*zenn-articles*")
(defconst zenn-cli-process-buffer-name "*zenn-cli-process*")
(defvar zenn-cli-window-configuration nil)

;;;
;;; Customizing zenn-cli-mode
;;;
(defcustom zenn-cli-default-directory
  "~/zenn-contents/"
  "set zenn-cli default directory"
  :type 'string)

(defgroup zenn-cli nil
  "Minor mode for zenn-cli."
  :group 'tools
  :prefix "zenn-cli-")

;; Variables
(defvar zenn-cli-current-buffer nil
  "Current buffer.")
(defvar zenn-cli-select-mode-map (make-sparse-keymap)
  "Keymap used in zenn-cli select mode.")

;; Key mapping of zenn-cli-select-mode.
(define-key zenn-cli-select-mode-map "\C-m" 'zenn-cli-select-article)
(define-key zenn-cli-select-mode-map "f" 'zenn-cli-select-article)
(define-key zenn-cli-select-mode-map "g" 'zenn-cli-list-articles)
(define-key zenn-cli-select-mode-map "p" 'previous-line)
(define-key zenn-cli-select-mode-map "n" 'next-line)
(define-key zenn-cli-select-mode-map "c" 'zenn-cli-new-article)

; internal function

; start synchronous process
(defun zenn-cli-command-sync(program &rest program-args)
  "use sync shell"
  (setq zenn-cli-window-configuration (current-window-configuration))
  (let ((default-directory zenn-cli-default-directory))
        (apply 'call-process program nil zenn-cli-output-buffer-name nil program-args)
        )
)

; start asynchronous process
(defun zenn-cli-command-async(process-name pop-to-buffer-mode program &rest program-args)
  "use async shell"
  (setq zenn-cli-window-configuration (current-window-configuration))
  (let ((default-directory zenn-cli-default-directory))
    (prog1
        (save-current-buffer
          (save-selected-window
            (apply 'start-process process-name nil program program-args)
            (set-process-sentinel (get-process process-name) 'zenn-cli-sentinel)
            (if (not pop-to-buffer-mode)
                (get-buffer-create zenn-cli-output-buffer-name)
              (pop-to-buffer zenn-cli-output-buffer-name) )
            (set-process-filter (get-process process-name) 'zenn-cli-command-output)
            (setq truncate-lines nil
                  buffer-read-only nil)
            (set-buffer zenn-cli-output-buffer-name)
            (goto-char (point-max))
            (let (temp-args temp-string)
              (setq temp-string (concat ">" program " "))
              (setq temp-args program-args)
              (while (progn
                       (setq temp-string (concat temp-string (car temp-args) " "))
                       (setq temp-args (cdr temp-args))))
              (setq temp-string (concat temp-string "\n"))
              (insert temp-string)
              )
            ))
      )
    ))

;filter function for process
(defun zenn-cli-command-output (process output)
  (with-current-buffer (set-buffer zenn-cli-output-buffer-name)
    (goto-char (point-max))
    (insert output)
    )
)

;sentinel for process
(defun zenn-cli-sentinel (process event)
  (with-current-buffer (set-buffer zenn-cli-output-buffer-name)
    (goto-char (point-max))
    (insert (concat "zenn-cli: " (car (split-string event "\n"))))
    (insert "\n")
    )
  (if (string-equal event "finished\n")
      (progn
        (sit-for 1) ; wait 1sec
        (message "zenn-cli process finished"))
    )
  )

;;
;; interactive command
;;

(defun zenn-cli-new-article ()
  "create new article"
  (interactive)
  (if (get-buffer zenn-cli-output-buffer-name)
      (with-current-buffer (get-buffer zenn-cli-output-buffer-name)
        (erase-buffer))
    )
  (zenn-cli-command-sync "npx" "zenn" "new:article")
  (with-current-buffer (set-buffer zenn-cli-output-buffer-name)
    (let (start-point end-point)
      (goto-char (point-min))
      (setq start-point (search-forward "articles/"))
      (setq end-point (search-forward ".md"))
      (find-file (concat zenn-cli-default-directory "articles/" (buffer-substring start-point end-point)))
      )
    )
  )

(defun zenn-cli-preview()
  "preview"
  (interactive)
  (unless (get-process zenn-cli-process-name)
    (zenn-cli-command-async zenn-cli-process-name t "npx" "zenn" "preview")
    )
)

(defun zenn-cli-install()
  "install"
  (interactive)
  (unless (get-process zenn-cli-process-name)
    (zenn-cli-command-async zenn-cli-process-name t "npm" "install" "zenn-cli@latest")
    )
)

(defun zenn-cli-list-articles ()
  "Display list of article."
  (interactive)

  (let (buffer-for-display buffer-for-process lines)

    (if (setq buffer-for-display (get-buffer zenn-cli-list-buffer-name))
        (kill-buffer buffer-for-display)
      )
    (setq buffer-for-display (get-buffer-create zenn-cli-list-buffer-name))
;    (set-buffer buffer-for-display)
    (message "Executing ...")

    (if (setq buffer-for-process (get-buffer zenn-cli-process-buffer-name))
        (kill-buffer buffer-for-process)
      )
    (setq buffer-for-process (get-buffer-create zenn-cli-process-buffer-name))
    (set-buffer buffer-for-process)

    (let (status temp-string temp-list temp-temp-list other-list)
      (prefer-coding-system 'utf-8-unix)
      (setq default-directory zenn-cli-default-directory)
      (setq status (call-process "npx" nil t zenn-cli-process-buffer-name "zenn" "list:articles" "--format" "json"))
;      (message "status is %d" status)

      (let (start-point end-point)
        (goto-char (point-min))
        (while (not (eobp))
          (beginning-of-line)
          (setq start-point (point))
          (end-of-line)
          (setq end-point (point))
          (setq temp-string (buffer-substring start-point end-point))
;          (message "temp-string: %s" temp-string)
          (if (string-match "^\{.+\}$" temp-string)
              (setq temp-list (cons temp-string temp-list))
              (setq other-list (cons temp-string other-list))
            )
          (forward-line 1)
          )
        )

      (set-buffer buffer-for-display)

      (setq temp-temp-list (reverse temp-list))
;      (setq temp-temp-list (cdr temp-temp-list))
;      (insert "\n")
      (let (temp-line json-object title emoji type topics published slug)
        (while (progn
                 (setq temp-line (car temp-temp-list))
                 (setq json-object (json-parse-string temp-line))
                 (if json-object
                     (progn
;                       (insert temp-line)
                       (setq title (gethash "title" json-object))
                       (setq emoji (gethash "emoji" json-object))
                       (setq type (gethash "type" json-object))
                       (setq topics (gethash "topics" json-object))
                       (setq published (gethash "published" json-object))
                       (setq slug (gethash "slug" json-object))

                       (if (eq published t)
                           (setq published "     ")
                         (setq published "draft")
                           )

                       (insert (concat slug " " emoji " " type " " published " " title))
                       (insert "\n")))
                 (setq temp-temp-list (cdr temp-temp-list))))
        )

;      (insert "---\n")
;      (let (rvs-other-list)
;        (setq rvs-other-list (reverse other-list))
;        (while (progn
;                 (setq temp-line (car rvs-other-list))
;                 (insert temp-line "\n")
;                 (setq rvs-other-list (cdr rvs-other-list))))
;        )

;      (message "car=%s" (car temp-list))

      (if (not (= 0 status))
        (goto-char (point-min))
        (setq lines (count-lines (point-min) (point-max)))

        (switch-to-buffer buffer-for-display)
        (zenn-cli-select-mode)

        )
      )
    )
  )

(defun zenn-cli-select-article ()
  "Select the article."
  (interactive)
;  (message "zenn-cli-select-article")

  (let (start-point end-point temp-line temp-list now-buffer)
    (beginning-of-line)
    (setq start-point (point))
    (end-of-line)
    (setq end-point (point))
    (setq temp-line (buffer-substring start-point end-point))
;    (message "%s" temp-line)
    (setq temp-list (split-string temp-line " "))
;    (message "%s" (car temp-list))
;    (setq now-buffer (current-buffer))
    (find-file-other-window (concat zenn-cli-default-directory "articles/" (car temp-list) ".md"))
;    (switch-to-prev-buffer)
    )
)

;; make zenn-cli select-mode
(defun zenn-cli-select-mode ()
  "Major mode for choosing the article from list.

Select the article.
	\\[zenn-cli-select-article]

Key definitions:
\\{zenn-cli-select-mode-map}
Turning on Zenn-Cli-Select mode calls the value of the variable
`zenn-cli-select-mode-hook' with no args, if that value is non-nil."
  (interactive)
;  (message "zenn-cli-select-mode")
  (kill-all-local-variables)
  (use-local-map zenn-cli-select-mode-map)
  (setq buffer-read-only t
        truncate-lines t
        major-mode 'zenn-cli-select-mode
        mode-name "Zenn-Cli-Select")
  (setq zenn-cli-current-buffer (current-buffer))
  (goto-char (point-min))
  (message "[zenn-cli articles] %d lines" (count-lines (point-min) (point-max)))
;  (setq hl-line-face 'underline)
  (hl-line-mode 1)
  (run-hooks 'zenn-cli-select-mode-hook)
)

;;; zenn-cli.el ends here

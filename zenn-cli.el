;;; zenn-cli.el -- functions for using Zenn CLI

;; Author: Wurly, July 2022
;; Version: 0.01

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

;;; History:
;; 2022.07.23          (ver 0.01) | new

;;; Code:

(provide 'zenn-cli)

(defconst zenn-cli-process-name "zenn-cli-command-process")
(defconst zenn-cli-output-buffer-name "*zenn-cli-output*")
(defvar zenn-cli-window-configuration nil)

(defcustom zenn-cli-default-directory
  "~/zenn-contents/"
  "set zenn-cli default directory"
  :type 'string)

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

;;; zenn-cli.el ends here

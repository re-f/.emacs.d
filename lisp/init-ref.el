;; init-rel.el --- Initialize Ref configurations.	-*- lexical-binding: t -*-

(defun ref/org-timeS-less-p (x y)
  "比较两个org-mode时间格式的时间大小，其中x，y为string类型"
  (org-time-less-p (org-timestamp-to-time(org-timestamp-from-string x))
                   (org-timestamp-to-time (org-timestamp-from-string y)))
  )


(defun ref/create-org-file ()
  "Create an org file in org-directory"
  (interactive)
  (let ((name (read-string "Filename: ")))
    (expand-file-name (format "%s.org"
                              name) org-directory)))



;; 计算 org-directory 下的字数统计
(defun ref/reset-count ()
  (interactive)
  (shell-command  (expand-file-name (format "%s/scripts/update_record.sh" org-directory)))
  )

;; 快捷键设置
(define-prefix-command 'ref/prefix-command)
(global-set-key (kbd "M-m") 'ref/prefix-command)

(define-key ref/prefix-command (kbd "c") 'ref/reset-count)

(defun ref/newline-return ()
  "append (open) a new line below the current line, and execute return"
  (interactive)
  (move-end-of-line 1)
  (org-newline-and-indent))
(defun ref/newline-meta-return ()
  "append (open) a new line below the current line, org-meta-return"
  (interactive)
  (move-end-of-line 1)
  (org-meta-return))




(require 'init-org-ql)
(provide 'init-ref)

;; init-ref.el --- Initialize Ref's configurations.	-*- lexical-binding: t -*-
;; 本文件由 org 文件管理，请在对应的 org 文件中编辑，不要修改本文件

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

(defun export-org-to-html-and-reload ()
  "导出当前 Org 文件为 HTML 并根据环境刷新 Webkit 视图。"
  (interactive)
  ;; 确保当前 buffer 是 Org Mode buffer
  (when (eq major-mode 'org-mode)
    ;; 检查文档头是否包含 #+EXPORT_HTML: yes
    (let ((export-html (car (org-element-map (org-element-parse-buffer 'element) 'keyword
                              (lambda (el)
                                (when (string-equal (org-element-property :key el) "EXPORT_HTML")
                                  (org-element-property :value el)))))))
      (when (and export-html (string-equal (string-trim export-html) "yes"))
        ;; 导出 HTML
        (org-html-export-to-html)
        ;; 如果 xwidget-webkit 函数存在，则调用之
        (when (fboundp 'xwidget-webkit-reload)
          (xwidget-webkit-reload))))))

(defun timer-running-p (timer)
  "Check if TIMER is running."
  (memq timer timer-list))

(defvar preview-org-timer nil "Timer for org real-time preview.")

(defun ref/reset-count ()
  (interactive)
  (shell-command  (expand-file-name (format "%s/scripts/update_record.sh" org-directory)))
  )

(require 'gptel)
(defun ref/flow/optimize-region ()
  "Optimize the text in the currently marked region using the LLM."
  (interactive)
  (when (use-region-p)
    (let ((region-text (buffer-substring-no-properties (region-beginning) (region-end))))
      (gptel-request
       (format "优化代码块中的文字，要求：\n1. 要书面化一点\n2. 修正错别字\n3. 修正语句不通顺的地方\n\n```org\n%s\n```\n输出要求：\n1. 禁止修改原有格式\n2. 只返回代码块中优化后文字，不要用代码块包裹" region-text)
       :callback (lambda (response info)
                   (if response
                       (progn
                         (delete-region (region-beginning) (region-end))
                         (insert response)
                         (deactivate-mark)
                         (message "优化完成"))
                     (message "Optimization failed with message: %s"
                              (plist-get info :status)))))
      )))

(require 'init-org-ql)
(provide 'init-ref)

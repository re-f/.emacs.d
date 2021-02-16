;; 设置 stuck project ，参考：https://oomake.com/question/2338872
(setq org-stuck-projects
  '("TODO={.+}/-DONE" nil nil "SCHEDULED:\\|DEADLINE:"))


;; 设置一些 agenda 视图， 参考：http://www.mamicode.com/info-detail-2316948.html
(setq org-agenda-custom-commands
      '(
        ("f" "灵光一闪" tags "NOTE")
        ("w" . "任务安排")
        ("wa" "重要且紧急的任务" tags-todo "+PRIORITY=\"A\"")
        ("wb" "重要且不紧急的任务" tags-todo "-weekly-monthly-daily+PRIORITY=\"B\"")
        ("wc" "不重要且紧急的任务" tags-todo "+PRIORITY=\"C\"")
        ("W" "Weekly Review"
         ((stuck "") ;; review stuck projects as designated by org-stuck-projects
          (tags-todo "project")
          (tags-todo "daily")
          (tags-todo "weekly")
          (tags-todo "school")
          (tags-todo "code")
          (tags-todo "theory")
          ))
        ("g" "Get Things Done (GTD)" ;; ref: https://github.com/rougier/emacs-gtd
         ((agenda ""
                  ((org-agenda-skip-function
                    '(org-agenda-skip-entry-if 'deadline))
                   (org-deadline-warning-days 0)))
          (todo "NEXT"
                ((org-agenda-skip-function
                  '(org-agenda-skip-entry-if 'deadline))
                 (org-agenda-prefix-format "  %i %-12:c [%e] ")
                 (org-agenda-overriding-header "\nTasks\n")))
          (agenda nil
                  ((org-agenda-entry-types '(:deadline))
                   (org-agenda-format-date "")
                   (org-deadline-warning-days 7)
                   (org-agenda-skip-function
                    '(org-agenda-skip-entry-if 'notregexp "\\* NEXT"))
                   (org-agenda-overriding-header "\nDeadlines")))
          (tags-todo "inbox"
                     ((org-agenda-prefix-format "  %?-12t% s")
                      (org-agenda-overriding-header "\nInbox\n")))
          (tags "CLOSED>=\"<today>\""
                ((org-agenda-overriding-header "\nCompleted today\n"))))
         )
        ))


(setq org-capture-templates
      `(
        ("j" "TODO of Job" entry (,(if emacs/>=26p 'file+olp+datetree 'file+datetree)
                                  ,(concat org-directory "/daily.org"))
         "* TODO %?\n%u\n%a\n" :tree-type week)
        ("l" "TODO of life" entry (,(if emacs/>=26p 'file+olp+datetree 'file+datetree)
                                   ,(concat org-directory "/daily.org"))
         "* TODO \%^{任务标题}  :日常:\n\%u\n \%^{任务描述}" :tree-type week)
        ("n" "Note" entry (,(if emacs/>=26p 'file+olp+datetree 'file+datetree)
                           ,(concat org-directory "/daily.org"))
         "* %^{标题} %^{标签} \n%U\n%a\n%?" :tree-type week)
        ("r" "Record flashes" entry (,(if emacs/>=26p 'file+olp+datetree 'file+datetree)
                                     ,(concat org-directory "/daily.org"))
         "*  %^{Title} %?\n%U\n%a\n"  :tree-type week :jump-to-captured t)
        ("z" "需要删除的capture")
        ("zi" "Idea" entry (file ,(concat org-directory "/idea.org"))
         "*  %^{Title} %?\n%U\n%a\n")
        ("zb" "Book" entry (,(if emacs/>=26p 'file+olp+datetree 'file+datetree)
                            ,(concat org-directory "/book.org"))
         "* Topic: %^{Description}  %^g %? Added: %U")
        ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; config Refile: ref:  [[file:~/Dropbox/knowledgebase/README.org::*Refile][Refile]]
(setq org-refile-targets '((org-agenda-files :maxlevel . 3))
      org-refile-use-outline-path 'file
      org-outline-path-complete-in-steps nil
      org-refile-allow-creating-parent-nodes 'confirm)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; config Refile
(setq org-refile-targets '((org-agenda-files :maxlevel . 3)))
(setq org-refile-use-outline-path 'file)
(setq org-outline-path-complete-in-steps nil)
(setq org-refile-allow-creating-parent-nodes 'confirm)

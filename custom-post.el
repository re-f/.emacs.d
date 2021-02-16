;; 设置 stuck project ，参考：https://oomake.com/question/2338872
(setq org-stuck-projects
  '("TODO={.+}/-DONE" nil nil "SCHEDULED:\\|DEADLINE:"))

(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
              (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "PHONE" "MEETING")))

      org-todo-keyword-faces (quote (
                                     ("NEXT" :foreground "blue" :weight bold)
                                     ("WAITING" . warning)
                                     ("HOLD" :foreground "magenta" :weight bold)
          ))

      org-use-fast-todo-selection t
      org-treat-S-cursor-todo-selection-as-state-change nil
      org-todo-state-tags-triggers      (quote (("CANCELLED" ("CANCELLED" . t))
                                                ("WAITING" ("WAITING" . t))
                                                ("HOLD" ("WAITING") ("HOLD" . t))
                                                (done ("WAITING") ("HOLD"))
                                                ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
                                                ("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
                                                ("DONE" ("WAITING") ("CANCELLED") ("HOLD"))))
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
         "* %^{标题} %^{标签(要加冒号)} \n%U\n%a\n%?" :tree-type week)
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



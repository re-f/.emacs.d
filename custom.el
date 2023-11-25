;;; custom.el --- user customization file    -*- lexical-binding: t no-byte-compile: t -*-
;;; Commentary:
;;;       Add or change the configurations in custom.el, then restart Emacs.
;;;       Put your own configurations in custom-post.el to override default configurations.
;;; Code:


(setq centaur-logo "~/.emacs.d/logo.png")                        ; Logo file or nil (official logo)
(setq centaur-full-name "ref")           ; User full name
(setq centaur-mail-address "z.ref@outlook.com")   ; Email address

(setq centaur-proxy "127.0.0.1:7890")          ; HTTP/HTTPS proxy
(setq centaur-socks-proxy "127.0.0.1:7890")    ; SOCKS proxy
(setq centaur-org-directory (expand-file-name "~/knowledgebase-blog/content-org"))
;; (setq centaur-server nil)                      ; Enable `server-mode' or not: t or nil
;; (setq centaur-icon nil)                        ; Display icons or not: t or nil
(setq centaur-package-archives 'tuna); Package repo: melpa, emacs-china, netease, ustc, tencent or tuna
(setq centaur-theme 'night)                    ; Color theme: auto, random, default, classic, colorful, dark, light, day or night
;; (setq centaur-dashboard nil)                   ; Use dashboard at startup or not: t or nil
;; (setq centaur-restore-frame-geometry nil)      ; Restore the frame's geometry at startup: t or nil
;; (setq centaur-lsp 'eglot)                      ; Set LSP client: lsp-mode, eglot or nil
;; (setq centaur-lsp-format-on-save-ignore-modes '(c-mode c++-mode)) ; Ignore format on save for some languages
;; (setq centaur-chinese-calendar t)              ; Use Chinese calendar or not: t or nil
;; (setq centaur-prettify-symbols-alist nil)      ; Alist of symbol prettifications. Nil to use font supports ligatures.
;; (setq centaur-prettify-org-symbols-alist nil)  ; Alist of symbol prettifications for `org-mode'
;; (setq centaur-benchmark-init t)                ; Enable initialization benchmark or not: t or nil

;; For Emacs devel
;; (setq package-user-dir (locate-user-emacs-file (format "elpa-%s" emacs-major-version)))
;; (setq desktop-base-file-name (format ".emacs-%s.desktop" emacs-major-version))
;; (setq desktop-base-lock-name (format ".emacs-%s.desktop.lock" emacs-major-version))

;; Fonts
(when (display-graphic-p)
  ;; Set default font
  (cl-loop for font in '("Fira Code" "SF Mono" "Hack" "Source Code Pro" "Fira Code"
                          "Menlo" "Monaco" "DejaVu Sans Mono" "Consolas")
           when (font-installed-p font)
           return (set-face-attribute 'default nil
                                      :font font
                                      :height (cond (sys/mac-x-p 170)
                                                    (sys/win32p 110)
                                                    (t 100))))

  ;; Specify font for all unicode characters
  (cl-loop for font in '("Apple Color Emoji" "Symbola" "Symbol")
           when (font-installed-p font)
           return(set-fontset-font t 'unicode font nil 'prepend))

  ;; Specify font for Chinese characters
  (cl-loop for font in '("Source Han Serif SC" "WenQuanYi Micro Hei" "Microsoft Yahei")
           when (font-installed-p font)
           return (set-fontset-font t '(#x4e00 . #x9fff) font)))

;; Mail
;; (setq message-send-mail-function 'smtpmail-send-it
;;       smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
;;       smtpmail-auth-credentials '(("smtp.gmail.com" 587
;;                                    user-mail-address nil))
;;       smtpmail-default-smtp-server "smtp.gmail.com"
;;       smtpmail-smtp-server "smtp.gmail.com"
;;       smtpmail-smtp-service 587)

;; org-mode

;; Calendar
;; Set location , then press `S' can show the time of sunrise and sunset
;; (setq calendar-location-name "Chengdu"
;;       calendar-latitude 30.67
;;       calendar-longitude 104.07)

;; Misc.
;; (setq confirm-kill-emacs 'y-or-n-p)

;; Display on the specified monitor
;; (when (and (> (length (display-monitor-attributes-list)) 1)
;;            (> (display-pixel-width) 1920))
;;   (set-frame-parameter nil 'left 1920))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#1D252C" "#D95468" "#8BD49C" "#EBBF83" "#5EC4FF" "#E27E8D" "#70E1E8" "#A0B3C5"])
 '(centaur-completion-style 'minibuffer)
 '(centaur-org-directory "~/OneDrive/knowledgebase-blog/content-org")
 '(centaur-theme 'warm)
 '(custom-safe-themes
   '("467dc6fdebcf92f4d3e2a2016145ba15841987c71fbe675dcfe34ac47ffb9195" "d14f3df28603e9517eb8fb7518b662d653b25b26e83bd8e129acea042b774298" "7661b762556018a44a29477b84757994d8386d6edee909409fabe0631952dad9" "83e0376b5df8d6a3fbdfffb9fb0e8cf41a11799d9471293a810deb7586c131e6" "c4bdbbd52c8e07112d1bfd00fee22bf0f25e727e95623ecb20c4fa098b74c1bd" default))
 '(fci-rule-color "#56697A")
 '(jdee-db-active-breakpoint-face-colors (cons "#10151C" "#5EC4FF"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#10151C" "#8BD49C"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#10151C" "#41505E"))
 '(objed-cursor-color "#D95468")
 '(org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)
 '(org-modules '(org-crypt org-habit org-id))
 '(org-sidebar-default-fns
   '(org-sidebar--upcoming-items my/org-sidebar org-sidebar--todo-items))
 '(org-speed-commands
   '(("Outline Navigation")
     ("n" org-speed-move-safe 'org-next-visible-heading)
     ("p" org-speed-move-safe 'org-previous-visible-heading)
     ("f" org-speed-move-safe 'org-forward-heading-same-level)
     ("b" org-speed-move-safe 'org-backward-heading-same-level)
     ("F" . org-next-block)
     ("B" . org-previous-block)
     ("u" org-speed-move-safe 'outline-up-heading)
     ("j" . org-goto)
     ("g" org-refile
      '(4))
     ("Outline Visibility")
     ("c" . org-cycle)
     ("C" . org-shifttab)
     (" " . org-display-outline-path)
     ("s" . org-toggle-narrow-to-subtree)
     ("k" . org-cut-subtree)
     ("=" . org-columns)
     ("Meta Data Editing")
     ("t" . org-todo)
     ("," org-priority)
     ("0" org-priority-up)
     ("Show headline level")
     ("1" progn
      (org-content
       (+ 1
          (org-outline-level))))
     ("2" progn
      (org-content
       (+ 2
          (org-outline-level))))
     ("3" progn
      (org-content
       (+ 3
          (org-outline-level))))
     ("4" progn
      (org-content
       (+ 4
          (org-outline-level))))
     ("5" progn
      (org-content
       (+ 5
          (org-outline-level))))
     ("Outline Structure Editing")
     ("U" . org-metaup)
     ("D" . org-metadown)
     ("r" . org-metaright)
     ("l" . org-metaleft)
     ("R" . org-shiftmetaright)
     ("L" . org-shiftmetaleft)
     ("i" progn
      (forward-char 1)
      (call-interactively 'org-insert-heading-respect-content))
     ("w" . org-refile)
     ("a" . org-archive-subtree-default-with-confirmation)
     ("@" . org-mark-subtree)
     ("#" . org-toggle-comment)
     ("Agenda Views etc")
     ("v" . org-agenda)
     ("/" . org-sparse-tree)
     ("Misc")
     ("o" . org-open-at-point)
     ("?" . org-speed-command-help)
     ("<" org-agenda-set-restriction-lock 'subtree)
     (">" org-agenda-remove-restriction-lock)))
 '(org-use-property-inheritance '("CATEGORY"))
 '(org-use-speed-commands t)
 '(paradox-github-token t)
 '(pdf-view-midnight-colors (cons "#A0B3C5" "#1D252C") t)
 '(pyim-dicts
   '((:name "计算机词汇大全" :file "~/.emacs.d/pyim/dict/计算机词汇大全.pyim")
     (:name "开发词库" :file "~/.emacs.d/pyim/dict/开发大神专用词库.pyim")
     (:name "全国县及县以上行政区划地名" :file "~/.emacs.d/pyim/dict/全国县及县以上行政区划地名.pyim")
     (:name "中国高等院校（大学）大全" :file "~/.emacs.d/pyim/dict/中国高等院校（大学）大全.pyim")))
 '(rustic-ansi-faces
   ["#1D252C" "#D95468" "#8BD49C" "#EBBF83" "#5EC4FF" "#E27E8D" "#70E1E8" "#A0B3C5"])
 '(smtpmail-smtp-server "smtp.office365.com")
 '(smtpmail-smtp-service 25)
 '(vc-annotate-background "#1D252C")
 '(vc-annotate-color-map
   (list
    (cons 20 "#8BD49C")
    (cons 40 "#abcd93")
    (cons 60 "#cbc68b")
    (cons 80 "#EBBF83")
    (cons 100 "#e5ae6f")
    (cons 120 "#df9e5b")
    (cons 140 "#D98E48")
    (cons 160 "#dc885f")
    (cons 180 "#df8376")
    (cons 200 "#E27E8D")
    (cons 220 "#df7080")
    (cons 240 "#dc6274")
    (cons 260 "#D95468")
    (cons 280 "#b35365")
    (cons 300 "#8d5163")
    (cons 320 "#675160")
    (cons 340 "#56697A")
    (cons 360 "#56697A")))
 '(vc-annotate-very-old-color nil)
 '(warning-suppress-log-types '((lsp-mode))))

(defun my/org-sidebar (source-buffer)
  "Import TODO."
  (let ((display-buffer
         (generate-new-buffer (format "org-sidebar<%s>" (buffer-name source-buffer))))
        (title (concat "Import items in: " (buffer-name source-buffer))))
    (with-current-buffer display-buffer
      (setf org-sidebar-source-buffer source-buffer))
    (save-window-excursion
      ;; `org-ql-search' displays the buffer, but we don't want to do that here.
      (org-ql-search source-buffer
        '(and (priority > "B")
              (not (done)))
        :narrow t :sort 'date
        :super-groups '((:auto-planning))
        :buffer display-buffer
        :title title))
    display-buffer))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(aw-leading-char-face ((t (:inherit font-lock-keyword-face :bold t :height 2.0))))
 '(aw-minibuffer-leading-char-face ((t (:inherit font-lock-keyword-face :bold t :height 1.0))))
 '(aw-mode-line-face ((t (:inherit mode-line-emphasis :bold t))))
 '(cfrs-border-color ((t (:background "#96A7A9"))))
 '(dashboard-heading ((t (:inherit (font-lock-string-face bold)))))
 '(diff-hl-change ((t (:foreground "#51afef" :background nil))))
 '(diff-hl-delete ((t (:inherit diff-removed :background nil))))
 '(diff-hl-insert ((t (:inherit diff-added :background nil))))
 '(doom-modeline-buffer-file ((t (:inherit (mode-line bold)))))
 '(flycheck-posframe-background-face ((t (:inherit tooltip))))
 '(flycheck-posframe-border-face ((t (:inherit font-lock-comment-face))))
 '(flycheck-posframe-face ((t (:foreground "#859900"))))
 '(flycheck-posframe-info-face ((t (:foreground "#859900"))))
 '(git-timemachine-minibuffer-author-face ((t (:inherit success))))
 '(git-timemachine-minibuffer-detail-face ((t (:inherit warning))))
 '(hl-todo ((t (:inherit variable-pitch :box (:line-width -1) :height 0.85 :width condensed :weight semibold :underline nil :inverse-video t))))
 '(ivy-minibuffer-match-face-1 ((t (:foreground "#83898d"))))
 '(ivy-minibuffer-match-face-2 ((t (:distant-foreground nil :background nil))))
 '(ivy-minibuffer-match-face-3 ((t (:distant-foreground nil :background nil))))
 '(ivy-minibuffer-match-face-4 ((t (:distant-foreground nil :background nil))))
 '(ivy-posframe ((t (:inherit tooltip))))
 '(ivy-posframe-border ((t (:background "#96A7A9"))))
 '(lsp-headerline-breadcrumb-path-error-face ((t :underline (:style wave :color "#ff6c6b") :inherit lsp-headerline-breadcrumb-path-face)))
 '(lsp-headerline-breadcrumb-path-hint-face ((t :underline (:style wave :color "#98be65") :inherit lsp-headerline-breadcrumb-path-face)))
 '(lsp-headerline-breadcrumb-path-info-face ((t :underline (:style wave :color "#98be65") :inherit lsp-headerline-breadcrumb-path-face)))
 '(lsp-headerline-breadcrumb-path-warning-face ((t :underline (:style wave :color "#ECBE7B") :inherit lsp-headerline-breadcrumb-path-face)))
 '(lsp-headerline-breadcrumb-symbols-error-face ((t :inherit lsp-headerline-breadcrumb-symbols-face :underline (:style wave :color "#ff6c6b"))))
 '(lsp-headerline-breadcrumb-symbols-hint-face ((t :inherit lsp-headerline-breadcrumb-symbols-face :underline (:style wave :color "#98be65"))))
 '(lsp-headerline-breadcrumb-symbols-info-face ((t :inherit lsp-headerline-breadcrumb-symbols-face :underline (:style wave :color "#98be65"))))
 '(lsp-headerline-breadcrumb-symbols-warning-face ((t :inherit lsp-headerline-breadcrumb-symbols-face :underline (:style wave :color "#ECBE7B"))))
 '(lsp-ui-sideline-code-action ((t (:inherit warning))))
 '(macrostep-expansion-highlight-face ((t (:inherit tooltip :extend t))))
 '(org-done ((t (:inherit org-headline-done :strike-through nil :weight bold))))
 '(org-ellipsis ((t (:foreground nil))))
 '(org-headline-done ((t (:foreground "#5B6268" :strike-through nil))))
 '(org-pomodoro-mode-line ((t (:inherit warning))))
 '(org-pomodoro-mode-line-break ((t (:inherit success))))
 '(org-pomodoro-mode-line-overtime ((t (:inherit error))))
 '(paradox-archive-face ((t (:inherit font-lock-doc-face))))
 '(paradox-description-face ((t (:inherit completions-annotations))))
 '(posframe-border ((t (:background "#7c6f64"))))
 '(pulse-highlight-face ((t (:inherit region))))
 '(pulse-highlight-start-face ((t (:inherit region))))
 '(symbol-overlay-default-face ((t (:inherit (region bold)))))
 '(transient-posframe ((t (:inherit tooltip))))
 '(transient-posframe-border ((t (:background "#96A7A9"))))
 '(which-key-posframe ((t (:inherit tooltip))))
 '(which-key-posframe-border ((t (:background "#96A7A9"))))
 '(ztreep-arrow-face ((t (:inherit font-lock-comment-face))))
 '(ztreep-diff-header-face ((t (:inherit (diff-header bold)))))
 '(ztreep-diff-header-small-face ((t (:inherit diff-file-header))))
 '(ztreep-diff-model-add-face ((t (:inherit diff-nonexistent))))
 '(ztreep-diff-model-diff-face ((t (:inherit diff-removed))))
 '(ztreep-diff-model-ignored-face ((t (:inherit font-lock-doc-face :strike-through t))))
 '(ztreep-diff-model-normal-face ((t (:inherit font-lock-doc-face))))
 '(ztreep-expand-sign-face ((t (:inherit font-lock-function-name-face))))
 '(ztreep-header-face ((t (:inherit diff-header))))
 '(ztreep-leaf-face ((t (:inherit diff-index))))
 '(ztreep-node-face ((t (:inherit font-lock-variable-name-face)))))

;; ref: https://yiming.dev/blog/2018/03/02/my-org-refile-workflow/
;; This config lets org-refile searches all the opened org mode files and fetches all the headings in them.
;;


;; 尝试做加密，但是失败了
;; 参考的文章：
;; https://devbins.github.io/post/gpg5/
;; https://coldnew.github.io/4bb1df06/
;; 失败的原因：在 terminal 中可以加密，但是在客户端加密就显示 Encrypt failed，但是可以解密
(require 'org-crypt)
(org-crypt-use-before-save-magic)
(setq org-tags-exclude-from-inheritance (quote("日常")))
(setq org-crypt-key nil)
(setq org-crypt-tag-matcher "日常")

(defun +org/opened-buffer-files ()
  "Return the list of files currently opened in emacs"
  (delq nil
        (mapcar (lambda (x)
                  (if (and (buffer-file-name x)
                           (string-match "\\.org$"
                                         (buffer-file-name x)))
                      (buffer-file-name x)))
                (buffer-list))))

(setq org-refile-targets '((+org/opened-buffer-files :maxlevel . 9)))

;;; custom.el ends here

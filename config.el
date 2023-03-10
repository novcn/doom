;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;;
;; Identity
;;
(setq user-full-name "Colin Gabrielson"
      user-mail-address "colin.gabrielson@gmail.com"
      ispell-dictionary "english")
      ;;doom-theme 'doom-palenight)

;; Change theme based on if in terminal or gui
(if (display-graphic-p)
    (setq doom-theme 'doom-palenight)
  (setq doom-theme 'doom-laserwave))

;;
;; Indentation
;;
(setq tab-width 2)
(setq standard-indent 2
      js-indent-level standard-indent
      rust-indent-offset standard-indent
      sh-basic-offset standard-indent)
(setq-default indent-tabs-mode nil
              tab-width standard-indent)

;;
;; Font
;;
(setq doom-font (font-spec :family "Fira Code" :size 14)
      doom-variable-pitch-font (font-spec :family "Fira Code")
      doom-unicode-font (font-spec :family "Fira Code" :size 12)
      doom-big-font (font-spec :family "Fira Code" :size 18))
(add-hook! 'doom-load-theme-hook :append
           (doom/reload-font))


;;(add-hook 'org-mode-hook 'variable-pitch-mode)
;;  (add-hook 'org-mode-hook 'visual-line-mode)

(custom-theme-set-faces
 'user
 '(org-block ((t (:inherit fixed-pitch))))
 '(org-code ((t (:inherit (shadow fixed-pitch)))))
 '(org-document-info ((t (:foreground "dark orange"))))
 '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
 '(org-indent ((t (:inherit (org-hide fixed-pitch)))))
 '(org-link ((t (:foreground "royal blue" :underline t))))
 '(org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch)))))
 '(org-property-value ((t (:inherit fixed-pitch))) t)
 '(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
 '(org-table ((t (:inherit fixed-pitch :foreground "#83a598"))))
 '(org-tag ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.8))))
 '(org-verbatim ((t (:inherit (shadow fixed-pitch))))))


;;
;; Browser
;;
(setq browse-url-browser-function 'browse-url-generic)
(when (executable-find "qutebrowser")
  (setq browse-url-generic-program "qutebrowser"))
(when (getenv "BROWSER")
  (setq browse-url-generic-program (getenv "BROWSER")))

;;
;; Misc
;;
(evil-terminal-cursor-changer-activate)
(which-key-setup-side-window-right)
(setq display-line-numbers-type 'relative
      ;; one-more
      evil-move-beyond-eol t
      ;; Use block cursor in normal mode and underscore in insert
      evil-insert-state-cursor  'hbar
      which-key-idle-delay 0.01)

;; The maximum displayed length of the branch name of version control.
(setq doom-modeline-vcs-max-length 24)
;; Whether display the modal state icon.
(setq doom-modeline-modal-icon nil)
(setq doom-modeline-env-enable-rust t)
;; This should fix branch reflection however might cause perf issues
(setq auto-revert-check-vc-info t)
;; Auto refresh buffers to what is changed on disk. This should help with syncthing and org agenda
(global-auto-revert-mode t)


;;
;; Rust settings
;;
(after! rustic
  (setq rustic-format-trigger 'on-compile))

(add-hook 'rustic-mode-hook
          (lambda () (setq indent-tabs-mode nil)))

;;
;; Magit
;;
(after! magit
  (setq git-commit-summary-max-length 100)
)

;; LSP shit might change this if I switch back to eglot
;; Warning: Elgot seems slower plus I couldn't get go-to completion to work. Ran into a lot of errors,
;; some related to clippy (need to remove -Z from the rustic-flychec-clippy-params variable) Ran into errors with racer.
;; Overall lsp over eglot gave me a better experience.
(after! lsp-mode
  (setq lsp-ui-doc-position 'top)
  (setq lsp-rust-clippy-preference "on")
  (setq lsp-rust-cfg-test t)
  (setq lsp-rust-analyzer-diagnostics-enable-experimental nil)
  (setq lsp-rust-build-on-save t))

;; For some rust projects, like the engine, compiling them with the default features will cause
;; rust-analyzer to complain with many errors due to functions only compiled with the `test`
;; feature flag. This causes a lot of noise to sift through to find real compile errors.
;; This function allows these projects to be configured with lsp features such that these errors don't occur.
(defun novcn/set-rust-project-features ()
  "Set the configured lsp features for the current rust project."
  (setq lsp-rust-features-before lsp-rust-features)
  (defvar local-repo)
  (setq local-repo (shell-command-to-string "basename $(git rev-parse --show-toplevel) | tr -d '\n'"))
  (pcase local-repo
    ("engine-rs" (setq lsp-rust-features ["test"]))
    ;; Can configure any other repos that should use rust-analyzer with other than default features.
    ;; default
    (default (setq lsp-rust-features [])))
  ;; Errors occur without a short sleep
  (sleep-for 1)
  ;; Check to see if we're in an enabled lsp workspace and if our features are different than before
  (if (and (fboundp 'lsp-restart-workspace) (not (eq lsp-rust-features-before lsp-rust-features)))
      (lsp-restart-workspace)))

;; Hook this into every project switch
(add-hook 'projectile-after-switch-project-hook 'novcn/set-rust-project-features)

(add-hook! rustic-mode (set-fill-column 100))
;;(add-hook 'rust-mode-hook (lambda ()
;;                            (lsp-activate-if-already-activated 'rust-analyzer)))

;;
;; Binding Overrides
;;

;; Easier window navigation via c-<key>
(evil-global-set-key 'normal (kbd "C-h") 'evil-window-left)
(evil-global-set-key 'normal (kbd "C-l") 'evil-window-right)
(evil-global-set-key 'normal (kbd "C-j") 'evil-window-down)
(evil-global-set-key 'normal (kbd "C-k") 'evil-window-up)

;; Easier navigation to home and end
(evil-global-set-key 'normal (kbd "H") 'evil-first-non-blank)
(evil-global-set-key 'normal (kbd "L") 'evil-last-non-blank)
(evil-global-set-key 'visual (kbd "H") 'evil-first-non-blank)
(evil-global-set-key 'visual (kbd "L") 'evil-last-non-blank)

;; Hack to get Super-Esc QMK binding to work in gui mode
(evil-global-set-key 'insert (kbd "s-`") "`")

;;
;; Parens
;;
;;(after! smartparens
;;  (smartparens-global-mode -1))

(after! smartparens
    (let ((unless-list '(sp-point-before-word-p
                         sp-point-after-word-p
                         sp-point-before-same-p)))
      (sp-pair "`" nil :unless unless-list)))

;;
;; Org mode settings
;; This is why we are here. This is why you even write elisp.
;;
(setq novcn/org-agenda-directory (file-truename "~/org"))

(use-package org-bullets
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))


;;(when window-system
;;  (let* ((variable-tuple
;;          (cond ((x-list-fonts   "ETBembo")         '(:font   "ETBembo"))
;;                ((x-list-fonts   "Source Sans Pro") '(:font   "Source Sans Pro"))
;;                ((x-list-fonts   "Lucida Grande")   '(:font   "Lucida Grande"))
;;                ((x-list-fonts   "Verdana")         '(:font   "Verdana"))
;;                ((x-family-fonts "Sans Serif")      '(:family "Sans Serif"))
;;                (nil (warn "Cannot find a Sans Serif Font."))))
;;         (base-font-color (face-foreground 'default nil 'default))
;;         (headline `(:inherit default :weight bold
;;                     :foreground ,base-font-color)))
;;  
;;    (custom-theme-set-faces
;;     'user
;;     `(org-level-8        ((t (,@headline ,@variable-tuple))))
;;     `(org-level-7        ((t (,@headline ,@variable-tuple))))
;;     `(org-level-6        ((t (,@headline ,@variable-tuple))))
;;     `(org-level-5        ((t (,@headline ,@variable-tuple))))
;;     `(org-level-4        ((t (,@headline ,@variable-tuple :height 1.1))))
;;     `(org-level-3        ((t (,@headline ,@variable-tuple :height 1.25))))
;;     `(org-level-2        ((t (,@headline ,@variable-tuple :height 1.5))))
;;     `(org-level-1        ((t (,@headline ,@variable-tuple :height 1.75))))
;;     `(org-headline-done  ((t (,@headline ,@variable-tuple :strike-through t))))
;;     `(org-document-title ((t (,@headline ,@variable-tuple
;;                                          :height 2.0 :underline nil)))))))

(after! org
  (setq org-export-backends (quote (md  ascii html latex)))

  (setq org-pomodoro-finished-sound "/home/novcn/dot/media/cp77_dial_tone.mp3")
  (setq org-pomodoro-short-break-sound "/home/novcn/dot/media/cp77_dial_tone.mp3")
  (setq org-pomodoro-long-break-sound "/home/novcn/dot/media/cp77_dial_tone.mp3")
  (setq org-pomodoro-start-sound "/home/novcn/dot/media/cp77_dial_tone.mp3")

  ;; Beautify
  (setq org-hide-emphasis-markers t)

  (custom-theme-set-faces
   'user
   '(variable-pitch ((t (:family "ETBembo" :height 180 :weight thin))))
   '(fixed-pitch ((t ( :family "Fira Code Retina" :height 160)))))
  
    (font-lock-add-keywords 'org-mode
                            '(("^ *\\([-]\\) "
                               (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

  ;; Org General settings
  (setq org-directory "~/org/")
  (setq org-ellipsis " ▼ ") ;; just to be fancy
  (setq org-archive-location "~/org/archive/")

  ;; Org journal settings
  (setq org-journal-file-type 'monthly)
  (setq org-journal-file-format "%Y-%m.org")
  (setq org-journal-date-format "%A, %m-%d-%Y")
  (setq org-journal-encrypt-journal t)
  ;; This will encrypt specific entries which isn't ideal. We want to encrypt the whole thing
  ;;(setq org-journal-enable-encryption t)
  (setq org-journal-enable-agenda-integration t)

  ;; Attempt to no longer try to decrypt all journal files simply when a date is chosen
  (remove-hook 'calendar-today-visible-hook 'org-journal-mark-entries)
  (remove-hook 'calendar-today-invisible-hook 'org-journal-mark-entries)

  ;; Disable exporting with Table of Contents
  (setq org-export-with-toc nil)

  ;; Org agenda settings
  (setq org-extend-today-until 4)
  (setq org-deadline-warning-days 7)
  ;; set faces to error when the deadline has passed
  (setq org-agenda-deadline-faces
      '((1.001 . error)
        (1.0 . org-warning)
        (0.5 . org-upcoming-deadline)
        (0.0 . org-upcoming-distant-deadline)))

  ;; Org Capture templates
  (setq org-capture-templates
  '(("c" "Capture" entry (file "capture.org")
     "* TODO %?")
    ("t" "Task" entry (file "tasks.org")
     "* TODO %?")
    ("n" "Note" entry (file "notes.org")
     "* %?")
    ("l" "Listen" entry (file "listen.org")
     "* %?")
    ("w" "Watch" entry (file "watch.org")
     "* %?")
    ("m" "Meeting" entry (file "meetings.org")
     "* TODO %?")
    ("r" "Remember" entry (file "memory.org")
     "* TODO %?")
    ("R" "Read" entry (file "read.org")
     "* TODO %?"
     :immediate-finish t)))

  ;; Org keywords
  (setq org-todo-keywords
        (quote ((sequence "TODO(t)" "TRIAGE(g)" "UNDERWAY(u)" "|" "DONE(d)" "FAILED(f)")
                (sequence "WAIT(w@/!)" "REVIEW(r@/!)" "|" "CANCELLED(c@/!)" "MEETING"))))

  ;; Org keyword faces
  (setq org-todo-keyword-faces
        (quote (("TODO" :foreground "red" :weight bold)
                ("TRIAGE" :foreground "goldenrod1" :weight bold)
                ("UNDERWAY" :foreground "white smoke" :weight bold)
                ("DONE" :foreground "medium slate blue" :weight bold)
                ("FAILED" :foreground "dark red" :weight bold)
                ("WAIT" :foreground "black" :weight bold)
                ("REVIEW" :foreground "chartreuse3" :weight bold)
                ("CANCELLED" :foreground "DeepSkyBlue4" :weight bold)))))

;;
;; Run org agenda after start-up
;;
;; (add-hook 'after-init-hook 'org-agenda-list)

;;
;; Org agenda mode settings
;;
(after! org-agenda
  ;; less clutter
  (setq org-agenda-skip-scheduled-if-done t)
  (setq org-agenda-skip-deadline-if-done t)

  (setq org-agenda-include-deadlines t)
  (setq org-agenda-block-separator nil)
  (setq org-agenda-tags-column 100)
  (setq org-agenda-compact-blocks t)
  (org-super-agenda-mode)

  ;; save all org buffers - SPC m s
  (map!
      :map org-agenda-mode-map
      :localleader
      "s" 'org-save-all-org-buffers)

  ;; After agenda reload agenda-files variable for any new entries
  (setq agendaSkipDirs
        [
         "/home/novcn/org/.stfolder"
         "/home/novcn/org/.stversions"
         "/home/novcn/org/.undodir"
         ])
  (add-hook 'org-agenda-mode-hook
            (lambda ()
              (setq org-agenda-files
                    (seq-filter
                     (lambda (path)
                       (not (seq-some
                             (lambda (x) (string-match x path))
                             agendaSkipDirs )))
                     (directory-files-recursively "/home/novcn/org" "\.org$")))))

  ;;
  ;; Org super-agenda settings
  ;;
  (use-package! org-super-agenda
    :commands (org-super-agenda-mode))

  ;;
  ;; Org super agenda custom
  ;;
  (setq org-agenda-custom-commands
        '(("o" "Overview"
           ((agenda "" ((org-agenda-span 'day)
                        (org-super-agenda-groups
                         '((:name "Today"
                            :time-grid t
                            :date today
                            :todo "TODAY"
                            :scheduled today
                            :order 1))))))))))

(setq projectile-project-search-path '("~/src/rust/" "~/src/node" "~/src/infra" "~/prj"))

(add-hook 'sh-mode-hook (lambda () (sh-electric-here-document-mode -1)))


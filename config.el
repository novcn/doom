;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;;
;; Identity
;;
(setq user-full-name "Colin Gabrielson"
      user-mail-address "colin.gabrielson@gmail.com"
      ispell-dictionary "english"
      doom-theme 'doom-palenight)

;;
;; Indentation
;;
(setq tab-width 2)
(setq standard-indent 2
      js-indent-level standard-indent
      rustic-indent-offset standard-indent
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

;;
;; Rust settings
;;
(after! rustic
  (setq rustic-indent-offset standard-indent)
  (setq rustic-format-trigger 'on-compile))

(add-hook 'rustic-mode-hook
          (lambda () (setq indent-tabs-mode nil)))

;; LSP shit might change this if I switch back to eglot
;; Warning: This seems slower plus I couldn't get go-to completion to work. Ran into a lot of errors,
;; some related to clippy (need to remove -Z from the rustic-flychec-clippy-params variable) Ran into errors with racer.
;; Overall lsp over eglot gave me a better experience.
(after! lsp-mode
  (setq lsp-ui-doc-position 'top)
  (setq lsp-rust-clippy-preference "on")
  (setq lsp-rust-cfg-test t)
  (setq lsp-rust-build-on-save t))

(add-hook 'rust-mode-hook (lambda ()
                            (lsp-activate-if-already-activated 'rust-analyzer)))

(add-hook! rustic-mode (set-fill-column 100))

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
(after! smartparens
  (smartparens-global-mode -1))

;;
;; Org mode settings
;; This is why we are here. This is why you even write elisp.
;;
(setq novcn/org-agenda-directory (file-truename "~/org"))

(after! org
  (setq org-export-backends (quote (md confluence ascii html latex)))
  (require 'ox-confluence)

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

  ;; Org agenda settings
  (setq org-extend-today-until 4)
  (setq org-pomodoro-play-sounds nil)
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
      "s" #'org-save-all-org-buffers)

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

;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;;
;; Identity
;;
(setq user-full-name "Colin Gabrielson"
      user-mail-address "colin.gabrielson@gmail.com"
      ispell-dictionary "english")

;; Change theme based on if in terminal or gui
(if (display-graphic-p)
    (setq doom-theme 'doom-gruvbox)
  (setq doom-theme 'doom-gruvbox))

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


(require 'json)

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
;; For commit signing passphrase caching and finding other programs
;;
(require 'exec-path-from-shell)

(with-eval-after-load 'exec-path-from-shell
  (dolist (var '("SSH_AUTH_SOCK" "SSH_AGENT_PID" "GPG_AGENT_INFO" "LANG" "LC_CTYPE"))
    (add-to-list 'exec-path-from-shell-variables var)))

(when (or (memq window-system '(mac ns x pgtk))
          (unless (memq system-type '(ms-dos windows-nt))
            (daemonp)))
  (exec-path-from-shell-initialize))

(provide 'init-exec-path)


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

;;;; For some rust projects, like the engine, compiling them with the default features will cause
;;;; rust-analyzer to complain with many errors due to functions only compiled with the `test`
;;;; feature flag. This causes a lot of noise to sift through to find real compile errors.
;;;; This function allows these projects to be configured with lsp features such that these errors don't occur.
;;(defun novcn/set-rust-project-features ()
;;  "Set the configured lsp features for the current rust project."
;;  (setq lsp-rust-features-before lsp-rust-features)
;;  (defvar local-repo)
;;  (setq local-repo (shell-command-to-string "basename $(git rev-parse --show-toplevel) | tr -d '\n'"))
;;  (pcase local-repo
;;    ("engine-rs" (setq lsp-rust-features ["test"]))
;;    ;; Can configure any other repos that should use rust-analyzer with other than default features.
;;    ;; default
;;    (default (setq lsp-rust-features [])))
;;  ;; Errors occur without a short sleep
;;  (sleep-for 1)
;;  ;; Check to see if we're in an enabled lsp workspace and if our features are different than before
;;  (if (and (fboundp 'lsp-restart-workspace) (not (eq lsp-rust-features-before lsp-rust-features)))
;;      (lsp-restart-workspace)))
;;
;;;; Hook this into every project switch
;;(add-hook 'projectile-after-switch-project-hook 'novcn/set-rust-project-features)
;;
;;(add-hook! rustic-mode (set-fill-column 100))
;;(add-hook 'rust-mode-hook (lambda ()
;;                            (lsp-activate-if-already-activated 'rust-analyzer)))

(setq treemacs-no-delete-other-windows nil)

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
;; Configure evil surround to no longer add a space between word and parens
;;
(after! evil-surround
  (push '(?w . ("(" . ")")) evil-surround-pairs-alist))

;;
;; Org mode settings
;; This is why we are here. This is why you even write elisp.
;;
(setq novcn/org-agenda-directory (file-truename "~/org"))

;; Save whatever's in the system clipboard before
;; replacing it with Emacs's text.
(setq save-interprogram-paste-before-kill t)

;; Custom faces for markdown headers
(custom-set-faces!
'(markdown-header-delimiter-face :foreground "#616161" :height 0.9)
'(markdown-header-face-1 :height 1.8 :foreground "#A3BE8C" :weight extra-bold :inherit markdown-header-face)
'(markdown-header-face-2 :height 1.4 :foreground "#EBCB8B" :weight extra-bold :inherit markdown-header-face)
'(markdown-header-face-3 :height 1.2 :foreground "#D08770" :weight extra-bold :inherit markdown-header-face)
'(markdown-header-face-4 :height 1.15 :foreground "#BF616A" :weight bold :inherit markdown-header-face)
'(markdown-header-face-5 :height 1.1 :foreground "#b48ead" :weight bold :inherit markdown-header-face)
'(markdown-header-face-6 :height 1.05 :foreground "#5e81ac" :weight semi-bold :inherit markdown-header-face))

(after! org
  (customize-set-variable 'org-blank-before-new-entry'((heading . nil)(plain-list-item . nil)))(setq org-cycle-separator-lines 1)
  (setq org-export-backends (quote (md  ascii html latex)))

  ;; Ultradian cycle pomodoros. Disabled for now
  (setq org-pomodoro-finished-sound "/home/novcn/dot/media/cp77_dial_tone.mp3")
  (setq org-pomodoro-short-break-sound "/home/novcn/dot/media/cp77_dial_tone.mp3")
  (setq org-pomodoro-long-break-sound "/home/novcn/dot/media/cp77_dial_tone.mp3")
  (setq org-pomodoro-start-sound "/home/novcn/dot/media/cp77_dial_tone.mp3")

  ;; Beautify
  (setq org-hide-emphasis-markers t)

  ;; Org General settings
  (setq org-directory "~/org/")
  (setq org-ellipsis " ▼ ") ;; just to be fancy

  ;; These are to speed up agenda https://orgmode.org/manual/Speeding-Up-Your-Agendas.html
  (setq org-agenda-dim-blocked-tasks nil)
  (setq org-agenda-use-tag-inheritance nil)
  (setq org-agenda-ignore-properties '(effort appt stats category))

;;
;;

      ;;'(())

  (setq org-archive-location "~/org/archive/%s::datetree/* Archived tasks")

  (defun novcn/org-archive-done-tasks ()
    "Archive all done tasks."
    (interactive)
    (org-map-entries 'org-archive-subtree "/DONE" 'file)
    (org-map-entries 'org-archive-subtree "/CANCELED" 'file)
    (org-map-entries 'org-archive-subtree "/FAILED" 'file))

  ;; Org journal settings
  (setq org-journal-file-type 'monthly)
  (setq org-journal-file-format "%Y-%m.org")
  (setq org-journal-date-format "%A, %m-%d-%Y")
  ;; This will encrypt specific entries which isn't ideal. We want to encrypt the whole thing
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

  (setq org-capture-templates
      `(("i" "Inbox" entry  (file "inbox.org")
         ,(concat "* TODO %?\n"
                  "/Entered on/ %U"))
        ("s" "Slipbox" entry  (file "braindump/org/inbox.org")
         "* %?\n")))

  (defun novcn/org-capture-inbox ()
    (interactive)
    (org-capture nil "i"))

  (defun novcn/org-capture-slipbox ()
    (interactive)
    (org-capture nil "s"))

  (defun novcn/org-agenda ()
    (interactive)
    (org-agenda nil " "))

  (bind-key "C-c <tab>" #'novcn/org-capture-inbox)
  (bind-key "C-c SPC" #'novcn/org-agenda)

  (map! :map org-agenda-mode-map
        "i" #'org-agenda-clock-in
        "R" #'org-agenda-refile
        "c" #'novcn/org-inbox-capture)

  ;; Org keywords
  (setq org-todo-keywords
        (quote ((sequence "TODO(t)" "TRIAGE(g)" "UNDERWAY(u)" "|" "DONE(d)" "FAILED(f)")
                (sequence "WAIT(w)" "REVIEW(r)" "|" "CANCELLED(c@/!)" "MEETING"))))

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

(defun my/org-insert-heading-newline ()
  "Add an extra newline before a new org heading."
  (org-return))

(advice-add 'org-insert-heading :after 'my/org-insert-heading-newline)
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
                     (directory-files-recursively "/home/novcn/org/issues" "\.org$")))))

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


  (setq org-agenda-custom-commands
      '(
        (" " "Jira issues" agenda "Jira issues assigned to me"
         ((org-agenda-files '("~/org/issues"))))
        (" " "Inbox" agenda "To Refile"
         ((org-agenda-files '("~/org/inbox.org"))))
       )
      )


(setq projectile-project-search-path '("~/src/rust/" "~/src/node" "~/src/infra" "~/prj"))

(add-hook 'sh-mode-hook (lambda () (sh-electric-here-document-mode -1)))


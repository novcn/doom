;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;;
;; Identity
;;
(setq user-full-name "Colin Gabrielson"
      user-mail-address "colin.gabrielson@gmail.com"
      doom-scratch-initial-major-mode 'lisp-interaction-mode
      doom-theme 'doom-one
      display-line-numbers-type nil
      load-prefer-newer t
      +zen-text-scale 1
      writeroom-extra-line-spacing 0.3

      lsp-ui-sideline-enable nil
      lsp-enable-symbol-highlighting nil
      search-highlight t
      search-whitespace-regexp ".*?"
      org-directory "~/org/"
      org-ellipsis " ▼ "
      org-adapt-indentation nil
      org-habit-show-habits-only-for-today t)

(use-package! dired-narrow
  :commands (dired-narrow-fuzzy)
  :init
  (map! :map dired-mode-map
        :desc "narrow" "/" #'dired-narrow-fuzzy))

;; Consider these
;;(use-package! smartparens
;;  :init
;;  (map! :map smartparens-mode-map
;;        "C-M-f" #'sp-forward-sexp
;;        "C-M-b" #'sp-backward-sexp
;;        "C-M-u" #'sp-backward-up-sexp
;;        "C-M-d" #'sp-down-sexp
;;        "C-M-p" #'sp-backward-down-sexp
;;        "C-M-n" #'sp-up-sexp
;;        "C-M-s" #'sp-splice-sexp
;;        "C-)" #'sp-forward-slurp-sexp
;;        "C-}" #'sp-forward-barf-sexp
;;        "C-(" #'sp-backward-slurp-sexp
;;        "C-M-)" #'sp-backward-slurp-sexp
;;        "C-M-)" #'sp-backward-barf-sexp))

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
;; Magit
;;
(after! magit
  (setq git-commit-summary-max-length 100)
)


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

;; Save whatever's in the system clipboard before
;; replacing it with Emacs's text.
(setq save-interprogram-paste-before-kill t)

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
  (setq novcn/org-agenda-directory (file-truename "~/org/gtd"))
  (setq org-ellipsis " ▼ ") ;; just to be fancy

  ;; These are to speed up agenda https://orgmode.org/manual/Speeding-Up-Your-Agendas.html
  (setq org-agenda-dim-blocked-tasks nil)
  (setq org-agenda-use-tag-inheritance nil)
  (setq org-agenda-ignore-properties '(effort appt stats category))

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
      `(("i" "Inbox" entry  (file "gtd/inbox.org")
         ,(concat "* TODO %?\n"
                  "/Entered on/ %U"))
        ("s" "Slipbox" entry  (file "gtd/braindump/inbox.org")
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

;;
;; Org agenda mode settings
;;
(after! org-agenda
  ;; less clutter
  (setq org-agenda-skip-scheduled-if-done t)
  (setq org-agenda-skip-deadline-if-done t)

  (setq org-agenda-include-deadlines 'unencrypted)
  (setq org-agenda-block-separator nil)
  (setq org-agenda-tags-column 100)
  (setq org-agenda-compact-blocks t)

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

  (require 'find-lisp)
  (setq novcn/org-agenda-directory
        (expand-file-name "gtd/" org-directory))
  (setq org-agenda-files
        (directory-files-recursively novcn/org-agenda-directory "\.org$")))

(setq projectile-project-search-path '("~/src/rust/" "~/src/node" "~/src/infra" "~/prj"))
(add-hook 'sh-mode-hook (lambda () (sh-electric-here-document-mode -1)))

(setq debug-on-error t)

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
; MELPA packages: string-inflection, whitespace-cleanup-mode, reveal-in-osx-finder, golden-ratio-scroll-screen
; (setq package-check-signature nil)

(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/")))
(package-initialize)

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

(add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes")
(el-get 'sync)

(setq my:el-get-packages '(
   ag
   anzu
   company-lsp
   browse-at-remote
   company-mode
   dtrt-indent
   docker-tramp
   el-get
   flycheck
   go-mode
   helm
   helm-ag
   helm-ls-git
   json-mode
   lsp-mode
   lsp-ui
   neotree
   tabbar
   typescript-mode
   sr-speedbar
   web-mode
   yaml-mode
   ;help-mode+
   yascroll
   tabbar
   tide
   use-package
))
(el-get 'sync my:el-get-packages)

(setq shell-file-name "bash")
(setq shell-command-switch "-ic")

(setq tramp-default-method "ssh")
(setq explicit-shell-file-name "/bin/bash")

(setq yas-snippet-dirs '("~/.emacs.d/snippets"))

(require 're-builder)
(setq reb-re-syntax 'string)

(when (display-graphic-p) (setq confirm-kill-emacs 'yes-or-no-p))
(helm-mode 0)
(global-anzu-mode +1)
;; (yascroll-bar-mode 1)
;; (yas-global-mode 1)
(setq column-number-mode t)
(add-hook 'after-init-hook 'global-company-mode)
(setq company-dabbrev-downcase nil)
(setq company-dabbrev-ignore-case nil)
(when (display-graphic-p) (tool-bar-mode -1))

(defun set-background-for-terminal (&optional frame)
  (or frame (setq frame (selected-frame)))
  "unsets the background color in terminal mode"
  (unless (display-graphic-p frame)
    (set-face-background 'default "unspecified-bg" frame)))
(add-hook 'after-make-frame-functions 'set-background-for-terminal)
(add-hook 'window-setup-hook 'set-background-for-terminal)

(blink-cursor-mode nil)
(windmove-default-keybindings)

(require 'string-inflection)
(global-set-key (kbd "C-c i") 'string-inflection-cycle)
(global-set-key (kbd "C-c L") 'string-inflection-lower-camelcase)  ;; Force to lowerCamelCase
;(require 'misc)
;(global-set-key "\M-f" 'forward-to-word)

(add-hook 'ruby-mode-hook 'whitespace-cleanup-mode)
(add-hook 'js-mode-hook 'whitespace-cleanup-mode)
(add-hook 'python-mode-hook 'whitespace-cleanup-mode)
(add-hook 'web-mode-hook 'whitespace-cleanup-mode)
(setq whitespace-cleanup-mode-preserve-point t)

(defun indent-or-complete ()
  (interactive)
  (if (looking-at "\\_>")
      (company-complete-common)
    (indent-according-to-mode)))

(global-set-key (kbd "TAB") 'indent-or-complete)
(with-eval-after-load 'company
  (define-key company-active-map (kbd "M-n") nil)
  (define-key company-active-map (kbd "M-p") nil)
  (define-key company-active-map (kbd "C-n") #'company-select-next)
  (define-key company-active-map (kbd "C-p") #'company-select-previous))

(setq css-indent-offset 4)
(setq web-mode-markup-indent-offset 4)
(setq web-mode-code-indent-offset 4)
(setq web-mode-css-indent-offset 4)
(setq web-mode-attr-indent-offset 2)
(setq web-mode-indentation-params '("case-extra-offset" . nil))
(setq web-mode-enable-auto-quoting nil)

(require 'golden-ratio-scroll-screen)
(global-set-key [remap scroll-down-command] 'golden-ratio-scroll-screen-down)
(global-set-key [remap scroll-up-command] 'golden-ratio-scroll-screen-up)

(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  (company-mode +1)
  )

;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

;; formats the buffer before saving (comment it out if using prettier instead)
;; (add-hook 'before-save-hook 'tide-format-before-save)

;; (eval-after-load 'web-mode
;;     '(progn
;;        (add-hook 'web-mode-hook #'add-node-modules-path)
;;        (add-hook 'web-mode-hook #'prettier-js-mode)))

(add-hook 'typescript-mode-hook #'setup-tide-mode)
(eval-after-load 'typescript-mode
    '(progn
       (add-hook 'typescript-mode-hook #'add-node-modules-path)
       (add-hook 'typescript-mode-hook #'prettier-js-mode)))

(setq lsp-auto-guess-root t)
(setq lsp-prefer-flymake nil)
(add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.scss\\'" . css-mode))

(use-package lsp-mode
  :config
  (add-hook 'python-mode-hook #'lsp))

(add-hook 'go-mode-hook 'lsp-deferred)

(defadvice web-mode-highlight-part (around tweak-jsx activate)
  (if (equal web-mode-content-type "jsx")
      (let ((web-mode-enable-part-face nil))
        ad-do-it)
    ad-do-it))

(global-set-key [?\C-c ?f] (quote helm-ls-git-ls))
(setq neo-autorefresh nil)
(global-set-key [?\C-c ?t] 'neotree-find)

(require 'recentf)
(setq recentf-auto-cleanup 'never) ;; disable before we start recentf! (because of Tramp mode)
(setq recentf-keep '(file-remote-p file-readable-p)) ;; don't check if remote files exist
(recentf-mode 1)
(setq recentf-max-menu-items 50)
(setq recentf-max-saved-items 50)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)
(global-set-key "\C-x\ \C-b" 'helm-mini)

(defun helm-do-ag-recursive (&optional non-recursive)
  "Like `helm-do-ag', but ags recursively by default."
  (interactive "P")
  (let* ((current-prefix-arg (not non-recursive))
         (helm-current-prefix-arg non-recursive))
    (call-interactively 'helm-do-ag)))

(global-set-key [?\C-c ?a] (quote helm-do-ag-recursive))

(setf inhibit-splash-screen t)
(setq initial-scratch-message "")
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default js-indent-level 4)
(setq-default helm-ag-always-set-extra-option t)
(setq-default helm-ag-insert-at-point (quote word))

(add-hook 'python-mode-hook
          (lambda ()
            (setq electric-indent-chars '(?\n))))

;; (with-eval-after-load 'flycheck
;;   (flycheck-add-mode 'javascript-eslint 'web-mode))

(defun my/use-eslint-from-node-modules ()
  (let ((root (locate-dominating-file
               (or (buffer-file-name) default-directory)
               (lambda (dir)
                 (let ((eslint (expand-file-name "node_modules/eslint/bin/eslint.js" dir)))
                  (and eslint (file-executable-p eslint)))))))
    (when root
      (let ((eslint (expand-file-name "node_modules/eslint/bin/eslint.js" root)))
        (setq-local flycheck-javascript-eslint-executable eslint)))))

;; (add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules)

(defun my/use-tslint-from-node-modules ()
  (let ((root (locate-dominating-file
               (or (buffer-file-name) default-directory)
               (lambda (dir)
                 (let ((tslint (expand-file-name "node_modules/tslint/bin/tslint" dir)))
                  (and tslint (file-executable-p tslint)))))))
    (when root
      (let ((tslint (expand-file-name "node_modules/tslint/bin/tslint" root)))
        (setq-local flycheck-typescript-tslint-executable tslint)))))

(add-hook 'flycheck-mode-hook #'my/use-tslint-from-node-modules)

(add-hook 'after-init-hook #'global-flycheck-mode)
;; make underscore part of word
(add-hook 'ruby-mode-hook #'(lambda () (modify-syntax-entry ?_ "w")))
(add-hook 'python-mode-hook #'(lambda () (modify-syntax-entry ?_ "w")))

(setq helm-always-two-windows t)
(setq
 neo-smart-open t
 neo-persist-show nil
 neo-window-width 30
)

;(global-yascroll-bar-mode t)
;(setq yascroll:delay-to-hide nil)

;(set-face-attribute 'yascroll:thumb-text-area nil :background "white")

;;  A hash table mapping from file names to stacks of vc-annotate calls
(defvar vc-annotate-call-stacks (make-hash-table :test 'equal))

;; Define a structure type to store the details needed to redisplay a revision
;; rev: the revision annotated
;; point: the cursor point when the annotation was first displayed (perhaps this
;;        could be improved to be the last point moved to on the annotation)
(require 'cl-lib)
(cl-defstruct annotation-details rev point)

;; The vc-annotate-mode-hook can't be used because it is run before
;; the vc-annotate-parent-* variables are set.
;;
;; So instead use a advise function for vc-annotate, called before
;; vc-annotate, which stores the arguments on the appropriate stack.
(defun record-annotation-call (file rev &optional display-mode buf move-point-to vc-bk)
  (message "Recording annotation: file %S rev %S" file rev)
  (let ((annotation-stack (gethash file vc-annotate-call-stacks)))
    (push (make-annotation-details :rev rev
                                   :point move-point-to)
          annotation-stack)
    (puthash file annotation-stack vc-annotate-call-stacks)))

(advice-add 'vc-annotate :before #'record-annotation-call)

(defun vc-annotate-previous-annotation ()
  "Go back to showing the annotation of the previous displayed annotation"
  (interactive)
  (when (not (equal major-mode 'vc-annotate-mode))
    (error "Can only be used in vc-annotate-mode"))
  (let ((annotation-stack (gethash vc-annotate-parent-file vc-annotate-call-stacks)))
       (when (< (length annotation-stack) 2)
         (error "No previous vc-annotate calls"))
       ;; The entry at the top of the stack is the current annotation.
       ;; So need to pop two entries to get the previous annotation.
       (let
           ((curr-annotation (pop annotation-stack))
            (prev-annotation (pop annotation-stack)))
         ;; Update the annotation-stack in the hash table after removing the entries.
         ;; The entry for the one we're returning to will be re-added by
         ;; the advise function for vc-annotate.
         (puthash vc-annotate-parent-file annotation-stack vc-annotate-call-stacks)

         (vc-annotate vc-annotate-parent-file
                      (annotation-details-rev prev-annotation)
                      vc-annotate-parent-display-mode
                      (current-buffer)
                      (annotation-details-point prev-annotation)
                      vc-annotate-backend))))

(add-to-list
 'save-some-buffers-action-alist
 '(?r
   (lambda (buf)
       (set-buffer buf)
       (revert-buffer :ignore-auto :noconfirm))
   "revert this buffer"))

(setq revert-without-query '(".*"))
(save-place-mode 1)

(defun my-tabbar-buffer-groups () ;; customize to show all normal files in one group
  "Returns the name of the tab group names the current buffer belongs to.
 There are two groups: Emacs buffers (those whose name starts with '*', plus
 dired buffers), and the rest.  This works at least with Emacs v24.2 using
 tabbar.el v1.7."
  (list (cond ((string-equal "*" (substring (buffer-name) 0 1)) "emacs")
              ((eq major-mode 'dired-mode) "emacs")
              (t "user"))))
(setq tabbar-buffer-groups-function 'my-tabbar-buffer-groups)
(global-set-key (kbd "s-}") 'tabbar-forward)
(global-set-key (kbd "s-{") 'tabbar-backward)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-mode nil)
 '(company-backends
   (quote
    (company-tide company-bbdb company-eclim company-semantic company-clang company-xcode company-cmake company-capf company-files
                  (company-dabbrev-code company-gtags company-etags company-keywords)
                  company-oddmuse company-dabbrev)))
 '(flycheck-flake8rc nil)
 '(flycheck-python-pycompile-executable "/usr/local/bin/python3")
 '(global-flycheck-mode t)
 '(helm-always-two-windows t)
 '(helm-buffers-truncate-lines nil)
 '(helm-ff-file-name-history-use-recentf nil)
 '(helm-ls-git-fuzzy-match t)
 '(helm-recentf-fuzzy-match t)
 '(minimap-always-recenter nil)
 '(minimap-automatically-delete-window nil)
 '(minimap-dedicated-window t)
 '(minimap-display-semantic-overlays nil)
 '(minimap-hide-fringes t)
 '(minimap-major-modes (quote (web-mode)))
 '(minimap-mode t)
 '(minimap-recenter-type (quote free))
 '(minimap-tag-only nil)
 '(minimap-update-delay 0)
 '(minimap-window-location (quote right))
 '(neo-auto-indent-point t)
 '(neo-force-change-root t)
 '(neo-theme (quote ascii))
 '(ns-confirm-quit nil)
 '(package-selected-packages
   (quote
    (dumb-jump add-node-modules-path golden-ratio-scroll-screen reveal-in-osx-finder whitespace-cleanup-mode string-inflection csv-mode)))
 '(tabbar-mode t nil (tabbar))
 '(tabbar-mwheel-mode t nil (tabbar))
 '(web-mode-enable-auto-indentation nil))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "White" :foreground "Black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 180 :width normal :foundry "nil" :family "Menlo")))))

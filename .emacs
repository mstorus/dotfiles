(setq debug-on-error t)

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
; MELPA packages: string-inflection, whitespace-cleanup-mode

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
   el-get
   ag
   dtrt-indent
   helm
   helm-ag
   helm-ls-git
   help-mode+
   neotree
   anzu
   company-mode
   flycheck
   web-mode
   yaml-mode
   yascroll
))
(el-get 'sync my:el-get-packages)

(helm-mode 0)
(global-anzu-mode +1)
(yascroll-bar-mode 1)
(setq column-number-mode t)
(add-hook 'after-init-hook 'global-company-mode)
(setq company-dabbrev-downcase nil)

(require 'string-inflection)

(add-hook 'ruby-mode-hook 'whitespace-cleanup-mode)
(add-hook 'js-mode-hook 'whitespace-cleanup-mode)
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

(setq css-indent-offset 2)
(setq web-mode-markup-indent-offset 2)
(setq web-mode-code-indent-offset 2)
(setq web-mode-css-indent-offset 2)
(setq web-mode-attr-indent-offset 2)

(add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(defadvice web-mode-highlight-part (around tweak-jsx activate)
  (if (equal web-mode-content-type "jsx")
      (let ((web-mode-enable-part-face nil))
        ad-do-it)
    ad-do-it))

(global-set-key [?\C-c ?f] (quote helm-ls-git-ls))
(global-set-key [?\C-c ?t] 'neotree-toggle)

(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)

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
(setq-default tab-width 2)
(setq-default js-indent-level 2)
(setq-default helm-ag-always-set-extra-option t)

(add-hook 'python-mode-hook
          (lambda ()
            (setq electric-indent-chars '(?\n))))

(setq flycheck-javascript-eslint-executable "eslint-project-relative")
(with-eval-after-load 'flycheck
  (flycheck-add-mode 'javascript-eslint 'web-mode))


(add-hook 'after-init-hook #'global-flycheck-mode)
;; make underscore part of word
(add-hook 'ruby-mode-hook #'(lambda () (modify-syntax-entry ?_ "w")))

(setq helm-always-two-windows t)
(setq
 neo-smart-open t
 neo-persist-show nil
 neo-window-width 40
)

(global-yascroll-bar-mode t)
(setq yascroll:delay-to-hide nil)

(set-face-attribute 'yascroll:thumb-text-area nil :background "white")

(add-to-list
 'save-some-buffers-action-alist
 '(?r
   (lambda (buf)
       (set-buffer buf)
       (revert-buffer :ignore-auto :noconfirm))
   "revert this buffer"))

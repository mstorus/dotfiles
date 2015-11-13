(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

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
   neotree
   anzu
   company-mode
   golden-ratio
))
(el-get 'sync my:el-get-packages)

(global-anzu-mode +1)
(setq column-number-mode t)
(golden-ratio-mode 1)

(setq web-mode-markup-indent-offset 2)
(setq web-mode-code-indent-offset 2)
(setq web-mode-css-indent-offset 2)

(add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(defadvice web-mode-highlight-part (around tweak-jsx activate)
  (if (equal web-mode-content-type "jsx")
      (let ((web-mode-enable-part-face nil))
        ad-do-it)
    ad-do-it))

(global-set-key [?\C-c ?f] (quote helm-ls-git-ls))
(global-set-key [?\C-c ?t] (quote neotree-find))

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
(setq-default helm-ag-always-set-extra-option t)

(add-hook 'python-mode-hook
          (lambda ()
            (setq electric-indent-chars '(?\n))))

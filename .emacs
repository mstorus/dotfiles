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
))
(el-get 'sync my:el-get-packages)

(global-anzu-mode +1)
(setq column-number-mode t)

(global-set-key [?\C-c ?f] (quote helm-ls-git-ls))
(global-set-key [?\C-c ?t] (quote neotree-find))

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

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

(add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes")
(el-get 'sync)

(global-set-key [?\C-c ?f] (quote helm-ls-git-ls))
(global-set-key [?\C-c ?t] (quote neotree-find))

(defun helm-do-ag-recursive (&optional non-recursive)
  "Like `helm-do-ag', but ags recursively by default."
  (interactive "P")
  (let* ((current-prefix-arg (not non-recursive))
         (helm-current-prefix-arg non-recursive))
    (call-interactively 'helm-do-ag)))

(global-set-key [?\C-c ?a] (quote helm-do-ag-recursive))

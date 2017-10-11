(setq debug-on-error t)

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
; MELPA packages: string-inflection, whitespace-cleanup-mode, react-snippets

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
(yas-global-mode 1)
(setq column-number-mode t)
(add-hook 'after-init-hook 'global-company-mode)
(setq company-dabbrev-downcase nil)
(setq company-dabbrev-ignore-case nil)
(tool-bar-mode nil)
(blink-cursor-mode nil)
(windmove-default-keybindings)

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

(add-to-list 'load-path "~/src/react-snippets.el")
(require 'react-snippets)

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
(setq recentf-max-menu-items 50)
(setq recentf-max-saved-items 50)
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

(with-eval-after-load 'flycheck
  (flycheck-add-mode 'javascript-eslint 'web-mode))

(defun my/use-eslint-from-node-modules ()
  (let ((root (locate-dominating-file
               (or (buffer-file-name) default-directory)
               (lambda (dir)
                 (let ((eslint (expand-file-name "node_modules/eslint/bin/eslint.js" dir)))
                  (and eslint (file-executable-p eslint)))))))
    (when root
      (let ((eslint (expand-file-name "node_modules/eslint/bin/eslint.js" root)))
        (setq-local flycheck-javascript-eslint-executable eslint)))))
(add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules)

(add-hook 'after-init-hook #'global-flycheck-mode)
;; make underscore part of word
(add-hook 'ruby-mode-hook #'(lambda () (modify-syntax-entry ?_ "w")))

(setq helm-always-two-windows t)
(setq
 neo-smart-open t
 neo-persist-show nil
 neo-window-width 30
)

(global-yascroll-bar-mode t)
(setq yascroll:delay-to-hide nil)

;;  vc-annotate-previous-revision
;;
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
;;  end of vc-annotate-previous-revision

(set-face-attribute 'yascroll:thumb-text-area nil :background "white")

(add-to-list
 'save-some-buffers-action-alist
 '(?r
   (lambda (buf)
       (set-buffer buf)
       (revert-buffer :ignore-auto :noconfirm))
   "revert this buffer"))

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

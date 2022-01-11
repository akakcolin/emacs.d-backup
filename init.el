;;;;
;; Packages
;;;;

;; Define package repositories
(require 'package)
  ;(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
;			   ("melpa" . "https://melpa.org/packages/")))

 ; (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t) ; Org-mode's repository
 ; (add-to-list 'package-archives '("popkit" . "http://elpa.popkit.org/packages/"))
  (setq package-archives '(("gnu"   . "https://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                           ("melpa" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
                           ("org"   . "https://mirrors.tuna.tsinghua.edu.cn/elpa/org/")

                           ))
  (add-to-list 'package-archives
           '("popkit" . "http://elpa.popkit.org/packages/"))

  (package-initialize)
;; set user name and email address
(setq user-full-name "Zenghui Liu"
      user-mail-address "akakcolin@163.com")

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(org-babel-load-file (expand-file-name "~/.emacs.d/conf/conf.org"))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(org2ctex ox-latex-subfigure company-maxima maxima gnuplot solaire-mode solidity-flycheck solidity-mode solarized-theme ivy-yasnippet ws-butler undo-tree smex smart-mode-line rainbow-mode pydoc ivy-bibtex helm-bibtex helm parsebib ov mustache magithub lispy jedi-direx jedi ivy-hydra hydra hy-mode ht git-messenger flx f esup elpy elfeed dashboard dash counsel-projectile swiper drag-stuff button-lock auctex avy auto-complete aggressive-indent org-plus-contrib org-ref ob-ipython slime command-log-mode buffer-move fill-column-indicator google-this pdf-tools tablist whitespace-cleanup-mode paren-face docker-compose-mode dockerfile-mode json-mode toml-mode yaml-mode ox-reveal ox-gfm toc-org htmlize org-bullets ob-restclient restclient expand-region company exunit elixir-mode rust-mode cider clojure-mode lsp-ivy flycheck yasnippet-snippets which-key use-package spaceline smartparens rainbow-delimiters projectile paredit neotree multiple-cursors magit lsp-ui exec-path-from-shell dracula-theme diminish counsel beacon ace-window)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background nil)))))
(put 'dired-find-alternate-file 'disabled nil)


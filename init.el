;;; hacking-emacs.pdf Emacs Prelude pragmaticemacs.net

(when (>= emacs-major-version 25)
  (require 'package)
  (setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
			   ("melpa" . "https://melpa.org/packages/")))
  (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t) ; Org-mode's repository
  (add-to-list 'package-archives
	       '("popkit" . "http://elpa.popkit.org/packages/"))
  ;(setq package-archives '(("gnu"   . "https://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
   ;                        ("melpa" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
  ;                         ("org"   . "https://mirrors.tuna.tsinghua.edu.cn/elpa/org/")
   ;                        ))
  (package-initialize))  ;; Uses mirrors from Tsinghua University

;; set user name and email address
(setq user-full-name "Zenghui Liu"
      user-mail-address "akakcolin@163.com")
;(use-package company-tabnine :ensure t)
(add-to-list 'default-frame-alist '(fullscreen . maximized))
;; turn off various UI elements such as menu toolbar
(setq inhibit-x-resources t) ;; fix cursor face using Zenburn and daemon mode
(tooltip-mode 1)
(tool-bar-mode -1)
(line-number-mode 1)
(menu-bar-mode -1)
(setq visible-bell 0)
(setq ring-bell-function 'ignore)
(blink-cursor-mode 0)
(global-hl-line-mode 1)
(global-visual-line-mode 1)
(line-number-mode 1)
(flyspell-prog-mode)
(global-auto-revert-mode t)

(setq initial-major-mode 'emacs-lisp-mode) ;; make scratch buffer emacs-lisp-mode
(add-hook 'emacs-lisp-mode-hook
          (lambda()
            (setq mode-name "Elisp")))
;; startup screen customization
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)

(setq enable-recursive-minibuffers t)
;; turn on view-mode for readonly buffers
;; (setq view-read-only t)

;; Use new bytecodes from Emacs 24.4
(setq byte-compile--use-old-handlers nil)
(setq ad-redefinition-action 'accept)
(load-file "~/.emacs.d/geiser/build/elisp/geiser-load.el")
(load-file "~/.emacs.d/cdlatex.el")
(setq-default TeX-master nil)
(mapc (lambda (mode)
	(add-hook 'LaTeX-mode-hook mode))
      (list 'turn-on-cdlatex 
	    'reftex-mode     
	    'outline-minor-mode
	    'auto-fill-mode
	    'flyspell-mode
	    'hide-body 0
))

(add-hook 'LaTeX-mode-hook 'flyspell-mode)

(add-to-list 'load-path "~/.emacs.d/elisp/")
(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/")
(delete-selection-mode 1) ;; overwrite highlighted texts
(global-set-key (kbd "C-x C-b") 'ibuffer)
(setq visible-bell t)
;; (global-set-key (kbd "C-+") 'text-scale-increase)
;; (global-set-key (kbd "C--") 'text-scale-decrease)
(require 'company-tabnine)
(require 'ggtags)

(add-hook 'c-mode-hook 'ggtags-mode)
(add-hook 'c++-mode-hook 'ggtags-mode)
(define-key global-map (kbd "C-c f") 'ggtags-find-file)
(define-key global-map (kbd "M-.") 'ggtags-find-tag-dwim)
(setq gtags-suggested-key-mapping t)
(setq ggtags-global-ignore-case t)
(setq ggtags-sort-by-nearness t)

;; Turn off the bell on Mac OS X
(defun nil-bell ())
(setq ring-bell-function 'nil-bell)

(require 'highlight-parentheses)
(define-globalized-minor-mode global-highlight-parentheses-mode
  highlight-parentheses-mode
  (lambda ()
    (highlight-parentheses-mode t)))
(global-highlight-parentheses-mode t)

;; gnuplot
(defvar gnuplot-program "/usr/local/bin/gnuplot")
(defvar gnuplot-flags "-persist -pointsize 2")

(eval-after-load 'gnuplot-mode
  '(add-hook 'gnuplot-mode-hook
	     (lambda ()
	       (flyspell-prog-mode)
	       (add-hook 'before-save-hook
			 'whitespace-cleanup nil t))))


;; newLISP mode
(add-to-list 'load-path (expand-file-name "~/.emacs.d/newlisp-mode/"))
(load-file "~/.emacs.d/newlisp-mode/newlisp.el")
;;(add-to-list 'auto-mode-alist '("\\.lsp$" . newlisp-mode
;;                             ))

;; latex magic
;;(require 'magic-latex-buffer)
;;(setq magic-latex-enable-block-highlight nil
;;     magic-latex-enable-suscript        t
;;      magic-latex-enable-pretty-symbols  t
;;      magic-latex-enable-block-align     nil
;;      magic-latex-enable-inline-image    nil
;;     magic-latex-enable-minibuffer-echo nil)

;;(add-hook 'latex-mode-hook 'magic-latex-buffer)
(defun insert-timeofday ()
   (interactive "*")
   (insert (format-time-string "---------------- %a, %d %b %y: %I:%M%p")))

(require 'rtags)
(require 'company-rtags)
(require 'company-c-headers)
(setq rtags-completions-enabled t)
(eval-after-load 'company
  '(add-to-list
    'company-backends 'company-rtags))
(setq rtags-autostart-diagnostics t)
(rtags-enable-standard-keybindings)

(setq auto-mode-alist
      (append
       '(("\\.cpp$"    . c++-mode)
         ("\\.hin$"    . c++-mode)
         ("\\.cin$"    . c++-mode)
         ("\\.inl$"    . c++-mode)
         ("\\.rdc$"    . c++-mode)
         ("\\.h$"    . c++-mode)
         ("\\.c$"   . c++-mode)
         ("\\.src$"   . fortran-mode)
         ("\\.cc$"   . c++-mode)
         ("\\.cu$"   . c++-mode)
         ("\\.c8$"   . c++-mode)
         ("\\.tex$"   . latex-mode)
         ("\\.stex$"  . latex-mode)
	 ("\\.pamphlet$" . latex-mode)
	 ("Makefile.*$" . makefile-mode)
         ("\\.txt$" . indented-text-mode)
         ("\\.emacs$" . emacs-lisp-mode)
	 ("\\.cl$" . emacs-lisp-mode)
	 ("\\.sls$" . scheme-mode)
	 ("\\.ss$" . scheme-mode)
	 ("\\.ss$" . scheme-mode)
	 ("\\.nl$" . newlisp-mode)
	 ("\\.lsp$" . newlisp-mode)
         ("\\.gen$" . gen-mode)
         ("\\.ms$" . fundamental-mode)
         ("\\.m$" . objc-mode)
         ("\\.mm$" . objc-mode)
	 ("\\.plt$" . gnuplot-mode)
         ) auto-mode-alist))

;;;
;; Latex Mode
;;(require 'mmm-mode)
;;(require 'mmm-vars)
;;(setq  mmm-global-mode 'maybe)
;;(setq mmm-submode-decoration-level 0）
;;
;; set up an mmm group for fancy html editing
;;(mmm-add-group
;; 'fancy-latex
;; '((latex-c-tagged
;;   :submode c++-mode
;;    :face mmm-code-submode-face
;;    :front "begin{chunk}$"
;;    :back "end{chunk}$")))
;;(add-to-list 'mmm-mode-ext-classes-alist '(nil ".tex" latex-c-tagged))

(defun c-lineup-arglist-tabs-only (ignored)
  "Line up argument lists by tabs, not spaces"
  (let* ((anchor (c-langelem-pos c-syntactic-element))
         (column (c-langelem-2nd-pos c-syntactic-element))
         (offset (- (1+ column) anchor))
         (steps (floor offset c-basic-offset)))
    (* (max steps 1)
       c-basic-offset)))

(add-hook 'c-mode-common-hook
          (lambda ()
            ;; Add kernel style
            (c-add-style
             "linux-tabs-only"
             '("linux" (c-offsets-alist
                        (arglist-cont-nonempty
                         c-lineup-gcc-asm-reg
                         c-lineup-arglist-tabs-only))))))

(add-hook 'c-mode-hook
          (lambda ()
            (let ((filename (buffer-file-name)))
              ;; Enable kernel mode for the appropriate files
              (when (and filename
                         (string-match (expand-file-name "~/src/linux-trees")
                                       filename))
                (setq indent-tabs-mode t)
                (setq show-trailing-whitespace t)
                (c-set-style "linux-tabs-only")))))


;; DocView / Image Viewer
(eval-after-load 'doc-view
  '(progn
     (define-key doc-view-mode-map (kbd "C-v") 'doc-view-scroll-up-or-next-page)
     (define-key doc-view-mode-map (kbd "M-v") 'doc-view-scroll-down-or-previous-page)
     (define-key doc-view-mode-map (kbd "C-e") 'image-eol)
     (define-key doc-view-mode-map (kbd "C-a") 'image-bol)
     (define-key doc-view-mode-map (kbd "C-c t") 'doc-view-open-text)))

;; C++ indentation style
(defconst casey-big-fun-c-style
  '((c-electric-pound-behavior   . nil)
    (c-tab-always-indent         . t)
    (c-comment-only-line-offset  . 0)
    (c-hanging-braces-alist      . ((class-open)
                                    (class-close)
                                    (defun-open)
                                    (defun-close)
                                    (inline-open)
                                    (inline-close)
                                    (brace-list-open)
                                    (brace-list-close)
                                    (brace-list-intro)
                                    (brace-list-entry)
                                    (block-open)
                                    (block-close)
                                    (substatement-open)
                                    (statement-case-open)
                                    (class-open)))
    (c-hanging-colons-alist      . ((inher-intro)
                                    (case-label)
                                    (label)
                                    (access-label)
                                    (access-key)
                                    (member-init-intro)))
    (c-cleanup-list              . (scope-operator
                                    list-close-comma
                                    defun-close-semi))
    (c-offsets-alist             . ((arglist-close         .  c-lineup-arglist)
                                    (label                 . -4)
                                    (access-label          . -4)
                                    (substatement-open     .  0)
                                    (statement-case-intro  .  4)
                                    (statement-block-intro .  c-lineup-for)
                                    (case-label            .  4)
                                    (block-open            .  0)
                                    (inline-open           .  0)
                                    (topmost-intro-cont    .  0)
                                    (knr-argdecl-intro     . -4)
                                    (brace-list-open       .  0)
                                    (brace-list-intro      .  4)))
    (c-echo-syntactic-information-p . t))
    "Casey's Big Fun C++ Style")



; CC++ mode handling

(setq company-backends (delete 'company-semantic company-backends))
(define-key c-mode-map  [(tab)] 'company-complete)
(define-key c++-mode-map  [(tab)] 'company-complete)

(add-to-list 'company-backends 'company-c-headers)

(defun casey-big-fun-c-hook ()
  ; Set my style for the current buffer
  (c-add-style "BigFun" casey-big-fun-c-style t)
  
  ; 4-space tabs
  (setq tab-width 4
        indent-tabs-mode nil)

  ; Additional style stuff
  (c-set-offset 'member-init-intro '++)

  ; No hungry backspace
  (c-toggle-auto-hungry-state -1)

  ; Newline indents, semi-colon doesn't
  (define-key c++-mode-map "\C-m" 'newline-and-indent)
  (setq c-hanging-semi&comma-criteria '((lambda () 'stop)))

  ; Handle super-tabbify (TAB completes, shift-TAB actually tabs)
  (setq dabbrev-case-replace t)
  (setq dabbrev-case-fold-search t)
  (setq dabbrev-upcase-means-case-search t)

  ; Abbrevation expansion
  (abbrev-mode 1)
  
  (defun casey-header-format ()
     "Format the given file as a header file."
     (interactive)
     (setq BaseFileName (file-name-sans-extension (file-name-nondirectory buffer-file-name)))
     (insert "/*\n")
     (insert "* ***** BEGIN GPL LICENSE BLOCK *****\n")
     (insert "*\n")
     (insert "* This program is free software; you can redistribute it and/or\n")
     (insert "* modify it under the terms of the GNU General Public License\n")
     (insert "* as published by the Free Software Foundation; either version 2\n")
     (insert "* of the License, or (at your option) any later version.\n")
     (insert "*\n")
     (insert "* This program is distributed in the hope that it will be useful,\n")
     (insert "* but WITHOUT ANY WARRANTY; without even the implied warranty of")
     (insert "* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the\n")
     (insert "* GNU General Public License for more details.\n")
     (insert "*\n")
     (insert "* You should have received a copy of the GNU General Public License\n")
     (insert "* along with this program; if not, write to the Free Software Foundation,\n")
     (insert "* Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.\n")
     (insert "*\n")
     (insert "*\n")
     (insert "* ***** END GPL LICENSE BLOCK *****\n")
     (insert "* */\n")
     (insert "\n")
     
     (insert "/**************************************************************************\n")
     (insert "                        " (file-name-nondirectory buffer-file-name) "  -  description                            \n")
     (insert "                        ----------------------                                \n")
     (insert "   begin                : "(format-time-string "%Y-%m-%d")"     \n")
     (insert "   copyright            : (C) 2019 by "user-full-name "                          \n")
     (insert "   email                : "user-mail-address"                                \n")
     
     (insert "**************************************************************************/\n")
     (insert "\n")
     (insert "/** \brief " (file-name-nondirectory buffer-file-name) "\n")
     (insert " * \ingroup \n")
     (insert " * \n")
     (insert " */\n\n")
     
     (insert "#ifndef ")     
     (push-mark)
     (insert BaseFileName)
     (upcase-region (mark) (point))
     (pop-mark)
     (insert "_H\n")
     (insert "#define ")
     (push-mark)
     (insert BaseFileName)
     (upcase-region (mark) (point))
     (pop-mark)
     (insert "_H\n")
     (insert "\n\n")
     (insert "#endif")
  )

  (defun casey-source-format ()
     "Format the given file as a source file."
     (interactive)
     (setq BaseFileName (file-name-sans-extension (file-name-nondirectory buffer-file-name)))
     (insert "/*\n")
     (insert "* ***** BEGIN GPL LICENSE BLOCK *****\n")
     (insert "*\n")
     (insert "* This program is free software; you can redistribute it and/or\n")
     (insert "* modify it under the terms of the GNU General Public License\n")
     (insert "* as published by the Free Software Foundation; either version 2\n")
     (insert "* of the License, or (at your option) any later version.\n")
     (insert "*\n")
     (insert "* This program is distributed in the hope that it will be useful,\n")
     (insert "* but WITHOUT ANY WARRANTY; without even the implied warranty of")
     (insert "* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the\n")
     (insert "* GNU General Public License for more details.\n")
     (insert "*\n")
     (insert "* You should have received a copy of the GNU General Public License\n")
     (insert "* along with this program; if not, write to the Free Software Foundation,\n")
     (insert "* Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.\n")
     (insert "*\n")
     (insert "*\n")
     (insert "* ***** END GPL LICENSE BLOCK *****\n")
     (insert "* */\n")
     (insert "\n")
     
     (insert "/**************************************************************************\n")
     (insert "                        " (file-name-nondirectory buffer-file-name) "  -  description                            \n")
     (insert "                        ----------------------                                \n")
     (insert "   begin                : "(format-time-string "%Y-%m-%d")"     \n")
     (insert "   copyright            : (C) 2019 by "user-full-name "                          \n")
     (insert "   email                : "user-mail-address"                                \n")
     
     (insert "**************************************************************************/\n")
     (insert "\n")
     (insert "/** \brief " (file-name-nondirectory buffer-file-name) "\n")
     (insert " * \ingroup \n")
     (insert " * \n")
     (insert " */\n")
  )

  (cond ((file-exists-p buffer-file-name) t)
        ((string-match "[.]hin" buffer-file-name) (casey-source-format))
        ((string-match "[.]cin" buffer-file-name) (casey-source-format))
        ((string-match "[.]h" buffer-file-name) (casey-header-format))
        ((string-match "[.]hpp" buffer-file-name) (casey-header-format))
        ((string-match "[.]cpp" buffer-file-name) (casey-source-format))
        ((string-match "[.]c" buffer-file-name) (casey-source-format)))

   (defun casey-find-corresponding-file ()
    "Find the file that corresponds to this one."
    (interactive)
    (setq CorrespondingFileName nil)
    (setq BaseFileName (file-name-sans-extension buffer-file-name))
    (if (string-match "\\.c" buffer-file-name)
       (setq CorrespondingFileName (concat BaseFileName ".h")))
    (if (string-match "\\.h" buffer-file-name)
       (if (file-exists-p (concat BaseFileName ".c")) (setq CorrespondingFileName (concat BaseFileName ".c"))
	   (setq CorrespondingFileName (concat BaseFileName ".cpp"))))
    (if (string-match "\\.hin" buffer-file-name)
       (setq CorrespondingFileName (concat BaseFileName ".cin")))
    (if (string-match "\\.cin" buffer-file-name)
       (setq CorrespondingFileName (concat BaseFileName ".hin")))
    (if (string-match "\\.cpp" buffer-file-name)
       (setq CorrespondingFileName (concat BaseFileName ".h")))
    (if CorrespondingFileName (find-file CorrespondingFileName)
       (error "Unable to find a corresponding file")))
  (defun casey-find-corresponding-file-other-window ()
    "Find the file that corresponds to this one."
    (interactive)
    (find-file-other-window buffer-file-name)
    (casey-find-corresponding-file)
    (other-window -1))
  (define-key c++-mode-map [f12] 'casey-find-corresponding-file)
  (define-key c++-mode-map [M-f12] 'casey-find-corresponding-file-other-window)

  ; Alternate bindings for F-keyless setups (ie MacOS X terminal)
  (define-key c++-mode-map "\ec" 'casey-find-corresponding-file)
  (define-key c++-mode-map "\eC" 'casey-find-corresponding-file-other-window)

  (define-key c++-mode-map "\es" 'casey-save-buffer)

  (define-key c++-mode-map "\t" 'dabbrev-expand)
  (define-key c++-mode-map [S-tab] 'indent-for-tab-command)
  ;;(define-key c++-mode-map "\C-y" 'indent-for-tab-command)
  (define-key c++-mode-map [C-tab] 'indent-region)
  (define-key c++-mode-map "	" 'indent-region)

  (define-key c++-mode-map "\ej" 'imenu)

  (define-key c++-mode-map "\e." 'c-fill-paragraph)

  (define-key c++-mode-map "\e/" 'c-mark-function)

  (define-key c++-mode-map "\e " 'set-mark-command)
  (define-key c++-mode-map "\eq" 'append-as-kill)
  (define-key c++-mode-map "\ea" 'yank)
  (define-key c++-mode-map "\ez" 'kill-region)

  ; devenv.com error parsing
  (add-to-list 'compilation-error-regexp-alist 'casey-devenv)
  (add-to-list 'compilation-error-regexp-alist-alist '(casey-devenv
   "*\\([0-9]+>\\)?\\(\\(?:[a-zA-Z]:\\)?[^:(\t\n]+\\)(\\([0-9]+\\)) : \\(?:see declaration\\|\\(?:warnin\\(g\\)\\|[a-z ]+\\) C[0-9]+:\\)"
    2 3 nil (4)))
)


(defun casey-replace-string (FromString ToString)
  "Replace a string without moving point."
  (interactive "sReplace: \nsReplace: %s  With: ")
  (save-excursion
    (replace-string FromString ToString)
  ))
(define-key global-map [f8] 'casey-replace-string)

(add-hook 'c-mode-common-hook 'casey-big-fun-c-hook)


(defun casey-save-buffer ()
  "Save the buffer after untabifying it."
  (interactive)
  (save-excursion
    (save-restriction
      (widen)
      (untabify (point-min) (point-max))))
  (save-buffer))


; Navigation
(defun previous-blank-line ()
  "Moves to the previous line containing nothing but whitespace."
  (interactive)
  (search-backward-regexp "^[ \t]*\n")
)

(defun next-blank-line ()
  "Moves to the next line containing nothing but whitespace."
  (interactive)
  (forward-line)
  (search-forward-regexp "^[ \t]*\n")
  (forward-line -1)
)


(define-key global-map [C-right] 'forward-word)
(define-key global-map [C-left] 'backward-word)
(define-key global-map [C-up] 'previous-blank-line)
(define-key global-map [C-down] 'next-blank-line)
(define-key global-map [home] 'beginning-of-line)
(define-key global-map [end] 'end-of-line)
(define-key global-map [pgup] 'forward-page)
(define-key global-map [pgdown] 'backward-page)
(define-key global-map [C-next] 'scroll-other-window)
(define-key global-map [C-prior] 'scroll-other-window-down)


; ALT-alternatives
(defadvice set-mark-command (after no-bloody-t-m-m activate)
  "Prevent consecutive marks activating bloody `transient-mark-mode'."
  (if transient-mark-mode (setq transient-mark-mode nil)))


(defadvice mouse-set-region-1 (after no-bloody-t-m-m activate)
  "Prevent mouse commands activating bloody `transient-mark-mode'."
  (if transient-mark-mode (setq transient-mark-mode nil))) 

(defun append-as-kill ()
  "Performs copy-region-as-kill as an append."
  (interactive)
  (append-next-kill) 
  (copy-region-as-kill (mark) (point))
  )


(define-key global-map "\e " 'set-mark-command)
(define-key global-map "\eq" 'append-as-kill)
(define-key global-map "\ea" 'yank)

(define-key global-map [M-up] 'previous-blank-line)
(define-key global-map [M-down] 'next-blank-line)
(define-key global-map [M-right] 'forward-word)
(define-key global-map [M-left] 'backward-word)

(define-key global-map "\e:" 'View-back-to-mark)
(define-key global-map "\e;" 'exchange-point-and-mark)

(define-key global-map [f9] 'first-error)
(define-key global-map [f10] 'previous-error)
(define-key global-map [f11] 'next-error)

(define-key global-map "\en" 'next-error)
(define-key global-map "\eN" 'previous-error)

(define-key global-map "\eg" 'goto-line)
(define-key global-map "\ej" 'imenu)

; Editting
(define-key global-map "" 'copy-region-as-kill)
(define-key global-map "" 'yank)
(define-key global-map "" 'nil)
(define-key global-map "" 'rotate-yank-pointer)
(define-key global-map "\eu" 'undo)
(define-key global-map "\e6" 'upcase-word)
(define-key global-map "\e^" 'captilize-word)
(define-key global-map "\e." 'fill-paragraph)



(defun casey-replace-in-region (old-word new-word)
  "Perform a replace-string in the current region."
  (interactive "sReplace: \nsReplace: %s  With: ")
  (save-excursion (save-restriction
		    (narrow-to-region (mark) (point))
		    (beginning-of-buffer)
		    (replace-string old-word new-word)
		    ))
  )
; \377 is alt-backspace
(define-key global-map "\377" 'backward-kill-word)
(define-key global-map [M-delete] 'kill-word)

; Buffers
(define-key global-map "\er" 'revert-buffer)
(define-key global-map "\ek" 'kill-this-buffer)
(define-key global-map "\es" 'save-buffer)


; Compilation
(setq compilation-context-lines 0)
(setq compilation-error-regexp-alist
    (cons '("^\\([0-9]+>\\)?\\(\\(?:[a-zA-Z]:\\)?[^:(\t\n]+\\)(\\([0-9]+\\)) : \\(?:fatal error\\|warnin\\(g\\)\\) C[0-9]+:" 2 3 nil (4))
	  compilation-error-regexp-alist))


(defun find-project-directory-recursive ()
  "Recursively search for a makefile."
  (interactive)
  (if (file-exists-p casey-makescript) t
      (cd "../")
      (find-project-directory-recursive)))

(defun lock-compilation-directory ()
  "The compilation process should NOT hunt for a makefile"
  (interactive)
  (setq compilation-directory-locked t)
  (message "Compilation directory is locked."))

(defun unlock-compilation-directory ()
  "The compilation process SHOULD hunt for a makefile"
  (interactive)
  (setq compilation-directory-locked nil)
  (message "Compilation directory is roaming."))

(defun find-project-directory ()
  "Find the project directory."
  (interactive)
  (setq find-project-from-directory default-directory)
  (switch-to-buffer-other-window "*compilation*")
  (if compilation-directory-locked (cd last-compilation-directory)
  (cd find-project-from-directory)
  (find-project-directory-recursive)
  (setq last-compilation-directory default-directory)))

(defun make-without-asking ()
  "Make the current build."
  (interactive)
  (if (find-project-directory) (compile casey-makescript))
  (other-window 1))
(define-key global-map "\em" 'make-without-asking)


; Commands
(set-variable 'grep-command "grep -irHn ")

(setq scroll-step 3)

; Clock
(display-time)

; Startup windowing
(setq next-line-add-newlines nil)
(setq-default truncate-lines t)
(setq truncate-partial-width-windows nil)
(split-window-horizontally)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-save-default nil)
 '(auto-save-interval 0)
 '(auto-save-list-file-prefix nil)
 '(auto-save-timeout 0)
 '(auto-show-mode t t)
 '(custom-enabled-themes (quote (tsdh-dark)))
 '(delete-auto-save-files nil)
 '(delete-old-versions (quote other))
 '(display-battery-mode t)
 '(imenu-auto-rescan t)
 '(imenu-auto-rescan-maxout 500000)
 '(kept-new-versions 5)
 '(kept-old-versions 5)
 '(make-backup-file-name-function (quote ignore))
 '(make-backup-files nil)
 '(mouse-wheel-follow-mouse nil)
 '(mouse-wheel-progressive-speed nil)
 '(mouse-wheel-scroll-amount (quote (15)))
 '(package-selected-packages
   (quote
    (company company-lsp company-math company-tabnine gnuplot-mode gnuplot newlisp-mode ## paredit-everywhere julia-shell julia-repl julia-mode org-pdfview pdf-tools paredit geiser autopair highlight-parentheses mmm-mode magic-latex-buffer company-c-headers slime zenburn-theme which-key volatile-highlights use-package undo-tree swiper sublime-themes smooth-scroll smex smartscan smartparens smart-mode-line rtags phi-search-mc mc-extras magit hungry-delete flycheck flx expand-region exec-path-from-shell elpy dired-narrow crux company-ghc cmake-ide cl-format bm beacon avy-zap auto-complete aggressive-indent ace-window)))
 '(version-control nil))


;; diff-hl - highlight changes/diffs
;; https://github.com/dgutov/diff-hl
;;(use-package diff-hl
;;  :config
;;  (global-diff-hl-mode))


(autoload 'paredit-mode "paredit"
  "Minor mode for pseudo-structurally editing Lisp code."
  t)

(define-key global-map "\t" 'dabbrev-expand)
(define-key global-map [S-tab] 'indent-for-tab-command)
(define-key global-map [backtab] 'indent-for-tab-command)
;;(define-key global-map "\C-y" 'indent-for-tab-command)
(define-key global-map [C-tab] 'indent-region)
(define-key global-map "	" 'indent-region)

;; use company-mode in all buffers
(add-hook 'after-init-hook 'global-company-mode)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background nil)))))

(add-hook 'ess-mode-hook 'hs-minor-mode) 

(eval-after-load 'hideshow 
'(progn 
    (global-set-key (kbd "C-+") 'hs-toggle-hiding))) 
;;;; Things that might make life easier:

;; Make Emacs' "speedbar" recognize newlisp files
(eval-after-load "speedbar" '(speedbar-add-supported-extension ".lsp"))

;; Another way to use C-x C-e to eval stuff and doesn't jump to next function
(define-key newlisp-mode-map [(control x) (control e)] 'newlisp-evaluate-prev-sexp)

(add-to-list 'company-backends #'company-tabnine)

;;  tabnine
;; Trigger completion immediately.
(setq company-idle-delay 0)

;; Number the candidates (use M-1, M-2 etc to select completions).
(setq company-show-numbers t)

;; Use the tab-and-go frontend.
;; Allows TAB to select and complete at the same time.
(company-tng-configure-default)
(setq company-frontends
      '(company-tng-frontend
        company-pseudo-tooltip-frontend
        company-echo-metadata-frontend))

(load (expand-file-name "~/quicklisp/slime-helper.el"))
;;(setq inferior-lisp-program "/Volumes/Merlin/ccl/dx86cl64")

;;(setq inferior-lisp-program "/Volumes/Merlin/ccl/dx86cl64")
;;(setq inferior-lisp-program "/usr/local/bin/sbcl")
(setq inferior-lisp-program "/usr/local/bin/newlisp")
;;(setq inferior-lisp-program "/usr/local/bin/ecl")
(setq slime-contribs '(slime-fancy))
;;(put 'upcase-region 'disabled nil)
(add-to-list 'exec-path "/usr/local/bin")
(setq scheme-program-name "/usr/local/bin/scheme")
(setq geiser-chez-binary "/usr/local/bin/scheme")
(setq geiser-active-implementations '(chez))

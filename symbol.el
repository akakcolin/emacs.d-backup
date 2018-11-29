;; symbol.el: Allows easy insertion of Greek letters and other symbols in Emacs,
;; following the input method used by TeXmacs.
;;
;; Version: 0.3
;;
;; Authors:
;;  Chris Gilbreth (cngilbreth@gmail.com), 2010-2012
;;
;; Usage: 
;;  (1) Load into Emacs via e.g. (load "~/emacs/symbol.el") in your .emacs file
;;  (2) Type a character and use the backtick ` key to cycle through associated
;;      symbols:
;;       a <backtick> --> α <backtick> --> a
;;      (C-q ` will insert an actual backtick character)
;;  (3) If you like, customize the key used for this below.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keybindings                                                                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar symbol-hotkey (kbd "`") 
  "Hot key for cycle-symbols")
;; Note: Use C-q ` to insert an actual backquote

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Supporting code for keybindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key symbol-hotkey 'cycle-symbols)
(define-key isearch-mode-map symbol-hotkey 'cycle-symbols-isearch)

;; Fortran mode seems to have some keybinding interrupting things
(add-hook 'fortran-mode-hook
          '(lambda () (local-unset-key symbol-hotkey)))
(add-hook 'f90-mode-hook
          '(lambda () (local-unset-key symbol-hotkey)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; List of symbols, names and and ASCII shortcuts                             ;;
;; (Perhaps we should load this from a file)                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq symbols '(
  ;; Greek capital letters
; ("Alpha"     "A"     "Α" )
; ("Beta"      "B"     "Β" )
  ("Gamma"     "G"     "Γ" )
  ("Delta"     "D"     "Δ" )
; ("Epsilon"   "E"     "Ε" )
; ("Zeta"      "Z"     "Ζ" )
; ("Eta"       "H"     "Η" )
  ("Theta"     "J"     "Θ" )
;  ("Iota"      "I"     "Ι" )
; ("Kappa"     "K"     "Κ" )
  ("Lambda"    "L"     "Λ" )
; ("Mu"        "M"     "Μ" )
; ("Nu"        "N"     "Ν" )
  ("Xi"        "X"     "Ξ" )
; ("Omicron"   "O"     "Ο" )
  ("Pi"        "P"     "Π" )
; ("Rho"       "R"     "Ρ" )
  ("Sigma"     "S"     "Σ" )
; ("Tau"       "T"     "Τ" )
; ("Upsilon"   "U"     "Υ" )
  ("Phi"       "F"     "Φ" )
; ("Chi"       "Q"     "Χ" )
  ("Psi"       "Y"     "Ψ" )
  ("Omega"     "W"     "Ω" )
  ;; Greek lowercase letters
  ("alpha"     "a"     "α" )
  ("beta"      "b"     "β" )
  ("gamma"     "g"     "γ" )
  ("delta"     "d"     "δ" )
  ("varepsilon" "e"    "ϵ" )
  ("epsilon"   "e"     "ε" )
  ("zeta"      "z"     "ζ" )
  ("eta"       "h"     "η" )
  ("theta"     "j"     "θ" )
  ("vartheta"  "j"     "ϑ" )
  ("iota"      "i"     "ι" )
  ("kappa"     "k"     "κ" )
  ("lambda"    "l"     "λ" )
  ("mu"        "m"     "μ" )
  ("nu"        "n"     "ν" )
  ("xi"        "x"     "ξ" )
  ("omicron"   "o"     "ο" )
  ("pi"        "p"     "π" )
  ("varpi"     "p"     "ϖ" ) ; variant of pi symbol
  ("rho"       "r"     "ρ" )
  ("sigma"     "s"     "σ" )
  ("varsigma"  "s"     "ς" ) ; "final sigma"
  ("tau"       "t"     "τ" )
  ("upsilon"   "u"     "υ" )
  ("varphi"    "f"     "φ" )
  ("phi"       "f"     "ϕ" )
  ("chi"       "q"     "χ" )
  ("psi"       "y"     "ψ" )
  ("omega"     "w"     "ω" )
  ("upsih"     "u"     "ϒ" )
  ;; Other letters
  ("hbar"      "h"     "ℏ")
  ("fitalic"   "f"     "ƒ" )
  ;; Mathematical symbols
  ("elipses"   "..."   "…" )
  ("wedge"     "&"     "∧" )
  ("vee"       "|"     "∨" )
  ("forall"    "A"     "∀" )
  ("exist"     "E"     "∃" )
  ("notexist"  "E/"    "∄" )
  ("not"       "!"     "¬" )
  ("empty"     "0"     "∅" )
  ("partial"   "d"     "∂" )
  ("grad"      "D"     "∇" )
  ("sum"       "+"     "∑" )
  ("integral"  "+"     "∫" )
  ("prod"      "*"     "∏" )
  ("lesseq"    "<="    "≤" )
  ("greateq"   ">="    "≥" )
  ("element"   "<"     "∈" )
  ("subset"    "<"     "⊂" )
  ("notsubset" "</"    "⊄" )
  ("notelem"   "</"    "∉" )
  ("lelement"  ">"     "∋" )
  ("notlelem"  ">/"    "∌" )
  ("plusminus" "+-"    "±" )
  ("minusplus" "-+"    "∓" )
  ("noteq"     "=/"    "≠" )
  ("equiv"     "="     "≡" )
  ("propto"    "="     "∝" )
  ("approx"    "~~"    "≈" )
  ("approxeq"  "~="    "≃" )
  ("approxlt"  "<~"    "≲" )
  ("approxgt"  ">~"    "≳" )
  ("divsign"   "/."    "÷" )
  ("timesx"    "*"     "×" )
  ("cdot"      "."     "⋅" )
  ("sqroot"    "-/"    "√" )
  ("infty"     "@@"    "∞" )
  ("aleph"     "A"     "ℵ" )
  ("prime"     "'"     "′" )
  ("prime2"    "''"    "″" )
  ("prime3"    "'''"   "‴" )
  ("therefore" ":"     "∴" )
  ("langle"    "<"     "⟨" )
  ("rangle"    ">"     "⟩" )
  ("lceil"     "["     "⌈" )
  ("rceil"     "]"     "⌉" )
  ("lfloor"    "["     "⌊" )
  ("rfloor"    "]"     "⌋" )
  ("oplus"     "@+"    "⊕" )
  ("otimes"    "@x"    "⊗" )
  ("union"     "|"     "∪" )
  ("intersection" "&"  "∩" )  
  ;; Arrows
  ("rarrow"    "->"    "→" )
  ("darrow"    "->"    "↓" )
  ("larrow"    "->"    "←" )
  ("uarrow"    "->"    "↑" )
  ("bolt"      "!"     "↯" )
  ;; Superscript symbols
  ("sup0"      "^0"   "⁰" )
  ("sup1"      "^1"   "¹" )
  ("sup2"      "^2"   "²" )
  ("sup3"      "^3"   "³" )
  ("sup4"      "^4"   "⁴" )
  ("sup5"      "^5"   "⁵" )
  ("sup6"      "^6"   "⁶" )
  ("sup7"      "^7"   "⁷" )
  ("sup8"      "^8"   "⁸" )
  ("sup9"      "^9"   "⁹" )
  ("supplus"   "^+"   "⁺" )
  ("supminus"  "^-"   "⁻" )
  ("supeq"     "^="   "⁼" )
  ("suplparen" "^("   "⁽" )
  ("suprparen" "^)"   "⁾" )
  ("supi"      "^i"   "ⁱ" )
  ("supn"      "^n"   "ⁿ" )
  ("dagger"    "^+"   "†" )
  ;; Superscript Greek
  ("supalpha"  "^α"   "ᵅ" )
  ("supbeta"   "^β"   "ᵝ" )
  ("supgamma"  "^γ"   "ᵞ" )
  ("supdelta"  "^δ"   "ᵟ" )
  ("supeps"    "^ε"   "ᵋ" )
  ("suptheta"  "^θ"   "ᶿ" )
  ("supiota"   "^ι"   "ᶥ" )
  ("supPhi"    "^Φ"   "ᶲ" )
  ("supphi"    "^φ"   "ᵠ" )
  ("supchi"    "^χ"   "ᵡ" )
  ;; Superscript uppercase
  ("supA"      "^A"   "ᴬ" )
  ("supB"      "^B"   "ᴮ" )
  ("subD"      "^D"   "ᴰ" )
  ("supE"      "^E"   "ᴱ" )
  ("supG"      "^G"   "ᴳ" )
  ("supH"      "^H"   "ᴴ" )
  ("supI"      "^I"   "ᴵ" )
  ("supJ"      "^J"   "ᴶ" )
  ("supK"      "^K"   "ᴷ" )
  ("supL"      "^L"   "ᴸ" )
  ("supM"      "^M"   "ᴹ" )
  ("supN"      "^N"   "ᴺ" )
  ("supO"      "^O"   "ᴼ" )
  ("supP"      "^P"   "ᴾ" )
  ("supR"      "^R"   "ᴿ" )
  ("supT"      "^T"   "ᵀ" )
  ("supU"      "^U"   "ᵁ" )
  ("supW"      "^W"   "ᵂ" )
  ;; Subscript symbols
  ("sub0"      "_0"    "₀")
  ("sub1"      "_1"    "₁")
  ("sub2"      "_2"    "₂")
  ("sub3"      "_3"    "₃")
  ("sub4"      "_4"    "₄")
  ("sub5"      "_5"    "₅")
  ("sub6"      "_6"    "₆")
  ("sub7"      "_7"    "₇")
  ("sub8"      "_8"    "₈")
  ("sub9"      "_9"    "₉")
  ("subplus"   "_+"    "₊")
  ("subminus"  "_-"    "₋")
  ("subeq"     "_="    "₌")
  ("sublparen" "_("    "₍")
  ("subrparen" "_)"    "₎")
  ;; Combining marks
  ("hat"        "^"     "̂")
  ("tilde"      "~"     "̃")
  ("overbar"    "-"     "̄")
  ("overdot"    "."     "̇")
  ("overdotdot" ".."    "̈")
  ("underbar"   "_"     "̱")
  ("slash"      "/"     "̸")
  ;; Other
  ("bullet"     "-"    "•")
  ("wbullet"    "-"    "◦")
  ("tbullet"    "-"    "‣")
  ("lbracetop"  "{"    "⎧")
  ("lbracemid"  "{"    "⎨")
  ("lbracebot"  "{"    "⎩")
  ("lbraceext"  "{"    "⎪")
  ("rbracetop"  "{"    "⎫")
  ("rbracemid"  "{"    "⎬")
  ("rbracebot"  "{"    "⎭")
  ("rbraceext"  "{"    "⎮")
  ("checkmark"  "/"    "✓")
   ))

(setq symtable (make-hash-table :test 'equal))
(setq symnames (make-hash-table :test 'equal))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Code for inserting & cycling through symbols                               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun cycle-symbols ()
  "Cycle through possible symbol replacements for the last few
   characters at point."
  (interactive)
  (let ((i 3))
    (while (> i 0)
      ;; get i characters before point
      (if (>= (- (point) i) 1)
	  (let* ((s (buffer-substring (point) (- (point) i)))
		 (x (gethash s symtable)))
	    (if x (progn (delete-region (point) (- (point) i))
			 (insert x)
			 (if (and (gethash x symnames)
				  (not (active-minibuffer-window)))
			     (message (gethash x symnames)))
			 (setq i 0))
	      (and (= i 1) (message "No symbol match found")))))
      (setq i (1- i)))))


(defun cycle-symbols-isearch ()
  "Same as cycle-symbols, but applies when the user is doing an
  incremental search."
  (interactive)
  (let ((i 3))
    (while (> i 0)
      ;; get last i characters in search string and corresponding replacement x
      (if (>= (length isearch-string) i)
	  (let* ((s (substring isearch-string (- (length isearch-string) i)))
		 (x (gethash s symtable))
		 (newlen (- (length isearch-string) i)))
	    (when x
	      ;; Replace last i characters with x
	      ;; Note isearch-delete-char doesn't actually delete if the
	      ;; user has found a 2nd match or greater; instead it goes back
	      ;; to the previous match. So some extra deletes may be
	      ;; required.
	      (while (> (length isearch-string) newlen) (isearch-delete-char))
	      ;; Insert replacement
	      (dotimes (k (length x)) (isearch-process-search-char (elt x k)))
	      ;; Exit loop
	      (setq i -100))))
      (setq i (1- i)))
    (when (= i 0) ;; no match
      (with-temp-message "No symbol match found" (sleep-for 1))
      (isearch-message))))


(defun insert-symbol (shortcut symbol orig)
  "Insert a symbol/shortcut pair into symtable, recursively
   creating circular references as needed. E.g., starting with
   an empty symtable,
     (1) (insert-symbol 'j' 'θ' 'j') yields
         |  j --> θ
         |  θ --> j
     (2) (insert-symbol 'j' 'ϑ' 'j') then yields
         |  j --> θ
         |  θ --> ϑ
         |  ϑ --> j
     etc.
   The third argument, orig, used for recursion, should be set to
   be equal to shortcut by the user."
  (let ((x (gethash shortcut symtable)))
    (cond
     ((not x) (progn (puthash shortcut symbol symtable)
                     (puthash symbol shortcut symtable)))
     ((equal x orig) (progn (puthash shortcut symbol symtable)
                            (puthash symbol orig symtable)))
     (t (insert-symbol x symbol orig)))))


;; Create the symtable
(dolist (elem symbols)
  (let ((name (nth 0 elem))
        (shortcut (nth 1 elem))
        (symbol (nth 2 elem)))
    (insert-symbol shortcut symbol shortcut)
    (puthash symbol name symnames)))


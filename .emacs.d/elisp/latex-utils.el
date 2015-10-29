;; This is just a port of functions defined in YaTeX mode
;; The functions are listed in function-call-relationship tree
;; The prefix "YaTeX" has been replaced by latex-utils:.
;;
;; Function call relationship (higher item calls low level functions)
;; Only not-builtin functions are listed
;;
;;   latex-utils:change-parentheses
;;   ├─ latex-utils:on-parenthesis-p
;;   │   └─ latex-utils:match-string
;;   ├─ latex-utils:match-string
;;   └─ latex-utils:goto-open-paren
;;        └─ latex-utils:on-parenthesis

(defvar latex-utils:ams-paren-modifier
  '(("Biggl" . "Biggr") ("biggl" . "biggr")
    ("Bigl" . "Bigr") ("bigl" . "bigr")
    ("left" . "right") ("" . ""))
  "Alist of modifier of parentheses.")
(defvar latex-utils:left-paren "(\\|\\[\\|\\\\{")
(defvar latex-utils:right-paren ")\\|\\]\\|\\\\}")
(defvar latex-utils:paren
  (concat latex-utils:left-paren "\\|" latex-utils:right-paren))

(defun latex-utils:match-string (n &optional m)
  "Return (buffer-substring (match-beginning n) (match-beginning m))."
  (if (match-beginning n)
      (buffer-substring-no-properties (match-beginning n)
			(match-end (or m n)))))

(defun latex-utils:on-parenthesis-p ()
  "If cursor is on an (AMS-LaTeX) parenthesis, return the parenthesis."
  (interactive)
  (let* ((list latex-utils:ams-paren-modifier)
	 (longest 0) ;; the longest length of parenthesis command strings
	 (flag t) ;; flag for whether on braces not following \
	 (point (point))
	 (move 0)
	 (paren))
    (while list
      (setq longest
	    (max longest (length (car (car list))) (length (cdr (car list)))))
      (setq list (cdr list)))
    (save-excursion
      ;; search {} and, if it does not follow `\', set flag nil.
      ;; if it is right after `\', set flag t and move to the position of \.
      ;; mmmmm.
      (if (looking-at "{\\|}")
	  (if (not (equal (char-after (1- (point))) 92))
	      (setq flag nil)
	    (forward-char -1)))
      ;; if flag is nil, do nothing.
      (if (and flag (re-search-forward latex-utils:paren
				       (+ (point) 3 longest) t))
	  (progn
	    (setq move (- (point) point))
	    (setq paren (latex-utils:match-string 0))
	    (setq list latex-utils:ams-paren-modifier)
	    ;; criterion for whether on [] () \{\} or not.
	    (if (string-match latex-utils:left-paren paren)
		(while (and list flag)
		  (let* ((mod (car (car list)))
			 (mod-length 0) ;; length of modifier
			 paren-regexp ;; regexp of paren.
			 mod-regexp) ;; regexp of modifier.
		    (if (> (length mod) 0)
			(setq mod-regexp (concat "\\\\" mod)
			      mod-length (1+ (length mod))))
		    (cond ((string= paren "\\{")
			   (setq paren-regexp (concat "\\" paren)))
			  ((string= paren "[")
			   (setq paren-regexp "\\["))
			  (t (setq paren-regexp paren)))
		    (save-excursion
		      (if (and (>= (- (point) (point-min))
				   (+ mod-length (length paren)))
			       (not (forward-char
				     (- 0 mod-length (length paren))))
			       (looking-at (concat "\\(" mod-regexp "\\)\\("
						   paren-regexp "\\)")))
			  (setq flag nil)))
		    (setq list (cdr list))))
	      (while (and list flag)
		(let* ((mod (cdr (car list)))
		       (mod-length 0)
		       paren-regexp
		       mod-regexp)
		  (if (> (length mod) 0)
		      (setq mod-regexp (concat "\\\\" mod)
			    mod-length (1+ (length mod))))
		  (cond ((string= paren "\\}")
			 (setq paren-regexp (concat "\\" paren)))
			((string= paren "]")
			 (setq paren-regexp "\\]"))
			(t (setq paren-regexp paren)))
		  (save-excursion
		    (if (and (>= (- (point) (point-min))
				 (+ mod-length (length paren)))
			     (not (forward-char
				   (- 0 mod-length (length paren))))
			     (looking-at (concat "\\(" mod-regexp "\\)\\("
						 paren-regexp "\\)")))
			(setq flag nil)))
		  (setq list (cdr list)))))
	    (if (<= move (length (latex-utils:match-string 0)))
		(latex-utils:match-string 0)))))))

(defun latex-utils:goto-open-paren (&optional jumpto-co)
  "Jump to the exact position of open parenthesis.
If optional argument JUMPTO-CO is non-nil, goto corresponding parentheses."
  (interactive)
  (let ((paren)
	(backslash-syntax (char-to-string (char-syntax ?\\))))
    (if (setq paren (latex-utils:on-parenthesis-p))
	(if (string-match "(\\|{\\|\\[" paren (1- (length paren)))
	    (progn
	      (re-search-forward "(\\|{\\|\\[" (+ (point) (length paren)) t)
	      (backward-char)
	      (if jumpto-co
		  (unwind-protect
		      (progn
			(modify-syntax-entry ?\\ " ")
			(forward-list)
			(backward-char))
		    (modify-syntax-entry ?\\ backslash-syntax)))
	      (point))
	  (re-search-forward ")\\|}\\|\\]" (+ (point) (length paren)) t)
	  (unwind-protect
	      (progn
		(modify-syntax-entry ?\\ " ")
		(backward-list)
		(point))
	    (modify-syntax-entry ?\\ backslash-syntax))))))

(defun latex-utils:change-parentheses ()
  "Change the size of parentheses, braces, and brackets of AMS-LaTeX."
  (interactive)
  (if (not (latex-utils:on-parenthesis-p))
      nil
    (let* ((mod (latex-utils:match-string 1)) ;; modifier
	   (paren (if mod (latex-utils:match-string 2) (latex-utils:match-string 0))) ;; paren
	   (mod-length (if (or (string= mod "\\left") (string= mod "\\right"))
			   5            ;; 5 in case left or right
			 (length mod))) ;; length of modifier
	   (paren-length (length paren)) ;; length of paren
	   (length (+ mod-length paren-length)) ;; length of whole string
	   (big-p t) ;; flag whether new modifier is "[Bb]ig+" or not.
	   size ;; left, big, Big etc.
	   type ;; parentheses type
	   lr   ;; "l" or "r".
	   char newsize newsize-length
	   (backslash-syntax (char-to-string (char-syntax ?\\)))
	   (case-fold-search))
      ;; decide lr and size from mod and paren.
      (cond ((string-match "\\(\\\\[Bb]ig+\\)[lr]" mod)
	     (setq size (substring mod 1 (match-end 1))
		   lr (substring mod (match-end 1) (match-end 0))))
	    ((string-match "\\\\left" mod)
	     (setq size "left-right" lr "l"))
	    ((string-match "\\\\right" mod)
	     (setq size "left-right" lr "r"))
	    ((string-match "(\\|\\[\\|\\\\{" paren)
	     (setq size "null" lr "l"))
	    ((string-match ")\\|\\]\\|\\\\}" paren)
	     (setq size "null" lr "r"))
	    (t
	     (setq size nil lr nil)))
      (while (not newsize)
	(message (format (concat "Change from %s: "
				 "l(big) L(Big) h(bigg) H(Bigg) "
				 "r(left-right) n(NONE) ( { [") size))
	(setq char (read-char)
	      newsize (cond ((char-equal char ?l) "\\big")
			    ((char-equal char ?L) "\\Big")
			    ((char-equal char ?h) "\\bigg")
			    ((char-equal char ?H) "\\Bigg")
			    ((char-equal char ?r)
			     (setq big-p nil) "\\left")
			    ((memq char '(?\( ?\)))
			     (setq big-p nil type '("(" . ")")) "")
			    ((memq char '(?\{ ?\}))
			     (setq big-p nil type '("\\{" . "\\}")) "")
			    ((memq char '(?\[ ?\]))
			     (setq big-p nil type '("[" . "]")) "")
			    ((char-equal char ?n)
			     (setq big-p nil) "")
			    (t nil))
	      newsize-length (length newsize)))
      (latex-utils:goto-open-paren)
      (forward-char)
      (cond
       (type
	(delete-region (point) (- (point) paren-length))
	(save-excursion (insert (car type))))
       (t
	(delete-region (- (point) length) (- (point) paren-length))
	(backward-char paren-length)))
      (insert newsize)
      (if big-p (insert ?l))
      (unwind-protect
	  (progn
	    (modify-syntax-entry ?\\ " ")
	    (forward-list)
	    (if (string= size "left-right") (setq length (1+ length)))
	    (if (eq char ?r) (setq newsize "\\right"))
	    (cond
	     (type
	      (delete-region (point) (- (point) paren-length))
	      (insert (cdr type)))
	     (t
	      (delete-region (- (point) length) (- (point) paren-length))
	      (backward-char paren-length)
	      (insert newsize)
	      (if big-p (insert ?r))
	      (forward-char paren-length)))
	    (if (string= lr "l") (backward-list)))
	(modify-syntax-entry ?\\ backslash-syntax))
      t)))



(provide 'latex-utils)

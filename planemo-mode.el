

(setq start-words '("if" "for")        ;; defines the indentation
      end-words '("end if" "end for")  ;; always matches start indent
      middle-words '("else" "else if") ;; also matches start indent
      other-words '("set" "echo" "def" ;; matches start indent +4?
                    "include" "extends" "import"))

(define-derived-mode planemo-mode nxml-mode "Pl[XML|Cheetah]"
  "Major mode for editing Galaxy XML files."
  (setq-local nxml-child-indent 4
              indent-line-function 'determine-indent-options)
  (make-face 'cheetah-variable-face)
  (let ((hashwords (append start-words end-words
                           middle-words other-words))
        (pyops '("or" "and" "in" "+" "-" "*" "/" "==" "!="))
        (pyfun '("enumerate" "str" "int" "open"))
        (bashcomms '("cat" "head" "tail" "awk" "cut" "ls" "grep" "echo" "touch"))
        (bashops '("&&" ">" "<" ">>" "<<" "|" )))
    (let ((rx-keywords (eval `(rx (group "#" (or ,@hashwords) eow))))
          (rx-bashcomms (eval `(rx bow (group (or,@bashcomms)) eow)))
          (rx-bashops (eval `(rx space (group (or,@bashops)) space)))
          (rx-pyops (eval `(rx (or bow space) (group (or ,@pyops)) (or eow space))))
          (rx-pyfun (eval `(rx (or bow space) (group (or ,@pyfun)) (or eow space)))))
      (font-lock-add-keywords
       nil
       `((,(rx (group bol (* space) "##" (* any) eol))
          1 font-lock-comment-face)                  ;; comments
         (,(rx (not "\\") (group "$" (? "{") (1+ (or alpha "." "_")) (? "}")))
          1 font-lock-string-face)                   ;; cheetah vars
         (,rx-keywords 1 font-lock-constant-face)    ;; cheetah keywords
         (,rx-bashcomms 1 font-lock-reference-face)  ;; bash commands
         (,rx-bashops 1 font-lock-variable-name-face) ;; bash operations
         (,rx-pyops 1 font-lock-function-name-face)  ;; python ops
         (,rx-pyfun 1 font-lock-variable-name-face)              ;; python functions
         ))
      (font-lock-mode 1))))

(defun current-alignpos ()
  "Number of left-aligned spaces."
  (save-excursion
    (beginning-of-line)
    (if (eq ?  (char-after)) ;; whitespace
        (progn (forward-whitespace 1)
               (- (point) (line-beginning-position)))
      0)))

(defun get-previous--hash ()
  "Obtain the spacing and tag of to the previous hashword.  Does not save the excursion because it may be used in succession to determine hierarchy."
  (let* ((words (append start-words end-words))
         (pointnow (point))
         (bounds
          (list (search-backward-regexp
                 (eval `(rx (group "#" (or ,@words) eow))) nil t)
                (match-beginning 0) (match-end 0)))
         (tag (buffer-substring-no-properties (1+ (nth 1 bounds))
                                              (nth 2 bounds)))
         (line-relative (count-lines (nth 1 bounds) pointnow)))
    (if (car bounds)
        (list (current-alignpos) tag line-relative))))

(defun get-previous-hash ()
  "Get the previous hashword without changing position."
  (save-excursion (get-previous--hash)))

(defun get-forward-hash ()
  "Get the first hashword on the current line."
  (let ((words (append start-words end-words
                       middle-words other-words)))
    (save-excursion
      (beginning-of-line)
      (search-forward-regexp
       (eval `(rx bol (* space) (group "#" (or ,@words) eow)))
       (line-end-position) t)
      (buffer-substring-no-properties
       (match-beginning 1) (match-end 1)))))

(defun get-first-word-or-hash ()
  "Get the first word or first hash on the current line."
  (let* ((fword (save-excursion
                 (beginning-of-line)
                 (string-trim
                  (buffer-substring-no-properties
                   (point) (progn (forward-word) (point))))))
         (start1 (substring fword nil 1))
         (start2 (substring fword nil 2)))
    (cond ((equal "##" start2) "##")
          ((equal "#" start1) (substring (get-forward-hash) 1))
          (t fword)))) ;; always end on an fword


(defun outcome-alignwith (prev-align)
  (message "outcome AlignWith: End word, with matching Start word")
  (perform-alignment prev-align))

(defun outcome-b (curr-word)
  (message "outcome B: End word. Looking for matching Start word")
  (perform-alignment (matchtag-back curr-word)))

(defun outcome-nestunder (prev-align)
  (message "outcome NestUnder: Nest under previous hash")
  (perform-alignment (+ prev-align 4)))

(defun outcome-d ()
  (message "outcome D: Do nothing"))

(defun outcome-prevline ()
  (message "outcome PrevLine: No previous tag. Align to previous line.")
  (perform-alignment (save-excursion (previous-line) (current-alignpos))))

(defun perform-alignment (new-alignpos)
  (save-excursion
    (beginning-of-line)
    (delete-char (current-alignpos))
    (insert-char ? new-alignpos)))



(defun determine-indent-options ()
  "Determine the available indentation options for the current line under cursor."
  (let* (;;(words (append start-words end-words))
         (words '("echo" "from" "else" "include" "extends" "set"
                  "def" "import" "for" "if" "end if" "end for"))
         (curr-word (get-first-word-or-hash))
         (curr-xmlp (equal "<" (substring curr-word nil 1)))
         (curr-hashp (member curr-word words)))
    (if curr-xmlp
        (nxml-indent-line)
      (let* ((previous-hash (get-previous-hash))
             (prev-align (car previous-hash))
             (prev-word (cadr previous-hash))
             (prev-ldiff (caddr previous-hash))
             (prev-ldiff1-p (eq 1 prev-ldiff)))
        (cond (curr-xmlp
               (nxml-indent-line))
              (previous-hash
               (let* ((curr-startp (member curr-word start-words))
                      (curr-endp (member curr-word end-words))
                      (curr-middp (member curr-word middle-words))
                      ;; TODO: Handle middp cases
                      (curr-othrp (member curr-word other-words))
                      (prev-startp (member prev-word start-words))
                      (prev-endp (member prev-word end-words))
                      (match-pairp (or (and (string= prev-word "if")
                                            (member curr-word '("else" "end if")))
                                       (and (string= prev-word "for")
                                            (string= curr-word "end for")))))
                 (cond (curr-hashp
                        (cond (curr-endp ;; current is end of a pair?
                               (cond (match-pairp (outcome-alignwith prev-align))  ;; Scenario 1
                                     (prev-startp (outcome-d)) ;; Scenario 3a
                                     (t (outcome-b curr-word)))) ;; Scenario 2+3 have the same outcome
                              ;;
                              (curr-startp ;; current is start of a pair?
                               (cond (prev-endp (outcome-alignwith prev-align)) ;; Scenario 4
                                     (prev-startp (outcome-nestunder prev-align))  ;; Scenario 5
                                     (t (outcome-align2prevhash))))
                              (curr-middp ;; current is e.g. "#set"
                               (cond (prev-startp (outcome-nestunder prev-align))
                                     (prev-endp (outcome-alignwith prev-align))
                                     (t (outcome-align2prevhash))))
                              (t (error "Umm."))))
                       ;; At this point we are not a hash word, but we
                       ;; can derive alignment from the previous hashes
                       ((not prev-ldiff1-p) ;; prioritise previous line alignment
                        (outcome-prevline))
                       (prev-endp
                        (outcome-alignwith prev-align))
                       (prev-startp
                        (outcome-nestunder prev-align))
                       (t ;; no previous tag? align to previous line or 0
                        (outcome-prevline)))))
              (t ;; not an xml, and no previous hash? align to previous line or 0
               (outcome-prevline)))))))
;; execute this only in the xml buffer
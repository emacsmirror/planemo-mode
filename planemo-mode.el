;;; planemo-mode.el --- Minor mode for editing Galaxy XML files -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Mehmet Tekman <mtekman89@gmail.com>

;; Author: Mehmet Tekman
;; URL: https://gitlab.com/mtekman/planemo-mode.el
;; Keywords: outlines
;; Package-Requires: ((emacs "27.1") (dash "2.17.0"))
;; Version: 0.1

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;;; Commentary:

;; This mode provides fontification and indentation rules for editing
;; Galaxy XML files.

;;; Code:
(require 'nxml-mode)
(require 'dash)

(defgroup planemo nil
  "Planemo customisable attributes"
  :group 'productivity)

(defcustom planemo--python-ops
  '("or" "and" "in" "+" "-" "*" "/" "==" "!=")
  "Python operations used by Cheetah."
  :type 'list
  :group 'planemo)

(defcustom planemo--python-fun
  '("enumerate" "str" "int" "open")
  "Python functions used by Cheetah."
  :type 'list
  :group 'planemo)

(defcustom planemo--bash-comms
  '("cat" "head" "tail" "awk" "cut" "ls" "grep" "echo" "touch")
  "Bash commands commonly found in the XML."
  :type 'list
  :group 'planemo)

(defcustom planemo--bash-ops
  '("&&" ">" "<" ">>" "<<" "|" )
  "Bash operations commonly found in the XML."
  :type 'list
  :group 'planemo)

(defconst planemo--start-tags '("if" "for")
  "Defines the Cheetah tags for beginning nested indentation.")

(defconst planemo--end-tags '("end if" "end for")
  "Defines the Cheetah tags for ending nested indentation.
Must complement the ``planemo--start-tags''")

(defconst planemo--middle-tags '("else" "else if")
  "Defines the Cheetah tags that remain un-indented relative to a starting tag within a clause.")

(defconst planemo--other-tags
  '("set" "echo" "def" "include" "extends" "import" "from")
  "Defines Cheetah tags that are nested like regular words relative to a starting tag.")

(defconst planemo--all-tags
  (append planemo--start-tags planemo--end-tags
          planemo--middle-tags planemo--other-tags)
  "All possible Cheetah tags.")

(defconst planemo--pair-tags
  (append planemo--start-tags planemo--end-tags)
  "All start and end tags.")

(defconst planemo--most-tags
  (append planemo--start-tags planemo--end-tags
          planemo--middle-tags)
  "Tags with the same alignment.")


(define-derived-mode planemo-mode nxml-mode "Pl[XML|Cheetah]"
  "Major mode for editing Galaxy XML files."
  (setq-local nxml-child-indent 4
              indent-line-function 'planemo-indent-line
              indent-region-function 'planemo-indent-region)
  (make-face 'cheetah-variable-face)
  (let ((rx-keywords (eval `(rx (group "#" (or ,@planemo--all-tags) eow))))
        (rx-bashcomms (eval `(rx bow (group (or,@planemo--bash-comms)) eow)))
        (rx-bashops (eval `(rx space (group (or,@planemo--bash-ops)) space)))
        (rx-pyops (eval `(rx (or bow space) (group (or ,@planemo--python-ops)) (or eow space))))
        (rx-pyfun (eval `(rx (or bow space) (group (or ,@planemo--python-fun)) (or eow space)))))
    (font-lock-add-keywords
     nil
     `((,(rx (group bol (* space) "##" (* any) eol))
        1 font-lock-comment-face)                   ;; comments
       (,(rx (not "\\") (group "$" (? "{") (1+ (or alpha "." "_")) (? "}")))
        1 font-lock-string-face)                    ;; cheetah vars
       (,rx-keywords 1 font-lock-constant-face)     ;; cheetah keywords
       (,rx-bashcomms 1 font-lock-reference-face)   ;; bash commands
       (,rx-bashops 1 font-lock-variable-name-face) ;; bash operations
       (,rx-pyops 1 font-lock-function-name-face)   ;; python ops
       (,rx-pyfun 1 font-lock-variable-name-face)   ;; python functions
       ))
    (font-lock-mode 1)))

(defun planemo--get-lalign ()
  "Number of left-aligned spaces."
  (save-excursion
    (beginning-of-line)
    (if (eq ?  (char-after)) ;; whitespace
        (progn (forward-whitespace 1)
               (- (point) (line-beginning-position)))
      0)))

(defun planemo--jump-prevtag ()
  "Obtain the spacing and tag of to the previous tag.  Does not save the excursion because it may be used in succession to determine hierarchy."
  (let* ((pointnow (point))
         (bounds
          (list (search-backward-regexp
                 (eval `(rx (group "#" (or ,@planemo--pair-tags) eow)))
                 nil t)
                (match-beginning 0) (match-end 0)))
         (tag (buffer-substring-no-properties (1+ (nth 1 bounds))
                                              (nth 2 bounds))))
    (if (car bounds)
        (list (planemo--get-lalign) tag
              (planemo--numlines (nth 1 bounds) pointnow))
      (list nil nil))))


(defun planemo--numlines (first second)
  "Calculate lines between FIRST and SECOND, taking into account the issue with calculating line numbers when SECOND is right at the beginning of the line."
  (- (line-number-at-pos (max first second))
     (line-number-at-pos (min first second))))

(defun planemo--get-prevtag ()
  "Get the previous tag without changing position."
  (save-excursion (planemo--jump-prevtag)))

(defun planemo--get-forwtag ()
  "Get the first tag on the current line."
  (save-excursion
    (beginning-of-line)
    (search-forward-regexp
     (eval `(rx bol (* space) (group "#" (or ,@planemo--all-tags) eow)))
     (line-end-position) t)
    (buffer-substring-no-properties
     (match-beginning 1) (match-end 1))))

(defun planemo--get-fwot ()
  "Get the first word or tag on the current line."
  (let* ((fword (save-excursion
                  (beginning-of-line)
                  (string-trim
                   (car
                    (split-string
                     (buffer-substring-no-properties
                      (point) (progn (forward-word) (point)))
                     "\n"))))))
    (if (equal "" fword)
        "     "  ;; return blank tag
      (cond ((equal "##" (substring fword nil 2)) "##")
            ((equal "#" (substring fword nil 1))
             (substring (planemo--get-forwtag) 1))
            (t fword)))))

(defun planemo--matchtag-back (curr-word)
  "Find the nearest previous start tag that would complement CURR-WORD."
  ;; The easy method would just be to do a reverse regex search, but
  ;; in the future I plan to look backwards for unpaired tags only.
  (let* ((assoc-map '(("end for" . "for")
                      ("end if" . "if")))
         (wanted-tag (alist-get curr-word assoc-map nil nil 'string=))
         (result nil))
    ;; - here we stack tags as we find them and pop them off
    ;;   when consecutive tags pair up
    ;;(poplist (list curr-word)))
    (save-excursion
      (while
          (not (-let (((align tag nl) (planemo--jump-prevtag)))
                 ;; exit condition
                 (if (string= tag wanted-tag)
                     (setq result (list align tag nl)))))))
    result))

;; BEGIN: Indentation outcomes
(defun planemo--ind-alignwith (prev-align)
  "Align the following line with PREV-ALIGN."
  (message "outcome AlignWith: End word, with matching Start word")
  (indent-line-to prev-align))

(defun planemo--ind-findprevmatch (curr-word)
  "Find a previous starting tag to complement CURR-WORD."
  (message "outcome B: End word. Looking for matching Start word")
  (-let (((align _tag) (planemo--matchtag-back curr-word)))
    (if align
        (indent-line-to align))))


(defvar cycle-indents nil
  "Toggle for nesting under.")

(defun planemo--ind-nestunder (prev-align &optional cycle)
  "Nest the current line under PREV-ALIGN.  If CYCLE is t, then a repeated call will toggle the indent."
  (message "outcome NestUnder: Nest under previous hash")
  (if cycle
      (if (setq cycle-indents (not cycle-indents)) ;; toggle
          (indent-line-to (+ prev-align 4))
        (indent-line-to prev-align))
    (indent-line-to (+ prev-align 4))))

(defun planemo--ind-nothing ()
  "Do nothing to the current line."
  (message "outcome D: Do nothing"))

(defun planemo--ind-prevline ()
  "Indent the current line to the previous line."
  (message "outcome PrevLine: No previous tag. Align to previous line.")
  (indent-line-to (save-excursion (forward-line -1) (planemo--get-lalign))))
;; END: Indentation outcomes

;;;###autoload
(defun planemo-indent-region (start end)
  "Indent the current region flanked by START and END positions."
  (interactive (list (region-beginning) (region-end)))
  (save-excursion
    (goto-char start)
    (while (< (point) end)
      (planemo-indent-line)
      (forward-line 1))))

;;;###autoload
(defun planemo-indent-line ()
  "Indent the current line."
  (interactive)
  (beginning-of-line)
  (let* ((curr-word (planemo--get-fwot))
         (curr-xmlp (or (equal "<" (substring curr-word nil 1))
                        (equal ">" (substring curr-word -1))))
         (curr-hashp (member curr-word planemo--all-tags)))
    (if curr-xmlp
        (nxml-indent-line)
      (let* ((previous-hash (planemo--get-prevtag))
             (prevhash-align (car previous-hash))
             (prevhash-word (cadr previous-hash))
             (prevhash-ldiff (caddr previous-hash))
             (prevline-word (save-excursion
                              (forward-line -1)
                              (planemo--get-fwot)))
             (prevline-isxml (equal "<" (substring
                                         prevline-word nil 1)))
             (prevhash-ldiff1-p (or (eq 1 prevhash-ldiff)
                                    (member prevline-word
                                            planemo--most-tags))))
        (cond
         (curr-xmlp (nxml-indent-line)) ;; <xmltag> : use nxml-indent
         (prevhash-word                     ;; previous tag exists
          (let* ((curr-startp (member curr-word planemo--start-tags))
                 (curr-endp (member curr-word planemo--end-tags))
                 (curr-middp (member curr-word planemo--middle-tags))
                 ;;(curr-othrp (member curr-word planemo--other-tags))
                 (prev-startp (member prevhash-word planemo--start-tags))
                 (prev-endp (member prevhash-word planemo--end-tags))
                 (prev-middp (member prevhash-word planemo--middle-tags))
                 (match-pairp (or (and (string= prevhash-word "if")
                                       (member curr-word '("else" "end if")))
                                  (and (string= prevhash-word "for")
                                       (string= curr-word "end for")))))
            (cond (curr-hashp
                   (cond ((or curr-endp curr-middp)
                          ;; current is end or middle of a pair?
                          (cond
                           ;; ["for"] and "end for": match alignment
                           (match-pairp (planemo--ind-alignwith prevhash-align))
                           ;; ["if"] and "end for" : user did something wrong, do nothing.
                           (prev-startp (planemo--ind-nothing))
                           ;; [ * ] and "end for" : look for a better previous match.
                           ;; - If match found, align to it
                           ;; - If none found, align to previous line
                           (t (planemo--ind-findprevmatch curr-word))))
                         ;;
                         (curr-startp ;; current is start of a pair?
                          (cond
                           ;; ["end for"] and "for" : unrelated clause, align to it
                           (prev-endp (planemo--ind-alignwith prevhash-align))
                           ;; ["if"] and "for" : nest current under parent
                           (prev-startp (planemo--ind-nestunder prevhash-align))
                           ;; [ * ] and "for" : align to previous line
                           (t (planemo--ind-prevline))))
                         ;;
                         (curr-middp  ;; current is e.g. "#set"
                          (cond
                           ;; ["if"] and "set" : nest current under parent
                           (prev-startp (planemo--ind-nestunder prevhash-align))
                           ;; ["end"] and "set" : unrelated clause, align to it
                           (prev-endp (planemo--ind-alignwith prevhash-align))
                           ;; * and "set" : align to previous line
                           (t (planemo--ind-prevline))))
                         ;; "#set"
                         (t (planemo--ind-prevline))))
                  ;; !!At this point curr-word is not a hash word!!
                  ;; ["tag"] but the last line is not one: align to it.
                  ((not prevhash-ldiff1-p) (planemo--ind-prevline))
                  ;; ["end for"] followed by "blah" : align to it
                  (prev-endp (planemo--ind-alignwith prevhash-align))
                  ;; ["for"] followed by "blah" : nest under it
                  ((or prev-startp prev-middp)
                   ;; Here we cycle the first line under the hash
                   (planemo--ind-nestunder prevhash-align t))
                  ;; no previous tag : align to previous line or 0
                  (t (planemo--ind-prevline)))
            ))
         ;; not xml, and no prev hash : align to previous line
         ;; - unless the previous line is an XML, in which case set to 0
         (prevline-isxml (indent-line-to 0))
         (t (planemo--ind-prevline))
         )))))

(provide 'planemo-mode)
;;; planemo-mode.el ends here
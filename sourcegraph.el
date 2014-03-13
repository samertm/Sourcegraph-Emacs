f;;; sourcegraph.el --- find the code you need quickly

;; Copyright (C) 2014 Samer Masterson

;; Author: Samer T. Masterson <samer@samertm.com>
;; Keywords: ???

;;; Commentary:

;; Correct form for url:
;; "https://sourcegraph.com/api/search?q=django&exported=1&_via=sourcegraph-emacs-01"

;; Should I use defvar for the via string?

;;; Code:

(require 'json)


;; todo: error catching
(defun sourcegraph-parse-json (url)
  (let* ((json-object-type 'plist)
         (json-key-type 'symbol) ; setting explicitly
         (json-buffer (url-retrieve-synchronously url))
         (parsed-json (vector)))
    (with-current-buffer json-buffer
      ;; delete headers
      (goto-char (point-min))
      (delete-region (point-min) (- (search-forward "[") 1))
      ;; set-up for json-read
      (goto-char (point-min))
      (setq parsed-json (json-read)))
    (kill-buffer json-buffer)
    parsed-json))


(defun first-sentence (string)
  (let ((build-string "")
        (index 0)
        (deep 0)
        (state :watch))
    ;; three states. One state reads the character and decides what to do
    ;; one state moves outside of a <tag>
    ;; one state munches the data and returns
    (while (not (eq state :done))
      (let* ((curr-char (elt string index)))
        (cond ((eq state :watch)
               (cond ((eq curr-char ?<)
                      (setq state :in-tag))
                     ((or (eq curr-char ?\s)
                          (eq curr-char ?\n))
                      (setq index (1+ index)))
                     (t
                      (setq state :munch))))
              ((eq state :in-tag)
               (setq index (1+ (string-match ">" string index)))
               (setq state :watch))
              ((eq state :munch)
               ;; match '<' or '. ', whichever comes sooner.
               ;; todo: handle errors from index+1 being out of bounds for string
               (cond ((eq curr-char ?<)
                      (setq state :done))
                     ((and (eq curr-char ?.) (eq (elt string (1+ index)) ?\s))
                      (setq build-string (concat build-string (char-to-string curr-char)))
                      (setq state :done))
                     (t
                      (setq build-string (concat build-string (char-to-string curr-char)))
                      (setq index (1+ index))))))))
    build-string))
                      
(defun sourcegraph-write-text (json-vector search-terms)
  (let* ((final-string (format "Sourcegraph Search Results for %s\n\n\n" search-terms))
         (json-index 0))
    (while (< json-index (length json-vector))
      (let ((json (elt json-vector json-index))
            (build-string ""))
        (setq build-string "Result: ")
        (setq build-string
              (concat build-string (plist-get json 'specificKind) "\t"
                      (plist-get json 'specificPath) "\t"
                      (plist-get json 'repo)))
        (if (plist-get json 'doc)
            (setq build-string (concat build-string "\n    " (first-sentence (plist-get json 'doc)))))
        (setq build-string (concat build-string "\n\n"))
        (setq final-string (concat final-string build-string))
        (setq json-index (1+ json-index))))
    final-string))



;; I thought I'd take a stab at parsing Sourcegraph's html using a
;; state machine. Most of this code will be scrapped after I get the
;; API.

;; NOTE: I've only tested this for the input 'django'. The code is
;; also kind of hacky because I'm new to Elisp, and because I finished
;; this in an 8 hour strech. :P

(defun sourcegraph-search-site ()
  (interactive)
  (let* ((input-string (read-string "Search Sourcegraph: "))
         (search-string (replace-regexp-in-string " " "+" input-string))
         (via "sourcegraph-emacs-01")
         (leading-string (format "Sourcegraph Search Results for %s\n" input-string))
         (search-buffer
          (url-retrieve-synchronously
           (format "https://sourcegraph.com/search?q=%s&_via=%s" search-string via))))
    (with-current-buffer search-buffer
      (rename-buffer "Sourcegraph Search" t) ; fixme: buffer says [no process] and undo
                                             ; doesn't work.
      ;; Remove everything up to the first search results
      (re-search-forward "search-results")
      (end-of-line)
      (delete-region (point-min) (point))
      (delete-blank-lines)
      (insert leading-string)
      ;; monolithic state machine for parsing html
      (let ((div-deep 1) ; starts at 1 b/c we deleted the 'div' keyword above
            (build-string "")
            (munch-char nil)
            (final-string "")
            (state :out))
        (while (> div-deep 0)
          (cond ((equal state :out)
                 (search-forward "<")
                 (backward-char) ; to compensate for the forward-char at the end
                 (setq state :in))
                ((equal state :in)      ;doublecheck the regex
                 ;; remove whitespace from beginning of string
                 (setq build-string (replace-regexp-in-string (rx (: bos (* (any " \t\n"))))
                                                              ""
                                                              build-string))
                 (cond ((string-match "[>= \n]" (char-to-string (char-after)))
                        (cond ((equal build-string "div")
                               (setq div-deep (+ div-deep 1))
                               (setq build-string ""))
                              ;; fixme: regexp matching was giving me trouble, but ideally
                              ;; I would compress these.
                              ((equal build-string "<div")
                               (setq div-deep (+ div-deep 1))
                               (setq build-string ""))
                              
                              ((equal build-string "span")
                               (setq build-string ""))
                              
                              ((equal build-string "<span")
                               (setq build-string ""))
                              
                              ((equal build-string "/div")
                               (setq div-deep (- div-deep 1))
                               (setq build-string ""))
                              
                              ((equal build-string "</div")
                               (setq div-deep (- div-deep 1))
                               (setq build-string ""))
                              
                              ((equal build-string "/span")
                               (setq build-string ""))
                              
                              ((equal build-string "</span")
                               (setq build-string ""))
                              
                              ((equal build-string "a")
                               (setq build-string ""))
                              
                              ((equal build-string "<a")
                               (setq build-string ""))
                              
                              ((equal build-string "/a")
                               (setq build-string ""))
                              
                              ((equal build-string "</a")
                               (setq build-string ""))
                              
                              ((equal (char-after) ?>)
                               (setq state :action))
                              
                              ((equal (char-after) ?=)
                               (setq state :action))
                              
                              (t
                               (setq build-string (concat build-string
                                                          (char-to-string (char-after)))))))
                       (t
                        (setq build-string (concat build-string
                                                   (char-to-string (char-after)))))))
                ((equal state :action)
                 (cond ((equal build-string "")
                        (setq state :out))
                       ((equal build-string "class")

                        (setq build-string "")
                        (setq state :class-munch))
                       (t
                        (setq build-string "")
                        (setq state :out))))
                ((equal state :class-munch)
                 (cond ((equal (char-after) ?\")
                        (setq state :class-do))
                       (t
                        (setq build-string (concat build-string (char-to-string (char-after)))))))
                ((equal state :class-do)
                 (cond ((string-match "result" build-string)
                        (setq build-string "")
                        (setq final-string (concat final-string "\nResult: "))
                        (setq state :out))
                       ((string-match "kind" build-string)
                        (setq build-string "")
                        (search-forward ">")
                        (backward-char) ;; to compensate for forward-char
                        (setq state :data-munch))
                       ((string-match "name" build-string)
                        (setq build-string "")
                        (search-forward ">")
                        (backward-char) ;; to compensate for forward-char
                        (setq state :data-munch))
                       ((string-match "description" build-string)
                        (setq build-string "\n    ")
                        (search-forward ">")
                        (backward-char) ;; to compensate for forward-char
                        (setq state :data-munch))
                       (t
                        (setq build-string "")
                        (setq state :out))))
                ((equal state :data-munch)
                 (cond ((equal (char-after) ?<)
                        (setq final-string (concat final-string " " build-string))
                        (setq build-string "")
                        (setq state :in))
                       (t
                        (setq build-string (concat build-string (char-to-string (char-after))))))))
          (forward-char))
        (delete-region (length leading-string) (point-max))
        (newline)
        (insert final-string)))
    (display-buffer search-buffer)))

(provide 'sourcegraph)

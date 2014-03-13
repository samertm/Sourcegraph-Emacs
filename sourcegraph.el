;;; sourcegraph.el --- find the code you need quickly

;; Copyright (C) 2014 Samer Masterson

;; Author: Samer T. Masterson <samer@samertm.com>
;; Keywords: ???

;;; Commentary:

;; Correct form for url:
;; "https://sourcegraph.com/api/search?q=django&exported=1&_via=sourcegraph-emacs-01"

;; Should I use defvar for the via string?

;; Currently using overlays for buttons with the button package.
;; Might switch to text properties if performance is an issue.

;; How does namespacing work in emacs? How do I keep from having
;; name clashes?

;; Change echo text on successful query

;; Requires libxml2 support

;;; Code:

(require 'json)
(require 'button)


;; todo: error catching
(defun parse-json (url)
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
    ;; state `:watch' reads the character and decides what to do
    ;; state `:in-tag' moves outside of a <tag>
    ;; state `:munch' munches the data and returns
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

;; html must be a lisp object in the form returned by the call
;; `libxml-parse-html-region'. html-text is a string, which starts as "".
;; maybe set html-text to an optional arg?

(defun text-from-html (html)
  (let ((values (cdr (cdr html)))
        (html-text ""))
    (while (car values)
      (let ((value (car values)))
        (cond ((stringp value)
               (setq html-text (concat html-text value)))
              (t
               (setq html-text (concat html-text (text-from-html value)))))
      (setq values (cdr values))))
    html-text))

(defun write-examples-text (json-vector name)
  (insert (format "Examples for %s\n\n\n" name))
  (let ((json-index 0))
    (while (< json-index (length json-vector))
      (let* ((json (elt json-vector json-index))
             (html-text (plist-get (plist-get json 'src) 'src))
             html)
        (insert "repo: " (plist-get json 'repo)
                " file: " (plist-get json 'file)
                "\n\n===start code===\n")
        (with-temp-buffer
          (insert html-text)
          (setq html (libxml-parse-html-region (point-min) (point-max))))
        (insert (text-from-html html)
                "\n=== end code ===\n\n"))
      (setq json-index (1+ json-index)))))
           
(defun nav-to-examples (button)
  (if (not (fboundp 'libxml-parse-html-region))
      (message "Emacs must be compiled with libxml2 support") ;; TODO or open webpage
    (let* ((sid (overlay-get button 'sid))
           (url (format "https://sourcegraph.com/api/refs?sid=%s" sid)) ;; add via?
           (json-vector (parse-json url))
           (name (overlay-get button 'name)))
      ;; TODO make this call more rebust
      (set-buffer "*Sourcegraph Search*") 
      (delete-region (point-min) (point-max))
      (write-examples-text json-vector name))))

;; change to insert string into buffer
(defun write-symbols-text (json-vector search-terms)
  (let* ((json-index 0))
    (insert (format "Sourcegraph Search Results for %s\n\n\n" search-terms))
    (while (< json-index (length json-vector))
      (let ((json (elt json-vector json-index))
            (build-string ""))
        (insert "Result: ")
        (insert (plist-get json 'specificKind))
        (insert "\t\t")
        (insert-button (plist-get json 'specificPath)
                       'name (plist-get json 'specificPath) ; TODO get name from overlay
                       'action 'nav-to-examples
                       'sid (plist-get json 'sid)
                       'follow-link t)
        (insert "\t\t")
        (insert-button (plist-get json 'repo))
        (if (plist-get json 'doc)
            (insert "\n    " (first-sentence (plist-get json 'doc))))
        (if (< json-index (1- (length json-vector)))
            (insert "\n\n"))
        (setq json-index (1+ json-index))))))
    

(defun sourcegraph-search-site ()
  (interactive)
  (let* ((input-string (read-string "Search Sourcegraph: "))
         (search-string (replace-regexp-in-string " " "+" input-string))
         (via "sourcegraph-emacs-01")
         (url-string (format "https://sourcegraph.com/api/search?q=%s&exported=1&_via=%s"
                             search-string
                             via))
         (buffer (get-buffer-create "*Sourcegraph Search*"))
         (json (parse-json url-string)))
    (with-current-buffer buffer
      (delete-region (point-min) (point-max))
      (write-symbols-text json input-string)
      (goto-char (point-min)))
    (display-buffer buffer)))

(provide 'sourcegraph)

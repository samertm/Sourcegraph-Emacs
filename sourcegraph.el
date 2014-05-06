;;; sourcegraph.el --- find the code you need quickly

;; Copyright (C) 2014 Samer Masterson

;; Author: Samer T. Masterson <samer@samertm.com>
;; Keywords: ???

;;; Commentary:

;; Call `sourcegraph-search-site' to use the package.
;; Currently using overlays for buttons with the button package.
;; Might switch to text properties if performance is an issue.

;;; Code:

;; ---- Requirements ----
(require 'json)
(require 'button)

;; ---- Global variables ----
(defvar sourcegraph-via "sourcegraph-emacs-01"
  "Indicates the version of the package when accessing sourcegraph")

(defvar sourcegraph-api-url "https://sourcegraph.com/api/"
  "Base string for accessing the api")

;; ---- Functions ----

;; TODO error catching
(defun sourcegraph-parse-json (url)
  "Parse the array of json objects from query to sourcegraph.com."
  (let* ((json-object-type 'plist)
         (json-key-type 'symbol) ; setting explicitly
         (json-buffer (url-retrieve-synchronously url))
         (parsed-json (vector)))
    (with-current-buffer json-buffer
      ;; delete headers
      (goto-char (point-min))
      (delete-region (point-min) (- (search-forward "[") 1)) ; TODO generalize for all json
      ;; set-up for json-read
      (goto-char (point-min))
      (setq parsed-json (json-read)))
    (kill-buffer json-buffer)
    parsed-json))

(defun sourcegraph-first-sentence (string)
  "Get first sentence from `string'.
The variable `string' is expected to be HTML formatted.

There are four states, :watch, :in-tag, :munch, and :done.
The state :watch reads the character and decides what to do.
The state :in-tag moves outside of a <tag>.
The state :munch appends the character to build-string
The state :done exits the state machine.
At every step of the state machine, the string index is advanced."
  (let ((build-string "")
        (index 0)
        (deep 0)
        (state :watch))
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
               ;; TODO handle errors from index+1 being out of bounds for string
               (cond ((eq curr-char ?<)
                      (setq state :done))
                     ((and (eq curr-char ?.) (eq (elt string (1+ index)) ?\s))
                      (setq build-string (concat build-string (char-to-string curr-char)))
                      (setq state :done))
                     (t
                      (setq build-string (concat build-string (char-to-string curr-char)))
                      (setq index (1+ index))))))))
    build-string))

(defun sourcegraph-text-from-html (html)
  "Return the text in html without any tags.

The variable html is a string consisting of HTML.
There are four states:
The state :out-tag moves to :in-tag if it sees '<', otherwise
it appends the char to html-text.
The state :in-tag moves to the outside of the tag.
The state :code reads an html code and appends it to html-text.
The state :done exists the state machine."
  (let ((state :out-tag)
        (html-index 0)
        (html-text "")
        (html-escape-chars '(("#34" . ?\") ; TODO test for which codes to add
                             ("quot" . ?\")
                             ("#35" . ?#)
                             ("#38" . ?&)
                             ("amp" . ?&)
                             ("#59" . ?\;)))
        (html-escape-code ""))
    (while (< html-index (length html))
      (let ((value (elt html html-index)))
        (cond ((eq state :out-tag)
               (cond ((eq value ?<)
                      (setq state :in-tag))
                     ((eq value ?&)
                      (setq state :code))
                     (t                 ; TODO append char without converting
                      (setq html-text (concat html-text (char-to-string value))))))
              ((eq state :in-tag)
               (if (eq value ?>)
                   (setq state :out-tag)))
              ((eq state :code)
               (cond ((eq value ?\;)
                      (if (assoc html-escape-code html-escape-chars)
                          (setq html-text
                                (concat html-text
                                        (char-to-string (cdr (assoc html-escape-code
                                                                    html-escape-chars)))))
                        (setq html-text (concat html-text "&" html-escape-code ";")))
                      (setq html-escape-code "")
                      (setq state :out-tag))
                     ((> (length html-escape-code) 4)
                      (setq html-text (concat html-text "&" html-escape-code))
                      (setq html-escape-code "")
                      (setq state :out-tag))
                     (t
                      (setq html-escape-code
                            (concat html-escape-code (char-to-string value))))))))
      (setq html-index (1+ html-index)))
    html-text))

(defun sourcegraph-strip-github (url)
  "Take a github url and return the tail.
For example, if url is 'github.com/samertm/Sourcegraph-Emacs',
sourcegraph-strip-github will return 'samertm/Sourcegraph-Emacs'"
  (if (string-match "\\(github\\.com/\\)\\(.+\\)" url)
      (match-string 2 url) ;; url must be passed in b/c it was used with string-match
    url))

(defun sourcegraph-nav-to-examples (button)
  "Feed api call to `sourcegraph-write-examples-text'."
  (let* ((sid (overlay-get button 'sid))
         (url (format (concat sourcegraph-api-url "refs?sid=%s&_via=%s")
                      sid sourcegraph-via))
         (json-vector (sourcegraph-parse-json url))
         (search-terms (overlay-get button 'search-terms))
         (name (overlay-get button 'name)))
    ;; TODO make this call more rebust
    (set-buffer "*Sourcegraph Search*") 
    (delete-region (point-min) (point-max))
    (sourcegraph-write-examples-text json-vector name search-terms)
    (goto-char (point-min))))

(defun sourcegraph-nav-to-srcgraph-url (button)
  "Open url associated with button in browser."
  (let ((url (format "https://sourcegraph.com/%s" (overlay-get button 'name))))
    (browse-url url)))

(defun sourcegraph-nav-to-search (button)
  "Call sourcegraph-search-site with button prop search-terms."
  (let ((search-terms (overlay-get button 'search-terms)))
    (sourcegraph-search-site search-terms)))

(defun sourcegraph-write-examples-text (json-vector name &optional prev-search-terms)
  "Write usage examples to a buffer"
  (insert (format "Examples for %s\n" name))
  (if (not (string= "" prev-search-terms))
      (progn
        (insert-button "[back]"
                       'search-terms prev-search-terms
                       'action 'sourcegraph-nav-to-search
                       'follow-link t)))
  (insert "\n\n")
  (let ((json-index 0))
    (while (< json-index (length json-vector))
      (let* ((json (elt json-vector json-index))
             (html-text (plist-get (plist-get json 'src) 'src))
             html)
        (insert "repo: ")
        (insert-button (sourcegraph-strip-github (plist-get json 'repo))
                       'name (plist-get json 'repo)
                       'action 'sourcegraph-nav-to-srcgraph-url
                       'follow-link t)
        (insert " file: ")
        (insert-button (plist-get json 'file)
                       'name (concat (plist-get json 'repo)
                                     "/tree/master/"
                                     (plist-get json 'file))
                       'action 'sourcegraph-nav-to-srcgraph-url
                       'follow-link t)
        (insert "\n\n")
        (let ((point-start (point))
              overlay)
          (insert (sourcegraph-text-from-html html-text))
          (insert "\n")
          (setq overlay (make-overlay point-start (point)))
          (overlay-put overlay 'face '(background-color . "LightGrey")))
        (insert "\n\n"))
      (setq json-index (1+ json-index)))))

(defun sourcegraph-write-symbols-text (json-vector search-terms)
  "Write search results to buffer"
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
                       'search-terms search-terms
                       'action 'sourcegraph-nav-to-examples
                       'sid (plist-get json 'sid)
                       'follow-link t)
        (insert "\t\t")
        (insert-button (sourcegraph-strip-github (plist-get json 'repo))
                       'name (plist-get json 'repo) ; TODO get name from overlay
                       'action 'sourcegraph-nav-to-srcgraph-url
                       'follow-link t)
        (if (plist-get json 'doc)
            (insert "\n    " (sourcegraph-first-sentence (plist-get json 'doc))))
        (if (< json-index (1- (length json-vector)))
            (insert "\n\n"))
        (setq json-index (1+ json-index))))))

(defun sense-environment ()
  "Sense the programming language in current buffer.
Return nil if no environment can be sensed. Return a string
with the programming language and function at the point
if either can be sensed.

The extra space after programming language names is
intentional. Compare:
Search Sourcegraph: python<point here>
Search Sourcegraph: python <point here>"
  (let ((name (buffer-name))
        env )
    (cond ((string-match ".+\.rb" name)
           (setq env "ruby "))
          ((string-match ".+\.py" name)
           (setq env "python "))
          ((string-match ".+\.js" name)
           (setq env "javascript "))
          ((string-match ".+\.go" name)
           (setq env "go ")))
    (setq fn-name (symbol-name (symbol-at-point)))
    (if fn-name
        (setq env (concat env fn-name)))
    env))

(defun sourcegraph-search-site (&optional search-terms)
  "Search Sourcegraph for usage examples for programming libraries"
  (interactive)
  (let* ((env (sense-environment))
         (input-string (cond ((and search-terms
                                   (not (string= "" search-terms))) search-terms)
                             (env (read-string "Search Sourcegraph: " env))
                             (t (read-string "Search Sourcegraph: "))))
         (search-string (replace-regexp-in-string " " "+" input-string))
         (url-string (format (concat sourcegraph-api-url "search?q=%s&exported=1&_via=%s")
                             search-string
                             sourcegraph-via))
         (buffer (get-buffer-create "*Sourcegraph Search*"))
         (json (sourcegraph-parse-json url-string)))
    (with-current-buffer buffer
      (delete-region (point-min) (point-max))
      (sourcegraph-write-symbols-text json input-string)
      (goto-char (point-min)))
    (display-buffer buffer)))

(provide 'sourcegraph)

;;; sourcegraph.el ends here

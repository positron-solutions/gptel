;;; gptel-org.el --- Org functions for gptel         -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Karthik Chikmagalur

;; Author: Karthik Chikmagalur <karthikchikmagalur@gmail.com>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:
(eval-when-compile (require 'gptel))
(require 'cl-lib)
(require 'org-element)
(require 'outline)

;; Functions used for saving/restoring gptel state in Org buffers
(defvar gptel--num-messages-to-send)
(defvar org-entry-property-inherited-from)
(defvar gptel-backend)
(defvar gptel--known-backends)
(defvar gptel--system-message)
(defvar gptel-model)
(defvar gptel-temperature)
(defvar gptel-max-tokens)

(defvar org-link-angle-re)
(defvar org-link-bracket-re)
(declare-function mailcap-file-name-to-mime-type "mailcap")
(declare-function gptel--model-capable-p "gptel")
(declare-function gptel--model-mime-capable-p "gptel")
(declare-function gptel--model-name "gptel")
(declare-function gptel--to-string "gptel")
(declare-function gptel--to-number "gptel")
(declare-function gptel--intern "gptel")
(declare-function gptel--get-buffer-bounds "gptel")
(declare-function gptel-backend-name "gptel")
(declare-function gptel--parse-buffer "gptel")
(declare-function gptel--parse-directive "gptel")
(declare-function gptel--with-buffer-copy "gptel")
(declare-function org-entry-get "org")
(declare-function org-entry-put "org")
(declare-function org-with-wide-buffer "org-macs")
(declare-function org-set-property "org")
(declare-function org-property-values "org")
(declare-function org-open-line "org")
(declare-function org-at-heading-p "org")
(declare-function org-get-heading "org")
(declare-function org-at-heading-p "org")

;; Bundle `org-element-lineage-map' if it's not available (for Org 9.67 or older)
(eval-and-compile
  (if (fboundp 'org-element-lineage-map)
      (progn (declare-function org-element-lineage-map "org-element-ast")
             (defalias 'gptel-org--element-lineage-map 'org-element-lineage-map))
    (defun gptel-org--element-lineage-map (datum fun &optional types with-self first-match)
      "Map FUN across ancestors of DATUM, from closest to furthest.

DATUM is an object or element.  For TYPES, WITH-SELF and
FIRST-MATCH see `org-element-lineage-map'.

This function is provided for compatibility with older versions
of Org."
      (declare (indent 2))
      (setq fun (if (functionp fun) fun `(lambda (node) ,fun)))
      (let ((up (if with-self datum (org-element-parent datum)))
	    acc rtn)
        (catch :--first-match
          (while up
            (when (or (not types) (org-element-type-p up types))
              (setq rtn (funcall fun up))
              (if (and first-match rtn)
                  (throw :--first-match rtn)
                (when rtn (push rtn acc))))
            (setq up (org-element-parent up)))
          (nreverse acc)))))
  (if (fboundp 'org-element-begin)
      (progn (declare-function org-element-begin "org-element")
             (declare-function org-element-end "org-element")
             (defalias 'gptel-org--element-begin 'org-element-begin)
             (defalias 'gptel-org--element-end 'org-element-end))
    (defun gptel-org--element-begin (node)
      "Get `:begin' property of NODE."
      (org-element-property :begin node))
    (defun gptel-org--element-end (node)
      "Get `:end' property of NODE."
      (org-element-property :end node))))

(defcustom gptel-org-ignore-elements '(property-drawer)
  "List of Org elements that should be stripped from the prompt
before sending it.

By default gptel will remove Org property drawers from the
prompt.  For the full list of available elements, please see
`org-element-all-elements'.

Please note: Removing property-drawer elements is fast, but
adding elements to this list can significantly slow down
`gptel-send'."
  :group 'gptel
  :type '(repeat symbol))

;; NOTE: This can be converted to a cl-defmethod for `gptel--parse-buffer'
;; (conceptually cleaner), but will cause load-order issues in gptel.el and
;; might be harder to debug.
(defun gptel-org--create-prompt (&optional prompt-end)
  "Return a full conversation prompt from the contents of this Org buffer.

If `gptel--num-messages-to-send' is set, limit to that many
recent exchanges.

The prompt is constructed from the contents of the buffer up to
point, or PROMPT-END if provided."
  (unless prompt-end (setq prompt-end (point)))
  (let ((max-entries (and gptel--num-messages-to-send
                          (* 2 gptel--num-messages-to-send))))
    (let ((org-buf (current-buffer))
            (beg (point-min)) (end (point-max)))
        (gptel--with-buffer-copy org-buf beg end
          (gptel-org--unescape-tool-results)
          (gptel-org--strip-elements)
          (gptel-org--strip-block-headers)
          (when gptel-org-ignore-elements (gptel-org--strip-elements))
          (save-excursion (run-hooks 'gptel-prompt-filter-hook))
          (gptel--parse-buffer gptel-backend max-entries)))))

(defun gptel-org--strip-elements ()
  "Remove all elements in `gptel-org-ignore-elements' from the
prompt."
  (let ((major-mode 'org-mode) element-markers)
    (if (equal '(property-drawer) gptel-org-ignore-elements)
        (save-excursion
          (goto-char (point-min))
          (while (re-search-forward org-property-drawer-re nil t)
            ;; ;; Slower but accurate
            ;; (let ((drawer (org-element-at-point)))
            ;;   (when (org-element-type-p drawer 'property-drawer)
            ;;     (delete-region (org-element-begin drawer) (org-element-end drawer))))

            ;; Fast but inexact, can have false positives
            (delete-region (match-beginning 0) (match-end 0))))
      ;; NOTE: Parsing the buffer is extremely slow.  Avoid this path unless
      ;; required.
      ;; NOTE: `org-element-map' takes a third KEEP-DEFERRED argument in newer
      ;; Org versions
      (org-element-map (org-element-parse-buffer 'element nil)
          gptel-org-ignore-elements
        (lambda (node)
          (push (list (gptel-org--element-begin node)
                      (gptel-org--element-end node))
                element-markers)))
      (dolist (bounds element-markers)
        (apply #'delete-region bounds)))))

(defun gptel-org--strip-block-headers ()
  "Remove all gptel-specific block headers and footers.
Every line that matches will be removed entirely.

This removal is necessary to avoid auto-mimicry by LLMs."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward
            (rx line-start (literal "#+")
                (or (literal "begin") (literal "end"))
                (or (literal "_tool") (literal "_reasoning")))
            nil t)
      (delete-region (match-beginning 0)
                     (min (point-max) (1+ (line-end-position)))))))

(defun gptel-org--unescape-tool-results ()
  "Undo escapes done to keep results from escaping blocks.
Scans backward for gptel tool text property, reads the arguments, then
unescapes the remainder."
  (save-excursion
    (goto-char (point-max))
    (let ((prev-pt (point)))
      (while (> prev-pt (point-min))
        (goto-char
         (previous-single-char-property-change (point) 'gptel))
        (let ((prop (get-text-property (point) 'gptel))
              (backward-progress (point)))
          (when (eq (car-safe prop) 'tool)
            ;; User edits to clean up can potentially insert a tool-call header
            ;; that is propertized.  Tool call headers should not be
            ;; propertized.
            (when (looking-at-p "[[:space:]]*#\\+begin_tool")
              (goto-char (match-end 0)))
            (condition-case nil
                (read (current-buffer))
              ((end-of-file invalid-read-syntax)
               (message "Could not read tool arguments")))
            ;; TODO this code is able to put the point behind prev-pt, which
            ;; makes the region inverted.  The `max' catches this, but really
            ;; `read' and `looking-at' are the culprits.  Badly formed tool
            ;; blocks can lead to this being necessary.
            (org-unescape-code-in-region
             (min prev-pt (point)) prev-pt))
          (goto-char (setq prev-pt backward-progress)))))))

;; Handle media links in the buffer
(cl-defmethod gptel--parse-media-links ((_mode (eql 'org-mode)) beg end)
  "Parse text and actionable links between BEG and END.

Return a list of the form
 ((:text \"some text\")
  (:media \"/path/to/media.png\" :mime \"image/png\")
  (:text \"More text\"))
for inclusion into the user prompt for the gptel request."
  (require 'mailcap)                    ;FIXME Avoid this somehow
  (let ((parts) (from-pt)
        (link-regex (concat "\\(?:" org-link-bracket-re "\\|"
                            org-link-angle-re "\\)")))
    (save-excursion
      (setq from-pt (goto-char beg))
      (while (re-search-forward link-regex end t)
        (when-let* ((link (org-element-context))
                    ((gptel-org--link-standalone-p link))
                    (raw-link (org-element-property :raw-link link))
                    (path (org-element-property :path link))
                    (type (org-element-property :type link))
                    ;; FIXME This is not a good place to check for url capability!
                    ((member type `("attachment" "file"
                                    ,@(and (gptel--model-capable-p 'url)
                                       '("http" "https" "ftp")))))
                    (mime (mailcap-file-name-to-mime-type path))
                    ((gptel--model-mime-capable-p mime)))
          (cond
           ((member type '("file" "attachment"))
            (when (file-readable-p path)
              ;; Collect text up to this image, and
              ;; Collect this image
              (when-let* ((text (string-trim (buffer-substring-no-properties
                                              from-pt (gptel-org--element-begin link)))))
                (unless (string-empty-p text) (push (list :text text) parts)))
              (push (list :media path :mime mime) parts)
              (setq from-pt (point))))
           ((member type '("http" "https" "ftp"))
            ;; Collect text up to this image, and
            ;; Collect this image url
            (when-let* ((text (string-trim (buffer-substring-no-properties
                                            from-pt (gptel-org--element-begin link)))))
              (unless (string-empty-p text) (push (list :text text) parts)))
            (push (list :url raw-link :mime mime) parts)
            (setq from-pt (point))))))
      (unless (= from-pt end)
        (push (list :text (buffer-substring-no-properties from-pt end)) parts)))
    (nreverse parts)))

(defun gptel-org--link-standalone-p (object)
  "Check if link OBJECT is on a line by itself."
  ;; Specify ancestor TYPES as list (#245)
  (when-let* ((par (org-element-lineage object '(paragraph))))
    (and (= (gptel-org--element-begin object)
            (save-excursion
              (goto-char (org-element-property :contents-begin par))
              (skip-chars-forward "\t ")
              (point)))                 ;account for leading space before object
         (<= (- (org-element-property :contents-end par)
                (org-element-property :end object))
             1))))


;;; Transforming responses
(defun gptel--convert-markdown->org (str)
  "Convert string STR from markdown to org markup.

This is a very basic converter that handles only a few markup
elements."
  (with-temp-buffer
    (insert str)
    (goto-char (point-min))
    (while (re-search-forward "`+\\|\\*\\{1,2\\}\\|_\\|^#+" nil t)
      (pcase (match-string 0)
        ;; Handle backticks
        ((and (guard (eq (char-before) ?`)) ticks)
         (gptel--replace-source-marker (length ticks))
         (save-match-data
           (catch 'block-end
             (while (search-forward ticks nil t)
               (unless (or (eq (char-before (match-beginning 0)) ?`)
                           (eq (char-after) ?`))
                 (gptel--replace-source-marker (length ticks) 'end)
                 (throw 'block-end nil))))))
        ;; Handle headings
        ((and (guard (eq (char-before) ?#)) heading)
         (cond
          ((looking-at "[[:space:]]")   ;Handle headings
           (delete-region (line-beginning-position) (point))
           (insert (make-string (length heading) ?*)))
          ((looking-at "\\+begin_src") ;Overeager LLM switched to using Org src blocks
           (save-match-data (re-search-forward "^#\\+end_src" nil t)))))
        ;; Handle emphasis
        ("**" (cond
               ;; ((looking-at "\\*\\(?:[[:word:]]\\|\s\\)")
               ;;  (delete-char 1))
               ((looking-back "\\(?:[[:word:][:punct:]\n]\\|\s\\)\\*\\{2\\}"
                              (max (- (point) 3) (point-min)))
                (delete-char -1))))
        ("*"
         (cond
          ((save-match-data
             (and (or (= (point) 2)
                      (looking-back "\\(?:[[:space:]]\\|\s\\)\\(?:_\\|\\*\\)"
                                    (max (- (point) 2) (point-min))))
                  (not (looking-at "[[:space:]]\\|\s"))))
           ;; Possible beginning of emphasis
           (and
            (save-excursion
              (when (and (re-search-forward (regexp-quote (match-string 0))
                                            (line-end-position) t)
                         (looking-at "[[:space]]\\|\s")
                         (not (looking-back "\\(?:[[:space]]\\|\s\\)\\(?:_\\|\\*\\)"
                                            (max (- (point) 2) (point-min)))))
                (delete-char -1) (insert "/") t))
            (progn (delete-char -1) (insert "/"))))
          ((save-excursion
             (ignore-errors (backward-char 2))
             (or (and (bobp) (looking-at "\\*[[:space:]]"))
                 (looking-at "\\(?:$\\|\\`\\)\n\\*[[:space:]]")))
           ;; Bullet point, replace with hyphen
           (delete-char -1) (insert "-"))))))
    (buffer-string)))

(defun gptel--replace-source-marker (num-ticks &optional end)
  "Replace markdown style backticks with Org equivalents.

NUM-TICKS is the number of backticks being replaced.  If END is
true these are \"ending\" backticks.

This is intended for use in the markdown to org stream converter."
  (let ((from (match-beginning 0)))
    (delete-region from (point))
    (if (and (= num-ticks 3)
             (save-excursion (beginning-of-line)
                             (skip-chars-forward " \t")
                             (eq (point) from)))
        (insert (if end "#+end_src" "#+begin_src "))
      (insert "="))))

(defun gptel--stream-convert-markdown->org (start-marker)
  "Return a Markdown to Org converter.

This function parses a stream of Markdown text to Org
continuously when it is called with successive chunks of the
text stream.

START-MARKER is used to identify the corresponding process when
cleaning up after."
  (letrec ((in-src-block nil)           ;explicit nil to address BUG #183
           (in-org-src-block nil)
           (temp-buf (generate-new-buffer " *gptel-temp*" t))
           (start-pt (make-marker))
           (ticks-total 0)      ;MAYBE should we let-bind case-fold-search here?
           (cleanup-fn
            (lambda (beg _)
              (when (and (equal beg (marker-position start-marker))
                         (eq (current-buffer) (marker-buffer start-marker)))
                (when (buffer-live-p (get-buffer temp-buf))
                  (set-marker start-pt nil)
                  (kill-buffer temp-buf))
                (remove-hook 'gptel-post-response-functions cleanup-fn)))))
    (add-hook 'gptel-post-response-functions cleanup-fn)
    (lambda (str)
      (let ((noop-p) (ticks 0))
        (with-current-buffer (get-buffer temp-buf)
          (save-excursion (goto-char (point-max)) (insert str))
          (when (marker-position start-pt) (goto-char start-pt))
          (when in-src-block (setq ticks ticks-total))
          (save-excursion
            (while (re-search-forward "`\\|\\*\\{1,2\\}\\|_\\|^#+" nil t)
              (pcase (match-string 0)
                ("`"
                 ;; Count number of consecutive backticks
                 (backward-char)
                 (while (and (char-after) (eq (char-after) ?`))
                   (forward-char)
                   (if in-src-block (cl-decf ticks) (cl-incf ticks)))
                 ;; Set the verbatim state of the parser
                 (if (and (eobp)
                          ;; Special case heuristic: If the response ends with
                          ;; ^``` we don't wait for more input.
                          ;; FIXME: This can have false positives.
                          (not (save-excursion (beginning-of-line)
                                               (looking-at "^```$"))))
                     ;; End of input => there could be more backticks coming,
                     ;; so we wait for more input
                     (progn (setq noop-p t) (set-marker start-pt (match-beginning 0)))
                   ;; We reached a character other than a backtick
                   (cond
                    ;; Ticks balanced, end src block
                    ((= ticks 0)
                     (progn (setq in-src-block nil)
                            (gptel--replace-source-marker ticks-total 'end)))
                    ;; Positive number of ticks, start an src block
                    ((and (> ticks 0) (not in-src-block))
                     (setq ticks-total ticks
                           in-src-block t)
                     (gptel--replace-source-marker ticks-total))
                    ;; Negative number of ticks or in a src block already,
                    ;; reset ticks
                    (t (setq ticks ticks-total)))))
                ;; Handle headings and misguided #+begin_src text
                ((and (guard (and (eq (char-before) ?#) (or (not in-src-block) in-org-src-block)))
                      heading)
                 (if in-org-src-block
                     ;; If we are inside an Org-style src block, look for #+end_src
                     (cond
                      ((< (- (point-max) (point)) 8) ;not enough information to close Org src block
                       (setq noop-p t) (set-marker start-pt (match-beginning 0)))
                      ((looking-at "\\+end_src") ;Close Org src block
                       (setq in-src-block nil in-org-src-block nil)))
                   ;; Otherwise check for Markdown headings, or for #+begin_src
                   (cond
                    ((eobp)       ; Not enough information about the heading yet
                     (setq noop-p t) (set-marker start-pt (match-beginning 0)))
                    ((looking-at "[[:space:]]") ; Convert markdown heading to Org heading
                     (delete-region (line-beginning-position) (point))
                     (insert (make-string (length heading) ?*)))
                    ((< (- (point-max) (point)) 11) ;Not enough information to check if Org src block
                     (setq noop-p t) (set-marker start-pt (match-beginning 0)))
                    ((looking-at "\\+begin_src ") ;Overeager LLM switched to using Org src blocks
                     (setq in-src-block t in-org-src-block t)))))
                ;; Handle other chars: emphasis, bold and bullet items
                ((and "**" (guard (not in-src-block)))
                 (cond
                  ;; TODO Not sure why this branch was needed
                  ;; ((looking-at "\\*\\(?:[[:word:]]\\|\s\\)") (delete-char 1))

                  ;; Looking back at "w**" or " **"
                  ((looking-back "\\(?:[[:word:][:punct:]\n]\\|\s\\)\\*\\{2\\}"
                                 (max (- (point) 3) (point-min)))
                   (delete-char -1))))
                ((and "*" (guard (not in-src-block)))
                 (if (eobp)
                     ;; Not enough information about the "*" yet
                     (progn (setq noop-p t) (set-marker start-pt (match-beginning 0)))
                   ;; "*" is either emphasis or a bullet point
                   (save-match-data
                     (save-excursion
                       (ignore-errors (backward-char 2))
                       (cond      ; At bob, underscore/asterisk followed by word
                        ((or (and (bobp) (looking-at "\\(?:_\\|\\*\\)\\([^[:space:][:punct:]]\\|$\\)"))
                             (looking-at ; word followed by underscore/asterisk
                              "[^[:space:][:punct:]\n]\\(?:_\\|\\*\\)\\(?:[[:space:][:punct:]]\\|$\\)")
                             (looking-at ; underscore/asterisk followed by word
                              "\\(?:[[:space:][:punct:]]\\)\\(?:_\\|\\*\\)\\([^[:space:][:punct:]]\\|$\\)"))
                         ;; Emphasis, replace with slashes
                         (forward-char (if (bobp) 1 2)) (delete-char -1) (insert "/"))
                        ((or (and (bobp) (looking-at "\\*[[:space:]]"))
                             (looking-at "\\(?:$\\|\\`\\)\n\\*[[:space:]]"))
                         ;; Bullet point, replace with hyphen
                         (forward-char (if (bobp) 1 2)) (delete-char -1) (insert "-"))))))))))
          (if noop-p
              (buffer-substring (point) start-pt)
            (prog1 (buffer-substring (point) (point-max))
                   (set-marker start-pt (point-max)))))))))

(provide 'gptel-org)
;;; gptel-org.el ends here

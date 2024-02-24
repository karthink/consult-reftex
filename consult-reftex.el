;;; consult-reftex.el --- Consulting reftex completions -*- lexical-binding: t -*-

;; Author: Karthik Chickmagular
;; Homepage: https://github.com/karthink/consult-reftex
;; Keywords: bib, tex
;; Version: 0.0.1
;; Package-Requires: ((emacs "27.1"))

;;; Commentary:
;;
;; TODO

(require 'consult)
(require 'reftex)
(require 'cl-lib)
(require 'consult-reftex-preview)

;;; Code:

(defgroup consult-reftex nil
  "Consult interface to reftex."
  :group 'latex
  :group 'minibuffer
  :group 'consult
  :group 'reftex
  :prefix "consult-reftex-")

(defcustom consult-reftex-style-descriptions '(("\\ref" . "reference")
                                               ("\\Ref" . "Reference")
                                               ("\\eqref" . "equation ref")
                                               ("\\autoref" . "auto ref")
                                               ("\\pageref" . "page ref")
                                               ("\\footref" . "footnote ref")
                                               ("\\cref" . "clever ref")
                                               ("\\Cref" . "Clever Ref")
                                               ("\\cpageref" . "clever page ref")
                                               ("\\Cpageref" . "Clever Page Ref")
                                               ("\\vref" . "vario ref")
                                               ("\\Vref" . "Vario Ref")
                                               ("\\vpageref" . "Vario Page Ref")
                                               ("\\fref" . "fancy ref")
                                               ("\\Fref" . "Fancy Ref")
                                               ("\\autopageref" . "auto page ref"))
  "Alist of descriptions for reference types."
  :type '(alist :key-type (string :tag "Reference Command (Prefix)")
                :value-type (string :tag "Description"))
  :group 'consult-reftex)

;; TODO: better descriptions may be appropriate
(defcustom consult-reftex-citation-style-descriptions
  '(("\\possessivecite" . "Possesive citation")
    ("\\citeasnoun" . "noun citation")
    ("\\bibentry" . "whole reference")
    ("\\footfullcite" . "whole reference in footnote")
    ("\\fullocite" . "full object-cite")
    ("\\fullcite" . "full reference")
    ("\\ycite" . "year citations")
    ("\\ocite" . "object citation")
    ("\\autocite" . "automatic citation")
    ("\\smartcite" . "automatic citation")
    ("\\footcite" . "footnote citation")
    ("\\parencite" . "parenthetical citation")
    ("\\textcite" . "textual citation")
    ("\\nocite" . "show in references")
    ("\\shortciteA" . "annotated short citation")
    ("\\citeA" . "annotated citation")
    ("\\shortcite" . "short citation")
    ("\\citeN" . "Capitalized Citation")
    ("\\citefield" . "specific field")
    ("\\citeyear" . "year")
    ("\\citeauthory" . "cite author and year")
    ("\\citeauthor" . "author name")
    ("\\citename" . "author name")
    ("\\citeyear" . "year")
    ("\\citeaffixed" . "affixed citation")
    ("\\citep" . "parenthetical citation")
    ("\\citetitle" . "title")
    ("\\citet" . "textual citation")
    ("\\cite" . "general citation"))
  "Alist of descriptions for citation types."
  :type '(alist :key-type (string :tag "Citation Command (Prefix)")
                :value-type (string :tag "Description"))
  :group 'consult-reftex)

(defcustom consult-reftex-preferred-style-order '("\\ref")
  "Order of reference commands to determine default."
  :group 'consult-reftex
  :type '(repeat (string :tag "Command")))


;; Embark integration

(with-eval-after-load 'embark
  (defvar consult-reftex-label-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd ".") '("reference | goto label"     . reftex-goto-label))
      (define-key map (kbd "r") '("reference | parse file"     . reftex-parse-one))
      (define-key map (kbd "R") '("reference | parse document" . reftex-parse-all))
      (define-key map (kbd "%") '("reference | change label"   . reftex-change-label))
      (make-composed-keymap map embark-general-map))
    "keymap for consult-reftex actions")

  (defun consult-reftex-embark-export (_cands)
    (reftex-toc))

  (add-to-list 'embark-exporters-alist '(reftex-label . consult-reftex-embark-export))
  (add-to-list 'embark-keymap-alist '(reftex-label . consult-reftex-label-map)))


;; Internal Functions

(defun consult-reftex--compile-categories ()
  "Compile reference categories available in the current document."
  (let ((styles-available (reftex-uniquify-by-car
                           (reftex-splice-symbols-into-list
                            (append reftex-label-alist
                                    (get reftex-docstruct-symbol
                                         'reftex-label-alist-style)
                                    reftex-default-label-alist-entries)
                            reftex-label-alist-builtin)))
        (categories-alist (list)))
    (dolist (entry styles-available)
      (cl-destructuring-bind (env-or-macro key &rest _ignore) entry
        (when-let ((display-name (if (and (stringp env-or-macro)
                                          (string-match-p (rx (or "[" "{" "\\")) env-or-macro))
                                     (save-match-data
                                       (when (string-match (rx bol "\\" (group-n 1 (* alpha))) env-or-macro)
                                         (format "\\%s" (match-string 1 env-or-macro))))
                                   env-or-macro)))
          (setf (alist-get key categories-alist) (if-let (label (alist-get key categories-alist))
                                                     (cons display-name label)
                                                   (list display-name))))))
    (let ((new-categories-alist (mapcar (lambda (entry)
                                          (cons (format "[%c] %s" (car entry)
                                                        (string-join  (cl-remove-duplicates (reverse (delq nil (cdr entry)))
                                                                                            :test #'string=)
                                                                      ", "))
                                                (char-to-string (car entry))))
                                        (cl-remove-if (apply-partially #'= ? )
                                                      categories-alist :key #'car))))
      (sort new-categories-alist (lambda (a b) (string< (downcase (cdr a)) (downcase (cdr b))))))))

(defun consult-reftex-active-citation-styles ()
  "Determine active citation styles."
  (when-let ((current-citation-formats (nth 2 (assq (reftex-get-cite-format) reftex-cite-format-builtin))))
    (if (listp current-citation-formats)
        (mapcar #'cdr current-citation-formats)
      current-citation-formats)))

(defun consult-reftex-active-styles ()
  "Determine active reference styles."
  (apply #'append
         (mapcar (lambda (style)
                   (cadr (alist-get style reftex-ref-style-alist
                                    nil nil #'equal)))
                 (reftex-ref-style-list))))

(defun consult-reftex--find-preferred-command (available-styles)
  "Find a preferred style from AVAILABLE-STYLES."
  (let ((out-commands (list)))
    (dolist (command consult-reftex-preferred-style-order (or (car (reverse out-commands)) "\\ref"))
      (when (cl-member command available-styles :test #'string= :key #'car)
        (push command out-commands)))))

(defun consult-reftex--label-marker (label file open-fn)
  "Return marker corresponding to LABEL location in FILE.

File is opened as necessary with OPEN-FN."
  (let ((backward t) found buffer marker)
    (setq buffer (funcall open-fn file))
    (setq re (format reftex-find-label-regexp-format (regexp-quote label)))
    (with-current-buffer buffer
      (save-excursion
        (setq found
              (if backward
                  (re-search-backward re nil t)
                (re-search-forward re nil t)))
        (unless found
          (goto-char (point-min))
          (unless (setq found (re-search-forward re nil t))
            ;; Ooops.  Must be in a macro with distributed args.
            (setq found
                  (re-search-forward
                   (format reftex-find-label-regexp-format2
                           (regexp-quote label))
                   nil t))))))
    (if (match-end 3)
        (setq marker (set-marker (make-marker) (match-beginning 3) buffer)))))

(defun consult-reftex--replace-optional-arguments (citation &optional promptp)
  "Replace optional arguments in CITATION.

If PROMPTP, prompt for their values, else, clean-up following
`reftex-cite-cleanup-optional-args', which see."
  (let ((citation (substring-no-properties citation))
        (start 0) (nth 0))
    (save-match-data
      (while (and promptp
                  (setq start (string-match (rx ?[ ?]) citation start)))
        (save-match-data
          (let* ((new-value
                  (save-match-data
                    (read-string (progn
                                   (add-text-properties (match-beginning 0)
                                                        (match-end 0)
                                                        '(face escape-glyph)
                                                        citation)
                                   (format "Optional Argument %d (in %s): "
                                           (1+ nth)
                                           citation)))))
                 (replacement (format "[%s]" new-value)))
            (setq citation (replace-match replacement t t citation)
                  start (+ start (length replacement))
                  nth (1+ nth))))))
    (when reftex-cite-cleanup-optional-args
      (save-match-data
        (cond
         ((string-match (rx (group-n 1 (or alpha digit)) ?[ ?] ?{) citation)
          (setq citation (replace-match "\\1{" nil nil citation)))
         ((string-match (rx ?[ ?] (group-n 1 ?[ (+ (or alpha digit ?. ?, ? )) ?])) citation)
          (setq citation (replace-match "\\1" nil nil citation)))
         ((string-match (rx ?[ ?] ?[ ?]) citation)
          (setq citation (replace-match "" t t citation))))))
    (substring-no-properties citation)))

(defun consult-reftex--format-citations (command citations &optional separator)
  "Format CITATIONS using COMMAND.

If CITATIONS"
  (format-spec command (list (cons ?l (if (stringp citations)
                                          citations
                                        (string-join citations (or separator ",")))))))


;; Labelling/Annotation

(defun consult-reftex--label-candidates (prefix)
  "Find all references in current document (multi-file) using reftex.

With prefix arg PREFIX, rescan the document for references."
  (reftex-access-scan-info prefix)
  ;; (when (equal prefix 4) (reftex-parse-all))
  (let ((all-candidates))
    (dolist (entry (symbol-value reftex-docstruct-symbol) all-candidates)
      (when (stringp (car entry))
        (push (consult-reftex--make-annotation (car entry) (nth 2 entry) (nth 3 entry) (cadr entry))
              (alist-get (cadr entry) all-candidates nil nil 'string=))))))

(defun consult-reftex--make-annotation (key annotation file type)
  "Annotate KEY with ANNOTATION, TYPE and FILE if the latter is not nil."
  (cond
   ((not annotation) key)
   (t (propertize key 'reftex-annotation annotation
                  'reftex-file       file
                  'reftex-type       type))))

(defun consult-reftex--get-annotation (cand)
  "Get the annotation for CAND."
  (when-let ((ann (get-text-property 0 'reftex-annotation cand)))
    (concat (propertize " " 'display '(space :align-to center)) ann)))

(defun consult-reftex--annotate-reference-command (candidate)
  "Add annotation to CANDIDATE.

This is determined from `consult-reftex-style-descriptions',
which see."
  (concat (propertize " " 'display '(space :align-to center))
          (propertize (alist-get candidate
                                 consult-reftex-style-descriptions
                                 "label only" nil #'string-prefix-p)
                      'face 'consult-key)))

(defun consult-reftex--annotate-citation-command (candidate)
  "Annotate citation command CANDIDATE.

Annotations are determined by
`consult-reftex-citation-style-descriptions', which see."
  (concat (propertize " " 'display '(space :align-to center))
          (propertize (alist-get candidate
                                 consult-reftex-citation-style-descriptions
                                 "bib-key only" nil #'string-prefix-p)
                      'face 'consult-key)))


;; Reference Manipulation

(defvar consult-reftex--reference-history nil)

(defun consult-reftex-select-reference (&optional arg)
  "Select a label with consult-based completing-read.

If ARG, force a reparse for label candidates."
  (when-let* ((all-candidates (consult-reftex--label-candidates arg))
              (categories-list (consult-reftex--compile-categories))
              (categories-string (mapconcat #'cdr categories-list ""))
              (sources
               (mapcar (lambda (class) `(:name ,(car class)
                                               :narrow ,(string-to-char (cdr class))
                                               :category reftex-label
                                               :items ,(alist-get (cdr class) all-candidates
                                                                  nil nil 'string=)))
                       categories-list))
              (label (car (save-excursion
                            (consult--multi
                             sources
                             :sort nil
                             :prompt (format "Label (%s): " categories-string)
                             :require-match t
                             :category 'reftex-label
                             :preview-key (or (plist-get (consult--customize-get) :preview-key)
                                              consult-preview-key)
                             :history 'consult-reftex--reference-history
                             :state  (funcall consult-reftex-preview-function)
                             :annotate #'consult-reftex--get-annotation)))))
    label))

;;;###autoload
(defun consult-reftex-insert-reference (&optional arg no-insert)
  "Insert reference with completion.

With prefix ARG rescan the document.  If NO-INSERT, only format
the reference."
  (interactive "P")
  (when-let* ((label (consult-reftex-select-reference arg))
              (active-styles (consult-reftex-active-styles))
              (default-style (consult-reftex--find-preferred-command active-styles))
              (reference
               (consult--read
                (cons label
                      (mapcar (lambda (ref-type) (concat (car ref-type) "{" label "}"))
                              active-styles))
                :sort nil
                :default (concat default-style "{" label "}")
                :prompt "Reference: "
                :require-match t
                ;; :category 'reftex-label
                :annotate #'consult-reftex--annotate-reference-command)))
    (if no-insert reference (insert (substring-no-properties reference)))))

;;;###autoload
(defun consult-reftex-goto-label (label &optional _arg)
  "Select LABEL using Consult and jump to it."
  (interactive (list (consult-reftex-select-reference current-prefix-arg)
                     current-prefix-arg))
  (if-let* ((open (consult--temporary-files))
            (marker (consult-reftex--label-marker (substring-no-properties label)
                                                  (get-text-property 0 'reftex-file label)
                                                  open)))
      (consult--jump marker)))


;; Citation Support

;; TODO: Get citations from reftex data (reftex-extract-bib-entries & reftcex-extract-bib-entries-from-thebibliography)

;;;###autoload
(defun consult-reftex-citation (arg &optional citations)
  "Insert CITATIONS.

If ARG, interactively replace optional arguments in formatted
citation."
  (interactive "P")
  (when-let* ((citations (or citations
                             ;; TODO: get citations from reftex data
                             "foo:_bar" ;Temporary testing value
                             ))
              (selected-citation-format
               (consult--read (mapcar (lambda (command) (consult-reftex--format-citations command citations))
                                      (consult-reftex-active-citation-styles))
                              :sort nil
                              :prompt "Citation: "
                              :require-match t
                              :annotate #'consult-reftex--annotate-citation-command))
              (formatted-citation (consult-reftex--replace-optional-arguments selected-citation-format arg)))
    (insert formatted-citation)))

(provide 'consult-reftex)
;;; consult-reftex.el ends here

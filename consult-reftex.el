;;; consult-reftex.el --- Consulting reftex completions -*- lexical-binding: t -*-
(require 'consult)
(require 'reftex)
(require 'cl-lib)
(require 'embark)
(require 'consult-reftex-preview)

(defgroup consult-reftex nil
  "Consult interface to reftex."
  :group 'latex
  :group 'minibuffer
  :group 'consult
  :prefix "consult-reftex-")

(add-to-list 'embark-keymap-alist '(reftex-label . consult-reftex-label-map))

(defvar consult-reftex-label-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd ".") '("reference | goto label"     . reftex-goto-label))
    (define-key map (kbd "r") '("reference | parse file"     . reftex-parse-one))
    (define-key map (kbd "R") '("reference | parse document" . reftex-parse-all))
    (define-key map (kbd "%") '("reference | change label"   . reftex-change-label))
    (make-composed-keymap map embark-general-map))
  "keymap for consult-reftex actions")

(defvar consult-reftex-reference-categories
  '(("(e)quation" . "e")
    ("(s)ection"  . "s")
    ("(f)igure"   . "f")
    ("(t)able"    . "t")
    ("end(N)ote"  . "N")
    ("foot(n)ote" . "n")
    ("Enumerate (i)" . "i")
    ("(l)isting"   . "l"))
  "Categories of reftex references and narrowing keys for
  consult-reftex selection commands.")

(defun consult-reftex-label-candidates (prefix)
  "Find all references in current document (multi-file) using reftex. 

With prefix arg PREFIX, rescan the document for references."
  (reftex-access-scan-info prefix)
  ;; (when (equal prefix 4) (reftex-parse-all))
  (let ((all-candidates))
    (dolist (entry (symbol-value reftex-docstruct-symbol) all-candidates)
      (when (stringp (car entry))
          (push (consult-reftex--make-annotation (car entry) (nth 2 entry) (nth 3 entry) (cadr entry))
                (alist-get (cadr entry) all-candidates nil nil 'string=))))))

(defun consult-reftex--reference (&optional arg)
  "Select a label with consult-based completing-read."
  (when-let* ((all-candidates (consult-reftex-label-candidates arg))
              (sources
               (mapcar (lambda (class) `(:name ,(car class)
                                          :narrow ,(string-to-char (cdr class))
                                          :category reftex-label
                                          :items ,(alist-get (cdr class) all-candidates
                                                             nil nil 'string=)))
                       consult-reftex-reference-categories))
              (label (car (save-excursion
                            (consult--multi
                             sources  
                             :sort nil
                             :prompt "Label (esftNn): "
                             :require-match t
                             :preview-key (if (plist-member (alist-get #'consult-reftex-reference
                                                                       consult--read-config)
                                                            :preview-key)
                                              (plist-get config :preview-key)
                                            consult-preview-key)
                             :history 'consult-reftex--reference-history
                             :state  (funcall consult-reftex-preview-function)
                             :annotate #'consult-reftex--get-annotation)))))
    label))

(defun consult-reftex--get-annotation (cand)
  (when-let ((ann (get-text-property 0 'reftex-annotation cand)))
      (concat (propertize " " 'display '(space :align-to center)) ann)))

(defvar consult-reftex--reference-history nil)

(defun consult-reftex--make-annotation (key annotation file type)
  "Annotate KEY with ANNOTATION and FILE if the latter is not nil."
  (cond
   ((not annotation) key)
   (t (propertize key 'reftex-annotation annotation
                      'reftex-file       file
                      'reftex-type       type))))

(defun consult-reftex--label-marker (label file open-fn)
  "Return marker corresponding to label location in tex document."
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

;;;###autoload
(defun consult-reftex-insert-reference (&optional arg no-insert)
  "Insert reference with completion.

With prefix ARG rescan the document."
  (interactive "P")
  (when-let* ((label (consult-reftex--reference arg))
              (reference
               (consult--read
                (cons label
                      (mapcar (lambda (ref-type) (concat (car ref-type) label (cdr ref-type)))
                              '(("\\ref{" . "}")
                                ("\\eqref{" . "}")
                                ("\\Ref{" . "}")
                                ("\\autoref{" . "}")
                                ("\\pageref{" . "}")
                                ("\\autopageref{" . "}"))))
                :sort nil
                :default (concat "\\ref{" label "}")
                :prompt "Reference:"
                :require-match t
                ;; :category 'reftex-label
                :annotate (lambda (cand)
                            (concat (propertize " " 'display '(space :align-to center))
                                    (propertize (or (alist-get (substring cand 0 4)
                                                               '(("\\ref" . "reference")
                                                                 ("\\Ref" . "Reference")
                                                                 ("\\eqr" . "equation ref")
                                                                 ("\\aut" . "auto ref")
                                                                 ("\\pag" . "page ref")
                                                                 ("\\aut" . "auto page ref"))
                                                               nil nil 'string=)
                                                    "label only")
                                                'face 'consult-key))))))
    (if no-insert reference (insert (substring-no-properties reference)))))

;;;###autoload
(defun consult-reftex-goto-label (label &optional arg)
  "Select label using Consult and jump to it."
  (interactive (list (consult-reftex--reference current-prefix-arg)
                     current-prefix-arg))
  (if-let* ((open (consult--temporary-files))
            (marker (consult-reftex--label-marker (substring-no-properties label)
                                                  (get-text-property 0 'reftex-file label)
                                                  open)))
      (consult--jump marker)))

(provide 'consult-reftex)
;;; consult-reftex.el ends here

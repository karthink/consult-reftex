;;; consult-reftex-preview.el -*- lexical-binding: t -*-
;; (require 'consult-reftex)

(defcustom consult-reftex-preview-function #'consult-reftex-preview-jump
  "Funtion to preview label locations when inserting references."
  :group 'consult-reftex
  :type '(radio
          (const :tag "Preview by jumping" 'consult-reftex-preview-jump)
          (const :tag "Preview in different window" 'consult-reftex-preview-make-window)
          (function :tag "User-defined function")))

;; Preview functions for consult-reftex
(defvar consult-reftex-preview--string "*RefTex Preview: %s*"
  "Format string for preview windows of consult-reftex.")

(defun consult-reftex-preview-jump ()
  "Create preview function for reftex labels."
  (let ((preview (consult--jump-preview))
        (open    (consult--temporary-files)))
    (lambda (action cand)
      (when cand
        (if (eq action 'return)
            (progn (funcall preview nil t)
                   (funcall open))
          (catch 'exit
            (let ((label (substring-no-properties (car cand)))
                  (file  (get-text-property 0 'reftex-file (car cand)))
                  (backward t)
                  re found marker buffer)
              (unless file
                (message "Unknown label - reparse file?")
                (throw 'exit nil))
              (setq marker (consult-reftex--label-marker label file open))
              (if marker
                  (funcall preview 'preview marker)
                (message "Label %s not found" label)))))))))

(defun consult-reftex-preview-make-window ()
  (let* ((all-preview-buffers)
         (open (consult--temporary-files)))
    (lambda (action cand)
      (if (or (not cand) (eq action 'return))
          (progn (funcall open)
                 (mapc #'kill-buffer all-preview-buffers))
        (catch 'exit
          (let ((label (substring-no-properties (car cand)))
                (file  (get-text-property 0 'reftex-file (car cand)))
                (type  (get-text-property 0 'reftex-type (car cand)))
                (backward t)
                re found marker buffer)
            (unless file
              (message "Unknown label - reparse file?")
              (throw 'exit nil))
            (setq marker (consult-reftex--label-marker label file open))
            (if marker
                ;; (funcall (consult--jump-preview) marker nil)
                (let ((preview-window (consult-reftex-preview--window marker type)))
                  (cl-pushnew (window-buffer preview-window) all-preview-buffers)
                  (while (> (length all-preview-buffers) consult-preview-max-count)
                    (kill-buffer (car (last all-preview-buffers)))
                    (setq all-preview-buffers (nbutlast all-preview-buffers))))
              (message "Label %s not found" label))))))))

(defun consult-reftex-preview-open-in-window (buffer)
  (let ((win (display-buffer
              buffer
              '((display-buffer-reuse-window display-buffer-below-selected)
                (direction . below)
                (window-height .  (lambda (win) (fit-window-to-buffer
                                            win
                                            (floor (frame-height) 4))))))))
    (fit-window-to-buffer win (floor (frame-height) 4))
    win))

(defun consult-reftex-preview--window (marker env-type)
  (when-let* ((buf (marker-buffer marker))
              (pos (marker-position marker))
              (preview-name (format consult-reftex-preview--string
                                    (buffer-name buf))))
    (consult-reftex-preview-open-in-window
     (if-let ((preview-window (get-buffer-window preview-name)))
         (with-selected-window preview-window
           (consult-reftex--narrow-to-env pos env-type)
           (current-buffer))
       (with-current-buffer (or (get-buffer preview-name)
                                (make-indirect-buffer buf preview-name t t))
         (setq-local mode-line-format nil)
         (consult-reftex--narrow-to-env pos env-type)
         (current-buffer))))))

(defun consult-reftex--narrow-to-env (pos env-type)
  (widen)
  (if (equal major-mode 'org-mode) (org-show-all))
  (goto-char pos)
  (pcase env-type
    ("s" (pcase major-mode
           ((or 'latex-mode 'LaTeX-mode)
            (let ((TeX-header-end LaTeX-header-end)
                  (TeX-trailer-start LaTeX-trailer-start))
              (LaTeX-mark-section t)))
           ('org-mode (org-mark-subtree))))
    (_ (LaTeX-mark-environment)))
  (narrow-to-region (region-beginning) (region-end))
  ;; (preview-region (point-min) (point-max))
  (deactivate-mark)
  (goto-char (point-min)))

(provide 'consult-reftex-preview)

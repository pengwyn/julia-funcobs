;; Author: Danny Cocks (daniel.cocks@gmail.com)
;; Version: 0.1
;; Package-Requires: ((deferred "0.5.1") (julia-repl "1.2.0") (s "1.12.0"))
;; Keywords: julia
;; URL: 

;;; Commentary:

;; A plugin to interface with FunctionObserving.jl.

;;; Code:

(require 'widget)
(require 'julia-repl)
(require 's)


(defvar jfo--field-size 25)
(defvar jfo--buffer-name "*Julia Observing*")

(defvar jfo-after-insert-hook '(jfo--render-svg)
  "Hooks to run after insert text from the process filter.")

(defvar-local jfo--form-mod-name nil)
(defvar-local jfo--form-name nil)
(defvar-local jfo--form-args nil)
(defvar-local jfo--form-kwds nil)
(defvar-local jfo--form-show-diffs nil)
(defvar-local jfo--form-submit-button nil)
(defvar-local jfo--output-start nil)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; * Widgets
;;----------------------------

(defun jfo--setup (mod func args kwds)
  (switch-to-buffer-other-frame jfo--buffer-name)
  (set-window-dedicated-p (selected-window) t)

  (kill-all-local-variables)

  (julia-funcobs-mode)
  
  (let ((inhibit-read-only t))
    (erase-buffer))
  (remove-overlays)

  (widget-insert "Function observing.\n\n")
  (setq jfo--form-mod-name
              (widget-create 'editable-field
                 :size jfo--field-size
                 :format "Module: %v " ; Text after the field!
                 mod))
  (widget-insert "\n")
  (setq jfo--form-name
              (widget-create 'editable-field
                 :size jfo--field-size
                 :format "Function name: %v " ; Text after the field!
                 func))
  (widget-insert "\n")

  (widget-insert "\n\n")
  (widget-insert "Argument mode:\n")
  (widget-create 'radio-button-choice
                 :value "Single eval"
                 ;; :notify (lambda (&rest ignore) (jfo--update-evaluation-style))
                 '(item "Single eval")
                 '(item "Multiple eval - zipped.")
                 '(item "Multiple eval - outer product."))
  (widget-insert "\n\n")
  (widget-insert "Arguments:")
  ;; TODO: These args need to have a checkbox (or some other identifying thing) for arg/kwd
  ;; (setq jfo--form-args-old
  ;;       (widget-create 'editable-list
  ;;                      :entry-format "%i %d %v"
  ;;                      :value arg-names
  ;;                      '(editable-field :value "")))
  (setq jfo--form-args
              (cl-loop for arg in args
                       collect (let ((name (nth 0 arg))
                                     (val (nth 1 arg)))
                                 (widget-insert "\n")
                                 (widget-create 'editable-field
                                                :size jfo--field-size
                                                :format (concat name ": %v")
                                                :name name
                                                :required (not (or val (s-ends-with-p "..." name)))
                                                :action #'jfo--field-changed
                                                (or val "")))))
  (widget-insert "\n")
  (widget-create 'push-button
                 :notify (lambda (&rest ignore) (jfo--add-arg))
                 "Add")

  (widget-insert "\n\n")
  (widget-insert "Keywords:")
  (setq jfo--form-kwds
              (cl-loop for arg in kwds
                       collect (let ((name (nth 0 arg))
                                     (val (nth 1 arg)))
                                 (widget-insert "\n")
                                 (widget-create 'editable-field
                                                :size jfo--field-size
                                                :format (concat name ": %v")
                                                :name name
                                                :required (not (or val (s-ends-with-p "..." name)))
                                                :action #'jfo--field-changed
                                                (or val "")))))
  (widget-insert "\n")
  (widget-create 'push-button
                 :notify (lambda (&rest ignore) (jfo--add-kwd))
                 "Add")

  ;; (widget-create 'editable-field :size 13 :format "arg name: %v "
  ;;                :notify
  ;;                (lambda (widget &rest ignore)
  ;;                  (let ((old (widget-get widget
  ;;                                         ':example-length))
  ;;                        (new (length (widget-value widget))))
  ;;                    (unless (eq old new)
  ;;                      (widget-put widget ':example-length new)
  ;;                      (message "You can count to %d." new))))
  ;;                )
  (widget-insert "\n\n")
  (setq jfo--form-show-diffs (widget-create 'checkbox t))
  (widget-insert " Show diffs\n")

  (widget-insert "\n")
  (setq jfo--form-submit-button
              (widget-create 'push-button
                             :notify (lambda (&rest ignore) (jfo--run-command))
                             "Start observing"))
  (widget-insert "\n")
  (widget-create 'push-button
                 :notify (lambda (&rest ignore) (jfo--break))
                 "Break observing")
  ;; (widget-insert " ")
  ;; (widget-create 'push-button
  ;;                :notify (lambda (&rest ignore)
  ;;                          (widget-example))
  ;;                "Reset Form")
  (widget-insert "\n")
  (widget-insert "\n")
  (widget-insert "--OUTPUT--\n")
  ;; Make an overlay so we can go back to this later.
  (setq jfo--output-start (make-overlay (point) (point) nil t))
  (widget-insert "\n")
  (widget-setup)
  ;; (goto-char (point-min))
  ;; (search-forward "[Start")
  (goto-char (widget-get jfo--form-submit-button :from))
  nil)

(define-derived-mode julia-funcobs-mode custom-mode "JFO")
(define-key julia-funcobs-mode-map (kbd "C-x C-s") 'jfo--field-changed)
(define-key julia-funcobs-mode-map "q" 'quit-window)
(when (featurep 'evil)
  (evil-define-key 'normal julia-funcobs-mode-map
    ;; motion
    (kbd "<tab>") 'widget-forward
    (kbd "S-<tab>") 'widget-backward
    (kbd "<backtab>") 'widget-backward
    (kbd "RET") 'Custom-newline
    (kbd "]]") 'widget-forward
    (kbd "[[") 'widget-backward
    ;; TODO: Should the following be added?
    (kbd "C-j") 'widget-forward
    (kbd "C-k") 'widget-backward
    "gj" 'widget-forward
    "gk" 'widget-backward

    ;; quit
    "q" 'Custom-buffer-done)
  (evil-initial-state 'julia-funcobs-mode 'insert)
  )

(defun jfo--running-p ()
  (when (boundp 'jfo--form-submit-button)
    (widget-get jfo--form-submit-button :submitted)))

(defun jfo--output-point ()
  (overlay-start jfo--output-start))

;; (widget-example "ExampleFunc" '("5" "[1,2,3]" "\"something\"") '(("flip" "true") ("other" ":maybe")))

(defun jfo--add-arg ()
  "Add an extra argument."
  (let* ((last-widget (car (last jfo--form-args)))
         (ind (length jfo--form-args))
         (pos (if last-widget
                  (widget-get last-widget :to)
                (save-excursion
                  (goto-char (point-min))
                  (search-forward "Arguments:")
                  (point)))))
    (goto-char pos)
    (forward-char)
    (let ((w (widget-create 'editable-field
                   :size jfo--field-size
                   :format (concat (format "Arg %d: " ind) "%v")
                   :name (format "Arg %d" ind)
                   :required nil
                   "")))
      (widget-insert "\n")
      (add-to-list 'jfo--form-args w t)
      ;; (remove-overlays)
      (widget-setup)
      )))
  
(defun jfo--get-widget-args ()
  (seq-filter #'identity (mapcar (lambda (w)
            (let ((name (widget-get w :name))
                  (val (string-trim (widget-value w)))
                  (required (widget-get w :required)))
              (if (string-empty-p val)
                (when required (error "Argument '%s' is missing" name))
                val)
              ))
          jfo--form-args)))

(defun jfo--get-widget-kwds ()
  (seq-filter #'identity (mapcar (lambda (w)
            (let ((val (string-trim (widget-value w)))
                  (name (widget-get w :name))
                  (required (widget-get w :required)))
              (if (string-empty-p val)
                (when required (error "Required keyword '%s' is missing" name))
                (concat ":" name "=>" val))))
          jfo--form-kwds)))

(defun jfo--field-changed (&rest ignored)
  (interactive)
  (message "Trying to update")
  (when (jfo--running-p)
    (jfo--run-command)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; * Interactives
;;----------------------------

;; These were stolen from julia-mode

(defconst jfo--capturing-function-regex
  (rx-to-string `(: line-start (regexp ,julia-prefixed-macro-list) symbol-start
      "function"
      (1+ space)
      ;; Don't highlight module names in function declarations:
      (* (seq (1+ (or word (syntax symbol))) "."))
      ;; The function name itself
      (group-n 1 (1+ (or word (syntax symbol))))
      ;; The arguments
      ;; Terrible! This won't always work. Need to parse properly.
      ;; "(" (group-n 2
      ;;      (* (or
      ;;         (seq "(" (* (not (any "(" ")"))) ")")
      ;;         (not (any "(" ")")))))
      ;; ")"
      "("
      (group-n 2 (*? (not (any "(" ")"))))
      (optional ";"
                (group-n 3 (* (not (any "(" ")")))))
      ")"
      )))

;; functions of form "f(x) = nothing"
(defconst jfo--capturing-function-assignment-regex
  (rx-to-string `(: line-start (regexp ,julia-prefixed-macro-list) symbol-start
      (* (seq (1+ (or word (syntax symbol))) ".")) ; module name
      (group-n 1 (1+ (or word (syntax symbol))))
      "("
      (group-n 2 (*? (not (any "(" ")"))))
      (optional ";"
                (group-n 3 (* (not (any "(" ")")))))
      ")"
      (* space)
      (? "::" (* space) (1+ (not (any space))))
      (* space)
      (* (seq "where" (or "{" (+ space)) (+ (not (any "=")))))
      "="
      (not (any "=")))))

(defconst jfo--capturing-function-combined-regex
  (concat jfo--capturing-function-regex "\\|"
          jfo--capturing-function-assignment-regex))

(defconst jfo--arg-parse
  (rx (* blank)
      (or (group-n 1 (1+ (or word (syntax symbol))) "...")
          (seq (group-n 1 (1+ (or word (syntax symbol)))) (optional (* blank) "=" (* blank) (group-n 2 (* any)))))
      (* blank)))

(defun julia-function-observe ()
  "Start a function observation.

Tries to identify the current function and arguments."
  (interactive)
  (let* ((orig-pos (point))
         (line (save-excursion
                 (beginning-of-defun)
                 (thing-at-point 'line t)))
         (mod-name (save-excursion
                     (goto-char (point-min))
                     (if (search-forward-regexp (rx line-start "module" (1+ blank) (group (1+ word))) orig-pos t)
                         (match-string-no-properties 1)
                       ;; ":auto"
                       (concat "\"" (buffer-file-name) "\"")
                       )))
         (func-name (if (string-match jfo--capturing-function-combined-regex line)
                        (if (string-prefix-p "\"" mod-name)
                          (match-string-no-properties 1 line)
                          (concat mod-name "." (match-string-no-properties 1 line)))
                      (error "Not at a function start")))
         (args-str (match-string-no-properties 2 line))
         (kwds-str (match-string-no-properties 3 line))
         (args (jfo--parse-args-string args-str))
         (kwds (jfo--parse-args-string kwds-str))
         )
    (jfo--setup mod-name func-name args kwds)))

(defun jfo--parse-args-string (args-str)
  ;; Need to parse properly, but for now something simple.
  (when args-str
    (mapcar (lambda (arg)
              (if (string-match jfo--arg-parse arg)
                  (list (match-string 1 arg) (match-string 2 arg))
                (error "Shouldn't get here")));;(list arg nil))
            (split-string args-str "," t))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; * Process stuff
;;----------------------------

(defun jfo--run-command ()
  (with-current-buffer jfo--buffer-name 
    (let ((mod-name (widget-value jfo--form-mod-name))
          (name (widget-value jfo--form-name))
          (args (jfo--get-widget-args))
          (kwds (jfo--get-widget-kwds))
          (show-diffs (widget-value jfo--form-show-diffs)))
      ;; (message "%S" jfo--form-submit-button)
      (let* ((arg-string (concat "[ " (s-join ", " args) " ]"
                                 ", [ " (s-join ", " kwds) " ]"))
             (option-kwds (s-join ", " (cl-loop for (name val) in `(("show_diffs" show-diffs)
                                                                    ("continuing" ,(jfo--running-p)))
                                                  collect (concat name "=" (if val "true" "false"))))))

        ;; Need to hack term-mode's filter
        (advice-remove 'term-emulate-terminal #'jfo--term-filter)
        (advice-add 'term-emulate-terminal :before #'jfo--term-filter)
        ;; TODO: Need to remove this when the terminal stops.

        ;; Cancel an existing run if there is one.
        (when (julia-repl--live-buffer)
          (with-current-buffer (julia-repl--live-buffer)
            (term-interrupt-subjob)))

        ;; Prep the package/file
        (jfo--send-to-repl "using FunctionObserving")
        (when (not (string= mod-name ":auto"))
          (if (string-prefix-p "\"" mod-name)
              ;; This seems to be broken
              (progn (jfo--send-to-repl (concat "Revise.includet("  mod-name ")"))
                     (jfo--send-to-repl (concat "include("  mod-name ")")))
            (jfo--send-to-repl (concat "import " mod-name))))

        (jfo--send-to-repl (concat
"pushdisplay(FunctionObserving.EMACS_DISPLAY());
try
    FunctionObserving.ObserveFunction(" mod-name ", " name ", " arg-string " ; " option-kwds ");
finally
    popdisplay(FunctionObserving.EMACS_DISPLAY());
end;"))
        (widget-put jfo--form-submit-button :submitted t)))

    ;; Change the button name to reflect the new behaviour
    (save-excursion
      (let ((inhibit-read-only t))
        ;; TODO: Replace this with an overlay or go directly from the widget
        (let ((beg (widget-get jfo--form-submit-button :from))
              (end (widget-get jfo--form-submit-button :to)))
          (goto-char beg)
          ;; TODO: shouldn't even need a re-search-forward here...
          (when (re-search-forward "Start observing" end t)
            (replace-match "Update"))
          )))
    ))

;; (jfo--run-command)

(defun jfo--break ()
  (with-current-buffer (julia-repl--live-buffer)
        (advice-remove 'term-emulate-terminal #'jfo--term-filter)
        (term-interrupt-subjob))
  (with-current-buffer jfo--buffer-name 
    (widget-put jfo--form-submit-button :submitted nil)
    (save-excursion
      (let ((inhibit-read-only t)
            (beg (widget-get jfo--form-submit-button :from))
            (end (widget-get jfo--form-submit-button :to)))
        (goto-char beg)
        ;; TODO: shouldn't even need a re-search-forward here...
        (when (re-search-forward "Update" end t)
          (replace-match "Start observing"))
        ))
    ))


(defun jfo--update-text (text &optional append)
  (with-current-buffer jfo--buffer-name
    (save-excursion
    (let ((inhibit-read-only t))
      (unless append
        (goto-char (point-min))
        (search-forward "--OUTPUT--")
        (forward-line)
        (delete-region (point) (point-max))
        )
      (goto-char (point-max))
      ;; (insert text))))
      ;; TODO: This should be replaced with custom font-locking
      ;; The replacement of endlines seems odd here. Just a hack I need?
      (insert (ansi-color-apply (replace-regexp-in-string "\r?\n" "\n" text)))

      ;; Handle images and other things
      (mapcar (lambda (func) (apply func text nil))
              jfo-after-insert-hook)
      ))))
      ;; (insert (ansi-color-apply text)))))
;; (jfo--update-text "asdf\n123123\n" t)
;; (jfo--update-text "asdf\n123123\n" nil)

(defun jfo--term-filter (process str)
  "Hijack term process filter and grab all text output."
  ;; (message str)
  (let* ((ind (string-match "<emacs-clear></emacs-clear>" str))
         (append (not ind))
         (text (if ind
                   (substring str (match-end 0))
                 str)))
    (jfo--update-text text append)))

(defun jfo--send-to-repl (command)
  (let ((display-buffer-overriding-action '((display-buffer-no-window) (allow-no-window . t))))
    (julia-repl--send-string command)))


(defun jfo--render-svg (text)
  "Look for any </svg> tags and term them into images."
  (when (string-match "</svg>" text)
    (save-excursion
      (goto-char (jfo--output-point))
      (let ((count 0))
        (while (re-search-forward (rx "<?xml " (*? anything) "</svg>") nil t)
          (unless (jfo--get-svg-overlay (match-beginning 0) (match-end 0))
            (jfo--replace-svg (match-beginning 0) (match-end 0))
            (incf count)
            ))
        (message "Rendered %d svgs" count)
        )))
  (when (string-match "</emacs-svg>" text)
    (save-excursion
      (goto-char (jfo--output-point))
      (let ((count 0))
        (while (re-search-forward (rx "<emacs-svg>" (group (*? anything)) "</emacs-svg>") nil t)
          (unless (jfo--get-svg-overlay (match-beginning 0) (match-end 0))
            (jfo--replace-emacs-svg (match-beginning 0) (match-end 0))
            (incf count)
            ))
        (message "Rendered %d emacs-svgs" count)
        )))
)

(defun jfo--get-svg-overlay (beg end)
  (let* ((overlays-beg (overlays-at beg))
         (overlays-end (overlays-at end))
         (overlays-common (union overlays-beg overlays-end)))
    (some (lambda (overlay) (and (eq (overlay-get :category overlay) 'svg-replace) overlay))
          overlays-common)))

(defun jfo--replace-svg (beg end)
  (let* ((text (buffer-substring beg end))
         (image (create-image text 'svg t))
         (inhibit-read-only t))
      (delete-region beg end)
      (goto-char beg)
      (insert (propertize "IMAGE" 'display image))
  ))

(defun jfo--replace-emacs-svg (beg end)
  (goto-char beg)
  (unless (looking-at (rx "<emacs-svg>" (group (*? anything)) "</emacs-svg>"))
    (error "Should be at an emacs-svg tag here"))
  (let* ((filename (match-string 1))
         (image (create-image filename 'svg))
         (inhibit-read-only t))
    (delete-region beg end)
    (goto-char beg)
    (insert (propertize "EMACS-IMAGE" 'display image))
  ))



(provide 'julia-funcobs)

;;; julia-funcobs.el ends here

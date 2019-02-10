(defvar annotation-mode-map nil "Keymap for `annotation-mode'")
;; make sure that the var name is your mode name followed by -map. That way, define-derived-mode will automatically set it as local map

;; also, by convention, variable names for keymap should end in -map

(progn
  (setq annotation-mode-map nil)
  (setq annotation-mode-map (make-sparse-keymap))
  (define-key annotation-mode-map (kbd "C-t") 'annotation-mode-toggle-mode)
  (define-key annotation-mode-map (kbd "C-y") 'annotation-mode-toggle-highlight-only)  
  (define-key annotation-mode-map (kbd "1") '(lambda () (interactive) (annotation-mode-user-add-annotation 1)))
  (define-key annotation-mode-map (kbd "2") '(lambda () (interactive) (annotation-mode-user-add-annotation 2)))
  (define-key annotation-mode-map (kbd "3") '(lambda () (interactive) (annotation-mode-user-add-annotation 3)))
  (define-key annotation-mode-map (kbd "4") '(lambda () (interactive) (annotation-mode-user-add-annotation 4)))
  (define-key annotation-mode-map (kbd "5") '(lambda () (interactive) (annotation-mode-user-add-annotation 5)))
  (define-key annotation-mode-map (kbd "6") '(lambda () (interactive) (annotation-mode-user-add-annotation 6)))  
  (define-key annotation-mode-map (kbd "0") 'annotation-mode-delete-current-annotation)

  ;; by convention, major mode's keys should begin with the form C-c C-‹key›
  ;; by convention, keys of the form C-c ‹letter› are reserved for user. don't define such keys in your major mode
  )

(define-derived-mode annotation-mode text-mode "Annotation"
  "Major mode for annotating text."
  (make-local-variable 'annotation-mode-variant)
  (setq text-mode-variant t))

(defvar annotation-mode-source-file nil "Annotation mode source file.")
(defvar annotation-mode-annotations-file nil "Annotation mode annotations file.")
(defvar annotation-mode-source-was-read-only nil "Whether the source file was read only when loaded.")
(defvar annotation-mode-annotations nil "The list of annotations.")
(defvar annotation-mode-current-annotation nil "The annotation at the point.")
(defvar annotation-mode-highlight-only t "Whether to just highlight and use nil text annotations.")
(defvar annotation-mode-faces
      [(:foreground "black" :background "yellow")
	   (:foreground "white" :background "blue")
	   (:foreground "black" :background "green")
	   (:foreground "white" :background "magenta")
	   (:foreground "black" :background "orange")
	   (:foreground "black" :background "red")
	   ] "Array of faces to use in annotations.")
(defvar annotation-mode-current-mode "add")

(defun annotation-mode-toggle-highlight-only ()
  (interactive)
  (setq annotation-mode-highlight-only (not annotation-mode-highlight-only))
  (message "Highlight only set to %s." annotation-mode-highlight-only)
  )
(defun annotation-mode-toggle-mode ()
  (interactive)
  (setq annotation-mode-current-mode
		(cond
		 ((string= annotation-mode-current-mode "edit") "add") (t "edit")))
  (message "Toggled current mode to: %s" annotation-mode-current-mode))

(defcustom annotation-mode-hook nil
  "Hook run when entering annotation mode."
  :type 'hook
  :options '(annotation-mode-initialize)
  :group 'data)
;;(add-hook 'annotation-mode-hook 'annotation-mode-initialize nil t)

(defun annotation-mode-initialize ()
  (interactive)
  (message "Initializing annotation-mode.")
  (setq annotation-mode-source-file (buffer-file-name (current-buffer)))
  (message (concat "source file: " annotation-mode-source-file))
  (setq annotation-mode-annotations-file (concat annotation-mode-source-file ".ann"))
  (message (concat "annotations file: " annotation-mode-annotations-file))
  (setq annotation-mode-source-was-read-only buffer-read-only)
  (message (concat "source was read only?  " (if annotation-mode-source-was-read-only "yes" "no")))
  (read-only-mode 0)
  (message "Loading annotations.")
  (annotation-mode-load-annotations)
  (message "Rendering annotations.")
  (annotation-mode-render-all)
  (message "Setting after-save-hook.")
  (add-hook 'after-save-hook 'annotation-mode-save-annotations nil t)
  (message "Setting post-command-hook.")
  (add-hook 'post-command-hook 'annotation-mode-update-current-annotation nil t)
  (message "Done initializing.")
  (set-buffer-modified-p nil) ;; Hack, not sure why saving changes the buffer
  )

;;(add-hook 'after-save-hook 'annotation-mode-save-annotations nil t)

(defun get-file-contents (filename)
  "Return the contents of FILENAME."
  (with-temp-buffer
    (insert-file-contents filename)
    (buffer-string)))

(defun wrap-read-only (body)
  (progn
	body
	))

(defun annotation-mode-render-single (ann)
;;  (message "Single: %s" ann)
	(destructuring-bind
		(start end face-ind text) ann
	  (progn
;;		(message "Rendering (%s, %s, %s, %s)" start end face-ind text)
		(wrap-read-only 
		 (put-text-property start end 'face (aref annotation-mode-faces (- face-ind 1)))
		 )
		)))

(defun annotation-mode-save-annotations ()
  (interactive)

  (when (equal major-mode 'annotation-mode)
	(message "Saving annotations.")
	(setq str "")
	(dolist (elt annotation-mode-annotations)
	  (destructuring-bind
		  (start end face-ind text)
		  elt
		(setq str (concat str (format "%s|%s|%s|%s\n" start end face-ind text)))))
	(save-current-buffer
	  (message "Saving current buffer.")
	  (write-region
	   str
	   nil annotation-mode-annotations-file))
	(message "Wrote annotations to %s." annotation-mode-annotations-file)
	(message "Current buffer: %s" (current-buffer))
	(message "Buffer file name: %s" (buffer-file-name))
	(message "Buffer modified: %s" (buffer-modified-p))	
	(annotation-mode-render-all)))

(defun annotation-mode-save-hook()
  "Save hook for annotations mode."
  (when (eq major-mode 'annotations-mode)
    (annotation-mode-save-annotations)))

(defun annotation-mode-update-current-annotation ()
  "Determine whether the current point is in any of the annotations and return it."
  (interactive)
  (setq annotation-mode-current-annotation
		(catch 'found
		  (dolist (elt annotation-mode-annotations)
			(destructuring-bind
				(start end face-ind text)
				elt
			  (when (and (>= (point) start) (< (point) end))
				(throw 'found elt))))))
  (when annotation-mode-current-annotation
	  (destructuring-bind
		  (start end face-ind text)
		  annotation-mode-current-annotation
		(message "%s" text))
	)
  )

(defun annotation-mode-add-annotation (new-annotation &optional render)
  (progn
	(message "Adding annotation: %s" new-annotation)
	(when render
	  (annotation-mode-render-single new-annotation))
	(push new-annotation annotation-mode-annotations)))

(defun annotation-mode-user-add-annotation (ind)
  "Add an annotation with the specified numerical type."
  (interactive)
  (unless (minibufferp)
	(cond (
		   ;; We're in add mode, so add an annotation if the region is valid
		   (string= annotation-mode-current-mode "add") ;
		   (if (mark)
			   (progn
				 (setq start (min (point) (mark)))
				 (setq end (max (point) (mark)))
				 (message "start: %s, end: %s" start end)
				 (if (> end start) ;; Valid region
					 (progn
					   (message "Adding annotation %s" ind)
					   (setq text (cond
								   (annotation-mode-highlight-only  "(empty)")
								   (t (read-string "Enter annotation: "))))
					   (annotation-mode-add-annotation (list start end ind text) t))
				   (message "Invalid region for annotation.")))
			 (message "Can't add annotation because mark is not set.")))

		  ;; We're in edit mode, so edit the current annotation if available
		  ((string= annotation-mode-current-mode "edit")
		   (if annotation-mode-current-annotation 
			   (destructuring-bind
				   (start end face-ind text)
				   annotation-mode-current-annotation
				 (annotation-mode-delete-current-annotation)
				 (annotation-mode-add-annotation (list start end ind text) t))
			 (message "No annotation found to edit.")))

		  ;; We're in some unknown mode.
		  (t (message "Don't know what to do for current edit mode '%s'." annotation-mode-current-mode)))
	)
  )


(defun annotation-mode-delete-current-annotation ()
  (interactive)
  (when annotation-mode-current-annotation
	(progn
	  (message "Deleting %s from annotations." annotation-mode-current-annotation)
	  (setq annotation-mode-annotations
			(delete annotation-mode-current-annotation annotation-mode-annotations))
	  (destructuring-bind
		  (start end face-ind text)
		  annotation-mode-current-annotation
		(remove-text-properties start end '(face nil)))
	  (annotation-mode-render-all))))

(defun annotation-mode-render-all ()
  (interactive)
  ;; go through the annotations and set the faces in the main buffer
  (dolist (elt annotation-mode-annotations)
	(annotation-mode-render-single elt)))

(defun annotation-mode-load-annotations ()
  (interactive)
  (setq annotation-mode-annotations nil)
  (unless (file-readable-p annotation-mode-annotations-file)
	(progn
	  (message "Annotations file does not exist, creating it.")
	  (write-region "" nil annotation-mode-annotations-file)))
	
  (message "Reading annotations file.")
  (setq lines
		(with-temp-buffer
		  (insert-file-contents annotation-mode-annotations-file)
		  (split-string (buffer-string) "\n" t)))
  (message "lines: %s" lines)	
  (dolist (line lines)
	(progn
	  (setq tokens (split-string line "|" t))
	  (when (= (length tokens) 4)
		(destructuring-bind
			(start end face-ind text)
			tokens
		  (annotation-mode-add-annotation
		   (list (string-to-number start)
				 (string-to-number end)
				 (string-to-number face-ind)
				 text)))
		)
	  )
	)
  )
  

(provide 'annotation-mode)
;;; annotation-mode.el ends here

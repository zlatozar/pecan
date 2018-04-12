;;; init-defuns --- Defines handy interactive functions

;;; Commentary:

;; Usage:

;;; Code:

(defun delete-this-file ()
  "Delete the current file, and kill the buffer."
  (interactive)
  (or (buffer-file-name) (error "No file is currently being edited"))
  (when (yes-or-no-p (format "Really delete '%s'?"
                             (file-name-nondirectory buffer-file-name)))
    (delete-file (buffer-file-name))
    (kill-this-buffer)))

(defun rename-this-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (unless filename
      (error "Buffer '%s' is not visiting a file!" name))
    (if (get-buffer new-name)
        (message "A buffer named '%s' already exists!" new-name)
      (progn
        (when (file-exists-p filename)
          (rename-file filename new-name 1))
        (rename-buffer new-name)
        (set-visited-file-name new-name)))))

(defun kill-all-buffers ()
  "Kill all buffers, leaving *scratch* only"
  (interactive)
  (mapc (lambda (x) (kill-buffer x))
    (buffer-list))
  (delete-other-windows))

(defun browse-current-file ()
  "Open the current file as a URL using `browse-url'."
  (interactive)
  (let ((file-name (buffer-file-name)))
    (if (tramp-tramp-file-p file-name)
        (error "Cannot open tramp file")
      (browse-url (concat "file://" file-name)))))

(defun untabify-buffer ()
  "Remove tabs from current buffer."
  (interactive)
  (untabify (point-min) (point-max)))

(defun indent-buffer ()
  "Indent current buffer."
  (interactive)
  (indent-region (point-min) (point-max)))

(defun cleanup-buffer ()
  "Perform a bunch of operations on the whitespace content of a buffer.
Including indent-buffer, which should not be called automatically on save."
  (interactive)
  (untabify-buffer)
  (delete-trailing-whitespace)
  (indent-buffer))

(defun toggle-fullscreen ()
  "Switch Emacs to full screen and back."
  (interactive)
  (defvar old-fullscreen)
  (let ((current-value (frame-parameter nil 'fullscreen)))
    (set-frame-parameter nil 'fullscreen
                         (if (equal 'fullboth current-value)
                             (if (boundp 'old-fullscreen) old-fullscreen nil)
                           (progn (setq old-fullscreen current-value)
                                  'fullboth)))))

(autoload 'zap-up-to-char "misc"
  "Kill up to, but not including ARGth occurrence of CHAR.")

(defun sudo-edit (&optional arg)
  "Edit buffer with given ARG as super user."
  (interactive "p")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:" (ido-read-file-name "File: ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

(defun insert-file-name (filename &optional args)
  "Insert full qualified FILENAME in place.
If you pass '-' as ARGS it will be relative."
  (interactive "*fInsert file name: \nP")
  (cond ((eq '- args)
         (insert (file-relative-name filename)))
        ((not (null args))
         (insert (expand-file-name filename)))
        (t
         (insert filename))))

(defun copy-line (n)
  "Copy N lines.  If N is not passed - copy current line."
  (interactive "p")
  (kill-ring-save (line-beginning-position) (line-beginning-position (1+ n))))

(defun pretty-json-region (&optional start end)
  "Shells out to Python to pretty print JSON.
With START END region is selected."
  (interactive "r")
  (shell-command-on-region start end "python -m json.tool" (current-buffer) t))

(defun occur-non-ascii-chars ()
  "Finds characters that aren't in the displayable range for ASCII."
  (interactive)
  (occur "[^\000-\177]"))

(defun comment-line (n)
  "Comment or uncomment current line and leave point after it.
With positive prefix, apply to N lines including current one.
With negative prefix, apply to -N lines above."
  (interactive "p")
  (comment-or-uncomment-region
   (line-beginning-position)
   (goto-char (line-end-position n)))
  (forward-line 1)
  (back-to-indentation))

;;________________________________________________________________________________
;;                                                                   Key-bindings

(bind-key "C-c x f" 'toggle-fullscreen)
(bind-key "M-Z" 'zap-up-to-char)
(bind-key "C-c C-;" 'comment-line)
(bind-key "C-c t t" 'copy-line)

(bind-key "C-c p f" 'cleanup-buffer)

(provide 'load-defuns)

;;; load-defuns.el ends here

;file imports
(load "~/emacs.d/init.d/word-navigation.el" nil t)

;custom control sequences that the ubuntu apparently isn't passing through
(unless (display-graphic-p)
  (progn
    (define-key input-decode-map "\033[5D" [(control left)])
    (define-key input-decode-map "\033[5C" [(control right)])
    (define-key input-decode-map "\033[5A" [(control up)])
    (define-key input-decode-map "\033[5B" [(control down)])
    (define-key input-decode-map "\033[51" [(control home)])
    (define-key input-decode-map "\033[54" [(control end)])
    (define-key input-decode-map "\033[53" [(control delete)])
    (define-key input-decode-map "\033[;55" [(control backspace)])
    (define-key input-decode-map "\033[55~" [(control prior)])
    (define-key input-decode-map "\033[56~" [(control next)])
    )
  )

(defun down-5-lines()
  (interactive)
  (next-line 5)
)

(defun up-5-lines()
  (interactive)
  (previous-line 5)
  )

(defun scroll-down-5-lines()
  (interactive)
  (setq num 0)
  (while (< num 5)
    (scroll-down-line)
    (setq num (1+ num))
    )
  )

(defun scroll-up-5-lines()
  (interactive)
  (setq num 0)
  (while (< num 5)
    (scroll-up-line)
    (setq num (1+ num))
    )
  )

(defun beginning-of-file()
  "Move the cursor to the beginning of the file"
  (interactive)
  (goto-char (point-min))
  )

(defun end-of-file()
  "Move the cursor to the end of the file"
  (interactive)
  (goto-char (point-max))
  )

(defun kill-whole-line2()
  "If on the last line of the file, then backspace instead"
  (interactive)
  (if
      (eq (+ 1 (count-lines (point-min) (point-max))) (line-number-at-pos))
      (delete-backward-char 1)
    (kill-whole-line))
  )

;Custom key definitions
(global-set-key (kbd "C-<up>") 'up-5-lines)
(global-set-key (kbd "C-<down>") 'down-5-lines)
(global-set-key (kbd "C-<home>") 'beginning-of-file)
(global-set-key (kbd "C-<end>") 'end-of-file)
(global-set-key (kbd "RET") (kbd "M-j"))

(global-unset-key (kbd "C-<prior>"))
(global-unset-key (kbd "C-<next>"))
(global-set-key (kbd "C-<next>") 'scroll-up-5-lines)
(global-set-key (kbd "C-<prior>") 'scroll-down-5-lines)
(global-set-key (kbd "C-k") 'kill-whole-line2)

;Functions found in /emacs.d/init.d/word-navigation
(global-set-key (kbd "C-<right>") 'forward-word2)
(global-set-key (kbd "C-<left>") 'backward-word2)
(global-set-key (kbd "C-<delete>") 'kill-word2)
(global-set-key (kbd "C-<backspace>") 'backward-kill-word2)

;sets line number formatting
(global-linum-mode)
(setq linum-format "%4d \u2502 ")

;sets file mode per "extension"
(add-to-list 'auto-mode-alist
	     '("\\.psql$" . (lambda ()
			      (sql-mode)
			      (sql-highlight-postgres-keywords))))

;Sets default file mode.
(setq-default major-mode 'shell-script-mode)

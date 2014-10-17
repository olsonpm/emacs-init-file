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
  (interactive)
  (goto-char (point-min))
  )

(defun end-of-file()
  (interactive)
  (goto-char (point-max))
  )

(defun kill-whole-line2()
  (interactive)
  (if
      (eq (+ 1 (count-lines (point-min) (point-max))) (line-number-at-pos))
      (delete-backward-char 1)
    (kill-whole-line))
  )

(defconst whitespace-chars (list ?\t ?\s ?\n)  
  "List of whitespace characters"
  )

(defconst symbol-chars (list ?) ?( ?\[ ?\] ?\{ ?\} ?\. ?\, ?\/ ?\\ ?\; ?\' ?\" ?\: ?\! ?\~ ?\` ?\@ ?\# ?\$ ?\% ?\^ ?\& ?\* ?\- ?\_ ?\+ ?\= ?\< ?\>)
  "List of symbol characters"
  )

(defconst alphanumeric-chars (list ?a ?b ?c ?d ?e ?f ?g ?h ?i ?j ?k ?l ?m ?n ?o ?p ?q ?r ?s ?t ?u ?v ?w ?x ?y ?z
				   ?A ?B ?C ?D ?E ?F ?G ?H ?I ?J ?K ?L ?M ?N ?O ?P ?Q ?R ?S ?T ?U ?V ?W ?X ?Y ?Z
				   ?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9)
  "List of alpha-numeric characters"
  )

(defun recursively-compare-char-white (inc-dec
				       char-after-or-before
				       cur-point
				       result-fn)
  (if
      (memq (funcall
	     char-after-or-before
	     cur-point)
	    whitespace-chars)  
      (recursively-compare-char-white
       inc-dec
       char-after-or-before
       (+ inc-dec cur-point)
       result-fn)  
    (funcall result-fn cur-point))
  ) 

(defun recursively-compare-char (inc-dec
				 char-after-or-before
				 char-set
				 cur-point
				 result-fn)
  (cond
   ((memq (funcall
	   char-after-or-before
	   cur-point)
	  whitespace-chars)
    (recursively-compare-char-white
     inc-dec
     char-after-or-before
     (+ inc-dec cur-point)
     result-fn))
   
   ((memq (funcall
	   char-after-or-before
	   cur-point)
	  char-set)
    (recursively-compare-char
     inc-dec
     char-after-or-before
     char-set
     (+ inc-dec cur-point)  
     result-fn))
   
   (t (funcall result-fn cur-point)))
  )

(defun simple-recur-compare-char (inc-dec
				  char-after-or-before
				  stop-char-set
				  cur-point
				  result-fn)
  (if
      (memq
	    (funcall char-after-or-before cur-point)
	    stop-char-set)
      (simple-recur-compare-char
       inc-dec
       char-after-or-before
       stop-char-set
       (+ inc-dec cur-point)
       result-fn)
    (funcall result-fn cur-point))
  )

(defun backward-kill-word2()
  (interactive)
  (setq all-chars (append whitespace-chars symbol-chars))
  (if  
      (memq (char-before) all-chars)  
      (progn
	(push-mark (point) nil t)
	(recursively-compare-char
	 -1
	 'char-before
	 all-chars
	 (1- (point))
	 (lambda (cur-point)
	   (kill-region cur-point (mark)))))
    (simple-recur-compare-char
     -1
     'char-before
     alphanumeric-chars
     (1- (point))
     (lambda (cur-point)
       (kill-region cur-point (mark)))))
  )
 
(defun kill-word2()
  (interactive)
  (setq all-chars (append whitespace-chars symbol-chars))
  (if
      (memq (char-after) all-chars)
      (progn  
	(push-mark (point) nil t)
	(recursively-compare-char
	 1
	 'char-after
	 all-chars
	 (1+ (point))
	 (lambda (cur-point)
	   (kill-region cur-point (mark)))))
    (simple-recur-compare-char
     1
     'char-after
     alphanumeric-chars
     (1+ (point))
     (lambda (cur-point)
       (kill-region cur-point (mark)))))
  )

(defun forward-word2()
  (interactive)
  (setq all-chars (append whitespace-chars symbol-chars))
  (if
      (memq (char-after) all-chars)
      (progn
	(recursively-compare-char
	 1  
	 'char-after
	 all-chars
	 (1+ (point))
	 (lambda (cur-point)
	   (goto-char cur-point))))
    (simple-recur-compare-char
     1
     'char-after
     alphanumeric-chars
     (1+ (point))
     (lambda (cur-point)
       (goto-char cur-point))))
  )

(defun backward-word2()
  (interactive)
  (setq all-chars (append whitespace-chars symbol-chars))
  (if
      (memq (char-before) all-chars)
      (progn
	(recursively-compare-char
	 -1
	 'char-before
	 all-chars
	 (1- (point))
	 (lambda (cur-point)
	   (goto-char cur-point))))
    (simple-recur-compare-char
     -1
     'char-before
     alphanumeric-chars
     (1- (point))
     (lambda (cur-point)
       (goto-char cur-point))))
  )
  
(global-set-key (kbd "C-<right>") 'forward-word2)
(global-set-key (kbd "C-<left>") 'backward-word2)
(global-set-key (kbd "C-<up>") 'up-5-lines)
(global-set-key (kbd "C-<down>") 'down-5-lines)
(global-set-key (kbd "C-<home>") 'beginning-of-file)
(global-set-key (kbd "C-<end>") 'end-of-file)
(global-set-key (kbd "C-<delete>") 'kill-word2)
(global-set-key (kbd "C-<backspace>") 'backward-kill-word2)
(global-set-key (kbd "RET") (kbd "M-j"))

(global-unset-key (kbd "C-<prior>"))
(global-unset-key (kbd "C-<next>"))
(global-set-key (kbd "C-<next>") 'scroll-up-5-lines)
(global-set-key (kbd "C-<prior>") 'scroll-down-5-lines)
(global-set-key (kbd "C-k") 'kill-whole-line2)

(global-linum-mode)
(setq linum-format "%4d \u2502 ")

(add-to-list 'auto-mode-alist
	     '("\\.psql$" . (lambda ()
			      (sql-mode)
			      (sql-highlight-postgres-keywords))))

(setq-default major-mode 'shell-script-mode)

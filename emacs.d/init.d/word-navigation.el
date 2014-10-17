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
    (progn
      (push-mark (point) nil t)
      (simple-recur-compare-char
       -1
       'char-before
       alphanumeric-chars
       (1- (point))
       (lambda (cur-point)
	 (kill-region cur-point (mark))))))
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
    (progn
      (push-mark (point) nil t)
      (simple-recur-compare-char
       1
       'char-after
       alphanumeric-chars
       (1+ (point))
       (lambda (cur-point)
	 (kill-region cur-point (mark))))))
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

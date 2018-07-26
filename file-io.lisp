;;;; slur
;;;; file i/o

; load-file-by-line
; returns list of strings, where each string is a single line of the opened file
; Note: semicolon-lead lines ignored
(defun load-file-by-line (filename)
	(let ((in (open filename :if-does-not-exist nil)))
		(when in
			(loop for line = (read-line in nil) while line 
				if (not (eql #\; (char line 0)))
				collect line))))

(defun tokenize-string (string delimeter)
  (loop for start = 0 then (1+ finish)
        for finish = (position delimeter string :start start)
        collecting (subseq string start finish)
        until (null finish)))

(defun tokenize-file-by-line (filename delimeter)
	(loop for line in (load-file-by-line filename)
		collect (tokenize-string line delimeter)))

(defun parse-float (target)
	(with-input-from-string (in target) (read in)))

(defun grain-parser (line)
	(list 
		(intern (first line))
		(with-input-from-string (in (second line)) (read in))
		(with-input-from-string (in (third  line)) (read in))))

(defun load-grain-data (raw-data)
	(loop for line in raw-data
		collect (grain-parser (tokenize-string line #\Space))))

(defun yeast-parser (line)
	(list 
		(intern (nth 0 line))
		(intern (nth 1 line))
		(list   (parse-integer (nth 2 line)) (parse-integer (nth 3 line)))
		(intern (nth 4 line))
		(list   (parse-integer (nth 5 line)) (parse-integer (nth 6 line)))
		(list   (parse-integer (nth 7 line)) (parse-integer (nth 8 line)))))

(defun load-yeast-data (raw-data)
	(loop for line in raw-data
		collect (yeast-parser (tokenize-string line #\Space))))

(defun load-slur-data (filename type)
	(let ((raw-data (load-file-by-line filename)))
		(if (eql type 'GRAIN)
			(load-grain-data raw-data)
			(load-yeast-data raw-data))))

(defun load-recipe-data (filename r)
	;(let ((raw-data (load-file-by-line filename)))
		(loop for line in (tokenize-file-by-line filename #\Space)
			if (string= (first line) "HEADER")
				do (setf (gethash 'HEADER r) (list 
					(intern (nth 1 line)) 
					(parse-float (nth 2 line))))
			if (string= (first line) "GRAIN")
				do (add-ingredient r 'GRAIN (grain 
					(intern (nth 1 line)) 
					(parse-float (nth 2 line)) 
					(intern (nth 3 line))))
			if (string= (first line) "HOP")
				do (add-ingredient r 'HOP (hop 
					(intern (nth 1 line)) 
					(parse-float (nth 2 line)) 
					(parse-float (nth 3 line)) 
					(intern (nth 4 line)) 
					(parse-integer (nth 5 line))))
			if (string= (first line) "MASH")
				do (add-ingredient r 'MASH (mash-step 
					(intern (nth 1 line)) 
					(parse-integer (nth 2 line)) 
					(parse-integer (nth 3 line)) 
					(parse-integer (nth 4 line))))
			if (string= (first line) "KETTLE")
				do (add-ingredient r 'KETTLE (kettle-step 
					(intern (nth 1 line)) 
					(parse-integer (nth 2 line)) 
					(parse-integer (nth 3 line))))
			if (string= (first line) "FERMENT")
				do (add-ingredient r 'FERMENT (ferm-step 
					(intern (nth 1 line)) 
					(parse-integer (nth 2 line)) 
			        (parse-integer (nth 3 line)) 
			        (intern (nth 4 line))))))

(defun asd ()
	(build-brewery 0.62 1 76)
	(setf APA (make-hash-table))
	(load-recipe-data "/home/zach/code/slur/recipes/neipa.slur" APA))


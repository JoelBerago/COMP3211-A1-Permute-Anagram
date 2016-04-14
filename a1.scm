;; Name: Berago Joel Arvin
;; SID: 20211631
;; COMP3211/2016Spring A1

;; Acknowledgement of people who worked with in this assignment
;; Study Group Member:
;; Yan Shing Ho: 20211772 


;; /////////////////////////////////////////////////////////////////////////////
;; PART 1

;; permute function acceps a list of any size and element.
;; permute implements the Steinhaus–Johnson–Trotter algorithm
;;   with the Shimon Even speed up, as stated in Wikipedia.
;; https://en.wikipedia.org/wiki/Steinhaus-Johnson-Trotter_algorithm#Even.27s_speedup
;; permute-step is called to update the reference list by swap
;; permute-find-mobile-position is called to determine if the function is finished.
(define (permute lst)
  (cond
   ((null? lst) '()) ;; if input empty do nothing.
   ((null? (cdr lst)) (print lst)) ;; if one item input, then print that item.
   (else
    ;; initialize reference (direction-position) pair list
    (let ((reference (permute-init-reference (length lst))))
      ;; in each iteration, permute-step each reference
      ;; and find the next available mobile-position
      (do ((reference reference (permute-step reference))
	   (mobile-position (permute-find-mobile-position reference)
			    (permute-find-mobile-position reference)))
	  ;; if mobile-position is #f, then return #f and end the program.
	  ((not mobile-position) #f)
	;; on each iteration, print the permuted list
	;; as dereferenced by the positions in reference.
	(print (permute-dereference lst reference)))))))

;; permute-step returns an updated reference list.
;; this is done in two steps;
;; 1) find and swap the mobile position to required direction
;;      1a) also set direction to zero if swapped to first, last,
;;           or if next element in direction is larger than the mobile position
;; 2) set all elements greater than the mobile element to either
;;      positive: if i is between start and mobile.
;;      negative: if i is between mobile and end.
(define (permute-step reference)
  (let ((position (permute-find-mobile-position reference)))
    ;; find-mobile-position could fail and return #f if already finished.
    (if (not position) '()
        ;; else initialize common variables.
        (let* ((index (permute-get-index-of-position reference position))
               (direction (permute-get-direction-by-index reference index))
               (reference (permute-reference-swap reference direction index position)))
          (permute-reference-update-all-directions reference position index)))))

;; returns an updated reference that completes step 1 of permute-step
;; 1) find and swap the mobile position to required direction;
;;      1a) also set direction to zero if swapped to first, last,
;;           or if next element in direction is larger than the mobile position
(define (permute-reference-swap reference direction index position)
  ;; first find the required position to swap
  ;; then set the direction to zero if necessary.
  (let ((positions (reference-get-positions reference))
        (directions (permute-directions-set-zero reference direction index position)))
    ;; do the actual swap.
    (if (< direction 0)
        (list-combine (swap-left directions index)
                      (swap-left positions index))
        (list-combine (swap-right directions index)
                      (swap-right positions index)))))

;; permute-directions-set-zero returns an updated direction list
;; it implements 1a) the setting part of permute-reference-swap
(define (permute-directions-set-zero reference direction index position)
  (let* ((positions (reference-get-positions reference))
         (nextelem (list-ref-try positions (+ index -1 (* 2 direction)))))
    (cond
     ;; if swap to first, then set to zero;
     ((and (< direction 0) (= 1 (- index 1)))
      (list-set (reference-get-directions reference) 0 index))
     ;; if swap to last, then set to zero;
     ((and (> direction 0) (= (length reference) (+ index 1)))
      (list-set (reference-get-directions reference) 0 index))
     ;; if nextelem's position > current position, then set to zero
     ((and (not (null? nextelem)) (> nextelem position))
      (list-set (reference-get-directions reference) 0 index))
     ;; else just return normal direction
     (else (reference-get-directions reference)))))

;; permute-reference-update-all-directions updates all the remaining directions
;; this function implements 2) by recursive looping.
(define (permute-reference-update-all-directions reference position index)
  (let loop ((result '())
             (reference reference)
             (i 1))
    (if (null? reference) ;; if there is no more to process 
        result ;; return the resulting reference list.
        (if (= i index) ;; skip if same
            (loop (append result (list (car reference)))
                  (cdr reference)
                  (+ i 1))
            (let ((pos (cadr (car reference))))
              ;; if this elem is greater than the swapped.
              (if (> pos position) 
                  (if (< i index)
                      ;; if to left, then +ve
                      (loop (append result (list (cons 1 (list pos))))
                            (cdr reference)
                            (+ i 1))
                      ;; if to right, then -ve
                      (loop (append result (list (cons -1 (list pos))))
                            (cdr reference)
                            (+ i 1)))
                  ;; if not greater than the swapped, then skip and continue;
                  (loop (append result (list (car reference)))
                        (cdr reference)
                        (+ i 1))))))))
  
;; returns the position of the mobile integer.
;; returns false if none found.
(define (permute-find-mobile-position reference)
  (let* ((directions (reference-get-directions reference))
        (positions (reference-get-positions reference))
        (zero-check (list-check-if-all-zero? directions)))
    (if zero-check ;; if all directions are zero, then no more swapping is needed and permutations are done.
        #f ;; return false when no more swapping required.
        (let loop ((result #f)
                   (directions directions)
                   (positions positions))
          (if (null? positions) ;; if positions is empty,
              result ;; just return null
              (let ((next_dir (car directions))
                    (next_pos (car positions))
                    (rest_dir (cdr directions))
                    (rest_pos (cdr positions)))
                (cond
                 ;; if next_dir is 0, skip.
                 ((= next_dir 0) (loop result rest_dir rest_pos)) 
                 ;; if result = #f (just initialized),
                 ;; then next pos is the result.
                 ((not result) (loop next_pos rest_dir rest_pos))
                 ;; if next position is greater than the previous found,
                 ;; then the next pos is the new result.
                 ((> next_pos result) (loop next_pos rest_dir rest_pos))
                 ;; else skip.
                 (else (loop result rest_dir rest_pos)))))))))


;; /////////////////////////////////////////////////////////////////////////////
;; // PART 1 HELPER FUNCTIONS
;; permute-dereference returns the list with corresponding positions
;;   as defined in the reference list
(define (permute-dereference lst reference)
  (let ((positions (reference-get-positions reference)))
    (map (lambda (x) (list-ref lst (- x 1)))
	 positions)))

;; permute-init-reference returns an initialized list of given size
;; this list will have all directions set to -1 except for the first one, which is 0.
;; and the positions will be in ascending order from 1 to size.
(define (permute-init-reference size)
  (list-combine (cons 0 (list-create (- size 1) -1)) ;; direction
		(list-range 1 size))) ;; positions

;; returns the index of the given position in the reference list.
(define (permute-get-index-of-position reference position)
  (do ((positions (reference-get-positions reference) (cdr positions))
       (index 1 (+ index 1)))
      ((= (car positions) position) index)
    '()))

;; where 1 <= index <= n
(define (permute-get-direction-by-index reference index)
  (let ((directions (reference-get-directions reference)))
    (list-ref directions (- index 1))))

;; where 1 <= index <= n
(define (permute-get-position-by-index reference index)
  (let ((positions (reference-get-positions reference)))
    (list-ref positions (- index 1))))

;; list-set returns a new list with the element in
;; the index of the lst set with the given value
(define (list-set lst val index)
  (let ((head (list-front lst index))
	(tail (list-tail lst index)))
    (append head (list val) tail)))

;; list-ref-try returns the value in lst of a given index,
;; but safely checks if index is valid before returning.
;; if unsafe, then returns '()
(define (list-ref-try lst index)
  (cond ((null? lst) '())
	((< index 0) '())
	((= index (length lst)) '())
	((> index (length lst)) '())
	(else (list-ref lst index))))

;; swap-left swaps the i-th element to the left
;; in a list 'lst[1...n]'
;; list-ref actually requires a list[0...n]
(define (swap-left lst i)
  (if (= i 1)
      lst ;; returns lst if attempting to swap the first element.
      (let ((yi (list (list-ref lst (- i 1))))
	      (xi (list (list-ref lst (- i 2))))
	      (head (list-head lst (- i 2)))
	      (tail (list-tail lst i)))
	(append head yi xi tail)))) ;; returns a new list from [head yi xi tail]

;; swap-right swaps the i-th element to the right
;; in a list 'lst[1...n]'
;; list-ref actually requires a list[0...n]
(define (swap-right lst i)
  (if (= i (length lst))
      lst ;; return lst if swapping last element.
      (let ((yi (list (list-ref lst i)))
	    (xi (list (list-ref lst (- i 1))))
	    (head (list-head lst (- i 1)))
	    (tail (list-tail lst (+ i 1))))
	(append head yi xi tail)))) ;; returns a new list from [head yi xi tail]

;; reference holds a list of pairs,
;; where in each pair, the first element is the direction
;; and the second element holds position
(define (reference-get-directions reference)
  (map car reference))
(define (reference-get-positions reference)
  (map cadr reference))

;; lists items in front of index, not inclusive.
(define (list-front lst index)
  (reverse (list-tail (reverse lst) (- (length lst) -1 index))))

;; list-head returns a sub-list from lst[0...i], inclusive.
(define (list-head lst i)
  (reverse (list-tail (reverse lst) (- (length lst) i))))

;; list-create returns a list of size and all elements are elem.
(define (list-create size elem)
  (if (= size 0)
      '()
      (append (list elem) (list-create (- size 1) elem))))

;; list-combine returns a single list
;;   where each item in the list is a list of ith item of list1 and list2.
(define (list-combine list1 list2)
  (map (lambda (x y)
	 (cons x (list y)))
       list1 list2))

;; creates a list of consecutive numbers from i to j inclusive.
;; where i <= j
(define (list-range i j)
  (if (= i j)
      (list i)
      (cons i (list-range (+ i 1) j))))

;; uses the summation of the absolute value in each item of list
(define (list-check-if-all-zero? lst)
  (= 0 (apply + (map abs lst))))

;; ////////////////////////////////////////////////////////////////////////////
;; // PART 2 ANAGRAM

;; dict: is a list of symbols representing legal words.
;; lst: is a list of symbols of any length
;; anagram extends the permute function to find all permutations of lst
;;   such that the word formed from the symbols in the permutation of lst
;;   exists in dict, i.e. the permutation is a legal word.
(define (anagram dict lst)
  (let ((dict (list-extract-by-size dict (length lst)))) ;; shorten dict to only items of same size.
    (cond
     ((null? lst) '()) ;; if lst is empty, just return.
     ((null? dict) '()) ;; if dict is empty, just return.
     (else ;; Use permute's code to find all permutations of lst
      ;; initialize reference (direction-position) pair list		  
      (let ((reference (permute-init-reference (length lst))))
	;; in each iteration, permute-step each reference
	;; and find the next available mobile-position
	(do ((reference reference (permute-step reference))
	     (mobile-position (permute-find-mobile-position reference)
			      (permute-find-mobile-position reference)))
	    ;; mobile-position can either be an integer representing a position, or #f
	    ((not mobile-position) ;; if mobile-position is #f
	     #f) ;; then return #f and end the program.
	  ;; MAIN ANAGRAM CHECKING PART:
	  ;; deference the permutation position reference to get the permuted word.
	  (let ((wordperm (permute-dereference lst reference)))
	    ;; foreach word in the dictionary,
	    (for-each (lambda (word)
			;; compare if the word and the permuted word match
			(if (compare-symbol-symbollist word wordperm)
			    ;; if so, then print.
			    (print wordperm)))
		      dict))))))))

;; returns the length of a symbol.
(define (symbol-length str)
  (length (string->list (symbol->string str))))

;; returns a new list of symbols with only symbols of length {size}.
(define (list-extract-by-size lst size)
  (let loop ((result '())
	     (lst lst))
    (if (null? lst) result ;; if lst empty; return result.
	(if (= size (symbol-length (car lst)))
	    (loop (append result (list (car lst))) (cdr lst))
	    (loop result (cdr lst))))))
	     
(define (compare-symbol-symbollist symb symlist)
  (let ((symb2 (string->symbol (apply string-append (map symbol->string symlist)))))
    (eqv? symb symb2)))
			    

;;; FIXME: these utilities should be in a separate file

(define (complain name . args)
  (apply error
         (string-append (symbol->string name) ": illegal arguments")
         args))

;;; 1-argument version for internal use

(define (%string->text s)
  (if (string? s)
      (text-tabulate (lambda (i) (string-ref s i))
                     (string-length s))
      (complain 'string->text s)))

;;; A portable implementation can't rely on inlining,
;;; but it can rely on macros.

(define N 128)

(define (length&i0 len i0)
  (+ (* N len) i0))

#;
(define (length&i0.length k)
  (quotient k N))

#;
(define (length&i0.i0 k)
  (remainder k N))

(define-syntax length&i0.length
  (syntax-rules ()
   ((_ k)
    (quotient k N))))

(define-syntax length&i0.i0
  (syntax-rules ()
   ((_ k)
    (remainder k N))))

(define-record-type text-rtd
  (new-text0 k chunks)
  text?
  (k      text.k)
  (chunks text.chunks))

(define (%new-text len i0 chunks)
  (new-text0 (length&i0 len i0) chunks))

(define the-empty-text
  (%new-text 0 0 (vector (make-string 0))))

;;; text? is defined by the record definition above.

;;; FIXME: should have non-checking versions of these for internal use

#;
(define-syntax %text?
  (syntax-rules ()
   ((_ txt0)
    (let ((txt txt0))
      (and (vector-like? txt)
           (eq? 3 (vector-like-length txt))
           (let ((v (vector-ref:trusted txt 0)))
             (and (vector? v)
                  (eq? 7 (vector-length v))
                  (eq? text-rtd (vector-ref:trusted v 0)))))))))

(define-syntax %text?
  (syntax-rules ()
   ((_ txt0)
    (let ((txt txt0))
      (and (vector-like? txt)
           (eq? 5 (typetag txt))
           (let ((v (vector-ref:trusted txt 0)))
             (eq? text-rtd (vector-ref:trusted v 0))))))))

(define-syntax text-length
  (syntax-rules ()
   ((_ txt0)
    (let ((txt txt0))
      (and (%text? txt)
           (quotient (vector-ref:trusted txt 1) N))))))

#;
(define (text-length txt)
  (if (text? txt)
      (length&i0.length (text.k txt))
      (error "text-length: not a text" txt)))

(define-syntax text-ref
  (syntax-rules ()
   ((_ txt0 idx0)
    (let ((txt txt0) (i idx0))
      (and (%text? txt)
           (exact-integer? i)
           (<= 0 i)
           (let* ((k      (vector-ref:trusted txt 1))
                  (chunks (vector-ref:trusted txt 2))
                  (len    (quotient k N))
                  (i0     (remainder k N))
                  (i+i0   (+ i i0))
                  (j      (quotient i+i0 N))
                  (ii     (remainder i+i0 N)))
             (if (< i len)
                 (let ((sj (vector-ref:trusted chunks j)))
                   (string-ref sj ii))
                 (/ 55 0))))))))

#;
(define (text-ref txt i)
  (if (and (text? txt)
           (exact-integer? i)
           (<= 0 i))
      (let* ((k      (text.k txt))
             (chunks (text.chunks txt))
             (len    (length&i0.length k))
             (i0     (length&i0.i0 k))
             (i+i0   (+ i i0))
             (j      (quotient i+i0 N))
             (ii     (remainder i+i0 N)))
        (if (< i len)
            (let ((sj (vector-ref chunks j)))
              (string-ref sj ii))
            (error "text-ref: index out of range" txt i)))
      (error "text-ref: illegal arguments" txt i)))

;;; Non-checking versions for internal use.

(define (%text-length txt)
  (length&i0.length (text.k txt)))

(define (%text-ref txt i)
  (let* ((k      (vector-ref:trusted txt 1))
         (chunks (vector-ref:trusted txt 2))
         (len    (length&i0.length k))
         (i0     (length&i0.i0 k))
         (i+i0   (+ i i0))
         (j      (quotient i+i0 N))
         (ii     (remainder i+i0 N)))
    (let ((sj (vector-ref:trusted chunks j)))
      (string-ref sj ii))))

(define (%subtext txt start end)
  (subtext txt start end))

;;; text-tabulate avoids side effects (in case proc returns more than once)

(define (text-tabulate proc len)
  (if (= 0 len)
      the-empty-text
      (let loop ((i len)       ; highest index that's been tabulated
                 (chunks '())
                 (chars '()))
        (cond ((= 0 i)
               (%new-text len
                           0
                           (list->vector
                            (cons (list->string chars)
                                  chunks))))
              ((and (= 0 (remainder i N))
                    (not (null? chars)))
               (loop i
                     (cons (list->string chars) chunks)
                     '()))
              (else
               (let* ((i-1 (- i 1))
                      (c (proc i-1)))
                 (if (char? c)
                     (loop i-1
                           chunks
                           (cons c chars))
                     (error "text-tabulate: proc returned a non-character"
                            proc len c))))))))

;;; FIXME: should the fast case do something different
;;; if the length of the result is sufficiently small?
;;; Probably not: splitting a 100-character text into
;;; 100 1-character texts should be fast.

(define (subtext txt start end)
  (cond ((and (text? txt)
              (exact-integer? start)
              (exact-integer? end)
              (<= 0 start end))
         (%subtext txt start end))
        ((string? txt)
         (%string->text (substring txt start end)))
        (else
         (complain 'subtext txt start end))))         

(define (%subtext txt start end)
  (let* ((k      (text.k txt))
         (chunks (text.chunks txt))
         (len    (length&i0.length k))
         (i0     (length&i0.i0 k))
         (i+i0   (+ start i0))
         (end+i0 (+ end i0))
         (len+i0 (+ len i0))
         (jstart (quotient i+i0 N))
         (jend   (quotient end+i0 N))
         (jlen   (quotient len N)))
    (if (<= end len)
        (cond ((= start end)
               the-empty-text)
              ((and (= 0 jstart)
                    (= jlen jend))
               ;; the fast case
               (%new-text (- end start) i+i0 chunks))
              (else
               (let* ((v (make-vector (+ 1 (- jend jstart)))))
                 (do ((j jstart (+ j 1)))
                     ((> j jend))
                   (vector-set! v (- j jstart) (vector-ref chunks j)))
                 (%new-text (- end start)
                             (remainder i+i0 N)
                             v))))
        (error "subtext: end out of range" txt start end))))

;;; FIXME: no sharing yet
;;; FIXME: portable code shouldn't rely on CSE

(define (text-concatenate texts)
  (cond ((not (list? texts))
         (complain 'text-concatenate texts))
        ((null? texts) the-empty-text)
        ((null? (cdr texts))
         (let ((txt (car texts)))
           (cond ((text? txt) txt)
                 ((string? txt)
                  (%string->text txt))
                 (else (complain 'text-concatenate texts)))))
        (else
         (let loop ((items (reverse texts))
                    (real-texts '())
                    (n 0))
           (cond ((null? items)
                  (%text-concatenate-n real-texts n))
                 ((text? (car items))
                  (loop (cdr items)
                        (cons (car items) real-texts)
                        (+ n (text-length (car items)))))
                 ((string? (car items))
                  (loop (cdr items)
                        (cons (%string->text (car items)) real-texts)
                        (+ n (string-length (car items)))))
                 (else
                  (complain 'text-concatenate texts)))))))

(define (%text-concatenate-n texts n)
  (if (= 0 n)
      the-empty-text
      (let* ((n/N     (quotient n N))
             (m       (remainder n N))
             (nchunks (if (= 0 m) n/N (+ 1 n/N)))
             (chunks  (make-vector nchunks)))
        (do ((i 0 (+ i 1)))
            ((= i n/N)
             (if (> m 0)
                 (vector-set! chunks i (make-string m))))
          (vector-set! chunks i (make-string N)))
        (let loop ((texts (cdr texts))
                   (txt (car texts))
                   (i 0)    ; index into result text
                   (j 0)    ; index into chunks
                   (k 0)    ; index into (vector-ref chunks j)
                   (ti 0))  ; index into txt
          (cond ((= i n)
                 (%new-text n 0 chunks))
                ((= k N)
                 (loop texts txt i (+ j 1) 0 ti))
                ((= ti (text-length txt))
                 (loop (cdr texts) (car texts) i j k 0))
                (else
                 (string-set! (vector-ref chunks j) k (text-ref txt ti))
                 (loop texts txt (+ i 1) j (+ k 1) (+ ti 1))))))))

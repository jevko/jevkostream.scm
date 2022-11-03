(define opener #\[)
(define closer #\])
(define escaper #\`)

(define (send next key . args)
  (let ((fn (assv-ref next key)))
    (if (not (eqv? fn #f)) 
      (apply fn args)
      ;; (display fn)
    )
  )
)

(define (parse-jevko-stream next)
  (let* (
    (is-escaped #f)
    (parents '())
    ;; line, col
    (chunk (lambda (chnk)
      (string-for-each (lambda (c)
        (cond
          (is-escaped (if 
            (or (eqv? c escaper) (eqv? c opener) (eqv? c closer)) 
            (begin
              (set! is-escaped #f)
              (send next 'character c)
            ) 
            ;; todo: error msgs
            (error "idi")
          ))
          ((eqv? c escaper)
            (set! is-escaped #t)
            (send next 'escaper c)
          )
          ((eqv? c opener)
            ;; no maxdepth for now
            (set! parents (cons "ln,col" parents))
            (send next 'opener c)
          )
          ((eqv? c closer)
            ;; error msgs
            (if (= (length parents) 0) (error "uc"))
            (set! parents (cdr parents))
            (send next 'closer c)
          )
          (else
            (send next 'character c)
          )
        )
      ) chnk)
    ))
    (end (lambda ()
      ;; todo: error msgs
      (if is-escaped (error "ueae"))
      ;; todo: say which ln,col
      (if (not (= (length parents) 0)) (error "uemc"))
      (send next 'end)
    ))
  )
    ;; sth like this
    (list (cons 'chunk chunk) (cons 'end end))
  )
)

(define stream (parse-jevko-stream (list
  (cons 'character (lambda (c) (display c)))
  (cons 'end (lambda () (display "END")))
)))

(send stream 'chunk "aaaa[bbb]")
(send stream 'end)
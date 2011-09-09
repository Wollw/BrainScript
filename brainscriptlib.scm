; A Brainfuck interpreter in Scheme
(define bf-stack-size 30000)

(define bf-file "")
(if (> (length (command-line)) 1)
    (set! bf-file (open-input-file (list-ref (command-line) 1))))

; Main program
(define (bf-run program stack-size)
    ; Create the stack filled with 0s
    (define stack (make-vector stack-size 0))
    ; Get first command and set program counter and stack pointer to initial address
    (let loop ((command (vector-ref program 0)) (program-counter 0) (stack-pointer 0))
;        (bf-debug-message program-counter stack-pointer command stack)
        ; Execute the current command
        (cond   
            ((equal? command #\>)
                (set! stack-pointer (+ stack-pointer 1))
                (if (equal? stack-pointer stack-size)
                    (bf-error (string-append
                            "Stack Overflow at "
                            (number->string program-counter)))))
            ((equal? command #\<)
                (set! stack-pointer (- stack-pointer 1))
                (if (negative? stack-pointer)
                    (bf-error (string-append
                            "Stack Underflow at "
                            (number->string program-counter)))))
            ((equal? command #\+)
                (vector-set! stack stack-pointer (+ (vector-ref stack stack-pointer) 1)))
            ((equal? command #\-)
                (vector-set! stack stack-pointer (- (vector-ref stack stack-pointer) 1)))
            ((equal? command #\.)
                (display (integer->char (vector-ref stack stack-pointer))))
            ((equal? command #\,)
                (vector-set! stack stack-pointer (char->integer (read-char)))) ; needs work
            ((equal? command #\[)
                (if (zero? (vector-ref stack stack-pointer))
                    (let loop ((cmd (vector-ref program program-counter))(depth 0))
                        (cond
                            ((equal? cmd #\[)
                                (set! depth (+ depth 1)))
                            ((equal? cmd #\])
                                (set! depth (- depth 1))))
                        (set! program-counter (+ program-counter 1))
                        (if (not (zero? depth))
                            (loop (vector-ref program program-counter) depth)))
                        (set! program-counter (+ program-counter 1))))
            ((equal? command #\])
                (if (not (zero? (vector-ref stack stack-pointer)))
                    (let loop ((cmd (vector-ref program program-counter))(depth 0))
                        (cond
                            ((equal? cmd #\])
                                (set! depth (+ depth 1)))
                            ((equal? cmd #\[)
                                (set! depth (- depth 1))))
                        (set! program-counter (- program-counter 1))
                        (if (not (zero? depth))
                            (loop (vector-ref program program-counter) depth))))
                (set! program-counter (+ program-counter 1)))
            (else
                (bf-error "Invalid command found.")))
        ; Increment program-counter if needed (if not changed by [ or ] that is)
        (if (and (not (equal? command #\[)) (not (equal? command #\])))
            (set! program-counter (+ program-counter 1)))
        ; Repeat for next command if available. Quit otherwise.
        (cond
            ((and (< program-counter (vector-length program)) (>= program-counter 0))
                (loop (vector-ref program program-counter) program-counter stack-pointer))
            (else
                (quit 0)))))

; Read the program from file striping out any characters that aren't commands
(define (bf-get-program-from-file file)
    (define program '())
    (let loop ((c (read-char file)))
        (cond
            ((eof-object? c)
                (list->vector program))
            ((or (equal? c #\>) (equal? c #\<) (equal? c #\+) (equal? c #\-)
                 (equal? c #\.) (equal? c #\,) (equal? c #\[) (equal? c #\]))
                (set! program (append program (list c)))
                (loop (read-char file)))
            (else
                (loop (read-char file))))))

; Print error message and quit with signal 1
(define (bf-error message)
    (newline)
    (display "Brainfuck Error: ")
    (display message)
    (newline)
    (quit 1))

; Debug message
(define (bf-debug-message pc sp cmd stk)
        (newline)
        (display pc)
        (display " ")(display sp)
        (display " ")(display cmd)(display " : ")
        (display stk)(display ": "))

; Read a trollscript file and convert to Brainf***
(define (ts-get-program-from-file file)
    (define program '())
    (define dict '(
        "ooo"  #\>
        "ool"  #\<
        "olo"  #\+
        "oll"  #\-
        "loo"  #\.
        "lol"  #\)
        "llo"  #\[
        "lll"  #\]))
    (let loop ((s (ts-read-command file)))
        (if (member s dict)
            (set! program (append program (list (cadr (member s dict))))))
        (if (equal? s "EOF")
            (list->vector program)
            (loop (ts-read-command file)))))

; read a trollscript command from the program file
(define (ts-read-command file)
    (define s "")
    (let loop ((c (read-char file)))
        (if (not (eof-object? c))
            (set! c (char-downcase c)))
        (if (or (equal? c #\o) (equal? c #\l))
                (set! s (string-append s (string c)))
                (set! s (string-append s "?")))
        (if (equal? (string-length s) 3)
            s
            (begin
                (if (eof-object? c)
                    "EOF"
                    (loop (read-char file)))))))
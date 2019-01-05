(import (scheme base))
(import (scheme char))
(import (scheme eval))
(import (scheme read))
(import (scheme write))


(import (chibi emscripten))
(import (chibi match))
(import (chibi parse))


(write-string "chibi scheme is running...\n")

;; (eval-script! "window.addEventListener('click', function () {
;;   document.inbox += 1;
;;   Module['resume'](); // give control back to the Scheme process
;; })")

(eval-script! "document.resume = Module['resume']")

;; scheme helpers

(define (scm->string scm)
  (let ((port (open-output-string)))
    (write scm port)
    (get-output-string port)))

(define (string->scm string)
  (let ((port (open-input-string string)))
    (read port)))

(define (dg . args)
  (write-string (scm->string args))
  (newline))

(define (pk . args)
  (apply dg args)
  (car (reverse args)))

(define (acons a b alist)
  (cons (cons a b) alist))

(define (rm alist key)
  (let loop ((alist alist)
             (out '()))
    (if (null? alist)
        out
        (if (equal? (caar alist) key)
            (append out (cdr alist))
            (loop (cdr alist) (cons (car alist) out))))))

(define (set alist key value)
  (acons key value (rm alist key)))

(define (ref alist key)
  (let loop ((alist alist))
    (cond
     ((null? alist) #f)
     ((equal? (caar alist) key) (cdar alist))
     (else (loop (cdr alist))))))

(define (ref* assoc . keys)
  (let loop ((keys keys)
             (assoc assoc))
    (cond
     ((eq? assoc #f) #f)
     ((null? keys) assoc)
     (else (loop (cdr keys) (ref assoc (car keys)))))))

(define (set* assoc . args)
  (let* ((args* (reverse args)) ;; XXX: WTF?!
         (value (car args*))
         (keys (reverse (cdr args*)))) ;; WTF?!
    (let loop ((keys keys)
               (assoc assoc))
      (if (null? keys)
          value
          (set assoc (car keys) (loop (cdr keys) (or (ref assoc (car keys)) '())))))))

(define (rm* assoc . keys)
  (if (null? (cdr keys))
      (rm assoc (car keys))
      (let ((new (apply rm* (ref assoc (car keys)) (cdr keys))))
        (if (null? new)
            (rm assoc (car keys))
            (set assoc (car keys) new)))))

;; parse json
;;
;; MIT License
;;
;; Copyright (c) 2018 Thunk NYC Corp.
;;
;; https://github.com/thunknyc/scm-json/blob/master/LICENSE
;;

(define-grammar json
  (space ((* ,(parse-char char-whitespace?))))

  ;;> The number parser is currently quite primitive; it reads a
  ;;> sequence of characters matching [-+0-9eE,.] and attempts to
  ;;> parse it as a Scheme number.

  (number ((-> n (+ (or ,(parse-char char-numeric?)
                        #\. #\- #\+ #\e #\E)))
           (string->number (list->string n))))

  (string ((: ,(parse-char #\")
              (-> s (* ,(parse-not-char #\")))
              ,(parse-char #\"))
           (list->string s)))

  (atom ((-> n ,number) n)
        ((-> s ,string) s)
        ("true" #t)
        ("false" #f))

  (datum ((or ,atom ,array ,hash)))

  (array-el ((: "," ,space (-> el ,datum)) el))

  (array ((: "[" ,space (-> el ,datum) ,space
             (-> els (* ,array-el)) ,space "]")
          (apply vector el els))
         ((: "[" ,space "]") (vector)))

  (hash-el ((: "," ,space (-> k ,string) ,space
               ":" ,space (-> v ,datum)) (cons (string->symbol k) v)))

  (hash ((: "{" ,space (-> k ,string) ,space
            ":" ,space (-> v ,datum) ,space
            (-> els (* ,hash-el)) ,space "}")
         (apply list (cons (string->symbol k) v) els))
        ((: "{" ,space "}") '()))

  (object ((: ,space (-> o ,datum) ,space) o)))

;;> Call the JSON parser on the \scheme{(chibi parse)} parse stream
;;> \var{source}, at index \var{index}, and return the result, or
;;> \scheme{#f} if parsing fails.

(define (json->sexp source . o)
  (let ((index (if (pair? o) (car o) 0)))
    (parse object source index)))

;;
;; render json
;;

(define (assoc->json sexp)
  (let loop ((out "{")
             (sexp sexp))
    (if (null? sexp)
        (if (string=? out "{")
            "{}"
            (string-append (string-copy out 0 (- (string-length out) 1)) "}"))
        (loop (string-append out
                             (sexp->json (caar sexp))
                             ":"
                             (sexp->json (cdar sexp))
                             ",")
              (cdr sexp)))))

(define (list->json sexp)
  (let loop ((out "[")
             (sexp sexp))
    (if (null? sexp)
        (if (string=? out "[")
            "[]"
            (string-append (string-copy out 0 (- (string-length out) 1)) "]"))
        (loop (string-append out (sexp->json (car sexp)) ",")
              (cdr sexp)))))

(define (sexp->json sexp)
  (match sexp
    (#f "false")
    (#t "true")
    ('() "undefined")
    (('@ rest ...) (assoc->json rest))
    ((? pair? sexp) (list->json sexp))
    ((? symbol? sexp) (string-append "\"" (symbol->string sexp) "\""))
    ((? number? sexp) (number->string sexp))
    ((? string? sexp) (string-append "\"" sexp "\""))))

(define (make-node tag options children)
  `(@ (tag . ,tag)
      (options . ,(cons '@ options))
      (children . ,children)))


(define (%magic options)
  ;; prepare options for sexp->json
  `((attrs . ,(cons '@ (or (ref options 'attrs) '())))
    (on . ,(cons '@ (or (ref options 'on) '())))
    (key . ,(or (ref* options 'attrs 'key) '()))))

(define (magic attrs next-identifier)
  ;; shake around the attrs to make them compatible with snabbdom
  ;; options, associate callbacks to integer identifiers. The event on
  ;; a given node is associated with an integer, the integer is
  ;; associated with a callback. Return both snabbdom options and
  ;; callbacks assoc.
  (let loop1 ((attrs attrs)
              (next-identifier next-identifier)
              (out '())
              (callbacks '()))
    (if (null? attrs)
        (values (%magic out) callbacks)
        (match attrs
          ((('on . handlers) rest ...)
           (let loop2 ((handlers handlers)
                       (next-identifier next-identifier)
                       (out out)
                       (callbacks callbacks))
             (match handlers
               ('() (loop1 rest next-identifier out callbacks))
               (((event-name callback) handlers ...)
                (loop2 handlers
                       (+ 1 next-identifier)
                       (set* out 'on event-name next-identifier)
                       (acons next-identifier callback callbacks))))))
          (((key value) attrs ...) (loop1 attrs
                                          next-identifier
                                          (set* out 'attrs key value)
                                          callbacks))))))

(define (%sxml->snabbdom+callbacks sxml callbacks)
  (match sxml
    ((? string? string)
     (values string '()))
    ((tag ('@ attrs ...) rest ...)
     (call-with-values (lambda () (magic attrs (length callbacks)))
       (lambda (attrs new-callbacks)
         (let loop ((callbacks (append callbacks new-callbacks))
                    (rest rest)
                    (out '()))
           (if (null? rest)
               (values (make-node tag attrs (reverse out)) callbacks)
               (call-with-values (lambda () (%sxml->snabbdom+callbacks (car rest) callbacks))
                 (lambda (snabbdom new-callbacks)
                   (loop (append callbacks new-callbacks)
                         (cdr rest)
                         (cons snabbdom out)))))))))
    ((tag rest ...)
     ;; there is no magic but almost the same as above loop.
     (let loop ((callbacks callbacks)
                (rest rest)
                (out '()))
       (if (null? rest)
           (values (make-node tag '() (reverse out)) callbacks)
           (call-with-values (lambda () (%sxml->snabbdom+callbacks (car rest) callbacks))
             (lambda (snabbdom callbacks)
               (loop callbacks (cdr rest) (cons snabbdom out)))))))))

(define (sxml->snabbdom+callbacks sxml)
  (%sxml->snabbdom+callbacks sxml '()))

(define (render! model)
  (let ((sxml (view model)))
    (call-with-values (lambda () (sxml->snabbdom+callbacks sxml))
      (lambda (snabbdom callbacks)
        (eval-script! (string-append "document.javascript = " (sexp->json snabbdom) ";"))
        (eval-script! "document.recv()") ;; patch the dom
        callbacks))))

(define (control model callbacks event)
  (let* ((event* (json->sexp event))
         (identifier (ref event* 'identifier)))
    ((ref callbacks identifier) model event*)))

(define (create-app init view)
  (let ((model (init))
        (callbacks '()))
      (let loop ()
        (set! callbacks (render! model))
        (wait-on-event!) ;; yields control back to the browser
        (let ((event (string-eval-script "document.scheme")))
          (set! model (control model callbacks event))
          (loop)))))

;; app

(define (eval-string string)
  (eval (string->scm string) (environment '(scheme base))))

(define (init)
  0)

(define intro "Learning a new language is long adventure of correct and wrong.  Here through this interface that mimics an REPL you will learn Scheme programming language.")

(define exercices
  '(("What is (+ 41 1)" . 42)
    ("What is (+ 1 99 1)" . 101)
    ("What is (* 2 3 4)" . 18)
    ("What is (+ (* 100 10) 330 7)" . 1337)
    ("Err!!!..." '(please-fix-the-bug))))

(define (make-stdout string)
  `(div (@ (class "stdout")) ,(string-append ";; " string)))

(define (callback model event)
  (let ((out (eval-string (pk (ref* event 'event 'target.value)))))
    (if (equal? out (pk (cdr (pk (list-ref exercices (pk model))))))
        (+ 1 model)
        model)))

(define (view model)
  `(div (@ (id "container"))
        ,(make-stdout intro)
        ,@(let loop ((exercices exercices)
                     (out '())
                     (index 0))
            (if (< model index)
                (reverse out)
                (loop (cdr exercices)
                      (cons (make-stdout (caar exercices)) out)
                      (+ 1 index))))
        (input (@ (id "input")
                  (focus "true")
                  (key ,(number->string model))
                  (type "text")
                  (on (change ,callback))))))

(create-app init view)

;; everything that follows is dead code

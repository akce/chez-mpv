;; ftypes util functions chez scheme.
;; Copyright (c) 2019-2020 Akce. License: GPLv3, see COPYING for details.
(library (mpv ftypes-util)
  (export
   u8 u8* u8**
   alloc
   c-function c-default-function
   define-enum
   locate-library-object
   ;; byte/string array handling functions.
   u8*->string u8**->string-list
   string->u8* string-list->u8**
   free-u8**
   ;; Chez scheme re-exports. Saves client code from having to import these themselves.
   define-ftype foreign-alloc foreign-free foreign-ref
   ftype-pointer-address ftype-&ref ftype-ref ftype-set! ftype-sizeof load-shared-object)
  (import
   (chezscheme))

  (define-ftype u8 unsigned-8)
  (define-ftype u8* (* u8))
  (define-ftype u8** (* u8*))

  ;; [syntax] (alloc ((var varptr type)) ...)
  (define-syntax alloc
    (syntax-rules ()
      [(_ ((var type) ...) first rest ...)
       (let ([var (foreign-alloc (ftype-sizeof type))] ...)
         (let ([r (begin first rest ...)])
           (foreign-free var) ...
           r))]
      [(_ ((var varptr type) ...) first rest ...)
       (let ([var (foreign-alloc (ftype-sizeof type))] ...)
         (let ([varptr (make-ftype-pointer type var)] ...)
           (let ([r (begin first rest ...)])
             ;; make-ftype-pointer implicitly locks var, so manually unlock before free.
             (unlock-object var) ...
             (foreign-free var) ...
             r)))]
      [(_ ((var varptr type num) ...) first rest ...)
       ;; Ensure num is at least 1, that's a requirement of foreign-alloc.
       (let ([var (foreign-alloc (* (if (= num 0) 1 num) (ftype-sizeof type)))] ...)
         (let ([varptr (make-ftype-pointer type var)] ...)
           (let ([r (begin first rest ...)])
             ;; make-ftype-pointer implicitly locks var, so manually unlock before free.
             (unlock-object var) ...
             (foreign-free var) ...
             r)))]))

  (meta define string-map
        (lambda (func str)
          (list->string (map func (string->list str)))))

  (meta define symbol->function-name-string
        (lambda (sym)
          (string-map (lambda (c)
                        (if (eqv? c #\-)
                            #\_ c))
                      (symbol->string sym))))

  ;; [syntax] c-function: converts scheme-like function names to c-like function names before passing to foreign-procedure.
  ;; ie, word separating hyphens are converted to underscores for c.
  ;; eg,
  ;; (c-function (str-length (string) int) ....)
  ;; is converted to:
  ;; (begin
  ;;   (define str-length (foreign-procedure "str_length" (string) int))
  ;;   ...)
  (define-syntax c-function
    (lambda (stx)
      (syntax-case stx ()
        [(_ (name args return) ...)
         (with-syntax ([(function-string ...)
                        (map (lambda (n)
                               (datum->syntax n
                                 (symbol->function-name-string (syntax->datum n))))
                             #'(name ...))])
            #'(begin
                (define name
                  (foreign-procedure function-string args return)) ...))])))

  ;; [syntax] c-default-function: define c functions that take a default argument.
  ;; This behaves like c-function, except it first takes a (type, instance) pair.
  ;; c-default-function is useful for those c modules that define a bunch of functions that take
  ;; the same struct as the first argument.
  ;;
  ;; The expansion of this definition:
  ;; (c-default-function (type (current-parameter))
  ;;   (func-name1 (arg1) int)
  ;;   ...)
  ;; will look like:
  ;; (begin
  ;;   (define func-name1
  ;;     (let ([ffi-func (foreign-procedure "func_name1" (type arg1) int)])
  ;;       (lambda args (apply ffi-func (current-parameter) args))))
  ;;   ...)
  (define-syntax c-default-function
    (lambda (stx)
      (syntax-case stx ()
        [(_ (type instance) (name (arg ...) return) ...)
         (with-syntax ([(function-string ...)
                        (map (lambda (n)
                               (datum->syntax n
                                 (symbol->function-name-string (syntax->datum n))))
                             #'(name ...))])
            #'(begin
                (define name
                  (let ([ffi-func (foreign-procedure function-string (type arg ...) return)])
                    (lambda args
                      (apply ffi-func instance args)))) ...))])))

  ;; [syntax] define-enum: generates a syntax transformer that evaluates the value of an enum at compile time.
  ;; eg, using trace-define-syntax:
  ;; > (define-enum e [a 1] [b 2] [c 3])
  ;; |(define-enum (define-enum e (a 1) (b 2) (c 3)))
  ;; |(define-syntax e
  ;;    (lambda (x)
  ;;      (syntax-case x ()
  ;;        [(_ v) (eq? (datum v) (syntax->datum #'a)) #'1]
  ;;        [(_ v) (eq? (datum v) (syntax->datum #'b)) #'2]
  ;;        [(_ v) (eq? (datum v) (syntax->datum #'c)) #'3])))
  ;; > (e a)
  ;; 1
  ;; > (e b)
  ;; 2
  ;; > (e c)
  ;; 3
  ;; > (e d)
  ;; Exception: invalid syntax (e d)
  ;; Type (debug) to enter the debugger.
  ;; >
  (define-syntax define-enum
    (syntax-rules ()
      [(_ group (var* val*) ...)
       (define-syntax group
         (lambda (x)
           (syntax-case x ()
             [(_ v)
              (eq? (datum v) (syntax->datum #'var*))
              #'val*] ...)))]))

  ;; [procedure] locate-library-object: find first instance of filename within (library-directories) object directories.
  ;; Returns full path of located file, including the filename itself. filename only if not found.
  (define locate-library-object
    (lambda (filename)
      (let loop ([fps (map (lambda (d) (string-append (cdr d) "/" filename)) (library-directories))])
        (cond
         [(null? fps)
          filename]
         [(file-exists? (car fps))
          (car fps)]
         [else
          (loop (cdr fps))]))))

  ;; [proc] return ftypes (* unsigned-8) as a UTF8 string.
  (define u8*->string
    (lambda (fptr)
      (utf8->string
       (let f ([i 0])
         (let ([c (foreign-ref 'unsigned-8 fptr i)])
           (if (fx= c 0)
             (make-bytevector i)
             (let ([bv (f (fx+ i 1))])
               (bytevector-u8-set! bv i c)
               bv)))))))

  ;; u8** = vector of u8*
  (define u8**->string-list
    (lambda (u8** nitems)
      (let ([ptr-size (ftype-sizeof void*)])
        (do ([i 0 (+ i 1)]
             [v (make-vector nitems)
                (let ([saddr (foreign-ref 'void* u8** (* i ptr-size))])
                  (vector-set! v i (u8*->string saddr))
                  v)])
            ((= i nitems) (vector->list v))))))

  ;; [proc] return scheme string object as a ftypes u8* memory block.
  (define string->u8*
    (lambda (str)
      ;; foreign-alloc string and copy in the bytes.
      (let* ([bv (string->utf8 str)]
             [len (bytevector-length bv)])
        (let ([ret
               (do ([i 0 (fx+ i 1)]
                    [fv (foreign-alloc (fx+ 1 len))
                        (begin
                          (foreign-set! 'unsigned-8 fv i (bytevector-u8-ref bv i))
                          fv)])
                   ((= i len) fv))])
          (foreign-set! 'unsigned-8 ret len 0)	;; null terminate.
          ret))))

  (define string-list->u8**
    (lambda (str*)
      (define string->u8*/null
        (lambda (str)
          (if str
            (string->u8* str)
            0)))
      (let ([len (length str*)]
            [ptr-sz (ftype-sizeof void*)])
        (do ([i 0 (+ i 1)]
             [v (foreign-alloc (* len ptr-sz))
                (let ([fstr (string->u8*/null (list-ref str* i))])
                  (foreign-set! 'void* v (* i ptr-sz) fstr)
                  v)])
            ((= i len) v)))))

  (define free-u8**
    (case-lambda
     ([u8**]
      (let loop ([i 0])
        (let ([p (foreign-ref 'void* u8** (* i (ftype-sizeof void*)))])
          (cond
           [(= p 0)
            ;; free containing u8** block.
            (foreign-free u8**)]
           [else
            ;; free individual u8 pointers.
            (foreign-free p)
            (loop (fx+ i 1))]))))
     ([u8** len]
      ;; free individual u8 pointers.
      (for-each
       (lambda (i)
         (foreign-free (foreign-ref 'void* u8** (* i (ftype-sizeof void*)))))
       (iota len))
      ;; free containing u8** block.
      (foreign-free u8**)))))

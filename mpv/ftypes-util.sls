;; chez scheme ftypes FFI util functions.
;; Released into the public domain.
(library (mpv ftypes-util)
  (export
   u8 u8* u8**
   c_funcs
   enum
   locate-library-object
   ;; byte/string array handling functions.
   u8*->string u8**->string-list
   string->u8* string-list->u8**
   free-u8**
   ;; Chez scheme re-exports. Saves client code from having to import these themselves.
   define-ftype load-shared-object)
  (import
   (chezscheme))

  (define-ftype u8 unsigned-8)
  (define-ftype u8* (* u8))
  (define-ftype u8** (* u8*))

  ;; [syntax] c_funcs: converts scheme-like function names to c-like function names before passing to foreign-procedure.
  ;; ie, word separating hyphens are converted to underscores for c.
  ;; eg,
  ;; (c_funcs (str-length (string) int) ....)
  ;; is converted to:
  ;; (begin
  ;;   (define str-length (foreign-procedure "str_length" (string) int))
  ;;   ...)
  (define-syntax c_funcs
    (lambda (stx)
      (define string-map
        (lambda (func str)
          (list->string (map func (string->list str)))))
      (define symbol->curses-name
        (lambda (sym)
          (string-map (lambda (c)
                        (if (eqv? c #\-)
                            #\_ c))
                      (symbol->string sym))))
      (syntax-case stx ()
        [(_ (name args return))
         (quasisyntax
          (define name
            (foreign-procedure (unsyntax (symbol->curses-name (syntax->datum #'name))) args return)))]
        [(_ f ...) (syntax (begin (c_funcs f) ...))])))

  (define-syntax enum
    (syntax-rules ()
      [(_ name (symbol value) ...)
       (begin (define symbol value) ...)]))

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
           [(fx=? p 0)
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

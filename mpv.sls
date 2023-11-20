;; libmpv bindings for Chez Scheme.
;; Copyright (c) 2019-2023 Jerry
;; SPDX-License-Identifier: GPL-3.0-or-later
(library (mpv)
  (export
   current-mpv-handle

   mpv-event? mpv-event-id mpv-event-error mpv-event-reply-userdata mpv-event-data
   mpv-client-message-event? mpv-client-message-event-args
   mpv-property-event? mpv-property-event-name mpv-property-event-value

   ;; mpv-event-end-file is only two fields, so just do as a list/pair.
   (rename
     (car mpv-event-end-file-reason)
     (cdr mpv-event-end-file-error))

   mpv-get-property/flag
   mpv-get-property/long
   mpv-get-property/string
   mpv-get-property/node

   mpv-set-option

   mpv-set-property/double
   mpv-set-property/flag
   mpv-set-property/int
   mpv-set-property/string

   mpv-client-api-version
   mpv-error-string
   mpv-free
   mpv-client-name
   mpv-create
   mpv-initialize
   mpv-destroy
   mpv-terminate-destroy
   mpv-create-client
   mpv-create-weak-client
   mpv-load-config-file
   mpv-get-time-us
   mpv-free-node-contents
   mpv-command
   mpv-command-node
   mpv-command-string
   mpv-command-async
   mpv-command-node-async
   mpv-set-property
   mpv-set-property-async
   mpv-get-property
   mpv-get-property-string
   mpv-get-property-osd-string
   mpv-get-property-async
   mpv-observe-property
   mpv-unobserve-property
   mpv-event-name
   mpv-request-event
   mpv-request-log-messages
   mpv-wait-event
   mpv-wakeup
   mpv-set-wakeup-callback
   mpv-wait-async-requests
   mpv-hook-add
   mpv-hook-continue
   ;; deprecated
   mpv-get-wakeup-pipe

   ;; enums
   mpv-error
   mpv-event-type
   mpv-event-deprecated
   mpv-file-end-reason
   mpv-format
   mpv-log-level
   )
  (import
   (rnrs)
   (mpv ftypes-util)
   (only (chezscheme) iota make-parameter))

  (define load-lib (load-shared-object "libmpv.so.2"))

  (define-ftype mpv-handle void*)

  (define-enum mpv-error
    (success			0)
    (event-queue-full		-1)
    (nomem			-2)
    (uninitialized		-3)
    (invalid-parameter		-4)
    (option-not-found		-5)
    (option-format		-6)
    (option-error		-7)
    (property-not-found		-8)
    (property-format		-9)
    (property-unavailable	-10)
    (property-error		-11)
    (command			-12)
    (loading-failed		-13)
    (ao-init-failed		-14)
    (vo-init-failed		-15)
    (nothing-to-play		-16)
    (unknown-format		-17)
    (unsupported		-18)
    (not-implemented		-19)
    (generic			-20))

  (define-enum mpv-event-type
    (none			0)
    (shutdown			1)
    (log-message		2)
    (get-property-reply		3)
    (set-property-reply		4)
    (command-reply		5)
    (start-file			6)
    (end-file			7)
    (file-loaded		8)
    (client-message		16)
    (video-reconfig		17)
    (audio-reconfig		18)
    (seek			20)
    (playback-restart		21)
    (property-change		22)
    (queue-overflow		24)
    (hook			25)
    )

  ;; Deprecated event types.
  ;; Most (but not all) of these can be observed as properties.
  ;; See <mpv/client.h> for details.
  ;; This exists here because mpv still generates some of these events so it's useful
  ;; for client code to ignore them and/or be aware of those likely to disappear in future!
  (define-enum mpv-event-deprecated
    (tracks-changed		9)
    (track-switched		10)
    (idle			11)
    (pause			12)
    (unpause			13)
    (tick			14)
    (script-input-dispatch	15)
    (metadata-update		19)
    (chapter-change		23)
    )

  (define-enum mpv-file-end-reason
    (eof			0)
    (stop			2)
    (quit			3)
    (error			4)
    (redirect			5))

  (define-enum mpv-format
    (none			0)
    (string			1)
    (osd-string			2)
    (flag			3)
    (int64			4)
    (double			5)
    (node			6)
    (node-array			7)
    (node-map			8)
    (byte-array			9))

  (define-enum mpv-log-level
    (none			0)
    (fatal			10)
    (error			20)
    (warn			30)
    (info			40)
    (v				50)
    (debug			60)
    (trace			70))

  (define-ftype mpv-byte-array
    (struct
     [data	void*]
     [size	size_t]))
  (define-ftype mpv-byte-array* (* mpv-byte-array))

  (define-ftype c/mpv-event
    (struct
     [event-id		int]
     [error		int]
     [reply-userdata	unsigned-64]
     [data		void*]))

  (define-ftype mpv-event-end-file
    (struct
     [reason	int]
     [error	int]))
  (define-ftype mpv-event-end-file* (* mpv-event-end-file))

  (define-ftype mpv-event-client-message
    (struct
     [num-args	int]
     [args	u8**]))
  (define-ftype mpv-event-client-message* (* mpv-event-client-message))

  (define-ftype mpv-event-hook
    (struct
     [name	u8*]
     [id	unsigned-64]))
  (define-ftype mpv-event-hook* (* mpv-event-hook))

  (define-ftype mpv-event-log-message
    (struct
     [prefix	u8*]
     [level	u8*]
     [text	u8*]
     [log-level	int]))
  (define-ftype mpv-event-log-message* (* mpv-event-log-message))

  (define-ftype
    [mpv-node
     (struct
      [u
       (union
        [string	u8*]
        [flag		boolean]
        [int64		integer-64]
        [double	double]
        [node-list	(* mpv-node-list)]
        [ba		mpv-byte-array*])]
      [mpv-format	int])]
    [mpv-node-list
     (struct
      [num	int]
      [values	(* mpv-node)]
      [keys	u8**])])
  (define-ftype mpv-node* (* mpv-node))
  (define-ftype mpv-node-list* (* mpv-node-list))

  (define-ftype mpv-event-property
    (struct
     [name	u8*]
     [format	int]
     [data	void*]))
  (define-ftype mpv-event-property* (* mpv-event-property))

  (define-ftype wakeup-cb-t
    (function (void*) void))

  (c-function
    (mpv-client-api-version	()				unsigned-long)
    (mpv-error-string		(int)				string)
    (mpv-free			(void*)				void)
    (mpv_create			()				mpv-handle)
    (mpv-free-node-contents	((* mpv-node))			void)
    (mpv-event-name		(int)				string)
    )

  (c-default-function (mpv-handle (current-mpv-handle))
    (mpv-initialize		()				int)
    (mpv-client-name		()				string)
    (mpv-destroy		()				void)
    (mpv-terminate-destroy	()				void)
    (mpv-create-client		(string)			mpv-handle)
    (mpv-create-weak-client	(string)			mpv-handle)
    (mpv-load-config-file	(string)			mpv-handle)
    (mpv-get-time-us		()				integer-64)
    (mpv_set_option		(string int (* mpv-node))	int)
    (mpv_set_option_string	(string string)			int)
    (mpv_command		((* u8*))			int)
    (mpv-command-node		((* mpv-node) (* mpv-node))	int)
    (mpv-command-string		(string)			int)
    (mpv-command-async		(unsigned-64 (* u8*))		int)
    (mpv-command-node-async	(unsigned-64 (* mpv-node))	int)
    (mpv_set_property		(string int (* mpv-node))	int)
    (mpv-set-property-string	(string string)			int)
    (mpv-set-property-async	(unsigned-64 string int void*)	int)
    (mpv-get-property		(string int void*)		int)
    (mpv-get-property-string	(string)			(* u8))
    (mpv-get-property-osd-string	(string)		(* u8))
    (mpv-get-property-async	(unsigned-64 string int)	int)
    (mpv-observe-property	(unsigned-64 string int)	int)
    (mpv-unobserve-property	(unsigned-64)			int)
    (mpv-request-event		(int int)			int)
    (mpv-request-log-messages	(string)			int)
    (mpv_wait_event		(double)			(* c/mpv-event))
    (mpv-wakeup			()				void)
    (mpv-set-wakeup-callback	((* wakeup-cb-t) void*)	        void)
    (mpv-wait-async-requests	()				void)
    (mpv-hook-add		(unsigned-64 string int)	int)
    (mpv-hook-continue		(unsigned-64)			int)
    ;; deprecated
    (mpv-get-wakeup-pipe	()				int))

  (define current-mpv-handle (make-parameter #f))

  (define mpv-create
    (lambda ()
      (current-mpv-handle (mpv_create))
      (current-mpv-handle)))

  (define mpv-command
    (lambda args
      ;; TODO the ftypes wrapper should hide the null (#f) terminator.
      (let ([args/c (string-list->u8** (append args '(#f)))])
        (let ([ret (mpv_command args/c)])
          (free-u8** args/c)
          ret))))

  (define int->bool
    (lambda (num)
      (if (fx=? num 0)
          #f
          #t)))

  (define-syntax switch
    (syntax-rules (else)
      [(_ var (val1 body1 ...) (valn bodyn ...) ... (else bodye))
       (let ([v var])
         (cond
          [(fx=? v val1) body1 ...]
          [(fx=? v valn) bodyn ...] ...
          [else bodye]))]
      [(_ var (val1 body1 ...) (valn bodyn ...) ...)
       (let ([v var])
         (cond
          [(fx=? v val1) body1 ...]
          [(fx=? v valn) bodyn ...] ...))]
      ))

  (define node->scheme
    (lambda (node)
      (switch (ftype-ref mpv-node (mpv-format) node)
       [(mpv-format string)
        (u8*->string (ftype-pointer-address (ftype-ref mpv-node (u string) node)))]
       [(mpv-format flag)
        (ftype-ref mpv-node (u flag) node)]
       [(mpv-format int64)
        (ftype-ref mpv-node (u int64) node)]
       [(mpv-format double)
        (ftype-ref mpv-node (u double) node)]
       [(mpv-format node-map)
        (node-map->alist (ftype-ref mpv-node (u node-list) node))]
       [(mpv-format node-array)
        (node-list->list (ftype-ref mpv-node (u node-list) node))]
       [(mpv-format node)
        (node->scheme (ftype-ref mpv-node (u node-list) node))])))

  (define node-list->list
    (lambda (node-list)
      (let ([num (ftype-ref mpv-node-list (num) node-list)])
        ;; slight optimisation: build the list from the back since we already know the size.
        ;; Saves having to reverse the accumulated list before returning.
        (let loop ([i (- num 1)] [acc '()])
          (cond
           [(< i 0) acc]
           [else
            (loop (- i 1) (cons (node->scheme (ftype-&ref mpv-node-list (values i) node-list)) acc))])))))

  ;; Use a restricted form of alist, suitable for mpv where key can only be an atom of string or some number.
  (define alist?
    (lambda (obj)
      (and
        (list? obj)
        (for-all
          (lambda (p)
            (and (pair? p)
                 ;; for mpv, key must be a number or string.
                 (or (number? (car p))
                     (string? (car p)))))
          obj))))

  (define node-map->alist
    (lambda (node-list)
      (let ([num (ftype-ref mpv-node-list (num) node-list)])
        ;; slight optimisation: build the list from the back since we already know the size.
        ;; Saves having to reverse the accumulated list before returning.
        (let loop ([i (- num 1)] [acc '()])
          (cond
           [(< i 0) acc]
           [else
            (loop (- i 1) (cons
                           (cons
                            (u8*->string
                             (ftype-pointer-address (ftype-ref mpv-node-list (keys i) node-list)))
                            (node->scheme
                             (ftype-&ref mpv-node-list (values i) node-list)))
                           acc))])))))

  (define mpv-get-property/flag
    (lambda (property)
      (auto-ptr ([flag int])
        (let ([rc (mpv-get-property property (mpv-format flag) flag)])
          (if (< rc 0)
              (error 'mpv-get-property/flag (mpv-error-string rc) property)
              (int->bool (foreign-ref 'int flag 0)))))))

  (define mpv-get-property/long
    (lambda (property)
      (auto-ptr ([num integer-64])
        (let ([rc (mpv-get-property property (mpv-format int64) num)])
          (if (< rc 0)
              (error 'mpv-get-property/long (mpv-error-string rc) property)
              (foreign-ref 'integer-64 num 0))))))

  (define mpv-get-property/string
    (lambda (property)
      (auto-ptr ([str u8*])
        (let ([rc (mpv-get-property property (mpv-format string) str)])
          (if (< rc 0)
              (error 'mpv-get-property/string (mpv-error-string rc) property)
              (let* ([ptr (foreign-ref 'void* str 0)]
                     [ret (u8*->string ptr)])
                (mpv-free ptr)
                ret))))))

  (define mpv-get-property/node
    (lambda (property)
      (auto-ptr ([data &data mpv-node])
        (let ([rc (mpv-get-property property (mpv-format node) data)])
          (if (< rc 0)
              (error 'mpv-get-property/node (mpv-error-string rc) property)
              (let* ([ret (node->scheme &data)])
                (mpv-free-node-contents &data)
                ret))))))

  ;;;; Start: mpv-node setters.
  ;; These all allocate their own mpv-node, set it to the appropriate mpv-format,
  ;; and then pass through to the wanted property or option setter.

  (define bool-node-set!
    (lambda (node* val)
      (ftype-set! mpv-node (mpv-format) node* (mpv-format flag))
      (ftype-set! mpv-node (u flag) node* val)))

  (define bool->node
    (lambda (val)
      (let ([p (alloc-node)])
        (bool-node-set! p val)
        p)))

  (define double-node-set!
    (lambda (node* val)
      (ftype-set! mpv-node (mpv-format) node* (mpv-format double))
      (ftype-set! mpv-node (u double) node* val)))

  (define double->node
    (lambda (val)
      (let ([p (alloc-node)])
        (double-node-set! p val)
        p)))

  (define int-node-set!
    (lambda (node* val)
      (ftype-set! mpv-node (mpv-format) node* (mpv-format int64))
      (ftype-set! mpv-node (u int64) node* val)))

  (define int->node
    (lambda (val)
      (let ([p (alloc-node)])
        (int-node-set! p val)
        p)))

  (define string-node-set!
    (lambda (node* val)
      (ftype-set! mpv-node (mpv-format) node* (mpv-format string))
      (ftype-set! mpv-node (u string) node* (string->u8* val))))

  (define string->node
    (lambda (val)
      (let ([p (alloc-node)])
        (string-node-set! p val)
        p)))

  (define list-node-set!
    (lambda (node* val)
      (ftype-set! mpv-node (mpv-format) node* (mpv-format node-array))
      (ftype-set! mpv-node (u node-list) node* (alloc-node-list val))))

  (define list->node
    (lambda (value-list)
      (let ([p (alloc-node)])
         (list-node-set! p value-list)
         p)))

  (define alist-node-set!
    (lambda (node* alist)
      (ftype-set! mpv-node (mpv-format) node* (mpv-format node-map))
      (ftype-set! mpv-node (u node-list) node*
        (alloc-node-list (map car alist) (map cdr alist)))))

  (define alist->node
    (lambda (alist)
      (let ([p (alloc-node (mpv-format node-map))])
        (alist-node-set! p alist)
        p)))

  (define map-node-set!
    (lambda (node* val)
      (ftype-set! mpv-node (mpv-format) node* (mpv-format node-map))
      (ftype-set! mpv-node (u node-list) node* val)))

  ;; Allocate mpv-node memory, either for one node (the default) or multiple if count is given.
  (define alloc-node
    (case-lambda
      [()
       (make-ftype-pointer mpv-node (foreign-alloc (ftype-sizeof mpv-node)))]
      [(count)
       (make-ftype-pointer mpv-node (foreign-alloc (fx* (ftype-sizeof mpv-node) count)))]))

  (define alloc-node-list
    (case-lambda
      [(value-list)
       (alloc-node-list #f value-list)]
      [(key-list value-list)
       (let* ([p (make-ftype-pointer mpv-node-list (foreign-alloc (ftype-sizeof mpv-node-list)))]
              [vlen (length value-list)]
              ;; vns = shorthand for values nodes.
              [vns (alloc-node vlen)])
         (for-each
           (lambda (i v)
             (node-set!
               (ftype-&ref mpv-node () vns i)
               v))
           (iota vlen) value-list)
         (ftype-set! mpv-node-list (num) p vlen)
         (ftype-set! mpv-node-list (values) p vns)
         (ftype-set! mpv-node-list (keys) p
           (cond
             [key-list
               (let* ([klen (length key-list)]
                      [key-ptrs (make-ftype-pointer u8* (foreign-alloc (fx* (ftype-sizeof u8*) klen)))])
                 (for-each
                   (lambda (i str)
                     (ftype-set! u8* () key-ptrs i (string->u8* str)))
                   (iota klen) key-list)
                 key-ptrs)]
             [else
               (make-ftype-pointer u8* 0)]))
         p)]))

  (define free-node-list
    (lambda (node-list)
      (define nums (iota (ftype-ref mpv-node-list (num) node-list)))
      (unless (null? nums)
        (let ([vns (ftype-ref mpv-node-list (values) node-list)]
              [key-ptrs (ftype-ref mpv-node-list (keys) node-list)])
           (for-each
             (lambda (i)
               (free-node-contents (ftype-&ref mpv-node () vns i)))
             nums)
           (ftype-free vns)
           (unless (ftype-pointer-null? key-ptrs)
             (for-each
               (lambda (i)
                 (ftype-free (ftype-ref u8* () key-ptrs i)))
               nums)
             (ftype-free key-ptrs)
             )))
      (ftype-free node-list)))

  (define free-node-contents
    (lambda (node)
      (let ([f (ftype-ref mpv-node (mpv-format) node)])
        (cond
          [(fx=? f (mpv-format string))
           (ftype-free (ftype-ref mpv-node (u string) node))]
          [(fx=? f (mpv-format node-array))
           (free-node-list (ftype-ref mpv-node (u node-list) node))]
          [(fx=? f (mpv-format node-map))
           (free-node-list (ftype-ref mpv-node (u node-list) node))]))))

  (define free-node
    (lambda (node)
      (free-node-contents node)
      (ftype-free node)))

  (define set-and-free
    (lambda (setter property value-node)
      (dynamic-wind
        (lambda () #f)
        (lambda ()
          (setter property (mpv-format node) value-node))
        (lambda ()
          (free-node value-node)))))

  (define mpv-set-option/double
    (lambda (option value)
      (set-and-free mpv_set_option option (double->node value))))

  (define mpv-set-option/flag
    (lambda (option value)
      (set-and-free mpv_set_option option (bool->node value))))

  (define mpv-set-option/int
    (lambda (option value)
      (set-and-free mpv_set_option option (int->node value))))

  (define mpv-set-option/string
    (lambda (option value)
      (set-and-free mpv_set_option option (string->node value))))

  (define mpv-set-option/array
    (lambda (option value)
      (set-and-free mpv_set_option option (list->node value))))

  (define mpv-set-option/alist
    (lambda (option value)
      (set-and-free mpv_set_option option (alist->node value))))

  ;; mpv-set-option sets the option value based on type of the given value.
  (define mpv-set-option
    (lambda (option value)
      (define (setter)
        ;; Checking order matters: flonum before integer, and alist before list.
        (cond
          [(string? value)	mpv-set-option/string]
          [(boolean? value)	mpv-set-option/flag]
          [(flonum? value)	mpv-set-option/double]
          [(integer? value)	mpv-set-option/int]
          [(alist? value)	mpv-set-option/alist]
          [(list? value)		mpv-set-option/array]
          [else
            (error 'mpv-set-option "Unknown option type" option value)]))
      (let ([rc ((setter) option value)])
        (cond
          [(= rc (mpv-error success))
           rc]
          [else
            (error 'mpv-set-option (mpv-error-string rc) rc option value)]))))

  (define mpv-set-property/double
    (lambda (property value)
      (set-and-free mpv_set_property property (double->node value))))

  (define mpv-set-property/flag
    (lambda (property value)
      (set-and-free mpv_set_property property (bool->node value))))

  (define mpv-set-property/int
    (lambda (property value)
      (set-and-free mpv_set_property property (int->node value))))

  (define mpv-set-property/string
    (lambda (property value)
      (set-and-free mpv_set_property property (string->node value))))

  (define mpv-set-property/array
    (lambda (property value)
      (set-and-free mpv_set_property property (list->node value))))

  (define mpv-set-property/alist
    (lambda (property value)
      (set-and-free mpv_set_property property (alist->node value))))

  (define mpv-set-property
    (lambda (property value)
      (define (setter)
        ;; Checking order matters: flonum before integer, and alist before list.
        (cond
          [(string? value)	mpv-set-property/string]
          [(boolean? value)	mpv-set-property/flag]
          [(flonum? value)	mpv-set-property/double]
          [(integer? value)	mpv-set-property/int]
          [(alist? value)	mpv-set-property/alist]
          [(list? value)	mpv-set-property/array]
          [else
            (error 'mpv-set-property "Unknown property type" property value)]))
      ((setter) property value)))

  ;; Write value into the given mpv-node ftype pointer.
  (define node-set!
    (lambda (node* value)
      (define (setter)
        ;; Checking order matters: flonum before integer, and alist before list.
        (cond
          [(string? value)	string-node-set!]
          [(boolean? value)	bool-node-set!]
          [(flonum? value)	double-node-set!]
          [(integer? value)	int-node-set!]
          [(alist? value)	alist-node-set!]
          [(list? value)	list-node-set!]
          [else
            (error 'mpv-node-set! "Unknown node value type" node* value)]))
      ((setter) node* value)))

  ;;;; End: mpv-node setters.

  (define-record-type mpv-event
    (fields
      id
      error
      reply-userdata
      data))

  (define-record-type mpv-client-message-event
    (parent mpv-event)
    (fields
      args))

  (define-record-type mpv-property-event
    (parent mpv-event)
    (fields
      name
      value))

  (define property-event->value
    (lambda (prop)
      (let ([data-ptr (ftype-ref mpv-event-property (data) prop)])
        (switch (ftype-ref mpv-event-property (format) prop)
          [(mpv-format double)
           (foreign-ref 'double data-ptr 0)]
          [(mpv-format int64)
           (foreign-ref 'integer-64 data-ptr 0)]
          [(mpv-format flag)
           (int->bool (foreign-ref 'int data-ptr 0))]
          [(mpv-format string)
           (u8*->string data-ptr)]
          [(mpv-format node)
           (node->scheme (make-ftype-pointer mpv-node data-ptr))]
          [else
            #f]))))

  (define mpv-wait-event
    (lambda (timeout)
      ;; c/mpv-event-data will point to the specific event object depending on c/mpv-event-eventid.
      (let* ([ev (mpv_wait_event timeout)]
             [eid (ftype-ref c/mpv-event (event-id) ev)]
             [err (ftype-ref c/mpv-event (error) ev)]
             [rud (ftype-ref c/mpv-event (reply-userdata) ev)]
             [dat (ftype-ref c/mpv-event (data) ev)])
        (cond
          [(or
             (= eid (mpv-event-type property-change))
             (= eid (mpv-event-type get-property-reply)))
           (let ([prop-ptr (make-ftype-pointer mpv-event-property dat)])
             (make-mpv-property-event
               eid err rud dat
               (u8*->string (ftype-pointer-address (ftype-ref mpv-event-property (name) prop-ptr)))
               (property-event->value prop-ptr)))]
          [(= eid (mpv-event-type end-file))
           (let ([ptr (make-ftype-pointer mpv-event-end-file dat)])
             (make-mpv-event eid err rud (cons
                                           (ftype-ref mpv-event-end-file (reason) ptr)
                                           (ftype-ref mpv-event-end-file (error) ptr))))]
          [(= eid (mpv-event-type client-message))
           (let ([ptr (make-ftype-pointer mpv-event-client-message dat)])
             (make-mpv-client-message-event eid err rud dat
               (u8**->string-list (ftype-pointer-address (ftype-ref mpv-event-client-message (args) ptr))
                                  (ftype-ref mpv-event-client-message (num-args) ptr))))]
          [else
            ;; TODO define records for the remaining event types.
            (make-mpv-event eid err rud dat)]))))
  )

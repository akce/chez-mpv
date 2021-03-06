;; libmpv bindings for Chez scheme.
;; Copyright (c) 2019-2020 Akce. License: GPLv3, see COPYING for details.
(library (mpv)
  (export
   current-mpv-handle

   mpv-event? mpv-event-id mpv-event-error mpv-event-reply-userdata mpv-event-data
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
   (only (chezscheme) make-parameter make-ftype-pointer))

  (define load-lib (load-shared-object "libmpv.so.1"))

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
    (idle			11)
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
        [flag		int]
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
    (mpv_set_option		(string int void*)		int)
    (mpv_set_option_string	(string string)			int)
    (mpv_command		(void*)				int)
    (mpv-command-node		((* mpv-node) (* mpv-node))	int)
    (mpv-command-string		(string)			int)
    (mpv-command-async		(unsigned-64 (* u8*))		int)
    (mpv-command-node-async	(unsigned-64 (* mpv-node))	int)
    (mpv_set_property		(string int void*)		int)
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
        (int->bool (ftype-ref mpv-node (u flag) node))]
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
      (alloc ([flag int])
        (let ([rc (mpv-get-property property (mpv-format flag) flag)])
          (if (< rc 0)
              (error #f (mpv-error-string rc) property)
              (int->bool (foreign-ref 'int flag 0)))))))

  (define mpv-get-property/long
    (lambda (property)
      (alloc ([num integer-64])
        (let ([rc (mpv-get-property property (mpv-format int64) num)])
          (if (< rc 0)
              (error #f (mpv-error-string rc) property)
              (foreign-ref 'integer-64 num 0))))))

  (define mpv-get-property/string
    (lambda (property)
      (alloc ([str u8*])
        (let ([rc (mpv-get-property property (mpv-format string) str)])
          (if (< rc 0)
              (error #f (mpv-error-string rc) property)
              (let* ([ptr (foreign-ref 'void* str 0)]
                     [ret (u8*->string ptr)])
                (mpv-free ptr)
                ret))))))

  (define mpv-get-property/node
    (lambda (property)
      (alloc ([data &data mpv-node])
        (let ([rc (mpv-get-property property (mpv-format node) data)])
          (if (< rc 0)
              (error #f (mpv-error-string rc) property)
              (let* ([ret (node->scheme &data)])
                (mpv-free-node-contents &data)
                ret))))))

  (define mpv-set-option/double
    (lambda (option value)
      (alloc ([i &i double])
        (ftype-set! double () &i value)
        (mpv_set_option option (mpv-format double) i))))

  (define mpv-set-option/flag
    (lambda (property value)
      (alloc ([i &i int])
        (ftype-set! int () &i (if value 1 0))
        (mpv_set_option property (mpv-format flag) i))))

  (define mpv-set-option/int
    (lambda (option value)
      (alloc ([i &i integer-64])
        (ftype-set! integer-64 () &i value)
        (mpv_set_option option (mpv-format int64) i))))

  (define mpv-set-option/string
    (lambda (property value)
      (mpv_set_option_string property value)))

  (define mpv-set-option
    (lambda (option value)
      (define (setter)
        (cond
         [(string? value)	mpv-set-option/string]
         [(integer? value)	mpv-set-option/int]
         [(boolean? value)	mpv-set-option/flag]
         [(flonum? value)	mpv-set-option/double]))
      (let ([rc ((setter) option value)])
        (cond
          [(= rc (mpv-error success))
           rc]
          [else
            (error #f (mpv-error-string rc) rc option value)]))))

  (define mpv-set-property/double
    (lambda (property value)
      (alloc ([i &i double])
        (ftype-set! double () &i value)
        (mpv_set_property property (mpv-format double) i))))

  (define mpv-set-property/flag
    (lambda (property value)
      (alloc ([i &i int])
        (ftype-set! int () &i (if value 1 0))
        (mpv_set_property property (mpv-format flag) i))))

  (define mpv-set-property/int
    (lambda (property value)
      (alloc ([i &i integer-64])
        (ftype-set! integer-64 () &i value)
        (mpv_set_property property (mpv-format int64) i))))

  (define mpv-set-property/string
    (lambda (property value)
      (mpv-set-property-string property value)))

  (define mpv-set-property
    (lambda (property value)
      (define (setter)
        (cond
         [(string? value)	mpv-set-property/string]
         [(integer? value)	mpv-set-property/int]
         [(boolean? value)	mpv-set-property/flag]
         [(flonum? value)	mpv-set-property/double]))
      ((setter) property value)))

  (define-record-type mpv-event
    (fields
      id
      error
      reply-userdata
      data))

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
          [else
            ;; TODO define records for the remaining event types.
            (make-mpv-event eid err rud dat)]))))
  )

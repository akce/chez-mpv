;; libmpv bindings for Chez scheme.
;; Copyright (c) 2019 Akce. License: GPLv3, see COPYING for details.
(library (mpv)
  (export
   current-mpv-handle

   get-mpv-event-id
   mpv-get-property/flag
   mpv-get-property/long
   mpv-get-property/string
   mpv-get-property/node

   mpv-set-option/double
   mpv-set-option/flag
   mpv-set-option/int
   mpv-set-option/string
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

   MPV_ERROR_SUCCESS MPV_ERROR_EVENT_QUEUE_FULL MPV_ERROR_NOMEM MPV_ERROR_UNINITIALIZED MPV_ERROR_INVALID_PARAMETER MPV_ERROR_OPTION_NOT_FOUND MPV_ERROR_OPTION_FORMAT MPV_ERROR_OPTION_ERROR MPV_ERROR_PROPERTY_NOT_FOUND MPV_ERROR_PROPERTY_FORMAT MPV_ERROR_PROPERTY_UNAVAILABLE MPV_ERROR_PROPERTY_ERROR MPV_ERROR_COMMAND MPV_ERROR_LOADING_FAILED MPV_ERROR_AO_INIT_FAILED MPV_ERROR_VO_INIT_FAILED MPV_ERROR_NOTHING_TO_PLAY MPV_ERROR_UNKNOWN_FORMAT MPV_ERROR_UNSUPPORTED MPV_ERROR_NOT_IMPLEMENTED MPV_ERROR_GENERIC

   MPV_EVENT_NONE MPV_EVENT_SHUTDOWN MPV_EVENT_LOG_MESSAGE MPV_EVENT_GET_PROPERTY_REPLY MPV_EVENT_SET_PROPERTY_REPLY MPV_EVENT_COMMAND_REPLY MPV_EVENT_START_FILE MPV_EVENT_END_FILE MPV_EVENT_FILE_LOADED MPV_EVENT_IDLE MPV_EVENT_TICK MPV_EVENT_CLIENT_MESSAGE MPV_EVENT_VIDEO_RECONFIG MPV_EVENT_AUDIO_RECONFIG MPV_EVENT_SEEK MPV_EVENT_PLAYBACK_RESTART MPV_EVENT_PROPERTY_CHANGE MPV_EVENT_QUEUE_OVERFLOW MPV_EVENT_HOOK
   ;; deprecated
   MPV_EVENT_METADATA_UPDATE

   MPV_END_FILE_REASON_EOF MPV_END_FILE_REASON_STOP MPV_END_FILE_REASON_QUIT MPV_END_FILE_REASON_ERROR MPV_END_FILE_REASON_REDIRECT

   MPV_FORMAT_NONE MPV_FORMAT_STRING MPV_FORMAT_OSD_STRING MPV_FORMAT_FLAG MPV_FORMAT_INT64 MPV_FORMAT_DOUBLE MPV_FORMAT_NODE MPV_FORMAT_NODE_ARRAY MPV_FORMAT_NODE_MAP MPV_FORMAT_BYTE_ARRAY

   MPV_LOG_LEVEL_NONE MPV_LOG_LEVEL_FATAL MPV_LOG_LEVEL_ERROR MPV_LOG_LEVEL_WARN MPV_LOG_LEVEL_INFO MPV_LOG_LEVEL_V MPV_LOG_LEVEL_DEBUG MPV_LOG_LEVEL_TRACE
   )
  (import
   (rnrs)
   (mpv ftypes-util)
   (only (chezscheme) make-parameter))

  (define load-lib (load-shared-object "libmpv.so"))

  (define-ftype mpv-handle void*)

  (enum mpv-error
    (MPV_ERROR_SUCCESS			0)
    (MPV_ERROR_EVENT_QUEUE_FULL		-1)
    (MPV_ERROR_NOMEM			-2)
    (MPV_ERROR_UNINITIALIZED		-3)
    (MPV_ERROR_INVALID_PARAMETER	-4)
    (MPV_ERROR_OPTION_NOT_FOUND		-5)
    (MPV_ERROR_OPTION_FORMAT		-6)
    (MPV_ERROR_OPTION_ERROR		-7)
    (MPV_ERROR_PROPERTY_NOT_FOUND	-8)
    (MPV_ERROR_PROPERTY_FORMAT		-9)
    (MPV_ERROR_PROPERTY_UNAVAILABLE	-10)
    (MPV_ERROR_PROPERTY_ERROR		-11)
    (MPV_ERROR_COMMAND			-12)
    (MPV_ERROR_LOADING_FAILED		-13)
    (MPV_ERROR_AO_INIT_FAILED		-14)
    (MPV_ERROR_VO_INIT_FAILED		-15)
    (MPV_ERROR_NOTHING_TO_PLAY		-16)
    (MPV_ERROR_UNKNOWN_FORMAT		-17)
    (MPV_ERROR_UNSUPPORTED		-18)
    (MPV_ERROR_NOT_IMPLEMENTED		-19)
    (MPV_ERROR_GENERIC			-20))

  (enum mpv-event-id
    (MPV_EVENT_NONE			0)
    (MPV_EVENT_SHUTDOWN			1)
    (MPV_EVENT_LOG_MESSAGE		2)
    (MPV_EVENT_GET_PROPERTY_REPLY	3)
    (MPV_EVENT_SET_PROPERTY_REPLY	4)
    (MPV_EVENT_COMMAND_REPLY		5)
    (MPV_EVENT_START_FILE		6)
    (MPV_EVENT_END_FILE			7)
    (MPV_EVENT_FILE_LOADED		8)
    (MPV_EVENT_IDLE			11)
    (MPV_EVENT_TICK			14)
    (MPV_EVENT_CLIENT_MESSAGE		16)
    (MPV_EVENT_VIDEO_RECONFIG		17)
    (MPV_EVENT_AUDIO_RECONFIG		18)
    (MPV_EVENT_SEEK			20)
    (MPV_EVENT_PLAYBACK_RESTART		21)
    (MPV_EVENT_PROPERTY_CHANGE		22)
    (MPV_EVENT_QUEUE_OVERFLOW		24)
    (MPV_EVENT_HOOK			25)
    ;; deprecated
    (MPV_EVENT_METADATA_UPDATE		19))

  (enum mpv-file-end-reason
    (MPV_END_FILE_REASON_EOF		0)
    (MPV_END_FILE_REASON_STOP		2)
    (MPV_END_FILE_REASON_QUIT		3)
    (MPV_END_FILE_REASON_ERROR		4)
    (MPV_END_FILE_REASON_REDIRECT	5))

  (enum mpv-format
    (MPV_FORMAT_NONE		0)
    (MPV_FORMAT_STRING		1)
    (MPV_FORMAT_OSD_STRING	2)
    (MPV_FORMAT_FLAG		3)
    (MPV_FORMAT_INT64		4)
    (MPV_FORMAT_DOUBLE		5)
    (MPV_FORMAT_NODE		6)
    (MPV_FORMAT_NODE_ARRAY	7)
    (MPV_FORMAT_NODE_MAP	8)
    (MPV_FORMAT_BYTE_ARRAY	9))

  (enum mpv-log-level
    (MPV_LOG_LEVEL_NONE		0)
    (MPV_LOG_LEVEL_FATAL	10)
    (MPV_LOG_LEVEL_ERROR	20)
    (MPV_LOG_LEVEL_WARN		30)
    (MPV_LOG_LEVEL_INFO		40)
    (MPV_LOG_LEVEL_V		50)
    (MPV_LOG_LEVEL_DEBUG	60)
    (MPV_LOG_LEVEL_TRACE	70))

  (define-ftype mpv-byte-array
    (struct
     [data	void*]
     [size	size_t]))
  (define-ftype mpv-byte-array* (* mpv-byte-array))

  (define-ftype mpv-event
    (struct
     [event-id		int]
     [error		int]
     [reply-userdata	unsigned-64]
     [data		void*]))
  (define-ftype mpv-event* (* mpv-event))

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
    (mpv-wait-event		(double)			(* mpv-event))
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

  (define get-mpv-event-id
    (lambda (ev)
      (ftype-ref mpv-event (event-id) ev)))

  (define int->bool
    (lambda (num)
      (if (fx=? num 0)
          #f
          #t)))

  (define-syntax switch
    (syntax-rules ()
      [(_ var (val1 body1 ...) (valn bodyn ...) ...)
       (let ([v var])
         (cond
          [(fx=? v val1) body1 ...]
          [(fx=? v valn) bodyn ...] ...))]))

  (define node->scheme
    (lambda (node)
      (switch (ftype-ref mpv-node (mpv-format) node)
       [MPV_FORMAT_STRING
        (u8*->string (ftype-pointer-address (ftype-ref mpv-node (u string) node)))]
       [MPV_FORMAT_FLAG
        (int->bool (ftype-ref mpv-node (u flag) node))]
       [MPV_FORMAT_INT64
        (ftype-ref mpv-node (u int64) node)]
       [MPV_FORMAT_DOUBLE
        (ftype-ref mpv-node (u double) node)]
       [MPV_FORMAT_NODE_MAP
        (node-map->alist (ftype-ref mpv-node (u node-list) node))]
       [MPV_FORMAT_NODE_ARRAY
        (node->scheme (ftype-ref mpv-node (u node-list) node))]
       [MPV_FORMAT_NODE
        (node->scheme (ftype-ref mpv-node (u node-list) node))])))

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
        (let ([rc (mpv-get-property property MPV_FORMAT_FLAG flag)])
          (if (< rc 0)
            ;; error - TODO raise an exception.
            "error"
            (int->bool (foreign-ref 'int flag 0)))))))

  (define mpv-get-property/long
    (lambda (property)
      (alloc ([num integer-64])
        (let ([rc (mpv-get-property property MPV_FORMAT_INT64 num)])
          (if (< rc 0)
            ;; error - TODO raise an exception.
            "error"
            (foreign-ref 'integer-64 num 0))))))

  (define mpv-get-property/string
    (lambda (property)
      (alloc ([str u8*])
        (let ([rc (mpv-get-property property MPV_FORMAT_STRING str)])
          (if (< rc 0)
            ;; error - TODO raise an exception.
            "error"
            (let* ([ptr (foreign-ref 'void* str 0)]
                   [ret (u8*->string ptr)])
              (mpv-free ptr)
              ret))))))

  (define mpv-get-property/node
    (lambda (property)
      (alloc ([data &data mpv-node])
        (let ([rc (mpv-get-property property MPV_FORMAT_NODE data)])
          (if (< rc 0)
            ;; error - TODO raise an exception.
            (mpv-error-string rc)
            (let* ([ret (node->scheme &data)])
              #;(mpv-free ptr)
              ret))))))

  (define mpv-set-option/double
    (lambda (option value)
      (alloc ([i &i double])
        (ftype-set! double () &i value)
        (mpv_set_option option MPV_FORMAT_DOUBLE i))))

  (define mpv-set-option/flag
    (lambda (property value)
      (alloc ([i &i int])
        (ftype-set! int () &i (if value 1 0))
        (mpv_set_option property MPV_FORMAT_FLAG i))))

  (define mpv-set-option/int
    (lambda (option value)
      (alloc ([i &i integer-64])
        (ftype-set! integer-64 () &i value)
        (mpv_set_option option MPV_FORMAT_INT64 i))))

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
      ((setter) option value)))

  (define mpv-set-property/double
    (lambda (property value)
      (alloc ([i &i double])
        (ftype-set! double () &i value)
        (mpv_set_property property MPV_FORMAT_DOUBLE i))))

  (define mpv-set-property/flag
    (lambda (property value)
      (alloc ([i &i int])
        (ftype-set! int () &i (if value 1 0))
        (mpv_set_property property MPV_FORMAT_FLAG i))))

  (define mpv-set-property/int
    (lambda (property value)
      (alloc ([i &i integer-64])
        (ftype-set! integer-64 () &i value)
        (mpv_set_property property MPV_FORMAT_INT64 i))))

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
  )

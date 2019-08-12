(library (mpv)
  (export
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
   mpv-set-option
   mpv-set-option-string
   mpv-command
   mpv-command-node
   mpv-command-string
   mpv-command-async
   mpv-command-node-async
   mpv-set-property
   mpv-set-property-string
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

   MPV_ERROR_SUCCESS MPV_ERROR_EVENT_QUEUE_FULL MPV_ERROR_NOMEM MPV_ERROR_UNINITIALIZED MPV_ERROR_INVALID_PARAMETER MPV_ERROR_OPTION_NOT_FOUND MPV_ERROR_OPTION_FORMAT MPV_ERROR_OPTION_ERROR MPV_ERROR_PROPERTY_NOT_FOUND MPV_ERROR_PROPERTY_FORMAT MPV_ERROR_PROPERTY_UNAVAILABLE MPV_ERROR_PROPERTY_ERROR MPV_ERROR_COMMAND MPV_ERROR_LOADING_FAILED MPV_ERROR_AO_INIT_FAILED MPV_ERROR_VO_INIT_FAILED MPV_ERROR_NOTHING_TO_PLAY MPV_ERROR_UNKNOWN_FORMAT MPV_ERROR_UNSUPPORTED MPV_ERROR_NOT_IMPLEMENTED MPV_ERROR_GENERIC

   MPV_EVENT_NONE MPV_EVENT_SHUTDOWN MPV_EVENT_LOG_MESSAGE MPV_EVENT_GET_PROPERTY_REPLY MPV_EVENT_SET_PROPERTY_REPLY MPV_EVENT_COMMAND_REPLY MPV_EVENT_START_FILE MPV_EVENT_END_FILE MPV_EVENT_FILE_LOADED MPV_EVENT_IDLE MPV_EVENT_TICK MPV_EVENT_CLIENT_MESSAGE MPV_EVENT_VIDEO_RECONFIG MPV_EVENT_AUDIO_RECONFIG MPV_EVENT_SEEK MPV_EVENT_PLAYBACK_RESTART MPV_EVENT_PROPERTY_CHANGE MPV_EVENT_QUEUE_OVERFLOW MPV_EVENT_HOOK

   MPV_END_FILE_REASON_EOF MPV_END_FILE_REASON_STOP MPV_END_FILE_REASON_QUIT MPV_END_FILE_REASON_ERROR MPV_END_FILE_REASON_REDIRECT

   MPV_FORMAT_NONE MPV_FORMAT_STRING MPV_FORMAT_OSD_STRING MPV_FORMAT_FLAG MPV_FORMAT_INT64 MPV_FORMAT_DOUBLE MPV_FORMAT_NODE MPV_FORMAT_NODE_ARRAY MPV_FORMAT_NODE_MAP MPV_FORMAT_BYTE_ARRAY

   MPV_LOG_LEVEL_NONE MPV_LOG_LEVEL_FATAL MPV_LOG_LEVEL_ERROR MPV_LOG_LEVEL_WARN MPV_LOG_LEVEL_INFO MPV_LOG_LEVEL_V MPV_LOG_LEVEL_DEBUG MPV_LOG_LEVEL_TRACE
   )
  (import
   (rnrs)
   (mpv ftypes-util))

  (define load-lib (load-shared-object "libmpv.so"))

  (define-ftype u8 unsigned-8)
  (define-ftype u8* (* u8))
  (define-ftype u8** (* u8*))
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
    (MPV_EVENT_HOOK			25))

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

  (define-ftype mpv-node
    (struct
     [u	(union
         [string	u8*]
         [flag		int]
         [int64		integer-64]
         [double	double]
         [node-list	(* mpv-node)]
         [ba		mpv-byte-array*])]
     [mpv-format	int]))
  (define-ftype mpv-node* (* mpv-node))

  (define-ftype mpv-node-list
    (struct
     [num	int]
     [values	mpv-node*]
     [keys	u8**]))
  (define-ftype mpv-node-list* (* mpv-node-list))

  (define-ftype mpv-event-property
    (struct
     [name	u8*]
     [format	int]
     [data	void*]))
  (define-ftype mpv-event-property* (* mpv-event-property))

  (define-ftype wakeup-cb-t
    (function (void*) void))

  (c_funcs
   (mpv-client-api-version	()					unsigned-long)
   (mpv-error-string		(int)					string)
   (mpv-free			(void*)					void)
   (mpv-client-name		(mpv-handle)				string)
   (mpv-create			()					mpv-handle)
   (mpv-initialize		(mpv-handle)				int)
   (mpv-destroy			(mpv-handle)				void)
   (mpv-terminate-destroy	(mpv-handle)				void)
   (mpv-create-client		(mpv-handle string)			mpv-handle)
   (mpv-create-weak-client	(mpv-handle string)			mpv-handle)
   (mpv-load-config-file	(mpv-handle string)			mpv-handle)
   (mpv-get-time-us		(mpv-handle)				integer-64)
   (mpv-free-node-contents	((* mpv-node))				void)
   (mpv-set-option		(mpv-handle string int void*)		int)
   (mpv-set-option-string	(mpv-handle string string)		int)
   (mpv-command			(mpv-handle (* u8*))			int)
   (mpv-command-node		(mpv-handle (* mpv-node) (* mpv-node))	int)
   (mpv-command-string		(mpv-handle string)			int)
   (mpv-command-async		(mpv-handle unsigned-64 (* u8*))	int)
   (mpv-command-node-async	(mpv-handle unsigned-64 (* mpv-node))	int)
   (mpv-set-property		(mpv-handle string int void*)		int)
   (mpv-set-property-string	(mpv-handle string string)		int)
   (mpv-set-property-async	(mpv-handle unsigned-64 string int void*)	int)
   (mpv-get-property		(mpv-handle string int void*)		int)
   (mpv-get-property-string	(mpv-handle string)			(* u8))
   (mpv-get-property-osd-string	(mpv-handle string)			(* u8))
   (mpv-get-property-async	(mpv-handle unsigned-64 string int)	int)
   (mpv-observe-property	(mpv-handle unsigned-64 string int)	int)
   (mpv-unobserve-property	(mpv-handle unsigned-64)		int)
   (mpv-event-name		(int)					string)
   (mpv-request-event		(mpv-handle int int)			int)
   (mpv-request-log-messages	(mpv-handle string)			int)
   (mpv-wait-event		(mpv-handle double)			(* mpv-event))
   (mpv-wakeup			(mpv-handle)				void)
   (mpv-set-wakeup-callback	(mpv-handle (* wakeup-cb-t) void*)	void)
   (mpv-wait-async-requests	(mpv-handle)				void)
   (mpv-hook-add		(mpv-handle unsigned-64 string int)	int)
   (mpv-hook-continue		(mpv-handle unsigned-64)		int)))

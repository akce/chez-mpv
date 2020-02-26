;; Integration between chez-mpv and chez-libev.
;; Copyright (c) 2019-2020 Akce. License: GPLv3, see COPYING for details.
(library (mpv ev)
  (export register-mpv-event-handler)
  (import
   (rnrs)
   (ev)
   (mpv)
   (only (chezscheme) open-fd-input-port input-port-ready? clear-input-port))

  (define register-mpv-event-handler
    (lambda (io-callback)
      (let* ([fd (mpv-get-wakeup-pipe)]
             [port (open-fd-input-port fd)])
        (ev-io fd (evmask 'READ)
          (lambda (w revent)
            (when (input-port-ready? port)
              (clear-input-port port)
              (let loop ([ev (mpv-wait-event 0.0)])
                (let ([eid (get-mpv-event-id ev)])
                  (cond
                   [(equal? eid MPV_EVENT_NONE)
                    #t]
                   [else
                    (io-callback eid)
                    (loop (mpv-wait-event 0.0))]))))))))))

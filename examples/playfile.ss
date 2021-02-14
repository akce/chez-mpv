#! /usr/bin/scheme --program

;; A simple media player.
;; Copyright (c) 2019-2020 Akce. License: GPLv3, see COPYING for details.

;; Demonstrates how an mpv based player could be written using chez-libev for event handling.

(import
 (rnrs)
 (mpv)
 (mpv ev)
 (ev)
 (only (chezscheme) format))

(define stdin-fd 0)

;; user-defined property event type ids.
(define time-pos-id 1)
(define pause-id 9)
(define tags-id 22)

(define init
  (lambda ()
    (mpv-create)
    (mpv-set-property "audio-display" #f)
    ;; Default image display time is only 1 second. Change to 4 seconds.
    (mpv-set-option "image-display-duration" 4)
    ;; Enable video window key & mouse controls.
    (mpv-set-option "input-default-bindings" "yes")
    (mpv-set-option "input-vo-keyboard" "yes")
    (mpv-set-option "osc" #t)
    (mpv-initialize)
    (mpv-observe-property pause-id "pause" (mpv-format flag))
    (mpv-observe-property tags-id "metadata" (mpv-format node))
    (ev-io stdin-fd (evmask 'READ) stdin-handler)
    (register-mpv-event-handler mpv-handler)))

(define quit
  (lambda ()
    (display "goodbye")(newline)
    (ev-break (evbreak 'ALL))))

(define mpv-handler
  (lambda (event)
    (let ([eid (mpv-event-id event)])
      (cond
        [(mpv-property-event? event)
         (let ([pid (mpv-event-reply-userdata event)])
           (cond
             [(= pid time-pos-id)
              ;; Need to guard against getting false time-pos when user presses q (shutdown).
              (when (mpv-property-event-value event)
                (display "time position ")
                (display (seconds->string (mpv-property-event-value event)))
                (display "\r")	; there's lot's of these, so print on the same line.
                )]
             [(= pid tags-id)
              (display "media tags ")
              (display (mpv-property-event-value event))
              (newline)]
             [(= pid pause-id)
              (display "pause state ")
              (display (mpv-property-event-value event))
              (newline)]
             [else
               (display "misc property-change: id ")(display pid)
               (display " ")(display (mpv-property-event-name event))
               (display " -> ")
               (display (mpv-property-event-value event))(newline)]))]
        [(= eid (mpv-event-type file-loaded))
         ;; Watch the time-pos property, that way we can report playback position.
         ;; Ignore error exception at this point.
         (mpv-observe-property time-pos-id "time-pos" (mpv-format int64))]
        [(= eid (mpv-event-type end-file))
         (mpv-unobserve-property tags-id)
         (mpv-unobserve-property time-pos-id)]
        [(or
           (= eid (mpv-event-deprecated idle))
           (= eid (mpv-event-type shutdown)))
         (quit)]
        [(or
           (= eid (mpv-event-deprecated metadata-update))
           (= eid (mpv-event-deprecated pause))
           (= eid (mpv-event-deprecated unpause)))
         (if #f #f)]
        [else
          (display "unhandled event ")
          (display (mpv-event-name eid))(newline)]))))

(define stdin-handler
  (lambda (w rev)
    (let ([in (get-line (current-input-port))])
      (cond
       [(eof-object? in)
        (ev-io-stop w)
        (quit)]
       [else
        (case (string-ref in 0)
          [(#\i)
           (display "paused: ")(display (mpv-get-property/flag "pause"))(newline)
           (display "time-pos: ")(display (mpv-get-property/long "time-pos"))(newline)
           (display "percent-pos: ")(display (mpv-get-property/long "percent-pos"))(newline)
           (display "duration: ")(display (mpv-get-property/long "duration"))(newline)
           (display "media-title: ")(display (mpv-get-property/string "media-title"))(newline)]
          [(#\t)
           (show-metadata)]
          [(#\p)
           (mpv-toggle-pause)]
          [(#\s)
           (mpv-stop)])]))))

(define show-metadata
  (lambda ()
    (display "tags:")(display (mpv-get-property/node "metadata"))(newline)))

(define mpv-play
  (lambda (file-or-url)
    (mpv-command "loadfile" file-or-url)))

(define mpv-pause
  (lambda ()
    (mpv-set-property/flag "pause" #t)))

(define mpv-toggle-pause
  (lambda ()
    (mpv-command "cycle" "pause")))

(define mpv-unpause
  (lambda ()
    (mpv-set-property/flag "pause" #f)))

(define mpv-seek
  (lambda (seconds)
    (mpv-command "seek" (number->string seconds))))

(define mpv-stop
  (lambda ()
    (mpv-command "stop")))

(define pad-num
  (lambda (num)
    (format "~2,'0d" num)))

(define seconds->string
  (lambda (secs)
    (string-append (pad-num (div secs 3600)) ":" (pad-num (div secs 60)) ":" (pad-num (mod secs 60)))))

(let ([argv (command-line)])
  (cond
   [(equal? 2 (length argv))
    (init)
    (mpv-play (list-ref argv 1))
    (ev-run)]
   [else
    (display "usage ")(display (car argv))(display " <media-file-or-url>")(newline)]))

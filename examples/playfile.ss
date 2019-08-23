#! /usr/bin/scheme --program

;; A simple media player.
;; Copyright (c) 2019 Akce. License: GPLv3, see COPYING for details.

;; Demonstrates how an mpv based player could be written using chez-libev for event handling.

(import
 (rnrs)
 (mpv)
 (mpv ev)
 (ev))

(define stdin-fd 0)

(define init
  (lambda ()
    (mpv-create)
    (mpv-set-property "audio-display" #f)
    (mpv-initialize)
    (ev-io stdin-fd EV_READ stdin-handler)
    (register-mpv-event-handler mpv-handler)))

(define mpv-handler
  (lambda (eid)
    (display (mpv-event-name eid))(newline)
    (cond
     [(equal? eid MPV_EVENT_METADATA_UPDATE)
      (show-metadata)])))

(define stdin-handler
  (lambda (w rev)
    (let ([in (get-line (current-input-port))])
      (cond
       [(eof-object? in)
        (display "goodbye")(newline)
        (ev-io-stop w)
        (ev-break EVBREAK_ALL)]
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

(let ([argv (command-line)])
  (cond
   [(equal? 2 (length argv))
    (init)
    (mpv-play (list-ref argv 1))
    (ev-run)]
   [else
    (display "usage ")(display (car argv))(display " <media-file-or-url>")(newline)]))

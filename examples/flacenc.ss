#! /usr/bin/scheme --program

;; Convert media file to FLAC.
;; Copyright (c) 2019 Akce. License: GPLv3, see COPYING for details.

;; Demonstrates how an mpv based convert-to-flac could be written.

(import
 (rnrs)
 (mpv))

(define show-metadata
  (lambda ()
    (display "tags:")(display (mpv-get-property/node "metadata"))(newline)))

(define show-events
  (lambda ()
    (let loop ([ev (mpv-wait-event -1.0)])
      (let ([eid (get-mpv-event-id ev)])
        (cond
         [(or (fx=? eid (mpv-event-type idle)) (fx=? eid (mpv-event-type none)))
          (display "idle: finished")(newline)
          ;; Ensures encoded output file is properly flushed & closed before exit.
          (mpv-terminate-destroy)]
         [else
          (display (mpv-event-name eid))(newline)
          (loop (mpv-wait-event -1.0))])))))

(define file->flac
  (lambda (src dest)
    (mpv-create)
    ;; these encoding options must be set before mpv-initialize.
    (mpv-set-property "o" dest)
    ;; not needed if dest has .flac suffix
    #;(mpv-set-property "oac" "flac")
    (mpv-initialize)
    (mpv-set-property "audio-display" #f)
    (mpv-set-property "audio-samplerate" 44100)
    (mpv-set-property "audio-format" "s16")
    (mpv-command "loadfile" src)
    (show-events)))

(let ([argv (command-line)])
  (cond
   [(equal? 3 (length argv))
    (file->flac (list-ref argv 1) (list-ref argv 2))]
   [else
    (display "usage ")(display (car argv))(display " <src-media-file> <dest-flac-file>")(newline)]))

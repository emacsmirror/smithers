
(setq dir-pack "." ;; (expand-file-name "smithers-startup" user-emacs-directory)
      dir-ascii (expand-file-name "ascii" dir-pack)
      dir-wavs (expand-file-name "wavs" dir-pack))

(setq fps 20)
(setq espeak-command "espeak -ven-us+m5 -p 80 -s 170 -g 10")


(defun assert-wavs (&optional overwrite)
  "Assert that directory for wavs exists, and if not create and populate it. If OVERWRITE is t, then overwrite."
  (when (or overwrite (not (file-exists-p (expand-file-name "hello.wav" dir-wavs))))
    (message "Generating WAVs")
    (mkdir dir-wavs t)
    (dolist (speech (--filter (plist-get it :espeak) timings))
      (let* ((key (plist-get speech :espeak))
             (fname (expand-file-name (format "%s.wav" key) dir-wavs))
             (phenoms (alist-get key sound-alist)))
        (call-process-shell-command
         (format "%s \"[[%s]]\" -w %s" espeak-command phenoms fname) nil 0)))))

(defun play-wav (speechkey)
  "Play WAV file associated with SPEECHKEY, and error out if not present."
  ;;(play-sound-file (expand-file-name (format "%s.wav" speechkey) dir-wavs)))
  ;; The above is synchronous and slow
  (call-process-shell-command (format "mpv '%s'"
                                      (expand-file-name (format "%s.wav" speechkey)
                                                        dir-wavs))
                              nil 0))
   


(defun get-text-contents (ascii-key &optional lpad)
  "Retrieve the text contents of 'ascii/ASCII-KEY.txt' and add
  LPAD left-padding of spaces to each line (default: nil)."
  (let ((fname (expand-file-name (format "%s.txt" ascii-key) dir-ascii))
        (rstring nil))
    (if (file-exists-p fname)
        (with-temp-buffer
          (insert-file-contents fname)
          (if lpad
              ;; Insert vertical column
              (replace-rectangle (point-min)
                                 (progn (point-max) (line-beginning-position))
                                 (make-string lpad ? )))
          (setq rstring
                (buffer-substring-no-properties
                 (point-min) (point-max)))))))


(setq sound-alist
      '((hello . "he@'ll',u:") (smithers . "smi,D3:ss")
        (youre . "juor") (quite . "kwaIt") (good . "gU@d")
        (at . "ad" ) (turning . "d3::nI2N")
        (me . "mi:::") (on . "A@:n")))

(setq audiodelay 5)

(setq timings
      ;; Espeak duration are designed only to overcome the overhead in
      ;; starting up the media player
  `(;; Startup sequence
    (:ascii start0 :duration 05)
    (:ascii start1 :duration 02)
    (:ascii start2 :duration 02)
    (:ascii start3 :duration 02)
    (:ascii start4 :duration 02)
    (:ascii start5 :duration 02)
    (:ascii start6 :duration 02)
    (:ascii closed :duration ,(- 08 audiodelay))
    ;; Talking Sequence
    (:espeak hello :duration 10)
    (:ascii opened :duration 11 :text (1 01 "Hel"))
    (:ascii closed :duration ,(- 06 audiodelay) :text (1 03 "lo"))
    (:espeak smithers :duration ,audiodelay)
    (:ascii opened :duration 10 :text (1 06 "Smi"))
    (:ascii closed :duration ,(- 16 audiodelay) :text (1 09 "thers"))
    (:espeak youre :duration ,audiodelay)
    (:ascii opened :duration 06 :text (2 01 "You"))
    (:ascii closed :duration ,(- 04 audiodelay) :text (2 04 "'re"))
    (:espeak quite :duration ,audiodelay)
    (:ascii opened :duration 06 :text (2 08 "Qui") )
    (:ascii closed :duration ,(- 06 audiodelay) :text (2 11 "te"))
    (:espeak good :duration ,audiodelay)
    (:ascii opened :duration 10 :text (2 15 "Go") )
    (:ascii closed :duration ,(- 08 audiodelay) :text (2 17 "od"))
    (:espeak at :duration ,audiodelay)
    (:ascii opened :duration 08 :text (3 01 "a") )
    (:ascii closed :duration ,(- 06 audiodelay) :text (3 02 "t"))
    (:espeak turning :duration ,audiodelay)
    (:ascii opened :duration 11 :text (3 05 "turn") )
    (:ascii closed :duration ,(- 06 audiodelay) :text (3 09 "ing"))
    (:espeak me :duration ,audiodelay)
    (:ascii opened :duration 06 :text (3 14 "m") )
    (:ascii closed :duration ,(- 06 audiodelay) :text (3 15 "e"))
    (:espeak on :duration ,audiodelay)
    (:ascii opened :duration 10 :text (3 18 "o") )
    (:ascii closed :duration 24 :text (3 19 "n"))
    ;; Winky wink
    (:ascii winked :duration 16)
    (:ascii closed :duration 7)))
;;  "Timings as derived as 'ticks' from the source video.")


(defun smithers-simple (left-pad fps)
  "Simple init."
  (let ((cbuff (current-buffer))
        (ascii-map (-map (lambda (x) (cons x (get-text-contents x left-pad)))
                         '(start0 start1 start2 start3 start4 start5 start6
                                  closed opened winked))))
    (with-current-buffer (get-buffer-create "*smithers*")
      (switch-to-buffer "*smithers*")
      (buffer-disable-undo)
      (toggle-truncate-lines 1)
      (message "")
      (text-scale-set -2.5)
      (setq-local cursor-type nil)
      (dolist (page timings)
        (let* ((asc-key (plist-get page :ascii))
               (ticks (plist-get page :duration))
               (xytext (plist-get page :text))
               (espeak (plist-get page :espeak))
               (asc-txt (alist-get asc-key ascii-map)))
          (if espeak (play-wav espeak))
          (when asc-txt
            (erase-buffer)
            (insert asc-txt))
          (if ticks
              (if (> ticks 0)
                  (sit-for (/ (float ticks) fps))))))
      (kill-buffer))
    (switch-to-buffer cbuff)))

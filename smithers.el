
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
  (let ((fname (expand-file-name (format "%s.txt" ascii-key) dir-ascii)))
    (if (file-exists-p fname)
        (with-temp-buffer
          (insert-file-contents fname)
          (if lpad
              ;; Insert vertical column
              (replace-rectangle (point-min) (point-max)
                                 (make-string lpad ? )))
          (buffer-substring-no-properties
           (point-min) (point-max))))))


(setq sound-alist
      '((hello "he@'ll',u:")
        (smithers "smi,D3:ss")
        (youre "juor")
        (quite "kwaIt")
        (good "gU@d")
        (at "ad")
        (turning "d3::nI2N")
        (me "mi:::")
        (on "A@:n")))

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
    (:ascii opened :duration 11 :text (15 46 "
┬ ┬┌─┐┬
├─┤├┤ │
┴ ┴└─┘┴─┘" t))
    (:ascii closed :duration ,(- 06 audiodelay) :text (24 46 "
┬  ┌─┐
│  │ │
┴─┘└─┘"))
    (:espeak smithers :duration ,audiodelay)
    (:ascii opened :duration 10 :text (32 46 "
┌─┐┌┬┐┬
└─┐││││
└─┘┴ ┴┴"))
    (:ascii closed :duration ,(- 16 audiodelay) :text (39 46 "
┌┬┐┬ ┬┌─┐┬─┐┌─┐
 │ ├─┤├┤ ├┬┘└─┐
 ┴ ┴ ┴└─┘┴└─└─┘"))
    (:espeak youre :duration ,audiodelay)
    (:ascii opened :duration 06 :text (21 52 "
┬ ┬┌─┐┬ ┬
└┬┘│ ││ │
 ┴ └─┘└─┘"))
    (:ascii closed :duration ,(- 04 audiodelay) :text (30 52 "
|┬─┐┌─┐
 ├┬┘├┤
 ┴└─└─┘"))
    (:espeak quite :duration ,audiodelay)
    (:ascii opened :duration 06 :text (40 52 "
┌─┐ ┬ ┬┬
│─┼┐│ ││
└─┘└└─┘┴"))
    (:ascii closed :duration ,(- 06 audiodelay) :text (48 52 "
┌┬┐┌─┐
 │ ├┤
 ┴ └─┘"))
    (:espeak good :duration ,audiodelay)
    (:ascii opened :duration 10 :text (33 58 "
┌─┐┌─┐
│ ┬│ │
└─┘└─┘") )
    (:ascii closed :duration ,(- 08 audiodelay) :text (39 58 "
┌─┐┌┬┐
│ │ ││
└─┘─┴┘"))
    (:espeak at :duration ,audiodelay)
    (:ascii opened :duration 08 :text (48 58 "
┌─┐
├─┤
┴ ┴"))
    (:ascii closed :duration ,(- 06 audiodelay) :text (51 58 "
┌┬┐
 │
 ┴"))
    (:espeak turning :duration ,audiodelay)
    (:ascii opened :duration 11 :text (15 64 "
┌┬┐┬ ┬┬─┐┌┐┌
 │ │ │├┬┘│││
 ┴ └─┘┴└-┘└┘") )
    (:ascii closed :duration ,(- 06 audiodelay) :text (27 64 "
-┬-┌┐┌┌─┐
 │ ││││ ┬
 ┴ ┘└┘└─┘"))
    (:espeak me :duration ,audiodelay)
    (:ascii opened :duration 06 :text (39 64 "
┌┬┐
│││
┴ ┴") )
    (:ascii closed :duration ,(- 06 audiodelay) :text (42 64 "
┌─┐
├┤
└─┘"))
    (:espeak on :duration ,audiodelay)
    (:ascii opened :duration 10 :text (48 64 "
┌─┐
│ │
└─┘"))
    (:ascii closed :duration 24 :text (51 64 "
┌┐┌
│││
┘└┘"))
    ;; Winky wink
    (:ascii winked :duration 16 :text (1 1 ""))
    (:ascii closed :duration 7 :text (1 1 " "))
    ))
;;  "Timings as derived as 'ticks' from the source video.")

(setq active-text nil
      backlen 50
      end-wait 100)

(defun place-text (xytext)
  "Place text and position from XYTEXT made up of (x y text
clear), where clear makes the active-text nil."
  (with-current-buffer "*smithers*"
    (if (nth 3 xytext) (setq active-text nil))
    (push xytext active-text)
    (dolist (xyt active-text)
      (let ((left (nth 0 xyt))
            (top (nth 1 xyt))
            (text (nth 2 xyt)))
        (artist-text-insert-common left top text nil)))
    (if (>= (length active-text) backlen)
        (setq active-text (subseq active-text 0 backlen)))))

(defun smithers-simple (leftpad fps)
  "Simple init."
  (let ((cbuff (current-buffer))
        (ascii-map (-map (lambda (x) (cons x (get-text-contents x leftpad)))
                         '(start0 start1 start2 start3 start4 start5 start6
                                  closed opened winked))))
    (with-current-buffer (get-buffer-create "*smithers*")
      (switch-to-buffer "*smithers*")
      (setq-local cursor-type nil)
      (buffer-disable-undo)
      (toggle-truncate-lines 1)
      (message "")
      (text-scale-set -3.5)
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
          (if xytext (place-text xytext))
          (if (> (or ticks 0) 0)
              (sit-for (/ (float ticks) fps)))))
      (sit-for (/ (float end-wait) fps))
      (kill-buffer))
    (switch-to-buffer cbuff)))

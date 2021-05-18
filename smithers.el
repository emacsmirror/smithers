;;; smithers.el --- Mr Smithers' startup message of Burns

;; Copyright (C) 2021 Mehmet Tekman <mtekman89@gmail.com>

;; Author: Mehmet Tekman
;; URL: https://gitlab.com/mtekman/smithers.el
;; Keywords: games
;; Package-Requires: ((emacs "26.1") (dash "2.17.0"))
;; Version: 0.1

;;; Commentary:
;;
;; Sometimes we all need a message of encouragement from our sexy
;; employer.  This is a recreation of Waylon Smithers' startup message
;; from the The Simpsons of Charles Montgomery Burns.

;;; Code:
(require 'dash)
(require 'artist)

(defgroup smithers nil
  "Smithers customization group."
  :group 'emacs)

(defcustom smithers-fps 25
  "Frames per second to scale the duration intervals given in the ``smithers--timings''."
  :type 'integer
  :group 'smithers)

(defcustom smithers-leftpad 100
  "Amount of left-padding to centre the graphics.  Decrease this value if the text is too far to the right."
  :type 'integer
  :group 'smithers)

(defcustom smithers-scale -3.5
  "Amount to scale the text in the '*smithers*' buffer.  Negative values reduce the size."
  :type 'integer
  :group 'smithers)

(defcustom smithers-espeakcomm "espeak -ven-us+m5 -p 80 -s 170 -g 10"
  "Command used to generate WAVs in the ``smithers-dirwavs'' directory.
This operation is only performed once for each sound key."
  :type 'string
  :group 'smithers)

(defcustom smithers-mediaplaycomm "mpv"
  "Media player command used to play WAVs."
  :type 'string
  :group 'smithers)

(defcustom smithers-audiodelay 5
  "The number of frames in which to delay the ascii rendering due to the media player being slow to spawn.  If the audio feels delayed, increase this value."
  :type 'integer
  :group 'smithers)

(defcustom smithers-activetext-limit 50
  "Maximum number of text items to render at any given time."
  :type 'integer
  :group 'smithers)

(defcustom smithers-enddelay 50
  "Number of frames to wait at the end of the animation before killing the '*smithers*' buffer."
  :type 'integer
  :group 'smithers)

(defcustom smithers-dirascii
  (expand-file-name "ascii" ".")
  "Directory where the ascii of Burns are."
  :type 'directory
  :group 'smithers)

(defcustom smithers-dirwavs
  (expand-file-name "wavs" ".")
  "The directory of WAVs containing the necessary files:
{hello,smithers,youre,quite,good,at,turning,me,on}.wav

Due to copyright infringement, all WAVs were generated using espeak, but it can technically use any WAVs you get it."
  :type 'directory
  :group 'smithers)

(defvar smithers--soundalist
  '((hello "he@'ll',u:")(smithers "smi,D3:ss")
    (youre "juor")(quite "kwaIt")(good "gU@d")
    (at "ad")(turning "d3::nI2N")(me "mi:::")(on "A@:n"))
  "Alist of sound keys and their phenomes.")

(defvar smithers--activetext nil
  "List of text to render in the next frame.  Elements are pushed from ``smithers--placetext''.")

(defvar smithers--timings
  `(;; Startup sequence
    (:ascii start0 :duration 05) (:ascii start1 :duration 02) (:ascii start2 :duration 02)
    (:ascii start3 :duration 02) (:ascii start4 :duration 02) (:ascii start5 :duration 02)
    (:ascii start6 :duration 02)
    ;; Talking Sequence
    (:ascii closed :duration ,(- 08 smithers-audiodelay))
    (:espeak hello :duration 10)(:ascii opened :duration 11 :text (15 45 "
┬ ┬┌─┐┬
├─┤├┤ │
┴ ┴└─┘┴─┘" t))(:ascii closed :duration ,(- 06 smithers-audiodelay) :text (24 45 "
┬  ┌─┐
│  │ │
┴─┘└─┘"))(:espeak smithers :duration ,smithers-audiodelay) (:ascii opened :duration 10 :text (32 45 "
┌─┐┌┬┐┬
└─┐││││
└─┘┴ ┴┴"))(:ascii closed :duration ,(- 16 smithers-audiodelay) :text (39 45 "
┌┬┐┬ ┬┌─┐┬─┐┌─┐
 │ ├─┤├┤ ├┬┘└─┐
 ┴ ┴ ┴└─┘┴└─└─┘"))(:espeak youre :duration ,smithers-audiodelay)(:ascii opened :duration 06 :text (21 51 "
┬ ┬┌─┐┬ ┬
└┬┘│ ││ │
 ┴ └─┘└─┘"))(:ascii closed :duration ,(- 04 smithers-audiodelay) :text (30 51 "
|┬─┐┌─┐
 ├┬┘├┤
 ┴└─└─┘"))(:espeak quite :duration ,smithers-audiodelay)(:ascii opened :duration 06 :text (40 51 "
┌─┐ ┬ ┬┬
│─┼┐│ ││
└─┘└└─┘┴"))(:ascii closed :duration ,(- 06 smithers-audiodelay) :text (48 51 "
┌┬┐┌─┐
 │ ├┤
 ┴ └─┘"))(:espeak good :duration ,smithers-audiodelay)(:ascii opened :duration 10 :text (33 57 "
┌─┐┌─┐
│ ┬│ │
└─┘└─┘"))(:ascii closed :duration ,(- 08 smithers-audiodelay) :text (39 57 "
┌─┐┌┬┐
│ │ ││
└─┘─┴┘"))(:espeak at :duration ,smithers-audiodelay)(:ascii opened :duration 08 :text (48 57 "
┌─┐
├─┤
┴ ┴"))(:ascii closed :duration ,(- 06 smithers-audiodelay) :text (51 57 "
┌┬┐
 │
 ┴"))(:espeak turning :duration ,smithers-audiodelay)(:ascii opened :duration 11 :text (15 63 "
┌┬┐┬ ┬┬─┐┌┐┌
 │ │ │├┬┘│││
 ┴ └─┘┴└-┘└┘"))(:ascii closed :duration ,(- 06 smithers-audiodelay) :text (27 63 "
-┬-┌┐┌┌─┐
 │ ││││ ┬
 ┴ ┘└┘└─┘"))(:espeak me :duration ,smithers-audiodelay)(:ascii opened :duration 06 :text (39 63 "
┌┬┐
│││
┴ ┴"))(:ascii closed :duration ,(- 06 smithers-audiodelay) :text (42 63 "
┌─┐
├┤
└─┘"))(:espeak on :duration ,smithers-audiodelay)(:ascii opened :duration 10 :text (48 63 "
┌─┐
│ │
└─┘"))(:ascii closed :duration 24 :text (51 63 "
┌┐┌
│││
┘└┘"))
    ;; Winky wink
    (:ascii winked :duration 16 :text (1 1 ""))(:ascii closed :duration 7 :text (1 1 " ")))
  "Timings, with durations as derived from counting frames from the source video.  The ``smithers-audiodelay' given is designed to overcome any overhead in spawning the media player so that the open mouth ascii syncs with the start of the synthesized speech.")

;; --{ Functions }---

(defun smithers--assert-wavs (&optional overwrite)
  "Assert that directory for wavs exists, and if not create and populate it.  If OVERWRITE is t, then overwrite."
  (when (or overwrite (not (file-exists-p (expand-file-name "hello.wav" smithers-dirwavs))))
    (message "Generating WAVs")
    (mkdir smithers-dirwavs t)
    (dolist (speech (--filter (plist-get it :espeak) smithers--timings))
      (let* ((key (plist-get speech :espeak))
             (fname (expand-file-name (format "%s.wav" key) smithers-dirwavs))
             (phenoms (alist-get key smithers--soundalist)))
        (call-process-shell-command
         (format "%s \"[[%s]]\" -w %s" smithers-espeakcomm phenoms fname) nil 0)))))

(defun smithers--playwav (speechkey)
  "Asynchronously play WAV file associated with SPEECHKEY, and error out if not present."
  (call-process-shell-command
   (format "%s '%s'"
           smithers-mediaplaycomm (expand-file-name (format "%s.wav" speechkey)
                                                    smithers-dirwavs))
   nil 0))

(defun smithers--getascii (ascii-key &optional lpad)
  "Retrieve the text contents of 'ascii/ASCII-KEY.txt' and left-pad by LPAD amount."
  (let ((fname (expand-file-name (format "%s.txt" ascii-key) smithers-dirascii)))
    (if (file-exists-p fname)
        (with-temp-buffer
          (insert-file-contents fname)
          (if lpad ;; Insert vertical column
              (replace-rectangle (point-min) (point-max)
                                 (make-string lpad ? )))
          (buffer-substring-no-properties
           (point-min) (point-max))))))

(defun smithers--placetext (xytext)
  "Place text at position and optionally add to active text space.
Options given in XYTEXT made up of (x y text clear) where clear sets the ``smithers--activetext'' nil."
  (with-current-buffer "*smithers*"
    (if (nth 3 xytext) (setq smithers--activetext nil))
    (push xytext smithers--activetext)
    (dolist (xyt smithers--activetext)
      (let ((left (nth 0 xyt))
            (top (nth 1 xyt))
            (text (nth 2 xyt)))
        (artist-text-insert-common left top text nil)))
    (if (>= (length smithers--activetext) smithers-activetext-limit)
        (setq smithers--activetext (cl-subseq smithers--activetext 0 smithers-activetext-limit)))))

;;;###autoload
(defun smithers-with-opts (lpad scale fps)
  "Run ``smithers'' with specific left-padding LPAD, scaling SCALE amount, and frames-per-second FPS.  Useful for testing."
  (interactive (list (read-number "Left-Padding: " smithers-leftpad)
                     (read-number "Scale: " -3.5)
                     (read-number "FPS: " smithers-fps)))
  (let ((cbuff (current-buffer))
        (ascii-map (-map (lambda (x) (cons x (smithers--getascii x lpad)))
                         '(start0 start1 start2 start3 start4 start5 start6
                                  closed opened winked))))
    (smithers--assert-wavs)
    (with-current-buffer (get-buffer-create "*smithers*")
      (switch-to-buffer "*smithers*")
      (setq-local cursor-type nil)
      (buffer-disable-undo)
      (toggle-truncate-lines 1)
      (message "")
      (text-scale-set scale)
      (dolist (page smithers--timings)
        (let* ((asc-key (plist-get page :ascii))
               (ticks (plist-get page :duration))
               (xytext (plist-get page :text))
               (espeak (plist-get page :espeak))
               (asc-txt (alist-get asc-key ascii-map)))
          (if espeak (smithers--playwav espeak))
          (when asc-txt
            (erase-buffer)
            (insert asc-txt))
          (if xytext (smithers--placetext xytext))
          (if (> (or ticks 0) 0)
              (sit-for (/ (float ticks) fps)))))
      (sit-for (/ (float smithers-enddelay) fps))
      (kill-buffer))
    (switch-to-buffer cbuff)))

;;;###autoload
(defun smithers ()
  "A prosaic message of encouragement from an employer to his supplicant.  Or a dispassionate statement of approval from a machine to its user."
  (interactive)
  (smithers-with-opts smithers-leftpad smithers-scale smithers-fps))

(provide 'smithers)

;;; smithers.el ends here

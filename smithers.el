;;; smithers.el --- Mr Smithers' startup message of Burns -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Mehmet Tekman <mtekman89@gmail.com>

;; Author: Mehmet Tekman
;; URL: https://gitlab.com/mtekman/smithers.el
;; Keywords: games
;; Package-Requires: ((emacs "26.1") (dash "2.17.0") (org "9.4.5"))
;; Version: 0.3

;;; Commentary:
;;
;; Sometimes we all need a message of encouragement from our sexy
;; employer.  This is a recreation of Waylon Smithers' startup message
;; from the The Simpsons of Charles Montgomery Burns.

;;; Code:
(require 'dash)
(require 'artist)
(require 'org)

(defgroup smithers nil
  "Smithers customization group."
  :group 'emacs)

(defcustom smithers-fps 25
  "Frames per second to scale the duration intervals given in the ``smithers--timings''."
  :type 'integer
  :group 'smithers)

(defcustom smithers-autoscale t
  "Automatically scale and center the graphics based on buffer dimensions.  It attempts to automatically set the  ``smithers-leftpad'', ``smithers-toppad'', ``smithers-scale'' variables.  Disable this if you wish to use custom settings."
  :type 'boolean
  :group 'smithers)


(defcustom smithers-leftpad 50
  "Amount of left-padding to centre the graphics.  Decrease this value if the text is too far to the right."
  :type 'integer
  :group 'smithers)

(defcustom smithers-toppad 2
  "Amount of top-padding to centre the graphics.  Decrease this value if the text is too far down."
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

(defcustom smithers-enddelay 20
  "Number of frames to wait at the end of the animation before killing the '*smithers*' buffer."
  :type 'integer
  :group 'smithers)

(defvar smithers-dir
  (expand-file-name "smithers" package-user-dir)
  "Directory where smimthers is installed.")

(defvar smithers-dirdata
  (expand-file-name "data" smithers-dir)
  "Directory where smithers data directory is.")

(defcustom smithers-dirgraphics
  (expand-file-name "graphics" smithers-dirdata)
  "Directory where the ascii graphics of Burns are kept."
  :type 'directory
  :group 'smithers)

(defcustom smithers-dirwavs
  (expand-file-name "wavs" smithers-dirdata)
  "The directory of WAVs containing the necessary files:
{hello,smithers,youre,quite,good,at,turning,me,on}.wav

Due to copyright infringement, all WAVs were generated using espeak, however it can technically use any WAVs you give it."
  :type 'directory
  :group 'smithers)

(defcustom smithers-dirwords
  (expand-file-name "words" smithers-dirdata)
  "The directory where the ascii graphics of words are kept."
  :type 'directory
  :group 'smithers)

(defvar smithers--soundalist
  '((hello . "he@'ll',u:")(smithers . "smi,D3:ss")
    (youre . "juor")(quite . "kwaIt")(good . "gU@d")
    (at . "ad")(turning . "d3::nI2N")(me . "mi:::")(on . "A@:n"))
  "Alist of sound keys and their phenomes.")

(defvar smithers--activetext nil
  "List of text to render in the next frame.  Elements are pushed from ``smithers--placeword''.")

(defvar smithers--timings
  `(;; Startup sequence
    (:ascii start0 :duration 05) (:ascii start1 :duration 02) (:ascii start2 :duration 02)
    (:ascii start3 :duration 02) (:ascii start4 :duration 02) (:ascii start5 :duration 02)
    (:ascii start6 :duration 02)
    ;; Talking Sequence
    (:ascii closed :duration ,(- 08 smithers-audiodelay))
    (:espeak hello :duration 10)
    (:ascii opened :duration 11 :text (15 45 hel t))
    (:ascii closed :duration ,(- 06 smithers-audiodelay) :text (24 45 lo))
    (:espeak smithers :duration ,smithers-audiodelay)
    (:ascii opened :duration 10 :text (32 45 smi))
    (:ascii closed :duration ,(- 16 smithers-audiodelay) :text (39 45 thers))
    (:espeak youre :duration ,smithers-audiodelay)
    (:ascii opened :duration 06 :text (21 51 you))
    (:ascii closed :duration ,(- 04 smithers-audiodelay) :text (30 51 re))
    (:espeak quite :duration ,smithers-audiodelay)
    (:ascii opened :duration 06 :text (40 51 qui))
    (:ascii closed :duration ,(- 06 smithers-audiodelay) :text (48 51 te))
    (:espeak good :duration ,smithers-audiodelay)
    (:ascii opened :duration 10 :text (33 57 go))
    (:ascii closed :duration ,(- 08 smithers-audiodelay) :text (39 57 od))
    (:espeak at :duration ,smithers-audiodelay)
    (:ascii opened :duration 08 :text (48 57 a))
    (:ascii closed :duration ,(- 06 smithers-audiodelay) :text (51 57 t))
    (:espeak turning :duration ,smithers-audiodelay)
    (:ascii opened :duration 11 :text (15 63 turn))
    (:ascii closed :duration ,(- 06 smithers-audiodelay) :text (27 63 ing))
    (:espeak me :duration ,smithers-audiodelay)
    (:ascii opened :duration 06 :text (39 63 m))
    (:ascii closed :duration ,(- 06 smithers-audiodelay) :text (42 63 e))
    (:espeak on :duration ,smithers-audiodelay)
    (:ascii opened :duration 10 :text (48 63 o))
    (:ascii closed :duration 24 :text (51 63 n))
    ;; Winky wink
    (:ascii winked :duration 16 :text (1 1 nil))
    (:ascii closed :duration 7 :text (1 1 nil)))
  "Timings, with durations as derived from counting frames from the source video.  The ``smithers-audiodelay' given is designed to overcome any overhead in spawning the media player so that the open mouth ascii syncs with the start of the synthesized speech.  Text positions are relative to the image root and will be shifted accordingly to frame size if ``smithers-autoscale'' is active.")

;; --{ Functions }---

(defun smithers--checktestfile (test-file test-dir typetext)
  "Check that TEST-FILE exists, and if not offer to regenerate it (and others) in TEST-DIR, mentioning TYPETEXT."
  (and (not (file-exists-p (expand-file-name test-file test-dir)))
       (y-or-n-p (format "Could not find %s in '%s', regenerate? "
                         typetext test-dir))))

(defun smithers--findsourcefile ()
  "Find the source.org file for generating ASCII."
  (let* ((source-org-file1 (expand-file-name "source.org" smithers-dir))
         (source-org-file2 (expand-file-name "source.org")))
    (cond ((file-exists-p source-org-file1) source-org-file1)
          ((file-exists-p source-org-file2) source-org-file2)
          (t (error "Could not find 'source.org' to generate ascii")))))

(defun smithers--tangleblocks (startheader endheader directory)
  "Tangle blocks from source `org-mode' file between STARTHEADER and ENDHEADER, and prepend tangle blocks with DIRECTORY."
  (with-current-buffer (find-file (smithers--findsourcefile))
    (goto-char 1)
    (setq-local org-src-preserve-indentation t)
    (let* ((beg (re-search-forward startheader))
           (end (re-search-forward endheader)))
      (goto-char beg)
      (while (let* ((_xx (org-next-block 1))
                    (now (point)))
               (and (< now end)
                    (>= now beg)))
        (let* ((block-info (org-babel-get-src-block-info))
               (text (cadr block-info))
               (tangle-name (cdar (--filter (eq (car it) :tangle) (caddr block-info))))
               (newfile-name (expand-file-name tangle-name directory)))
          (with-temp-buffer
            (insert text)
            (write-file newfile-name)))))
    (kill-buffer)))

(defun smithers--assert-graphics (&optional overwrite)
  "Assert that directory for ascii graphics exists, and if not create and populate it.  If OVERWRITE is t, then overwrite."
  (when (or overwrite (smithers--checktestfile "opened.txt" smithers-dirgraphics "ASCII graphics"))
    (mkdir smithers-dirgraphics t)
    (smithers--tangleblocks "^* Graphics" "^* Words" smithers-dirgraphics)
    (message "Graphics generated.")))

(defun smithers--assert-words (&optional overwrite)
  "Assert that directory for ascii words exists, and if not create and populate it.  If OVERWRITE is t, then overwrite."
  (when (or overwrite (smithers--checktestfile "a.txt" smithers-dirwords "ASCII words"))
    (mkdir smithers-dirwords t)
    (smithers--tangleblocks "^* Words" "^* Timings" smithers-dirwords)
    (message "Words generated.")))

(defun smithers--assert-wavs (&optional overwrite)
  "Assert that directory for wavs exists, and if not create and populate it.  If OVERWRITE is t, then overwrite."
  (when (or overwrite (smithers--checktestfile "hello.wav" smithers-dirwavs "WAVs"))
    (mkdir smithers-dirwavs t)
    (dolist (speech (--filter (plist-get it :espeak) smithers--timings))
      (let* ((key (plist-get speech :espeak))
             (fname (expand-file-name (format "%s.wav" key) smithers-dirwavs))
             (phenoms (alist-get key smithers--soundalist)))
        (shell-command
         (format "%s \"[[%s]]\" -w %s" smithers-espeakcomm phenoms fname) nil nil)))
    (message "Regenerating WAVs")))

(defun smithers--assertallmedia (&optional doit)
  "Detect if this is the first time that ``smithers'' has been run.  If so, regenerate everything, otherwise partially.  The DOIT option force all regeneration."
  (save-excursion
    (let ((genall (or doit (not (file-exists-p smithers-dirdata)))))
      (smithers--assert-graphics genall)
      (smithers--assert-words genall)
      (smithers--assert-wavs genall))))

(defun smithers--playwav (speechkey)
  "Asynchronously play WAV file associated with SPEECHKEY, and error out if not present."
  (call-process-shell-command
   (format "%s '%s'"
           smithers-mediaplaycomm (expand-file-name (format "%s.wav" speechkey)
                                                    smithers-dirwavs))
   nil 0))

(defun smithers--getgraphic (ascii-key &optional lpad)
  "Retrieve the text contents of 'graphics/ASCII-KEY.txt' and left-pad by LPAD amount."
  (let ((fname (expand-file-name (format "%s.txt" ascii-key) smithers-dirgraphics)))
    (if (file-exists-p fname)
        (with-temp-buffer
          (insert-file-contents fname)
          (if lpad ;; Insert a vertical column of space
              (indent-rigidly (point-min) (point-max) lpad))
          (buffer-substring-no-properties (point-min) (point-max))))))

(defun smithers--getword (ascii-key)
  "Retrieve the text contents of 'words/ASCII-KEY.txt'."
  (let ((fname (expand-file-name (format "%s.txt" ascii-key) smithers-dirwords)))
    (if (file-exists-p fname)
        (with-temp-buffer
          (insert-file-contents fname)
          (buffer-substring-no-properties (point-min) (point-max))))))

(defun smithers--placeword (xytext asciitext lpad tpad)
  "Place text at position and optionally add to active text space.
Options given in XYTEXT made up of (x y text clear) where clear wipes the ``smithers--activetext''.  The ASCIITEXT provides the actual ascii.  This function could be written a bit cleaner since we don't need the text component, only the ascii.  LPAD and TPAD provide top and left padding to compensate for positional changes."
  (with-current-buffer "*smithers*"
    (if (nth 3 xytext)
        (setq smithers--activetext nil))
    (if (nth 2 xytext) ;; a nil value will still render previous words
        (push (list (+ (nth 0 xytext) (floor (/ lpad 2.5)))
                    (- (nth 1 xytext) (* 2 tpad))
                    asciitext)
              smithers--activetext))
    (dolist (xyt smithers--activetext)
      (let ((left (nth 0 xyt))
            (top (nth 1 xyt))
            (text (nth 2 xyt)))
        (artist-text-insert-common left top text nil)))
    (if (>= (length smithers--activetext) smithers-activetext-limit)
        (setq smithers--activetext (cl-subseq smithers--activetext 0 smithers-activetext-limit)))))


(defun smithers--determine-sf ()
  "Determine Size Factors to resize the buffer to fit the graphic.  Coefficients and Intercepts derived by fitting a linear model to window and scale observations recorded in the source `org-mode' file."
  (let ((winW (window-total-width))
        (winH (window-total-height)))
    (let ((winSc (+ (* 0.014 winW) (* 0.125 winH) -11))
          (padl (+ (*  2 winW) (* -5 winH) 4))
          (padt (+ (* -0.178 winW) (* 0.479 winH) 8)))
      (let ((fixpadt (if (> padt 0) (floor padt) 0))
            (fixpadl (if (> padl 0) (floor padl) 0)))
        (list :sizefactors winSc :padtop fixpadt :padleft fixpadl)))))

;;;###autoload
(defun smithers-with-opts (lpad tpad scale fps)
  "Run ``smithers'' with specific left-padding LPAD, top-padding TPAD, scaling SCALE amount, and frames-per-second FPS."
  (interactive (list (read-number "Left-Padding: " smithers-leftpad)
                     (read-number "Top-Padding: " smithers-toppad)
                     (read-number "Scale: " -3.5)
                     (read-number "FPS: " smithers-fps)))
  (smithers--assertallmedia)
  (let ((cbuff (current-buffer))
        (top-pad (make-string tpad ?\n))
        (ascii-map (-map (lambda (x) (cons x (smithers--getgraphic x lpad)))
                         '(start0 start1 start2 start3 start4 start5 start6
                                  closed opened winked)))
        (word-map (-map (lambda (x) (cons x (smithers--getword x)))
                        '(hel lo smi thers you re qui te go od a t turn ing m e o n))))
    (with-current-buffer (get-buffer-create "*smithers*")
      (switch-to-buffer "*smithers*")
      (artist-mode 1) ;; required for insert to work without artefacts
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
               (asc-gfx (alist-get asc-key ascii-map))
               (asc-wrd (alist-get (nth 2 xytext) word-map)))
          (if espeak (smithers--playwav espeak))
          (when asc-gfx
            (erase-buffer)
            (insert top-pad asc-gfx)
            (goto-char 1))
          (if xytext ;; still render nil text
              (smithers--placeword xytext asc-wrd lpad tpad))
          (if (> (or ticks 0) 0)
              (sit-for (/ (float ticks) fps)))))
      (sit-for (/ (float smithers-enddelay) fps))
      (kill-buffer))
    (switch-to-buffer cbuff)))

;;;###autoload
(defun smithers ()
  "A prosaic message of encouragement from an employer to his supplicant.  Or a dispassionate statement of approval from a machine to its user."
  (interactive)
  (let ((pad-left smithers-leftpad)
        (pad-top smithers-toppad)
        (scale smithers-scale))
    (if smithers-autoscale
        (let ((x (smithers--determine-sf)))
          (setq pad-left (plist-get x :padleft)
                pad-top (plist-get x :padtop)
                scale (plist-get x :sizefactors))))
    (smithers-with-opts pad-left pad-top scale smithers-fps)))


(provide 'smithers)
;;; smithers.el ends here

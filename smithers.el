

(defun get-text-contents (key)
  "For KEY retrieve the text contents of 'ascii/KEY.txt'."
  (let ((fname (format "%s%s%s.txt" "ascii" "/" key))
        (rstring nil))
    (if (file-exists-p fname)
        (with-temp-buffer
          (insert-file-contents fname)
          (setq rstring
                (buffer-substring-no-properties
                 (point-min) (point-max)))))))

(defvar timings
  '(;; Startup sequence
    (:ascii start0 :duration 12)
    (:ascii start1 :duration 02)
    (:ascii start2 :duration 02)
    (:ascii start3 :duration 02)
    (:ascii start4 :duration 02)
    (:ascii start5 :duration 02)
    (:ascii start6 :duration 02)
    (:ascii closed :duration 08)
    ;; Talking Sequence
    (:ascii opened :duration 16 :pos (1 01) :speech "Hel")
    (:ascii closed :duration 06 :pos (1 03) :speech "lo")
    (:ascii opened :duration 10 :pos (1 06) :speech "Smi")
    (:ascii closed :duration 16 :pos (1 09) :speech "thers")
    (:ascii opened :duration 06 :pos (2 01) :speech "You")
    (:ascii closed :duration 04 :pos (2 04) :speech "'re")
    (:ascii opened :duration 06 :pos (2 08) :speech "Qui")
    (:ascii closed :duration 06 :pos (2 11) :speech "te")
    (:ascii opened :duration 10 :pos (2 15) :speech "Go")
    (:ascii closed :duration 08 :pos (2 17) :speech "od")
    (:ascii opened :duration 08 :pos (3 01) :speech "a")
    (:ascii closed :duration 06 :pos (3 02) :speech "t")
    (:ascii opened :duration 11 :pos (3 05) :speech "turn")
    (:ascii closed :duration 06 :pos (3 09) :speech "ing")
    (:ascii opened :duration 06 :pos (3 14) :speech "m")
    (:ascii closed :duration 06 :pos (3 15) :speech "e")
    (:ascii opened :duration 10 :pos (3 18) :speech "o")
    (:ascii closed :duration 24 :pos (3 19) :speech "n")
    ;; Winky wink
    (:ascii winked :duration 16)
    (:ascii closed :duration 7))
  "Timings as derived as 'ticks' from the source video.")

(setq ticker nil
      tickrate 0.01)

(defun smithers-old ()
  "Hello smithers, you're quite good a turning me on."
  (with-current-buffer (get-buffer-create "*smithers*")
    (if ticker
        (progn
          (cancel-timer ticker)
          (setq ticker nil))
      ;; Start Anim
      (switch-to-buffer (current-buffer))
      (buffer-disable-undo)
      (erase-buffer)
      (toggle-truncate-lines 1)
      (setq-local cursor-type nil)
      (text-scale-set -2.5)
      ;;
      (setq ticker
            (run-at-time nil (get-tickrate
                         (apply-partially #'update-buffer
                                          (get-buffer-create "*smithers*"))))
      (setq-local kill-buffer-hook (lambda ()
                                     (when ticker (cancel-timer ticker)
					                       (setq ticker nil))))))))
(setq fps 25)
(defun smithers-simple ()
  "Simple init."
  (let ((ascii-map (-map (lambda (x) (cons x (get-text-contents x)))
                         '(start0 start1 start2 start3 start4 start5 start6
                                  closed opened winked))))
    (with-current-buffer (get-buffer-create "*smithers*")
      (switch-to-buffer "*smithers*")
      (buffer-disable-undo)
      (toggle-truncate-lines 1)
      (message "")
      (text-scale-set -1.5)
      (setq-local cursor-type nil)
      (dolist (page timings)
        (let* ((asc-key (plist-get page :ascii))
               (ticks (plist-get page :duration))
               (xandy (plist-get page :pos))
               (speech (plist-get page :speech))
               (asc-txt (alist-get asc-key ascii-map)))
          (erase-buffer)
          (insert asc-txt)
          (sit-for (/ (float ticks) fps))
          )))))
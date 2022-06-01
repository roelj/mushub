;;;; track-visualisation.lisp

(in-package :mushub)

;; (defparameter *file-path* #P"/home/roel/Programming/mushub/test-files/session_Guide Left_BWAV 32float Export.wav")
;; (defparameter *file-path* #P"/home/roel/Programming/mushub/test-files/session.wav")
;; (defparameter *file-path* #P"/home/roel/Programming/mushub/test-files/session_guide_left.wav")
;; (defparameter *wav-stream* (open *file-path* :direction :input :element-type '(unsigned-byte 8)))

;; (defparameter *wav-reader-stream* (easy-audio.wav:open-wav *wav-stream*))
;; (defparameter *wav-header* (easy-audio.wav:read-wav-header *wav-reader-stream*))
;; (defparameter *wav-format* (car *wav-header*))
;; (defparameter *wav-num-samples* (easy-audio.wav:samples-num *wav-header*))
;; (easy-audio.wav:format-audio-format *wav-format*)

;; *wav-num-samples*

;; (easy-audio.wav:reader-position-to-audio-data *wav-reader-stream* *wav-header*)
;; (defparameter *wav-data* (easy-audio.wav:read-wav-data *wav-reader-stream* *wav-format* *wav-num-samples*))

;; (type-of *wav-data*)
;; (aref *wav-data* 10015)
;; *wav-header*

(defun reduce-array (input number-of-samples)
  "Returns a reduced number of data points in INPUT by sampling
evenly distributed over INPUT. When INPUT is a shorter sequence
than NUMBER-OF-SAMPLES, INPUT is returned."
  (let* ((total-samples (array-total-size input))
         (step          (truncate (/ total-samples number-of-samples))))
    (if (> number-of-samples total-samples)
        input
        (let ((output (make-sequence '(vector integer) number-of-samples)))
          (dotimes (index number-of-samples)
            (setf (aref output index)
                  (aref input (1- (* (1+ index) step)))))
          output))))

;; (defparameter *plottable-range* (reduce-array *wav-data* 112))
;; (apply #'min (map 'list #'identity *plottable-range*))
;; (map 'list #'abs *plottable-range*)

(defun waveform-svg (wav-data)
  (let* ((resolution 112)
         (values     (reduce-array wav-data resolution))
         (values-lst (map 'list #'abs values))
         (min-value  (apply #'min values-lst))
         (max-value  (apply #'max values-lst))
         (x-value    20.0)
         (scene      (cl-svg:make-svg-toplevel 'cl-svg:svg-1.1-toplevel
                                               :height 140 :width 880)))
    ;; Draw a background with a thick border.
    (cl-svg:make-group scene ()
      (cl-svg:draw* (:rect :x      5
                           :y      5
                           :height 130
                           :width  870
                           :fill   "#cfefe8"
                           :rx     10
                           :ry     10
                           :style  "stroke:#217867;stroke-width:5"))
      (cl-svg:draw* (:rect :x 10.0 :y 69.5 :height 1   :width 860  :fill "#87decd" :rx 0.5)))
    ;; Draw the reduced set of values from the audio data.
    (cl-svg:make-group scene ()
      (dotimes (index resolution)
        ;; Normalize values between 0 and 100.
        (let ((y-value (* (/ (- (aref values index) min-value)
                             (- max-value min-value))
                          100.0)))
          (cl-svg:draw* (:rect :x      (+ 20 (* index 7.5))
                               :y      (- (/ (- 140.0 y-value) 2.0) 2.5)
                               :height (+ y-value 5)
                               :width  5.0
                               :fill   "#217867"
                               :rx     2.5)))))
    ;; Write the SVG stream to a file.
    (with-open-file (s #p"test.svg" :direction :output :if-exists :supersede)
      (cl-svg:stream-out s scene))))

;; (waveform-svg *wav-data*)

(defun range (start end)
  "Returns a vector containing the sequence from START to END - 1."
  (let* ((iterations (- end start))
         (output     (make-sequence '(vector integer) iterations)))
    (dotimes (index iterations)
      (setf (aref output index) (+ start index)))
    output))

;; (gc :full t)
;; (defparameter *test-array* (range 1 100000001))
;; (makunbound '*test-array*)

;; (array-total-size *test-array*)
;; (format *standard-output* "~a~%~a~%~a~%~a~%"
;;         (reduce-array (range 1 1001) 12)
;;         (reduce-array (range 1 1001) 6)
;;         (reduce-array (range 1 1001) 3)
;;         (reduce-array (range 1 1001) 1))


;; (format *standard-output* "~a~%" (reduce-array *test-array* 5))


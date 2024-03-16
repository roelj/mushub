;;;; track-visualisation.lisp

(in-package :mushub)

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

(defun reduce-array-avg (input number-of-samples)
  "Returns a reduced number of data points in INPUT by averaging
the data points, evenly distributed over INPUT.  When INPUT is a
shorter sequence than NUMBER-OF-SAMPLES, INPUT is returned."

  (let* ((total-samples (array-total-size input))
         (step          (truncate (/ total-samples number-of-samples))))
    (if (> number-of-samples total-samples)
        input
        (let ((output (make-sequence '(vector integer) number-of-samples)))
          (dotimes (index (1- number-of-samples))
            (let ((start (1- (* (1+ index) step))))
              (setf (aref output index)
                    (truncate (/ (loop for i from start to (+ start step)
                                       summing (aref input i))
                                 (* 1.0 step))))))
          output))))

(defun reduce-array-max (input number-of-samples)
  "Returns a reduced number of data points in INPUT by taking
the maximum value out of a block of the data points, where the
blocks are evenly distributed over INPUT.  When INPUT is a
shorter sequence than NUMBER-OF-SAMPLES, INPUT is returned."
  (let* ((total-samples (array-total-size input))
         (step          (truncate (/ total-samples number-of-samples))))
    (if (> number-of-samples total-samples)
        input
        (let ((output (make-sequence '(vector integer) number-of-samples)))
          (dotimes (index (1- number-of-samples))
            (let ((start-index (1- (* (1+ index) step))))
              (setf (aref output index)
                    (loop for i from start-index to (+ start-index step)
                          maximize (aref input i)))))
          output))))

(defun reduce-array-median (input number-of-samples)
  "Returns a reduced number of data points in INPUT by taking
the minimum value out of a block of the data points, where the
blocks are evenly distributed over INPUT.  When INPUT is a
shorter sequence than NUMBER-OF-SAMPLES, INPUT is returned."
  (let* ((total-samples (array-total-size input))
         (step          (truncate (/ total-samples number-of-samples)))
         (median        (truncate (/ number-of-samples 2))))
    (if (> number-of-samples total-samples)
        input
        (let ((output (make-sequence '(vector integer) number-of-samples)))
          (dotimes (index (1- number-of-samples))
            (let* ((start (1- (* (1+ index) step)))
                   (values      (sort (loop for i from start to (+ start step)
                                            collecting (aref input i))
                                      #'<))
                   (median-value (nth median values)))
              (setf (aref output index) median-value)))
          output))))

(defun waveform-svg (wav-data output-stream)
  "Writes a visualisation of WAV-DATA to OUTPUT-STREAM."
  (let* ((resolution 224)
         (values     (reduce-array-max wav-data resolution))
         (values-lst (map 'list #'abs values))
         (min-value  (apply #'min values-lst))
         (max-value  (apply #'max values-lst))
         (scene      (cl-svg:make-svg-toplevel 'cl-svg:svg-1.1-toplevel
                                               :height 134 :width 880)))
    ;; Avoid dividing by zero.
    (when (= max-value 0)
      (setf max-value 0.1))

    ;; Draw a background with a thick border.
    (cl-svg:make-group scene ()
      (cl-svg:draw* (:rect :x      2
                           :y      2
                           :height 130
                           :width  876
                           :fill   "#cfefe8"
                           :rx     10
                           :ry     10
                           :style  "stroke:#217867;stroke-width:2"))
      (cl-svg:draw* (:rect :x 10.0 :y 69.5 :height 1 :width 866  :fill "#87decd" :rx 0.5)))

    ;; Draw the reduced set of values from the audio data.
    (cl-svg:make-group scene ()
      (dotimes (index resolution)
        ;; Normalize values between 0 and 100.
        (let ((y-value (* (/ (- (aref values index) min-value)
                             (- max-value min-value))
                          100.0)))
          (cl-svg:draw* (:rect :x      (+ 20 (* index 3.75))
                               :y      (- (/ (- 140.0 y-value) 2.0) 2.5)
                               :height (+ y-value 5)
                               :width  2.5
                               :fill   "#217867"
                               :rx     5.0)))))

    ;; Write the results to OUTPUT-STREAM.
    (cl-svg:stream-out output-stream scene)))

(defun range (start end)
  "Returns a vector containing the sequence from START to END - 1."
  (let* ((iterations (- end start))
         (output     (make-sequence '(vector integer) iterations)))
    (dotimes (index iterations)
      (setf (aref output index) (+ start index)))
    output))

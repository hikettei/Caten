(defpackage :caten/vision
  (:use :cl :caten)
  (:export #:preprocess-image))

(in-package :caten/vision)
;; [TODO] Prepare a decent image processing library which is compatible with transform!!
;; Just i am too lazy to implement it now. using gpto1 generated preprocessing code for mobilenetv2
;; Following code doesnot worth at all so feel free to replace it with a decent one!
(defun center-crop (image crop-width crop-height)
  (opticl:with-image-bounds (height width) image
    (let ((left (floor (- width crop-width) 2))
          (top (floor (- height crop-height) 2)))
      (opticl:crop-image image left top (+ crop-width left) (+ crop-height top)))))

(defun rearrange-axes (array)
  (let* ((dims (array-dimensions array))
         (h (first dims))
         (w (second dims))
         (c (third dims))
         (new-array (make-array (list c h w))))
    (dotimes (i h)
      (dotimes (j w)
        (dotimes (k c)
          (setf (aref new-array k i j)
                (aref array i j k)))))
    new-array))

(defun scale-to-unit (array)
  (let* ((dims (array-dimensions array))
         (size (reduce #'* dims))
         (new-array (make-array dims :element-type 'single-float)))
    (dotimes (i size)
      (setf (row-major-aref new-array i) (/ (row-major-aref array i) 255.0)))
    new-array))

(defun normalize-channels (array mean std)
  (let* ((c (first (array-dimensions array)))
         (h (second (array-dimensions array)))
         (w (third (array-dimensions array))))
    (loop for k from 0 below c
          do (loop for i from 0 below h
                   do (loop for j from 0 below w
                            do (setf (aref array k i j)
                                     (/ (- (aref array k i j) (svref mean k)) (svref std k))))))
    array))

(defun preprocess-image (image-path)
  "Preprocessing image for mobilenetv2"
  (let* ((image (opticl:read-png-file image-path))
         (cropped-image (center-crop image 224 224))
         (tensor (rearrange-axes cropped-image))
         (tensor-float (scale-to-unit tensor))
         (mean #(0.485 0.456 0.406))
         (std #(0.229 0.224 0.225))
         (normalized-tensor (normalize-channels tensor-float mean std)))
    (change-facet normalized-tensor :tensor)))

;;; xmine-solve.el --- Solves xmine, a mine game for XEmacs

;; Author:     Liu Lukai <liulukai@gmail.com>
;; Keywords:   games
;; Version:    0.9

;; This file is part of XEmacs.

;; XEmacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; XEmacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with XEmacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

(require 'xmine)
; Optionally sets width, height, %-of-mines.
(setq xmine-width 20 xmine-height 20 %-of-mines 15)

; Note: graphically, x-direction of XMine is right-left and y-direction up-down.
(defcustom slowness 0 "# seconds to wait before taking next step." :type 'integer)
(defcustom max-step 100 "maximum number of steps allowed to solve a game.
Set to 0 to allow as many steps as needed." :type 'integer)
; probe-field
(defvar probe-field (make-vector (* xmine-width xmine-height) -2)
  "0: blank; 1~8: # surround mines remaining to detect; -1: flagged; \
-2: yet to find out.")
(defvar game-ended nil "Ended??")
(defvar last-step nil "What's last step that led to this situation?")

(defun index(position)
  "Parameter position is (list x y) where x is in [1, xmine-width], \
y in [1, xmine-height]. Returns 0-based linear index."
  (destructuring-bind (x y) position
    (if (and (and (plusp x) (plusp y)) (and (<= x xmine-width) (<= y xmine-height)))
        (+ (* (1- y) xmine-width) (1- x)) nil)))
(defun reverse-index(index)
  "Convert from 0-based linear index to 1-based List (x y)."
  (when (and (>= index 0) (< (* xmine-width xmine-height)))
    (list (1+ (mod index xmine-width)) (1+ (/ index xmine-width)))))
(defun act-on(position action) "'stomp/'flag on given position."
  (let ((index (index position)))
    (when index       ; responsible for updating the block in probe-field
      (sleep-for slowness)
      (destructuring-bind (x y) position
        (if (eq action 'stomp)
            (let* ((ext (xmine-field-button-at x y))
                   (type (extent-property ext 'xmine-type)))
              (xmine-action1 ext)
              (unless game-ended
                (setq game-ended (or (xmine-mine-button-p ext)
                                     (xmine-game-solved-p))))
              (unless (string= type "mine")
                (setf (aref probe-field index) (string-to-number type))
                (when (string= type "0") (see))))
          (progn (setf (aref probe-field index) -1)
                 (xmine-action3 (xmine-field-button-at x y)))))
      position)))
(defun see()           ; side-effects: probe-field.
  "Revealing 'xmine-type field (0-8 or \"mine\") only when 'end-open -ed."
;Be honest. No cheating.
  (loop for x from 1 to xmine-width do
    (loop for y from 1 to xmine-height do
      (let ((index (index (list x y))))
        (when (and (not (extent-property
                         (xmine-field-button-at x y) 'xmine-hidden)) ; revealed
                   (= (aref probe-field index) -2)) ; and I don't know yet
          (let ((type (extent-property (xmine-field-button-at x y) 'xmine-type)))
            (cond
             ((plusp (string-to-number type)) ; number
              (setf (aref probe-field index) (string-to-number type)))
             ((onep (length type)) ; 0 or invalid number ("mine")
              (setf (aref probe-field index) 0))
             ))))))) ; undetected true mine: suicide
(defun neighbors(position)
  "Returns an alist indexed by neighbor-location from probe-field.
Unknown fields are returned as -2."
  (when (index position)
    (let (neighbor-peek)
      (destructuring-bind (x y) position
        (loop for offx from -1 upto 1 do
          (loop for offy from -1 upto 1 do
            (unless (or (and (zerop offx) (zerop offy)) ; at center or
                        (not (index (list (+ x offx) (+ y offy)))))
              (let ((xd (+ x offx)) (yd (+ y offy)))
                (setq neighbor-peek
                      (acons (list xd yd) (aref probe-field (index (list xd yd)))
                             neighbor-peek))))))
        neighbor-peek))))

(defun random-poke(&optional really-random) ; side-effect: probe-field, game-ended
  "Randomly poke an unknown block. Used when unsolved and have nothing to do.
Unsetting `really-random' picks a block surrounded by fewest smaller numbers at random, and is recommended."
  (let ((n (count -2 probe-field)))
    (when (plusp n)
      (if (not really-random)           ; pick a good spot
          (flet ((random-range (pos-score)
                   "randomly chooses a lowest score position."
                   (let* ((sc (cdr (car pos-score)))
                          (p (delete-if-not '(lambda(x)(= x sc))
                                            pos-score :key 'cdr)))
                     (car (nth (random (length p)) p))))
                 (score(pos) "Accumulates positive numbers in the neighborhood."
                   (reduce '+ (delete-if-not 'plusp (neighbors pos) :key 'cdr)
                           :key 'cdr)))
            (act-on
             (random-range (sort        ; in descending order
                            (loop with var = nil for x below (length probe-field)
                              for pos = '(0 0) then (reverse-index x)
                              when (= (aref probe-field x) -2) do
                              (setq var (acons pos (score pos) var)) ; (pos . score)
                              finally (return var))
                            '(lambda(x y)(<= (cdr x) (cdr y)))))
             'stomp))
        (loop with k = (random n) and u-indx = -1 and arr-indx = -1
          while (< u-indx k) do         ; just randomly pick one
          (when (= (aref probe-field (incf arr-indx)) -2) (incf u-indx))
          finally (act-on (reverse-index arr-indx) 'stomp))))))
(defun flag-dec-neighbor-numbers(pos)         ; side-effect: probe-field
  "Flag position and decrement its neighboring numbers. Invoked when one of x's neighbors has a new flag."
  (when (= (aref probe-field (index pos)) -2)
    (act-on pos 'flag)                  ; propogate by decrement block numbers\ 
    (let ((index (index pos)))
      (when index
        (let ((nb (delete-if-not 'plusp (neighbors pos) :key 'cdr)))
          (mapcar
           '(lambda(x)(decf (aref probe-field (index (car x)))))
           nb))))))
(defun scan-reduce()                    ;side-effect: probe-field, game-ended
  "Scans for unknown blocks with numbers around. Core algorithm resides herein."
  (loop for vec-index below (length probe-field) do
    (unless (minusp (aref probe-field vec-index)) ; For each unknown/blank neighbor
      (let* ((num (aref probe-field vec-index)) ; of a numbered block,
             (nb (delete-if-not 'minusp (neighbors (reverse-index vec-index))
                                :key 'cdr))
             (nb-len (length nb)))      ; # neighboring flagged/unknown blocks>=num
        (cond
         ((and (zerop num) (plusp (count -2 nb :key 'cdr))) ; if blank block\ 
          (mapcar                       ; surrounded by one or more unknown blocks,
           '(lambda(x)(act-on (car x) 'stomp)) ; then stomp on all unknown neighbors
           (delete-if-not '(lambda(x)(= x -2)) nb :key 'cdr))
          (setq last-step 'stomp)
          (return))
         ((and (plusp num) (= nb-len num) ; elseif all remainings need to be flagged,
               (plusp (count -2 nb :key 'cdr)))
          (setf num 0)                  ; this block can now be treated as blank;
          (mapcar                  ; flag all surrounding unknowns;
           '(lambda(x)                  ; propogate by decrement block numbers\ 
              (flag-dec-neighbor-numbers (car x))) ; around new flags.
           (delete-if-not '(lambda(x)(= x -2)) nb :key 'cdr))
          (setq last-step 'flag)
          (return)))))                  ; otherwise skip.
    finally (setq last-step 'random)(random-poke)))             ; Nothing to be done for all unknown blocks: poke a random block

(defun xmine-solve()                    ; top-level module
  (interactive)
  (xmine-field-create)
  (setq probe-field (make-vector (* xmine-width xmine-height) -2)
        game-ended nil)
  (loop with step = 0 until
    (or (and (plusp max-step) (>= step max-step)) game-ended) do
    (message "S%d" (incf step))
    (scan-reduce)(redraw-device)
    finally
    (message
     "%s" (if (xmine-game-solved-p)
              (format "Solved in %d steps." step)
            (format "Dead within %d steps. Last step is %s"
                    step
                    (case last-step
                      (flag "Cannot be flag??")
                      (stomp "Stomp prudently")
                      (random "Random shoot")))))))

;; Local Variables:
;; eval: (eval-buffer (current-buffer))
;; End:

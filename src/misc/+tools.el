;;; +tools.el --- misc tools                         -*- lexical-binding: t; -*-

;; Copyright (C) 2025  lispcat

;; Author: lispcat <187922791+lispcat@users.noreply.github.com>
;; Keywords: local

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(defun +benchmark-lambdas (f1 f2 &optional n)
  (let* ((n (or n 10000))
         (time1 (car (benchmark-run n (funcall f1))))
         (time2 (car (benchmark-run n (funcall f2))))
         (less (if (< time1 time2) "f1" "f2"))
         (symbol (if (< time1 time2) "<" ">"))
         (difference (if (> time1 time2)
                         (- time1 time2)
                       (- time2 time1))))
    (message "f1: %.6f %s f2: %.6f (%s faster by %.6f)"
             time1
             symbol
             time2
             less
             difference)))

;;; -- end --------------------------------------------------------------------

(provide '+tools)
;;; +tools.el ends here

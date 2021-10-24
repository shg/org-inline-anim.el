;;; org-inline-anim.el --- Animate inline GIF or PNG animation -*- lexical-binding: t -*-

;; Copyright (C) 2021 Shigeaki Nishina

;; Author: Shigeaki Nishina
;; Maintainer: Shigeaki Nishina
;; Created: October 24, 2021
;; URL: https://github.com/shg/org-inline-anim.el
;; Package-Requires: ((emacs "25.1") (org "9.4"))
;; Version: 0.1
;; Keywords: org, outlines, hypermedia

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see https://www.gnu.org/licenses/.

;;; Commentary:

;; Animate inline GIF or PNG animation.

;;; Usage:

;;

;;; Code:

(require 'org)
(require 'org-element)

(defconst org-inline-anim-key (kbd "C-c C-x m"))

(defun org-inline-anim--first-image-overlay (overlays)
  "Return first image overlay in OVERLAYS that has display property."
  (let ((ov nil))
    (while (and overlays (not ov))
      (if (overlay-get (car overlays) 'display)
	  (setq ov (car overlays)))
      (setq overlays (cdr overlays)))
    ov))

(defun org-inline-anim--get-image-overlay-at-point ()
  "Return image overlay at point if there's an image overlay or nil if not."
  (org-inline-anim--first-image-overlay (overlays-at (point))))

(defun org-inline-anim--get-image-overlay-in-result ()
  "Return image overlay if POS is in a result paragraph."
  (let ((element (org-element-at-point)))
    (if (and (eq (org-element-type element) 'paragraph)
	     (org-element-property :results element))
	(let* ((beg (org-element-property :contents-begin element))
	       (end (org-element-property :contents-end element))
	       (overlays (overlays-in beg end)))
	  (org-inline-anim--first-image-overlay overlays)))))

(defun org-inline-anim--get-image-overlay-in-result-of-this ()
  "Return image overlay of the result of the current source code block."
  (let ((element (org-element-at-point)))
    (if (eq (org-element-type element) 'src-block)
	(let ((rebeg (org-babel-where-is-src-block-result)))
	  (if rebeg
	      (save-excursion
		(goto-char rebeg)
		(org-inline-anim--get-image-overlay-in-result)))))))

(defun org-inline-anim-animate ()
  "Animate graphics at the current pos or in the result block of the current source block."
  (interactive)
  (save-excursion
    (let* ((ov (let ((element (org-element-at-point)))
		 (if (eq (org-element-type element) 'src-block)
		     (let ((rebeg (org-babel-where-is-src-block-result)))
		       (if rebeg
			   (save-excursion
			     (goto-char rebeg)
			     (org-inline-anim--get-image-overlay-in-result))))
		   (if (and (eq (org-element-type element) 'paragraph)
			    (org-element-property :results element))
		       (let* ((beg (org-element-property :contents-begin element))
			      (end (org-element-property :contents-end element))
			      (overlays (overlays-in beg end)))
			 (org-inline-anim--first-image-overlay overlays))
		     (org-inline-anim--get-image-overlay-at-point)))))
	   (disp (overlay-get ov 'display)))
      (when (image-multi-frame-p disp)
	(image-animate disp)))))

;;;###autoload
(define-minor-mode org-inline-anim-mode
  "Inline anim minor mode"
  nil "" nil
  (cond
   (org-inline-anim-mode
    (define-key org-mode-map org-inline-anim-key #'org-inline-anim-animate))
   (t
    (define-key org-mode-map org-inline-anim-key nil))))

(provide 'org-inline-anim)

;;; org-inline-anim.el ends here

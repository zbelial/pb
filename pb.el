;; pb.el --- Combine two buffers and treat them as one.  -*- lexical-binding: t; -*-

;; Filename: pb.el
;; Description: Combine two buffers and treat them as one.
;; Author:  zbelial <zjyzhaojiyang@gmail.com>
;; Maintainer:  zbelial <zjyzhaojiyang@gmail.com>
;; Copyright (C) 2020, zbelial, all rights reserved.
;; Created: 2020-09-21 13:08:12
;; Version: 0.1
;; URL: https://github.com/zbelial/pb.el
;; Keywords:
;; Compatibility: GNU Emacs 27.0.50
;;
;; Features that might be required by this library:
;;
;; Please check README
;;

;;; This file is NOT part of GNU Emacs

;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Variable and helpers

(defvar-local mainp nil
  "Non-nil if this is a first buffer.")

(defvar-local followerp nil
  "Non-nil if this is a secondary buffer.")

(defvar-local main-buffer-name nil
  "The first buffer name.")

(defvar-local follower-buffer-name nil
  "The secondary buffer name.")

(defvar-local window-layout nil
  "How to display the paird buffers, `vertical' or `horizontal'")

(defvar-local follower-buffer-ratio 0.5
  "The proportion of the second buffer's height or width.")

(defun pb-pair-buffers ()
  "Pair two buffers together."
  (interactive)
  (let* ((window (get-buffer-window (current-buffer)))
         (main (read-buffer "Main buffer: "))
         (follower (read-buffer "Follower buffer: "))
         w2 layout ratio wsize)

    (when (or (pb-paired-buffer? main) (pb-paired-buffer? follower))
      (user-error (format "%s or %s has been paired" main follower)))

    (when (or (minibufferp main) (minibufferp follower))
      (user-error (format "minibuffer should not be paired")))
    
    (setq layout (read-string "Layout(v for vertical and h for horizontal): " "v"))

    (setq ratio (read-string "Ratio of follower buffer: " "0.5"))
    (setq ratio (string-to-number ratio))
    (when (or (< ratio 0.0) (>= ratio 1.0))
      (setq ratio 0.5))

    (when (or (equal main (buffer-name)) (equal follower (buffer-name)))
      (delete-other-windows)
      (if (string-equal layout "v")
          (setq w2 (split-window window (* (- 1 ratio) (window-body-height window t)) 'below t))
        (setq w2 (split-window window (* (- 1 ratio) (window-body-width window t)) 'right t)))
      (set-window-buffer window main)
      (set-window-buffer w2 follower)
      )

    (with-current-buffer main
      (setq mainp t)
      (setq followerp nil)
      (setq main-buffer-name main)
      (setq follower-buffer-name follower)
      (setq window-layout layout)
      (setq follower-buffer-ratio ratio)
      )

    (with-current-buffer follower
      (setq mainp nil)
      (setq followerp t)
      (setq main-buffer-name main)
      (setq follower-buffer-name follower)
      (setq window-layout layout)
      (setq follower-buffer-ratio ratio)
      )
    )
  )

(defun pb-unpair-buffers (&optional buffer)
  ""
  (interactive)
  (let ((buffer (or buffer (buffer-name)))
        main follower)
    (when (pb-paired-buffer? buffer)
      (setq main (pb-main-buffer buffer))
      (setq follower (pb-follower-buffer buffer))
      (with-current-buffer main
        (setq mainp nil)
        (setq followerp nil)
        )
      (with-current-buffer follower
        (setq mainp nil)
        (setq followerp nil)))))

(defun pb-paired-buffer? (&optional buffer)
  ""
  (let ((buffer (or buffer (buffer-name))))
    (or mainp followerp)))


(defun pb-main-buffer (&optional buffer)
  ""
  (when (pb-paired-buffer? buffer)
    (if mainp
        (buffer-name)
      main-buffer-name)))

(defun pb-follower-buffer (&optional buffer)
  ""
  (when (pb-paired-buffer? buffer)
    (if followerp
        (buffer-name)
      follower-buffer-name)))

(defun pb-sync-window (_)
  ""
  (let* (
        (buffer (current-buffer))
        (window (get-buffer-window buffer))
        main-buffer follower-buffer w2 layout)
    (when (and t (pb-paired-buffer? buffer))
      (setq main-buffer (pb-main-buffer))
      (setq follower-buffer (pb-follower-buffer))
      (setq layout window-layout)
      (setq ratio follower-buffer-ratio)

      (dolist (w (window-list))
        (when (not (or (string-equal (buffer-name (window-buffer w)) "*Help*")
                       (minibufferp (window-buffer w))
                       (window-dedicated-p w)
                       (eq w window)))
          (delete-window w)))
      
      (if (string-equal layout "v")
          (setq w2 (split-window window (* (- 1 ratio) (window-body-height window t)) 'below t))
        (setq w2 (split-window window (* (- 1 ratio) (window-body-width window t)) 'right t)))

      (set-window-buffer window main-buffer)
      (set-window-buffer w2 follower-buffer)
      (if (eq (window-buffer window) buffer)
          (select-window window)
        (select-window w2)))

    (when (and t
               (not (pb-paired-buffer? buffer))
               )
      (let (main-window follower-window)
        (dolist (w (window-list))
          (with-current-buffer (window-buffer w)
            (when mainp
              (setq main-window w))
            (when followerp
              (setq follower-window w))))
        (when (not (and main-window follower-window))
          (when main-window
            (delete-window main-window))
          (when follower-window
            (delete-window follower-window)))))))

(add-hook 'window-buffer-change-functions #'pb-sync-window)
;; (remove-hook 'window-buffer-change-functions #'pb-sync-window)

(add-hook 'kill-buffer-hook #'pb-unpair-buffers)
;; (remove-hook 'kill-buffer-hook #'pb-unpair-buffers)

(provide 'pb)


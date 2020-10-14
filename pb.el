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
  "The main buffer name.")

(defvar-local follower-buffer-name nil
  "The follower buffer name.")

(defvar-local window-layout nil
  "How to display the paird buffers, v for vertical or h for horizontal")

(defvar-local follower-buffer-ratio 0.5
  "The proportion of the follower buffer's height or width.")

(defvar-local main-buffers nil
  "The main buffers a follower buffer attached to.")

(defvar pb--sync-window-enabled t)

(defun pb-pair-buffers ()
  "Pair two buffers together."
  (interactive)
  (let* ((window (get-buffer-window (current-buffer)))
         (main (read-buffer "Main buffer: "))
         (follower (read-buffer "Follower buffer: " nil nil #'(lambda (buffer) (not (equal buffer main)))))
         w2 layout ratio wsize)

    (when (or (pb--main-buffer? main) (pb--main-buffer? follower))
      (user-error (format "%s or %s has been paired" main follower)))

    (when (or (minibufferp main) (minibufferp follower))
      (user-error (format "minibuffer should not be paired")))
    
    (when (equal main follower)
      (user-error (format "main and follower should not be the same.")))

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
      (add-to-list 'main-buffers main)
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
  ;; (message "pb-unpair-buffers %S" buffer)
  (let ((buffer (or buffer (buffer-name)))
        main follower mains)
    (setq pb--sync-window-enabled nil)
    (ignore-errors
      (when (pb--main-buffer? buffer)
        (setq main buffer)
        (setq follower (pb--follower-buffer buffer))
        (with-current-buffer main
          (setq mainp nil)
          (setq followerp nil)
          )
        (with-current-buffer follower
          (setq mainp nil)
          (setq main-buffers (delete main main-buffers))
          (when (= (length main-buffers) 0)
            (setq followerp nil))
          ))
      (when (pb--follower-buffer? buffer)
        (dolist (b (buffer-list))
            (with-current-buffer b
              (when (and mainp (equal follower-buffer-name buffer))
                (setq mainp nil)
                (setq followerp nil))))))
    (setq pb--sync-window-enabled t)))

(defun pb-change-ratio ()
  (interactive)
  (let ((buffer (buffer-name))
        main follower ratio)
    (if (pb--main-buffer? buffer)
        (progn
          (setq ratio (read-string "New ratio of follower buffer: " (number-to-string (pb--follower-ratio buffer))))
          (setq ratio (string-to-number ratio))
          (when (or (< ratio 0.0) (>= ratio 1.0))
            (setq ratio 0.5))

          (with-current-buffer buffer
            (setq follower-buffer-ratio ratio)
            )
          (pb--sync-window))
      (user-error "Current buffer is not a main buffer."))))

(defun pb--paired-buffer? (&optional buffer)
  ""
  (let ((buffer (or buffer (buffer-name))))
    (with-current-buffer buffer
      (or mainp followerp))))

(defun pb--main-buffer? (buffer)
  (with-current-buffer buffer
    mainp))

(defun pb--follower-buffer? (buffer)
  (with-current-buffer buffer
    followerp))

(defun pb--main-buffer (buffer)
  "`buffer' must be a follower buffer."
  (with-current-buffer buffer
    (nth 0 main-buffers)))

(defun pb--follower-buffer (buffer)
  "`buffer' must be a main buffer."
  (with-current-buffer buffer
    follower-buffer-name))

(defun pb--follower-ratio (buffer)
  "`buffer' must be a main buffer."
  follower-buffer-ratio
  )

(defun pb--sync-window (&optional arg)
  ""
  (let* (
         (buffer (current-buffer))
         (window (get-buffer-window buffer))
         main-buffer follower-buffer w2 layout)
    (when pb--sync-window-enabled
      (when (and t (pb--main-buffer? buffer))
        (setq main-buffer buffer)
        (setq follower-buffer (pb--follower-buffer main-buffer))
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

      ;; (when (and t
      ;;            (not (pb--paired-buffer? buffer))
      ;;            )
      ;;   (let (main-window follower-window)
      ;;     (dolist (w (window-list))
      ;;       (with-current-buffer (window-buffer w)
      ;;         (when mainp
      ;;           (setq main-window w))
      ;;         (when followerp
      ;;           (setq follower-window w))))
      ;;     (when (not (and main-window follower-window))
      ;;       (when main-window
      ;;         (delete-window main-window))
      ;;       (when follower-window
      ;;         (delete-window follower-window)))))
      )))

(add-hook 'window-buffer-change-functions #'pb--sync-window)
;; (remove-hook 'window-buffer-change-functions #'pb--sync-window)

(add-hook 'kill-buffer-hook #'pb-unpair-buffers)
;; (remove-hook 'kill-buffer-hook #'pb-unpair-buffers)

(defun pb-mode-line()
  (let ((line ""))
    (with-current-buffer (current-buffer)
      (when (pb--main-buffer? (current-buffer))
        (setq line " [M]"))
      (when (pb--follower-buffer? (current-buffer))
        (setq line (format " [F:%d]" (length main-buffers)))))
    line))

(defvar mode-line-pb-info '(:eval (format "%s" (pb-mode-line))))
(put 'mode-line-pb-info 'risky-local-variable t)

(provide 'pb)


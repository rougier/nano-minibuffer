;;; nano-minibuffer.el --- N Λ N O Minibuffer -*- lexical-binding: t -*-

;; Copyright (C) 2022 Nicolas P. Rougier

;; Maintainer: Nicolas P. Rougier <Nicolas.Rougier@inria.fr>
;; URL: https://github.com/rougier/nano-minibuffer
;; Version: 0.1
;; Package-Requires: ((emacs "27.1") (mini-frame))
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;;  NΛNO minibuffer is essentially a configuration of the mini-frame
;;  by Andrii Kolomoiets
;;  (https://github.com/muffinmad/emacs-mini-frame). It works by
;;  adding an overlay header line on top of the buffer and compute the
;;  size and position quite precisely.
;;
;;; NEWS:
;;
;; Version 0.1
;; - First version
;;
;;; Code
(require 'mini-frame)

(defgroup nano-minibuffer nil
  "N Λ N O Minibuffer"
  :group 'nano)

(defcustom nano-minibuffer-position 'top
  "Minibuffer position, one of 'top or 'bottom"
  :type '(choice (const :tag "Top"    top)
                 (const :tag "Bottom" bottom))
  :group 'nano-minibuffer)

(defface nano-minibuffer-face
  '((t (:inherit default)))
  "Face for the minibuffer."
  :group 'nano-minibuffer)

(defface nano-minibuffer-header-face
  '((t (:inherit mode-line)))
  "Face for the minibuffer header."
  :group 'nano-minibuffer)

(defun nano-minibuffer-header ()
  "Header line function for the minibuffer"

  (let* ((depth (minibuffer-depth))
         (left (concat " ☰ "
                       (if (> depth 1)
                           (format "Minibuffer (%d)" depth)
                         "Minibuffer")))
         (right "C-g: abort")
         (spacer (propertize " " 
               'display `(space :align-to (- right
                                          (-1 . right-margin)
                                         ,(- (length right) -1))))))
    (concat left spacer right "\n")))

(defun nano-minibuffer-setup ()
  "Install a header line in the minibuffer via an overlay (and a hook)"
  
  (set-window-margins nil 0 0)
  (set-fringe-style '(0 . 0))
  (cursor-intangible-mode t)
  (face-remap-add-relative 'default
                           :inherit 'nano-minibuffer-face)
    (let* ((overlay (make-overlay (+ (point-min) 0) (+ (point-min) 0)))
         (inhibit-read-only t))
    (save-excursion
      (goto-char (point-min))
      (insert (propertize
               (concat (propertize (nano-minibuffer-header)
                                   'face 'nano-minibuffer-header-face)
                       (propertize "\n" 'face `(:height 0.33))
                       (propertize " "))
               'cursor-intangible t
               'read-only t
               'field t
               'rear-nonsticky t
               'front-sticky t)))))

(defun nano-minibuffer--frame-parameters ()
  "Compute minibuffer frame size and position."

  ;; Quite precise computation to align the minibuffer and the
  ;; modeline when they are both at top position
  (let* ((edges (window-pixel-edges)) ;; (left top right bottom)
         (body-edges (window-body-pixel-edges)) ;; (left top right bottom)
         (left (nth 0 edges)) ;; Take margins into account
         (top (nth 1 edges)) ;; Drop header line
         (right (nth 2 edges)) ;; Take margins into account
         (bottom (nth 3 body-edges)) ;; Drop header line
         (left (if (eq left-fringe-width 0)
                   left
                 (- left (frame-parameter nil 'left-fringe))))
         (right (nth 2 edges))
         (right (if (eq right-fringe-width 0)
                    right
                  (+ right (frame-parameter nil 'right-fringe))))
         (border 1)
         (width (- right left (* 0 border)))

         ;; Window divider mode
         (width (- width (if (and (bound-and-true-p window-divider-mode)
                                  (or (eq window-divider-default-places 'right-only)
                                      (eq window-divider-default-places t))
                                  (window-in-direction 'right (selected-window)))
                             window-divider-default-right-width
                           0)))
         (y (- top border)))

    (append `((left-fringe . 0)
              (right-fringe . 0)
              (user-position . t) 
              (foreground-color . ,(face-foreground 'nano-minibuffer-face nil 'default))
              (background-color . ,(face-background 'nano-minibuffer-face nil 'default)))
            (cond ((and (eq nano-minibuffer-position 'bottom))
                   `((top . -1)
                     (left . 0)
                     (width . 1.0)
                     (child-frame-border-width . 0)
                     (internal-border-width . 0)))
                  (t
                   `((left . ,(- left border))
                     (top . ,y)
                     (width . (text-pixels . ,width))
                     (child-frame-border-width . ,border)
                     (internal-border-width . ,border)))))))


(defun nano-minibuffer-mode--activate ()
  "Activate nano-minibuffer minor mode"
  
  ;; Mini-frame setup
  (set-face-background 'child-frame-border (face-foreground 'nano-faded))
  (setq mini-frame-default-height 3)
  (setq mini-frame-create-lazy t)
  (setq mini-frame-show-parameters 'nano-minibuffer--frame-parameters)
  (setq mini-frame-ignore-commands
        '("edebug-eval-expression" debugger-eval-expression))
  (setq mini-frame-internal-border-color (face-foreground 'nano-faded))
  (setq mini-frame-resize-min-height 3)
  
  ;; This does not work unless it is set via customize-variable
  (setq mini-frame-resize t)

  (add-hook 'minibuffer-setup-hook #'nano-minibuffer-setup)
  (mini-frame-mode 1))

(defun nano-minibuffer-mode--inactivate ()
  "Activate nano-minibuffer minor mode"

  (remove-hook 'minibuffer-setup-hook #'nano-minibuffer-setup)
  (mini-frame-mode -1))

;;;###autoload
(define-minor-mode nano-minibuffer-mode
  "Toggle nano-minibuffer minor mode"
  :group 'nano-minibuffer
  :global t
  :init-value nil

  ;; Toggle mode
  (if nano-minibuffer-mode
      (nano-minibuffer-mode--activate)
    (nano-minibuffer-mode--inactivate))

  ;; Run any registered hooks
  (run-hooks 'nano-minibuffer-mode-hook))

(provide 'nano-minibuffer)


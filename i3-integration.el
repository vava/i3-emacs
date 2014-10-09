;;; i3-integration.el -- using i3 IPC to integrate Emacs with i3.  -*- lexical-binding: t; -*-

;; Copyright (c) 2012, Vadim Atlygin.
;; All rights reserved.

;; Author:  Vadim Atlygin <vadim.atlygin@gmail.com>
;; Version: 0.1

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are met:

;; 1. Redistributions of source code must retain the above copyright notice, this
;;    list of conditions and the following disclaimer.
;; 2. Redistributions in binary form must reproduce the above copyright notice,
;;    this list of conditions and the following disclaimer in the documentation
;;    and/or other materials provided with the distribution.

;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
;; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;; DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
;; ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
;; (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
;; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
;; ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;; The views and conclusions contained in the software and documentation are those
;; of the authors and should not be interpreted as representing official policies,
;; either expressed or implied, of the FreeBSD Project.

;;; Commentary:
;;
;; this is a set of advises that allows Emacs to play nicely and in
;; predictable fashion with i3. Some of them are quite subjective so
;; they are disabled by default.

;;; Bugs/todo:
;; - only Emacs 24 supported

;;; Code:

(require 'i3)
(require 'cl-lib)

(defcustom i3-collect-windows-function 'i3-collect-only-visible-windows
  "Function used to select windows when used in
one-window-per-frame mode. You can choose between
i3-collect-only-visible-windows which ignores windows hidden in
stacked or tabbed containers, or i3-collect-all-windows, which
will use all of them."
  :type 'function
  :group 'i3)

(defun i3-advise-visible-frame-list-on ()
  "Turns on advising of visible-frame-list function. This has the
effect of (visible-frame-list) returning only frames that are
situated on visible workspaces. This is the default."
  (i3-switch-advice 'visible-frame-list 'after 'i3-visible-frame-list t))

(defun i3-advise-visible-frame-list-off ()
  "Turns off advising of visible-frame-list
function. (visible-frame-list) will return all frames as i3,
being a tiling wm, does not have minimized windows concept."
  (i3-switch-advice 'visible-frame-list 'after 'i3-visible-frame-list nil))

(defun i3-one-window-per-frame-mode-on ()
  "Turns on one window per frame mode. After switching it on,
emacs will not split your frames, instead it will reuse them, in
more or less sensible manner. It will not reuse frames from
invisible workspaces either and will prefer to replace special
kind of buffers or least recently used ones. Works only in Emacs 24."
  (i3-one-window-per-frame-mode t))

(defun i3-one-window-per-frame-mode-off ()
  "Turns off one window per frame mode. This is the default."
  (i3-one-window-per-frame-mode nil))

;;; Internal functions

(defun i3-one-window-per-frame-mode (turn-on)
  (i3-switch-advice 'select-frame 'before 'i3-timestamp-frame-selection turn-on)
  (if turn-on
      (unless (memq 'i3-display-buffer-use-some-frame
                    (car display-buffer-overriding-action))
       (push 'i3-display-buffer-use-some-frame
             (car display-buffer-overriding-action)))
    (setcar display-buffer-overriding-action
            (delq 'i3-display-buffer-use-some-frame
                  (car display-buffer-overriding-action)))))

(defun i3-switch-advice (function class advise turn-on)
  (if turn-on
      (ad-enable-advice function class advise)
    (ad-disable-advice function class advise))
  (ad-activate function))

;;; Advices
(defadvice visible-frame-list (after i3-visible-frame-list disable)
  (setq ad-return-value (i3-filter-visible-frame-list (i3-filter-other-display-frames ad-return-value))))

(defadvice select-frame (before i3-timestamp-frame-selection disable)
  (set-frame-parameter (ad-get-arg 0) 'i3-frame-selected-time (current-time)))

;;; i3 dependent pieces

(defun i3-filter-visible-frame-list (visible-frame-list)
  (condition-case nil
      (let ((visible-window-ids (i3-get-visible-windows-ids)))
        (i3-map-and-filter (lambda(f)
                             (when (member (string-to-number (frame-parameter f 'outer-window-id))
                                           visible-window-ids)
                               f))
                           visible-frame-list))
    (error visible-frame-list)))

(defun i3-get-visible-workspace-names ()
  (i3-map-and-filter (lambda(w) (when (i3-field-is 'visible #'eq t w)
                                  (i3-field 'name w)))
                     (i3-get-workspaces)))

(defun i3-get-visible-windows ()
  (let ((visible-workspace-names (i3-get-visible-workspace-names)))
    (i3-flatten
     (mapcar i3-collect-windows-function
             (i3-flatten (i3-map-and-filter (lambda(w)
                                              (when (i3-field-is 'name #'member visible-workspace-names w)
                                                (append (i3-field 'nodes w) nil)));convert vector to list
                                            (i3-collect-workspaces (i3-get-tree-layout))))))))

(defun i3-get-visible-windows-ids ()
  (mapcar (apply-partially #'i3-field 'window) (i3-get-visible-windows)))

(defun i3-collect-entities (checkp)
  (letrec ((collect (lambda (root)
                      (if (funcall checkp root)
                          (list root)
                        (i3-flatten (i3-map-and-filter collect
                                                       (i3-field 'nodes root)))))))
    collect))

(defalias 'i3-collect-workspaces
  (i3-collect-entities (apply-partially #'i3-field-is 'type #'equal "workspace")))

(defalias 'i3-collect-all-windows
  (i3-collect-entities (apply-partially #'i3-field 'window)))

(defun i3-collect-only-visible-windows (root)
  (if (i3-field 'window root)
      (list root)
    (let* ((folded (i3-field-is 'layout #'member '("tabbed" "stacked") root))
           (id (when folded (elt (i3-field 'focus root) 0)))
           (children (if folded
                         (list (cl-find-if (apply-partially #'i3-field-is 'id #'eq id) (i3-field 'nodes root)))
                       (i3-field 'nodes root))))
      (i3-flatten (i3-map-and-filter #'i3-collect-only-visible-windows children)))))

;;; Helper functions

(defun i3-flatten (list-of-lists)
  (apply 'append list-of-lists))

; json objects get converted to alists
(defun i3-field (symbol alist)
  (cdr (assq symbol alist)))
(defun i3-field-is (symbol pred compare alist)
  (funcall pred (i3-field symbol alist) compare))

(defun i3-map-and-filter (function list)
  (delq nil (mapcar function list)))

(defun i3-get-frame-buffer (frame)
  (car (frame-parameter frame 'buffer-list)))

(defun i3-get-frame-selected-time (frame)
  (float-time (frame-parameter frame 'i3-frame-selected-time)))

(defun i3-filter-all-but-special-buffer-frames (frames)
  (i3-map-and-filter (lambda (f) (when (not (buffer-file-name (i3-get-frame-buffer f)))
                                   f))
                     frames))

(defun i3-filter-frames-by-buffer (buffer frames)
  (i3-map-and-filter (lambda(f)
                       (when (memq buffer (frame-parameter f 'buffer-list))
                         f))
                     frames))

(defun i3-sort-frames-by-buffer (buffer frames)
  (sort frames
        (lambda(f1 f2) (< (cl-position buffer (frame-parameter f1 'buffer-list))
                          (cl-position buffer (frame-parameter f2 'buffer-list))))))

(defun i3-sort-frames-by-selected-time (frames)
  (sort frames (lambda (f1 f2) (< (i3-get-frame-selected-time f1) (i3-get-frame-selected-time f2)))))


(defun i3-get-frame-showing-buffer (buffer frames)
  (cl-find-if (lambda (f) (eq (car (frame-parameter f 'buffer-list)) buffer))
              frames))

(defun i3-get-frame-most-recently-displayed-buffer (buffer frames)
  (car (i3-sort-frames-by-buffer buffer (i3-filter-frames-by-buffer buffer frames))))

(defun i3-get-frame-least-recently-used (frames)
  (car (i3-sort-frames-by-selected-time frames)))


(defun i3-filter-other-display-frames (frames)
  (let ((selected-display (frame-parameter (selected-frame) 'display)))
    (i3-map-and-filter (lambda(f)
                         (when (and (not (frame-parameter f 'tty))
                                    (eq (frame-parameter f 'display) selected-display))
                           f))
                       frames)))

(defun i3-get-popup-frame-for-buffer (buffer)
  (let* ((frames (visible-frame-list))
         (frames-no-selected-frame (remove (selected-frame) frames))
         (special-frames (i3-filter-all-but-special-buffer-frames frames-no-selected-frame)))
    (or (i3-get-frame-showing-buffer buffer frames)
        (i3-get-frame-most-recently-displayed-buffer buffer special-frames)
        (i3-get-frame-least-recently-used special-frames)
        (i3-get-frame-most-recently-displayed-buffer buffer frames-no-selected-frame)
        (i3-get-frame-least-recently-used frames-no-selected-frame)
        (car frames))))

(defun i3-display-buffer-use-some-frame (buffer alist)
  (when (and (display-graphic-p)
             (not (member (buffer-name buffer) '("*Completions*"))))
    (let* ((frame (i3-get-popup-frame-for-buffer buffer))
           (window (frame-selected-window frame)))
      (window--display-buffer buffer window 'reuse))))

;;; Set defaults
(i3-advise-visible-frame-list-on)

(provide 'i3-integration)



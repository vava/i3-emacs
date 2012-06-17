;;; i3.el -- bindings for i3 WM IPC.

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
;; i3 is a set of bindings to i3 WM IPC, allowing you to send commands
;; and query for information from underlying i3 process.

;;; Bugs/todo:
;; - subscriptions are not supported
;; - no async api

;;; Code:

(require 'bindat)
(require 'json)

(defconst i3-process-name "*i3-process*"
  "Name of the buffer associated with i3 IPC process")

(defun i3-get-tree-layout ()
  "Returns a layout tree. See i3 wm IPC docs for details."
  (i3-command 4))

(defun i3-get-workspaces ()
  "Returns a list of current workspaces. See i3 wm IPC docs for details."
  (i3-command 1))

(defun i3-command (command &optional payload)
  "Sends command to i3 and returns the response. See i3 wm IPC docs for details."
  (let ((proc (i3-get-or-make-client))
        (length (length payload)))
    (process-put proc 'response nil)
    (process-put proc 'partial-response nil)
    (process-send-string proc (concat (bindat-pack i3-msg-spec `((:magic . "i3-ipc")
                                                                 (:length . ,length)
                                                                 (:command . ,command)))
                                      payload))
    (while (not (process-get proc 'response))
      (accept-process-output proc))
    (let ((response (process-get proc 'response)))
      (process-put proc 'response nil)
      (json-read-from-string response))))

(defun i3-get-or-make-client ()
  "Creates or returns a current process connected to i3 wm IPC."
  (unless (boundp 'i3-client)
    (setq i3-client (make-network-process :name i3-process-name :buffer (get-buffer-create i3-process-name)
                                          :coding '(raw-text-unix . raw-text-unix)
                                          :family 'local
                                          :filter 'i3-response-filter
                                          :sentinel 'i3-sentinel
                                          :service (i3-chomp (shell-command-to-string "i3 --get-socketpath")))))
  i3-client)

;;; Internal functions and data structures.

(defvar i3-msg-spec
  '((:magic str 6)
    (:length u32r)
    (:command u32r)))

(defun i3-response-filter (proc resp)
  (if (not (process-get proc 'partial-response))
      (let ((length (cdr (assq :length (bindat-unpack i3-msg-spec resp))))
            (payload (substring resp 14)))
        (process-put proc 'partial-response-length length)
        (process-put proc 'partial-response payload))
    (process-put proc 'partial-response (concat (process-get proc 'partial-response) resp)))
  (when (>= (length (process-get proc 'partial-response)) (process-get proc 'partial-response-length))
    (process-put proc 'response (process-get proc 'partial-response))
    (process-put proc 'partial-response nil)))

(defun i3-sentinel (proc event)
  (setq i3-client nil))

(defun i3-chomp (string)
  (replace-regexp-in-string "\n$" "" string))

(provide 'i3)

;;; -*- lexical-binding: t -*-

;; Copyright (C) 2016 Yuki Inoue <inouetakahiroki _at_ gmail.com>

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


(require 'json)
(require 'tabulated-list)
(require 'dash)
(require 'dash-functional)


(defun aws--shell-command-to-string (&rest args)
  (let ((cmd (funcall 'combine-and-quote-strings args)))
    (message cmd)
    (shell-command-to-string cmd)))

(defun aws-raw-instances ()
  (interactive)
  (json-read-from-string
   (aws--shell-command-to-string "aws" "ec2" "describe-instances")))

; (setq aws-instances-test (aws-raw-instances))


(defun aws-convert-raw-instances (raw-instances)

  (->>
   raw-instances
   (funcall (-compose 'cdr (apply-partially 'assoc 'Reservations)))
   ((lambda (v) (append v nil)))
   (mapcar (-compose 'cdr (apply-partially 'assoc 'Instances)))
   (mapcar (lambda (v) (append v nil)))
   (-mapcat 'identity)
   (mapcar 'aws-instance-fix-tag)
   (mapcar (lambda (instance)
     (list (cdr (assoc 'InstanceId instance))
           (vector (cdr (assoc 'InstanceId instance))
                   (cdr (assoc "Name" (cdr (assoc 'Tags instance))))
                   (cdr (assoc 'Name (cdr (assoc 'State instance))))
                   (prin1-to-string (cdr (assoc 'PrivateIpAddress instance)) t)
                   (or  "..." (prin1-to-string instance))
                   ))))
   ))

(defun aws-instance-fix-tag (instance)
  (mapcar
   (lambda (entry)
     (if (not (equal (car entry) 'Tags))
         entry
       (cons 'Tags
             (mapcar (lambda (kvassoc)
                       (cons (cdr (assoc 'Key kvassoc))
                             (cdr (assoc 'Value kvassoc))))
                     (cdr entry)))))
   instance))


(defvar aws-instances-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "I" 'aws-instances-inspect-popup)
    map)
  "Keymap for `aws-instances-mode'.")

(define-derived-mode aws-instances-mode tabulated-list-mode "Containers Menu"
  "Major mode for handling a list of docker containers."

  (define-key tabulated-list-mode-map "S" 'aws-instances-stop-popup)
  (define-key tabulated-list-mode-map "T" 'aws-instances-terminate-popup)

  (setq tabulated-list-format
        '[("Repository" 10 nil)
          ("Name" 30 nil)
          ("Status" 10 nil)
          ("IP" 15 nil)
          ("Settings" 20 nil)])
  (setq tabulated-list-padding 2)
  (add-hook 'tabulated-list-revert-hook 'aws-instances-refresh nil t)
  (tabulated-list-init-header)
  (tablist-minor-mode))


(defun aws-instances-refresh ()
  "Refresh elasticsearch snapshots."

  (setq tabulated-list-entries
        (aws-convert-raw-instances
         (aws-raw-instances))))

(defun aws-select-if-empty (&optional arg)
  "Select current row is selection is empty."
  (save-excursion
    (when (null (tablist-get-marked-items))
      (tablist-put-mark))))

(defmacro aws-define-popup (name doc &rest args)
  "Wrapper around `aws-utils-define-popup'."
  `(progn
     (magit-define-popup ,name ,doc ,@args)
     (add-function :before (symbol-function ',name) #'aws-select-if-empty)))

(aws-define-popup
 aws-instances-inspect-popup
 'aws-instances-popups
 :actions  '((?I "Inspect" aws-inspect-selection)))

(aws-define-popup
 aws-instances-stop-popup
 'aws-instances-popups
 :actions  '((?S "Stop" aws-ec2-stop-selection)))

(aws-define-popup
 aws-instances-terminate-popup
 'aws-instances-popups
 :actions  '((?T "Terminate" aws-ec2-terminate-selection)))


(defun aws-ec2-command-on-selection (command)
  (apply 'aws--shell-command-to-string
         "aws" "ec2" command "--instance-ids" (docker-utils-get-marked-items-ids)))

(defun aws-ec2-stop-selection ()
  (interactive)
  (aws-ec2-command-on-selection "stop-instances"))

(defun aws-ec2-terminate-selection ()
  (interactive)
  (aws-ec2-command-on-selection "terminate-instances"))

(defun aws-inspect-selection ()
  (interactive)
  (let ((result (->>
                 (tablist-get-marked-items)
                 (mapcar 'car)
                 (append '("aws" "ec2" "describe-instances" "--instance-ids"))
                 (combine-and-quote-strings)
                 (shell-command-to-string)))
        (buffer (get-buffer-create "*aws result*")))

    (with-current-buffer buffer
      (erase-buffer)
      (goto-char (point-max))
      (insert result))

    (display-buffer buffer)))

(defun aws-instances ()
  "List Elasticsearch snapshots."
  (interactive)
  (pop-to-buffer "*aws-instances*")
  (tabulated-list-init-header)
  (aws-instances-mode)
  (tabulated-list-revert))

(defvar aws-global-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map "c" 'aws-instances)
    map))

(provide 'aws-ec2)

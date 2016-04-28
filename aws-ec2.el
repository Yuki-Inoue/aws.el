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


(defun aws-raw-instances ()
  (interactive)
  (json-read-from-string
   (shell-command-to-string
    (combine-and-quote-strings
     (list "aws" "ec2" "describe-instances")))))

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
                   (prin1-to-string instance)
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

  (setq tabulated-list-format
        '[("Repository" 10 nil)
          ("Name" 30 nil)
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
 :actions  '((?I "Inspect" aws-inspect-selection))
 )




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

(provide 'aws-ec2)

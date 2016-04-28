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
                   (prin1-to-string instance))
           )))
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

(defun aws-instances ()
  "List Elasticsearch snapshots."
  (interactive)
  (pop-to-buffer "*aws-instances*")
  (aws-instances-refresh)
  (tabulated-list-init-header)
  (aws-instances-mode)
  (tabulated-list-revert))

(provide 'aws-ec2)

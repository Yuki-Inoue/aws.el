;;; aws-ec2.el --- Manage AWS EC2 instances -*- lexical-binding: t -*-

;; Copyright (C) 2016 Yuki Inoue

;; Author: Yuki Inoue <inouetakahiroki _at_ gmail.com>
;; URL: AWS, Amazon Web Service
;; Version: 0.0.1
;; Package-Requires: ((emacs "24.4") (dash "2.12.1") (dash-functional "1.2.0") (magit-popup "2.6.0") (tablist "0.70"))

;; This file is NOT part of GNU Emacs.

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

;;; Commentary:

;; Manipulate AWS ec2 from emacs.

;;; Code:

(require 'json)
(require 'tablist)
(require 'dash)
(require 'dash-functional)
(require 'magit-popup)

(defun aws-autoscale-get-raw-instances ()
  (interactive)
  (json-read-from-string
   (aws--shell-command-to-string "aws" "autoscaling" "describe-auto-scaling-groups")))

(defun aws-autoscale-convert-raw-instances (raw-instances)
  (->>
   raw-instances
   (funcall (apply-partially 'assoc-default 'AutoScalingGroups))
   (funcall (apply-partially (-flip 'append) nil))
   (mapcar (lambda (autoscaling-group)
     (list (assoc-default 'AutoScalingGroupName autoscaling-group)
           (vector (assoc-default 'AutoScalingGroupName autoscaling-group)
                   (or  (prin1-to-string autoscaling-group) "...")
                   ))))
   ))

; (aws-autoscale-convert-raw-instances autoscale-info)

(define-derived-mode aws-autoscale-mode tabulated-list-mode "Containers Menu"
  "Major mode for handling a list of docker containers."

  (setq tabulated-list-format
        '[("Name" 30 nil)
          ("Settings" 20 nil)])
  (setq tabulated-list-padding 2)
  (add-hook 'tabulated-list-revert-hook 'aws-autoscale-refresh nil t)
  (tabulated-list-init-header)
  (tablist-minor-mode))


(defun aws-autoscale-refresh ()
  "Refresh elasticsearch snapshots."

  (setq tabulated-list-entries
        (aws-autoscale-convert-raw-instances
         (aws-autoscale-get-raw-instances))))


;;;###autoload
(defun aws-autoscales ()
  "List aws autoscaling groups using aws-cli. (The `aws` command)."
  (interactive)
  (pop-to-buffer "*aws-autoscaling-groups*")
  (aws-autoscale-mode)
  (tabulated-list-revert))

(provide 'aws-ec2-autoscale)

;;; aws-ec2.el ends here

;;; network-watch.el --- Support for intermitent network connectivity
;; Copyright (C) 2010-2017 Juan Amiguet Vercher

;; Author: Juan Amiguet Vercher <jamiguet@gmail.com>
;; Created: 17 Oct 2017
;; Version: 1.0
;; Package-Version: 1.0
;; Keywords: unix tools hardware lisp
;; Homepage: https://github.com/jamiguet/network-watch
;; Package-Requires: ((emacs "24.3"))

;; This file is not part of GNU Emacs.

;; This file is free software...

;;; Commentary:

;; # network-watch
;;
;; Emacs global minormode for handling intermitent network access.  It provides
;; two hooks *network-watch-up-hook* and *network-watch-down-hook* every
;; *network-watch-time-interval* the network status is checked if
;; nothing changed since the previous time no hooks are invoked.  If
;; access to a network is possible then the *network-watch-up-hook* is run.
;; Conversely when network connectivity is lost the *network-watch-down-hook*
;; is run.
;;
;;
;; ## Setup
;;
;; Install via elpa then place *(require 'network-watch)* in your *.emacs* file.
;; You can also adapt the *network-watch-update-time-interval* to your liking.
;;
;;
;; ## Utility function
;;
;; Besides the two hooks the library also provides a *network-watch-active-p*
;; function which returns not nil when a listed interface is up.
;;
;;
;; ## Example
;;
;; In this example *gmail-notifier* is configured with the help of
;; *network-watch* it is automatically started and stopped when the network
;; 	is up or down respectively.
;;
;; 	(require 'network-watch)
;; 	(require 'gmail-notifier)
;;
;; 	(setq gmail-notifier-username "jamiguet")
;; 	(setq gmail-notifier-password ja-password)
;;
;;      (add-hook 'network-watch-up-hook 'gmail-notifier-start)
;; 	(add-hook 'network-watch-down-hook 'gmail-notifier-stop)
;;
;;

;;; Code:
(require 'cl-lib)

(defvar network-watch-up-hook)
(defvar network-watch-down-hook)
(defvar network-watch-timer)
(defvar network-watch-last-state)


;; Define base group for network information
(defgroup network nil
  "Customisation group for network availability hooks"
  :group 'Communication)


(define-minor-mode network-watch
  "Network is automatically on when there is a valid network
interface active."
  :init-value t
  :lighter network-watch-lighter
  :global t
  :require 'network-watch
  :group 'network
    (network-watch-update-lighter))


(defcustom network-watch-time-interval 120
  "Refersh reate for network status."
  :type 'integer
  :group 'network)

(defun network-watch-update-lighter()
  (setq network-watch-lighter
	(concat " N(" (if (network-watch-active-p) "+" "-") ")")))


(defun network-watch-active-p ()
  "Return nil if loopback is the only active interface"
  (remove-if #'(lambda (it) (equal (cdr it) [127 0 0 1 0] ))  (network-interface-list)))


(defun network-watch-update-system-state ()
  "Internal method update the network state variable."
  (setq network-watch-last-state (network-watch-active-p)))


(defun network-watch-update-state ()
  "Run hooks only on network status change."
  (interactive)
  (if (cl-set-exclusive-or
       (if (listp (network-watch-active-p))
	   (network-watch-active-p)
	 (list (network-watch-active-p)))
       (if (listp network-watch-last-state)
	   network-watch-last-state
	 (list network-watch-last-state)))
      (progn
	(if (network-watch-active-p) (run-hooks 'network-watch-up-hook)
	  (run-hooks 'network-watch-down-hook))
	(network-watch-update-system-state)
	))
  (setq network-watch-timer (run-with-timer network-watch-time-interval  nil 'network-watch-update-state))
  (network-watch-update-lighter))


(defun network-watch-init ()
  "Network-Watch-Initialises the inner state of the network."
  (interactive)
  (network-watch-update-system-state)
  (setq network-watch-timer (run-with-timer network-watch-time-interval  nil 'network-watch-update-state))
  (if (network-watch-active-p) (run-hooks 'network-watch-up-hook))
  (message "Network init")
  (network-watch-update-lighter))

(defun network-watch-stop()
  "Cancels the checking of the network state"
  (interactive)
  (cancel-timer network-watch-timer))

(network-watch-init)

(provide 'network-watch)
;;; network-watch.el ends here

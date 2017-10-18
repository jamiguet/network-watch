;;; ja-network.el --- Support for intermitent network connectivity
;; Copyright (C) 2010-2017 Your Name
     
;; Author: Juan Amiguet Vercher <jamiguet@gmail.com>
;; Created: 17 Oct 2017
;; Version: 1.0
;; Keywords: unix tools hardware lisp
;; Homepage: https://github.com/jamiguet/ja-network
;; Package-Requires: ((org "1.0"))

;; This file is not part of GNU Emacs.

;; This file is free software...


;;; Commentary:
;;; Adds support for intermitent netowrk connectivity.
;;; Through two hooks run when network connectivity appears and
;;; disappears.
;; 

;;; Code:
(require 'org)

(defvar network-up-hook)
(defvar network-down-hook)
(defvar machine-name)
(defvar ja-network--system-state)

;; define that we are on a specific machine
(setq machine-name (substring (eshell-command-result "hostname") 0 -1 ))

;; TODO:
;; Publish on melpa.

;; Define base group for network information
(defgroup network nil
  "Customisation group for network availability hooks"
  :group 'Emacs)


(defcustom ja-network-machine-interface-mapping ()
  "List of interfaces providing network per machine."
  :type 'sexp
  :group 'network)

(defcustom ja-network-update-time-interval 120
  "Refersh reate for network statatus."
  :type 'integer
  :group 'network)g


(defun ja-network--machine-add ()
  "Add the current machine to the machine interface mapping."
  (defvar intf-names)
  (setq intf-names ())
  (dolist (intf (network-interface-list) intf-names)
    (setq intf-names (cons (car intf) intf-names)))
  (set-variable 'ja-network-machine-interface-mapping (cons (list  machine-name intf-names ) ja-network-machine-interface-mapping) "List of machine interfaces.x "))



(defun ja-network-have-network-p ()
  "Return nil if there are no active network interfaces."
  (catch 'break
    (dolist (intf
	     (cadr (assoc machine-name ja-network-machine-interface-mapping))
	     value)
      (if (assoc intf (network-interface-list))
	  (progn (setq value t)
		 (throw 'break value))
	(setq value nil)))
    ))



(defun ja-network-update-system-state ()
  "Internal method update the network state variable."
  (setq ja-network--system-state (ja-network-have-network-p))
  )


(defun ja-network--update-network-state ()
  "Run hooks only on network status change."
  (if (org-xor (ja-network-have-network-p) ja-network--system-state)
      (progn
	(if (ja-network-have-network-p) (run-hooks 'network-up-hook)
	  (run-hooks 'network-down-hook))
	(ja-network-update-system-state)
	))
  (run-with-timer ja-network-update-time-interval  nil 'update-network-state)
  (message "Network state update")
  )


(defun ja-network--init ()
  "Initialises the inner state of the network and configures machine if neccessary."
  (if (not (assoc machine-name ja-network-machine-interface-mapping))
      (progn
	(message "Machine not pressent adding machine. Customise network group if needed.")
	(ja-network--machine-add)))
  (ja-network-update-system-state)
  (run-with-timer ja-network-update-time-interval  nil 'update-network-state)
  (if (ja-network-have-network-p) (run-hooks 'network-up-hook))
  (message "Network init")
  )


(add-hook 'after-init-hook 'ja-network--init)

(provide 'ja-network)
;;; ja-network.el ends here

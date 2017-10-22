;;; ja-network.el --- Support for intermitent network connectivity
;; Copyright (C) 2010-2017 Your Name
     
;; Author: Juan Amiguet Vercher <jamiguet@gmail.com>
;; Created: 17 Oct 2017
;; Version:
;; Package-Version: 1.0
;; Keywords: unix tools hardware lisp
;; Homepage: https://github.com/jamiguet/ja-network
;; Package-Requires: ((org "9.1") (emacs "24.3"))

;; This file is not part of GNU Emacs.

;; This file is free software...


;;; Commentary:

;; # ja-network
;; 
;; Emacs library for handling intermitent network access.  It provides
;; two hooks *network-up-hook* and *network-down-hook* every
;; *network-update-time-interval* the network status is checked if
;; nothing changed since the previous time no hooks are invoked.  If
;; access to a network is possible then the *network-up-hook* is run.
;; Conversely when network connectivity is lost the *network-down-hook*
;; is run.
;; 
;; The library is designed to support multiple machines on the same
;; customised variables setup.
;; I share all my set-up accross diefferent machines.
;; The network interfaces of any unknown machines are
;; automatically added to the customised variable
;; *machine-interface-mapping* which is part of the *network* group.
;; 
;; 
;; ## Setup
;; 
;; The first time the library is used on a computer it will add the
;; machine name and the set of all active interfaces to the
;; *machine-interface-mapping* customised variable.
;; Edit the variable
;; by hand to remove any loopback interfacess or to add the name of
;; any interfaces which are not connected.
;; 
;; You can also adapt the *network update-time-interval* to your liking.
;; The *have-network-hook* is run at the end of Emacs startup if you have
;; connectivity.
;; 
;; 
;; ## Utility function
;; 
;; Besides the two hooks the library also provides a *have-network-p*
;; function which returns not nil when a listed interface is up.
;; 
;; 
;; ## Example
;; 
;; In this example *gmail-notifier* is configured with the help of
;; *ja-network* it is automatically started and stopped when the network
;; 	is up or down respectively.
;; 
;; 	(require 'ja-network)
;; 	(require 'gmail-notifier)
;; 	
;; 	(setq gmail-notifier-username "jamiguet")
;; 	(setq gmail-notifier-password ja-password)
;; 
;;      (add-hook 'network-up-hook 'gmail-notifier-start)
;; 	g(add-hook 'network-down-hook 'gmail-notifier-stop)
;; 
;; 

;;; Code:
(require 'org)
(require 'cl-lib)

(defvar network-up-hook)
(defvar network-down-hook)
(defvar machine-name)
(defvar ja-network--system-state)

;; define that we are on a specific machine
(setq machine-name (substring (eshell-command-result "hostname") 0 -1 ))

;; TODO:
;; Convert into a global-minor-mode

;; Define base group for network information
(defgroup network nil
  "Customisation group for network availability hooks"
  :group 'Emacs)


(defcustom ja-network-machine-interface-mapping ()
  "List of interfaces providing network per machine."
  :type 'sexp
  :group 'network)

(defcustom ja-network-update-time-interval 120
  "Refersh reate for network status."
  :type 'integer
  :group 'network)


(defun ja-network--machine-add ()
  "Add the current machine to the machine interface mapping."
  (defvar intf-names)
  (setq intf-names ())
  (dolist (intf (network-interface-list) intf-names)
    (setq intf-names (cons (car intf) intf-names)))
  (customize-save-variable 'ja-network-machine-interface-mapping
		(cons (list  machine-name intf-names ) ja-network-machine-interface-mapping)
		"List of machine interfaces."))



(defun ja-network-have-network-p ()
  "Return nil if there are no active network interfaces."
  (catch 'break
    (let (value)
      (dolist (intf
	       (cadr (assoc machine-name ja-network-machine-interface-mapping)))
	(setq value (cl-find intf (cl-mapcar #'car (network-interface-list)) :test #'string= ) )
	(if value
	    (throw 'break value)))
      )))


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
  (run-with-timer ja-network-update-time-interval  nil 'ja-network--update-network-state)
  (message "Network state update")
  )


(defun ja-network--init ()
  "Initialises the inner state of the network and configures machine if neccessary."
  (if (not (assoc machine-name ja-network-machine-interface-mapping))
      (progn
	(message "Machine not pressent adding machine. Customise network group if needed.")
	(ja-network--machine-add)))
  (ja-network-update-system-state)
  (run-with-timer ja-network-update-time-interval  nil 'ja-network--update-network-state)
  (if (ja-network-have-network-p) (run-hooks 'network-up-hook))
  (message "Network init")
  )


(add-hook 'after-init-hook 'ja-network--init)

(provide 'ja-network)
;;; ja-network.el ends here

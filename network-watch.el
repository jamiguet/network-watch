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
;; two hooks *network-up-hook* and *network-down-hook* every
;; *network-watch-time-interval* the network status is checked if
;; nothing changed since the previous time no hooks are invoked.  If
;; access to a network is possible then the *network-up-hook* is run.
;; Conversely when network connectivity is lost the *network-down-hook*
;; is run.
;; 
;; The minor mode is designed to support multiple machines on the same
;; customised variables setup.
;; I share all my set-up accross diefferent machines.
;; The network interfaces of any unknown machines are
;; automatically added to the customised variable
;; *network-watch-interface-mapping* which is part of the *network*
;; customisation group.
;; 
;; 
;; ## Setup
;; 
;; The first time the library is used on a computer it will add the
;; machine name and the set of all active interfaces to the
;; *network-watch-interface-mapping* customised variable.
;; Edit the variable by hand to remove any loopback interfacess
;; or to add the name of any interfaces which are not connected.
;; 
;; You can also adapt the *network-watch-update-time-interval* to your liking.
;; The *have-network-hook* is run at the end of Emacs startup if you have
;; connectivity.
;; 
;; 
;; ## Utility function
;; 
;; Besides the two hooks the library also provides a *network-watch-p*
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
;;      (add-hook 'network-up-hook 'gmail-notifier-start)
;; 	(add-hook 'network-down-hook 'gmail-notifier-stop)
;; 
;; 

;;; Code:
(require 'cl-lib)

(defvar network-up-hook)
(defvar network-down-hook)
(defvar machine-name)
(defvar network-watch)

;; define that we are on a specific machine
(setq machine-name (substring (eshell-command-result "hostname") 0 -1 ))

;; Define base group for network information
(defgroup network nil
  "Customisation group for network availability hooks"
  :group 'Communication)


(define-minor-mode network-watch
  "Network is automatically on when there is a valid network
interface active."
  :initial-value t
  ;; The indicator for the mode line.
  :global t
  :variable (  '(network-watch-p) .  network-watch-update-wrap )
  :require 'network-watch
  :group 'network)


(defcustom network-watch-interface-mapping ()
  "List of interfaces providing network per machine."
  :type 'sexp
  :group 'network)

(defcustom network-watch-time-interval 120
  "Refersh reate for network status."
  :type 'integer
  :group 'network)


(defun network-watch-add-machine ()
  "Add the current machine to the machine interface mapping."
  (defvar intf-names)
  (setq intf-names ())
  (dolist (intf (network-interface-list) intf-names)
    (setq intf-names (cons (car intf) intf-names)))
  (customize-save-variable 'network-watch-interface-mapping
		(cons (list  machine-name intf-names ) network-watch-interface-mapping)
		"List of machine interfaces."))



(defun network-watch-p ()
  "Return nil if there are no active network interfaces."
  (catch 'break
    (let (value)
      (dolist (intf
	       (cadr (assoc machine-name network-watch-interface-mapping)))
	(setq value
	      (cl-find intf
		       (cl-mapcar #'car (network-interface-list))
		       :test #'string= ))
	(if value
	    (throw 'break value)))
      )))


(defun network-watch-update-system-state ()
  "Internal method update the network state variable."
  (setq network-watch (network-watch-p))
  ;; toggle the state of the global mode also
  )


(defun network-watch-update-wrap (state)
  "Blatantly ignore the set state as it is managed by the interface."
  (network-watch-update-state))


(defun network-watch-update-state ()
  "Run hooks only on network status change."
  (if (cl-set-exclusive-or
       (if (listp (network-watch-p))
	   (network-watch-p)
	 (list (network-watch-p)))
       (if (listp network-watch)
	   network-watch
	 (list network-watch)))
      (progn
	(if (network-watch-p) (run-hooks 'network-up-hook)
	  (run-hooks 'network-down-hook))
	(network-watch-update-system-state)
	))
  (run-with-timer network-watch-time-interval  nil 'network-watch-update-state)
  (message "Network state update")
  )


(defun network-watch-init ()
  "Network-Watch-Initialises the inner state of the network and configures machine if neccessary."
  (if (not (assoc machine-name network-watch-interface-mapping))
      (progn
	(message "Machine not pressent adding machine. Customise network group if needed.")
	(network-watch-add-machine)))
  (network-watch-update-system-state)
  (run-with-timer network-watch-time-interval  nil 'network-watch-update-state)
  (if (network-watch-p) (run-hooks 'network-up-hook))
  (message "Network init")
  )


(add-hook 'after-init-hook 'network-watch-init)

(provide 'network-watch)
;;; network-watch.el ends here

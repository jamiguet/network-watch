(require 'org)

;; define that we are on a specific machine
(setq machine-name (substring (eshell-command-result "hostname") 0 -1 ))

;; TODO:
;; Publish on melpa.

;; Define base group for network information
(defgroup network () "Customisation group for network availability hooks")


(defcustom machine-interface-mapping nil
  "List of interfaces providing network per machine"
  :type 'string
  :group 'network)

(defcustom network-update-time-interval 120
  "Refersh reate for network statatus"
  :type 'integer
  :group 'network)


(defun ja-machine-add ()
  "Adds the current machine to the machine interface mapping"
  (setq intf-names ())
  (dolist (intf (network-interface-list) intf-names)
    (setq intf-names (cons (car intf) intf-names)))
 (customize-save-variable machine-interface-mapping (cons (list  machine-name intf-names ) machine-interface-mapping)))



(defun have-network-p ()
  (catch 'break
    (dolist (intf 
	     (cadr (assoc machine-name machine-interface-mapping))
	     value)
      (if (assoc intf (network-interface-list))
	  (progn (setq value t)
		 (throw 'break value))
	(setq value nil)))
    ))


(defvar network-up-hook)
(defvar network-down-hook)

(defun ja-network-update-system-state () 
  (setq ja-network-system-state (have-network-p))
  )


(defun update-network-state ()
  (if (org-xor (have-network-p) ja-network-system-state)
      (progn
	(if (have-network-p) (run-hooks 'network-up-hook)
	  (run-hooks 'network-down-hook))
	(ja-network-update-system-state)
	))
  (run-with-timer network-update-time-interval  nil 'update-network-state)
  (message "Network state update")
  )


(defun ja-network-init ()
  (if (not (assoc machine-name machine-interface-mapping))
      (progn
	(message "Machine not pressent adding machine. Customise network group if needed.")
	(ja-machine-add)))
  (ja-network-update-system-state)
  (run-with-timer network-update-time-interval  nil 'update-network-state)
  (if (have-network-p) (run-hooks 'network-up-hook))
  (message "Network init")
  )


(add-hook 'after-init-hook 'ja-network-init)

(provide 'ja-network)

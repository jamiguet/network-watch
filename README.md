# network-watch

Emacs global minormode for handling intermitent network access.  It provides
two hooks *network-up-hook* and *network-down-hook* every
*network-watch-time-interval* the network status is checked if
nothing changed since the previous time no hooks are invoked.  If
access to a network is possible then the *network-up-hook* is run.
Conversely when network connectivity is lost the *network-down-hook*
is run.

The minor mode is designed to support multiple machines on the same
customised variables setup.
I share all my set-up accross diefferent machines.
The network interfaces of any unknown machines are
automatically added to the customised variable
*network-watch-interface-mapping* which is part of the *network*
customisation group.


## Setup

The first time the library is used on a computer it will add the
machine name and the set of all active interfaces to the
*network-watch-interface-mapping* customised variable.
Edit the variable by hand to remove any loopback interfacess
or to add the name of any interfaces which are not connected.

You can also adapt the *network-watch-update-time-interval* to your liking.
The *have-network-hook* is run at the end of Emacs startup if you have
connectivity.


## Utility function

Besides the two hooks the library also provides a *network-watch-p*
function which returns not nil when a listed interface is up.


## Example

In this example *gmail-notifier* is configured with the help of
*network-watch* it is automatically started and stopped when the network
	is up or down respectively.

	(require 'network-watch)
	(require 'gmail-notifier)
	
	(setq gmail-notifier-username "jamiguet")
	(setq gmail-notifier-password ja-password)

     (add-hook 'network-up-hook 'gmail-notifier-start)
     (add-hook 'network-down-hook 'gmail-notifier-stop)





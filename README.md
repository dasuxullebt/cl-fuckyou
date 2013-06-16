CL-FUCKYOU
==========

**Please note that this project is at an early stage, and by no means
well-tested.** Especially, there is still a lot of debug output done.

FUCKYOU is an acronym for **F**astcgi **U**sable through **C**ommon
lisp **K**its **Y**achting through the **O**ceans of
**U**nportability. It is (or at least tries to become) an
implementation of a FastCgi-Server in Common Lisp. 

Why?
----

I plan to use it through the
[Hunchentoot](http://weitz.de/hunchentoot/) framework, to get rid of
any additional servers. It might as well be useful as an interface to
rubbish languages which have a lot of library bindings due to their
large community.

Wat?
-----

It tries to implement the standard referenced
[here](http://www.fastcgi.com/drupal/node/6?q=node/22#S3), but has
some limitations, which are - as in any good FastCGI implementation -
not well-documented or transparent.

Major limitations are currently:

 - The code only runs under SBCL. I plan to change that.
 - It is not possible to connect to the cgi applications through an
   unix domain socket.

How?
----

Let's say, you have the following php-script, in, say,
`/tmp/fuck.php`, with the proper rights for php to read it:

    <?php
      echo $_GET['a'] + $_GET['b'];
    ?>

Now, run

    PHP_FCGI_CHILDREN=16 /usr/bin/php-cgi -b 127.0.0.1:13337

Now, in your SBCL REPL, load the package `cl-fuckyou`. Its dependencies
should be installable via [Quicklisp](http://www.quicklisp.org/). Then
do

	CL-USER> (in-package :fuckyou)
	#<PACKAGE "FUCKYOU">
	FUCKYOU> (flexi-streams:octets-to-string (car (direct-simple-php-request "127.0.0.1" 13337 "/tmp/" "fuck.php" "a=2&b=2")))
	Stream endet noch nicht!Eingabedaten vorhanden!
	Empfangene laenge: 96
	Juhu! Richtig geparst! Typ: 6, Laenge: 65
	Received stdout data
	Stream endet hier!
	"X-Powered-By: PHP/5.3.10-1ubuntu3.6^M
	Content-type: text/html^M
	^M
	4"
	FUCKYOU> 

W00t! Now you can calculate sums!

Who?
----

Send suggestions, bug reports, absinthe and kappa maki to the address
which will appear when you run the following thing in your REPL:

	(let ((n "Christoph-Simon Senjak")) (format nil "~A ~
	<~C~C~C~C~A>" n (elt n 0) (elt n 10) (elt n 16) #\@ "uxul.de"))

Can Haz?
--------

CL-FUCKYOU is licensed under the AGPL 3.0. See agpl.txt.

Owl
---

    {o,o}
    (( ))
    -"-"-


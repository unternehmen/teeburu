# What is teeburu?
teeburu is a low-security textboard server implemented in GNU Guile.

## Dependencies
I have only tested this server on GNU Guile 2.0.11.  There is a small
shell script at the top (used for smarter PATH resolution of the
guile-2.0 executable) which assumes /bin/sh is a Borne-compatible shell.

There are no database software dependencies, since all post data is kept
in RAM as a list.

teeburu uses a few of the modules that come with an installation of
Guile 2.0.11.  If those modules aren't on your system, you might try
compiling and installing Guile 2.0.11 from source.

## Usage
This command can be used to run the server:

    $ ./teeburu.scm

The server will then start on port 8080.  Have fun!

In order to run the script in that way, the script must be executable.
To make it executable, run this command:

    $ chmod a+x teeburu.scm

Alternatively, you can run the command directly using guile-2.0:

    $ guile-2.0 --debug --no-auto-compile -s teeburu.scm

There are a few configuration options you can change, such as the
port, the address to bind to (host), the number of posts kept in the
list, etc.  These options are defined at the top of the script, if
you wish to change them.

## Problems
There is no limit to the frequency of users' POST requests.
It would be easy to write a bot which stresses the server, perhaps
causing it to shut down.  I have not tested whether the server works
well when used by many people at once, but it doesn't mutate any
variables so there shouldn't be any data corruption if two people
post at the same time.

## Features
The entire textboard exists in RAM and makes no modifications to the
host system.  It doesn't evaluate any POST input as Scheme code and
as far as I can tell it escapes all input properly, so there should be
no way for a user to insert scripts onto the page.

## License
teeburu is licensed under the CC0 1.0 Universal license, a copy of which
should have come with this package.  To the extent possible under law, I
waive all copyright and related or neighboring rights to teeburu. I make
no warranty about the work and disclaim liability for all uses of the
work, to the extent possible under law.  *teeburu was not built with a
focus on security, so don't to use it on a production server without
patching up the holes!*  See the "Problems" section above.

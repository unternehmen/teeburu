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

## Problems

The list of posts is a global variable which is mutated every time
someone makes a post.  If two people make a post at the same time, there
is no telling what will happen.

There is no limit to the length or frequency of users' POST requests.
It would be easy to write a bot which fills the server with posts.

## Features

The entire textboard exists in RAM and makes no modifications to the
host system.  It doesn't evaluate any POST input as Scheme code and
as far as I can tell it escapes all input properly.

## License
teeburu is licensed under the CC0 1.0 Universal license, a copy of which
should have come with this package.  To the extent possible under law, I
waive all copyright and related or neighboring rights to teeburu. I make
no warranty about the work and disclaim liability for all uses of the
work, to the extent possible under law.  *teeburu was not built with a
focus on security, so don't to use it on a production server without
patching up the holes!*  See the "Problems" section above.
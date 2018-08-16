Trigger personal scripts from incoming HTTP requests
====================================================

Ever wish you could do something custom with all those web hooks
offered by various web service providers?  This package provides an
easy and safe way to do just that.

Example: Tagging a video will download and sync it to your phone
----------------------------------------------------------------

In the `examples` directory there is a script named
`download-video.sh`.  Here is how I use it:

  * When I want to watch a video later, probably on an airplane, I tag
    the video in my RSS feed reader (<https://feedly.com/>).

  * That triggers an [IFTTT](https://ifttt.com/) applet which uses the
    IFTTT support for calling a web hook and calls into this package.

  * This package runs the `download-video.sh` script which downloads
    the video to a directory that is automatically synced with my
    phone.

How this package works
----------------------

This package includes an executable called `webhooks`.  This
executable can be used to create new hooks from the command line or
run a web server to respond to incoming requests.  A hook is just an
indirect way to run a script if you know the hook's secret code.

For security reasons, the web server does *not* run scripts directly.
Instead, if the incoming request correctly maps to an existing hook,
the request data will be appended to an existing file as JSON.

"Okay, but how does appending JSON to a file help me?" you ask.  Good
question.  Thanks to the magic of POSIX named pipes (FIFOs), you can
feed that JSON data into a waiting script.

Setting up the example web hook
-------------------------------

  1. Run the HTTP server provided by this package:

         $ webhooks server --port 3000

  2. Create a new web hook that appends to a file.  In this example
     we'll configure a hook to append to the file `/tmp/foo.pipe`:

         $ webhooks create --append /tmp/foo.pipe

     This command will print a secret code for the newly created hook.
     (If you forget the hook's secret code you can use the `webhooks
     list` command to look it up again.)

  3. Run a script that creates the named pipe and then reads lines
     from it.  In the `examples` directory there is a script that will
     do this for you and then execute other commands as requests come
     in.  (The commands receive JSONs request on their stdin.)

         $ examples/watchfifo.sh -f /tmp/foo.pipe -- examples/download-video.sh

  4. Use the hook's secret code to trigger your script.  In this
     example we'll pretend that the secret code is `XXX`.

         $ curl --data url=https://player.vimeo.com/video/148946917 \
             http://localhost:3000/hooks/XXX

     This leads to the `download-video.sh` script running and being
     fed the following JSON:

         {"url": "https://player.vimeo.com/video/148946917"}

Ideally, you should run the `webhooks` server behind a reverse proxy
that is properly configured for TLS.  This will prevent hook codes
from being exposed to the network unencrypted.  To encourage this, the
server only binds to the loopback device.

More details about installing and running this package can be found in
the [installation guide](INSTALL.md).

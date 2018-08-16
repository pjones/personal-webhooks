Installing and Using Personal Webhooks
======================================

These instructions will help you install and get personal-webhooks up
and running.

External Dependencies
---------------------

You will need the following dependencies installed and running:

  * PostgreSQL.  (personal-webhooks stores hook information in the
    database.)

  * A properly configured web server that reverse proxy requests to
    personal webhooks.  Preferably the server is configured to handle
    TLS connections over port 443 (HTTPS).  This also allows the web
    server and webhooks processes to run with different security
    contexts.

Ways to Install this Package
----------------------------

  * Through your operating system's package manger.  (I don't know of
    any downstream packages for personal-webhooks as of right now.
    Let me know if hear otherwise.)

  * Build from source (time consuming but easy).

Building from Source
--------------------

This project is written in the Haskell programming language.  If you
already have a Haskell toolchain installed you can build it like any
other Haskell package.

However, if you don't know or don't care to know how to build Haskell
packages I highly recommend you consider installing the [Nix][]
package manager and building personal-webhooks via Nix.  Here's how:

  1. Install [Nix][] which is pretty simple and straight forward.

  2. Open a new terminal and run the following command:

        nix-env --install --file https://github.com/pjones/personal-webhooks/archive/master.tar.gz

  3. Wait for everything to build.

  4. You now have a `webhooks` executable in your `PATH`.

Running the Webhooks Server
---------------------------

Once you have personal-webhooks installed the hard part is over.  Now
we'll create a simple configuration file and start the server.

  1. Create a new PostgreSQL database.  I also prefer to create a new
     PostgreSQL user as well.

     The webhooks server will only need to read access to the database
     so you can even create a restricted PostgreSQL user for the
     server and a normal user for non-server operations such as
     creating new hooks from the command line.

  2. Using the [example configuration](examples/config.yml) file as a
     guide, create a new configuration file for the server and specify
     the database connection string.

     You can place the configuration file anywhere you like and use
     the `--config` option to the `webhooks` executable.  Or, you can
     place the file in the following location and webhooks will read
     it automatically:

         ~/.config/webhooks/config.yml

  3. Start the webhooks server.

     I prefer to run the server as a systemd service.  But you can run
     it anyway you'd like.  Here's how you start it:

        $ webhooks server --port 3000

     This starts the server (reading the configuration file from the
     default location) listening on port 3000 of the loopback device
     (127.0.0.1).


   4. Configure your favorite web server to reverse proxy requests to
      your webhooks server.

      The idea is that another web server is running with a very
      limited security context and is configured with a proper SSL/TLS
      certificate.

      Requests for `/hooks` can be forwarded to the webhooks server
      which is running with two abilities the main web server doesn't
      have:

      - Read access to the webhooks configuration file

      - Write access to named pipe (FIFO) files

      Of course, it's up to you to properly set up users and groups to
      achieve this.  (I like to have a `webhooks` user and group that
      the webhooks server runs as.  The group has write access to some
      FIFO files in `/var/lib/webhooks`.  I also put myself in the
      `webhooks` group so I can write scripts that create and listen
      on those FIFO files.)

[nix]: https://nixos.org/nix/download.html

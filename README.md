**For centuries, people have wondered**: "What's the fastest way to get a JPEG into a Slack channel?" Scientists now believe that **jpgtobot** is the answer. To verify these findings, please

* Install Haskell ([Windows](http://www.haskell.org/platform/), [Mac](http://ghcformacosx.github.io/), and [Linux](https://gist.githubusercontent.com/hlian/b5a975252997cb3e0020/raw/e4ecab3042225d321a88ee74e804c38ead38ed52/gistfile1.txt));
* `git clone https://github.com/hlian/jpgtobot/`
* `cd jpgtobot`
* `cabal sandbox init`
* `cabal install --only-dep -j` (meanwhile...)
* Create a Slack incoming token
* Write it down in a one-line `token` file
* `cabal run`
* Use [ngrok](https://ngrok.com/) to open a tunnel to port 83 e.g. `brew install ngrok; ngrok -authtoken yourtokenhere 83`
* Write down the ngrok URL
* Teach Slack the `/jpgto` slash command and have it point to the URL
* Type `/jpgto corgi`

If all goes well, you'll see something like this:

![/jpgto Corgi](https://raw.githubusercontent.com/hlian/linklater/master/corgi.jpg)

## What is this, really?

A demo app for [`Network.Linklater`](https://github.com/hlian/linklater), a library I wrote! It lets you write Slack bots very easily. Please check it out, whenever you have free time.

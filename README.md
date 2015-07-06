**For centuries, people have wondered**: "What's the fastest way to get a JPEG into a Slack channel?" Scientists now believe that **jpgtobot** is the answer. To verify these findings, please

## Up and running

* Install Haskell ([Windows](http://www.haskell.org/platform/), [Mac](http://ghcformacosx.github.io/), and [Linux](https://gist.githubusercontent.com/hlian/b5a975252997cb3e0020/raw/e4ecab3042225d321a88ee74e804c38ead38ed52/gistfile1.txt));
* Install [Stack](https://github.com/commercialhaskell/stack);
* `git clone https://github.com/hlian/jpegbot/ && cd jpegbot && stack build`;
* Create a Slack incoming token and save it to a file named `hook`;
* Create a Google Custom Search API key and save it to a file named `google-server-key` ([see this](https://github.com/dpatti/jpg-to/blob/master/README.md));
* Create a Google Search Engine and save the ID to a file named `google-search-engine-id` ([see this again](https://github.com/dpatti/jpg-to/blob/master/README.md));
* `stack exec jpgtobot`.

## And talking to Slack

* Use [ngrok](https://ngrok.com/) to open a tunnel to port 3000 (`ngrok 3000`);
* Copy down the ngrok URL;
* Create a Slack custom command `/jpeg` POSTing to that URL;
* In Slack, type `/jpeg corgi`.

If all goes well, you'll see something like this:

![/jpgto Corgi](https://raw.githubusercontent.com/hlian/linklater/master/corgi.jpg)

## What is this, really?

A demo app for [`Network.Linklater`](https://github.com/hlian/linklater), a library I wrote! It lets you write Slack bots very easily. Please check it out, whenever you have free time.

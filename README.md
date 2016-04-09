# typebot

A slack bot that will fetch type signatures via Hoogle:

<img src="http://i.imgur.com/TJzSA93.png" width="400">

## Deployment + Configuration

First, run the following:

```bash
$ git clone https://github.com/thoughtbot/typebot.git
$ cd typebot
$ heroku create --buildpack https://github.com/mfine/heroku-buildpack-stack.git
$ heroku ps:scale web=1
$ git push heroku master
```

This will give you the URL typebot is deployed at. If you `curl` the URL, you
should get a 200 response.

Go to your team's slack, and then Custom Integrations > Outgoing Webhooks. Set
the channel to whatever channel you wish, the trigger word to :t, and the URL to
your Heroku URL + `/type`. Grab the Token, and set that as the
`TYPEBOT_SLACK_TOKEN` environment variable on Heroku.

<img src="http://i.imgur.com/mLIScKq.png" width="800">

Then, go to Custom Integrations > Incoming Webhooks. Set the channel to be
whatever channel you want typebot to post to. Copy the Webhook URL, and set that
as the `TYPEBOT_SLACK_URL` environment variable on Heroku. All done!

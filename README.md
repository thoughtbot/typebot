# typebot

A slack bot that will fetch type signatures via Hoogle:

<img src="http://i.imgur.com/k10Aqrb.png" width="800">

## Configuration

* Create a custom outgoing webhook integration in slack, and save the token.
* Create a custom incoming webook integration in slack, and save the URL.
* Configure the app.cfg as:

```
token="URL"
```

You can have the same bot service multiple slack teams by adding additional
`token="URL"` lines. Deploy to your favorite server/provider.

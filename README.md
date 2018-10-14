Reason Dojo ?: Fruit Stamper
---

Reference project for our Reason Dojo ?.

![Animation of project](./fruitstamper.gif)

### Install

```
npm install
```

### Build
```
npm run build
```

### Start
```
npm start
```

When this fails with a 'No available audio device' error, you can run without audio with '`SDL_AUDIODRIVER="" npm start`'

To build to JS run `npm run build:web` and then run a static server, like `python -m SimpleHTTPServer` and go to `localhost:8000`. If you're using safari you can simply open the `index.html` and tick `Develop > Disable Cross-Origin Restrictions`.

To build to native run `npm run build:native` and run `npm run start:native`

The build system used is [bsb-native](https://github.com/bsansouci/bsb-native).

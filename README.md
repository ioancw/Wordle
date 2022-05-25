# Lit.Wordle

Forked from https://aaronmu.github.io/MathGame/

When I know how to, I will rename this to Lit.Wordle, but at the moment that seems to fail, when I publish (npm publish) although it works locally.

TODO
* Properly structure the code into modules etc.
* Add tests (especially around the masking function)
* Implement game statistics
* Implement persitence, i.e. saving the statistics locally.
* Properly handle terminal states, i.e. Won or Lost.
* Sharing the results.

[Fable.Lit](https://github.com/fable-compiler/Fable.Lit) app. To start a development server run:

```
npm install && npm start
```

Other commands:

```bash
npm run build   # Build optimized site for deployment and put in dist/
npm run publish # Build and publish to github pages
```

## Vite.js repository structure conventions

- Put static files in `public/` folder
- Put `index.html` in app root (next to `package.json`)
- Add a reference to the entry JS file (relative path is important):

```html
<script type="module" src="./build/client/App.js"></script>
```

# Giphy app to browse the giphy API and play with Purescript.

For a dev build, do:
```sh
yarn
yarn watch
yarn serve-dev
```

For a prod build, do:
```sh
yarn
yarn bundle
yarn serve
```

The API key must be provided via an env variable `GIPHY_API_KEY=some_api_key`.

Here, `parcel` automatically loads `.env` files, so adding a `.env` file like this in the root folder should work:
```
GIPHY_API_KEY=some_api_key
```
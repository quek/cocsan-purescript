````
yarn init
yarn add --dev purescript pulp psc-package purescript-psa \
  webpack webpack-cli webpack-dev-server purs-loader html-webpack-plugin foreman file-loader
yarn pulp --psc-package init --force
```

`.psc-package/psc-0.12.3/.set/packages.json` の version を書き換える。
see https://github.com/slamdata/purescript-halogen/blob/master/bower.json

halogen v5.0.0-rc.1
hologen-vdom v6.1.0
dom-indexed v7.0.0

```
yarn psc-package install halogen
```

```
firebase init
```

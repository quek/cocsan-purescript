const path = require('path');
const HtmlWebpackPlugin = require('html-webpack-plugin');
const webpack = require('webpack');

module.exports = {
  entry: './src/index.js',
  output: {
    path: path.resolve(__dirname, 'public'),
    filename: '[name]-[hash].js',
    publicPath: '/'
  },
  module: {
    rules: [
      {
        test: /\.purs$/,
        use: [
          {
            loader: 'purs-loader',
            options: {
              psc: 'psa',
              src: ['.spago/*/*/src/**/*.purs', 'src/**/*.purs']
            }
          }
        ]
      },
      {
        test: /\.(png|jpg|gif|eot|woff|ttf|svg|ico)$/,
        use: [
          {
            loader: 'file-loader'
          }
        ]
      },
      {
        test: /\.scss$/,
        use: [
          "style-loader",       // creates style nodes from JS strings
          "css-loader",         // translates CSS into CommonJS
          "sass-loader" // compiles Sass to CSS, using Node Sass by default
        ]
      }
    ]
  },
  resolve: {
    extensions: [ '.js', '.sass', '.scss', 'css', '.purs', 'png' ],
  },
  plugins: [
    new HtmlWebpackPlugin({
      title: 'CoC',
      template: 'src/index.html'
    }),
    new webpack.HotModuleReplacementPlugin()
  ],
  devServer: {
    contentBase: path.join(__dirname, 'public'),
    proxy: {
      '/__': 'http://localhost:5000',
    },
    historyApiFallback: {
      rewrites: [
         { from: /./, to: '/' }
      ]
    },
    hot: true
   }
};

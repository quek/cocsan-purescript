const path = require('path');
const HtmlWebpackPlugin = require('html-webpack-plugin');

module.exports = {
  entry: './src/index.js',
  output: {
    path: path.resolve(__dirname, 'public'),
    filename: '[name]-[contenthash].js',
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
              src: ['.psc-package/psc-*/*/v*/src/**/*.purs', 'src/**/*.purs']
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
      }
    ]
  },
  resolve: {
    extensions: [ '.js', '.sass', '.scss', 'css', '.purs', 'png' ],
  },
  plugins: [
    new HtmlWebpackPlugin({
      title: 'CoC',
      template: 'index.html'
    })
  ],
  devServer: {
    contentBase: path.join(__dirname, 'public'),
    proxy: {
      '/__': 'http://localhost:5000',
    },
    historyApiFallback: true
  }
};

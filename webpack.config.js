const path = require('path');

module.exports = {
  entry: './src/main/js/newTheorem.jsx',
  output: {
    path: path.join(__dirname, 'target', 'scala-2.12', 'classes', 'static', 'js'),
    publicPath: '/js/',
    filename: 'bundle.js'
  },
  module: {
    rules: [
      {
        test: /\.jsx?/,
        use: {
          loader: 'babel-loader'
        }
      }
    ]
  },
  target: 'web',
  mode: 'development',
  devtool: 'eval-source-map'
};

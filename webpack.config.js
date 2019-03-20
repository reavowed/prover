module.exports = {
  entry: './src/main/resources/static/js/newTheorem.jsx',
  output: {
    path: __dirname + '/src/main/resources/static/js/',
    filename: 'bundle.js'
  },
  module: {
    rules: [
      {
        test: /\.jsx?/,
        use: {
          loader: 'babel-loader',
          debug: true
        }
      }
    ]
  },
  target: 'web',
  mode: "production"
};

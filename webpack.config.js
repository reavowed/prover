module.exports = {
  entry: './src/main/js/App.jsx',
  output: {
    filename: 'bundle.js',
    libraryTarget: 'var',
    library: 'App'
  },
  resolve: {
    extensions: ['.js', '.jsx', '.ts', '.tsx'],
  },
  module: {
    rules: [
      {
        test: /\.tsx?/,
        exclude: /node_modules/,
        use: ['babel-loader', 'awesome-typescript-loader']
      },
      {
        test: /\.jsx?/,
        exclude: /node_modules/,
        loader: 'babel-loader'
      },
      {
        enforce: "pre",
        test: /\.js$/,
        loader: "source-map-loader"
      }
    ]
  },
  target: 'web',
  mode: 'development',
  devtool: 'source-map',
  devServer: {
    port: 8081
  },
  externals: {
    "react": "React",
    "react-dom": "ReactDOM"
  }
};

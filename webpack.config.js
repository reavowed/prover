const ReactRefreshWebpackPlugin = require('@pmmmwh/react-refresh-webpack-plugin');

module.exports = env => {
  return {
    entry: './src/main/js/App.jsx',
    output: {
      filename: 'bundle.js',
      libraryTarget: 'var',
      library: 'App',
      publicPath: `http://localhost:8079/`,
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
      port: 8079,
      hot: true,
      headers: {
        "Access-Control-Allow-Origin": "*"
      }
    },
    externals: {
      "react": "React",
      "react-dom": "ReactDOM"
    },
    plugins: [env.NODE_ENV === "development" && new ReactRefreshWebpackPlugin({useLegacyWDSSockets: true})].filter(Boolean)
  }
};

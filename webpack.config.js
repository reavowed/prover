const ReactRefreshWebpackPlugin = require('@pmmmwh/react-refresh-webpack-plugin');

const isDevelopment = process.env.NODE_ENV !== 'production';

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
      fallback: {
        path: require.resolve('path-browserify')
      }
    },
    module: {
      rules: [
        {
          test: /\.tsx?/,
          exclude: /node_modules/,
          use: ['babel-loader', 'ts-loader']
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
    mode: isDevelopment ? 'development' : 'production',
    devtool: 'source-map',
    devServer: {
      port: 8079,
      hot: true,
      headers: {
        "Access-Control-Allow-Origin": "*"
      }
    },
    plugins: [isDevelopment && new ReactRefreshWebpackPlugin()].filter(Boolean)
  }
};

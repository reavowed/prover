module.exports = {
  presets: [
    "@babel/preset-react",
    [
      "@babel/preset-env",
      {
        "useBuiltIns": "usage",
        "corejs": 3.4
      }
    ]
  ],
  plugins: [
    '@babel/plugin-proposal-class-properties',
    '@babel/plugin-proposal-optional-chaining',
    '@babel/plugin-proposal-nullish-coalescing-operator'
  ],
  env: {
    development: {
      plugins: ['react-refresh/babel', '@babel/plugin-transform-react-jsx-source']
    },
    test: {
      presets: ["@babel/preset-typescript"],
      plugins: ["transform-es2015-modules-commonjs"]
    }
  }
};

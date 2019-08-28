const HtmlWebpackPlugin = require("html-webpack-plugin");
const MiniCssExtractPlugin = require("mini-css-extract-plugin");
const elmSource = __dirname + "/";

module.exports = {
  // mode: (process.env.NODE_ENV == 'production' ? 'production' : 'development'),
  module: {
    rules: [
      {
        test: /\.elm$/,
        exclude: [/elm-stuff/, /node_modules/],
        use: {
          loader: "elm-webpack-loader",
          options: {
            cwd: elmSource
            // optimize: (process.env.NODE_ENV == 'production' ? true : false)
          }
        }
      },
      {
        test: /\.(sa|sc|c)ss$/,
        use: ["style-loader", "css-loader", "sass-loader"]
      },
      {
        test: /\.(png|svg|jpg|gif)$/,
        use: ["file-loader"]
      }
    ]
  },
  plugins: [
    new HtmlWebpackPlugin({ template: "src/index.html" }),
    new MiniCssExtractPlugin({
      filename: "[name].css",
      chunkFilename: "[id].css"
    })
  ],
  watchOptions: {
    ignored: [/node_modules/, /elm-stuff/]
  },
  devServer: {
    clientLogLevel: "info"
  }
};

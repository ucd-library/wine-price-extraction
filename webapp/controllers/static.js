const express = require('express');
const path = require('path');
const fs = require('fs');
const spaMiddleware = require('@ucd-lib/spa-router-middleware');
const config = require('../config');

module.exports = (app) => {
  // path to your spa assets dir
  let assetsDir = path.join(__dirname, '..', 'client', 'public');

  /**
   * Setup SPA app routes
   */
  spaMiddleware({
    app: app, // pass the express app
    htmlFile : path.join(assetsDir, 'index.html'), // pass the file you want to use
    isRoot : true, // are we serving from host root (/)?
    appRoutes : config.appRoutes, // array of root paths.  ie appRoutes = ['foo', 'bar'] to server /foo/* /bar/*
    getConfig : async (req, res) => {
      return {}
    }
  });

  /**
   * Setup static asset dir
   */
  app.use(express.static(assetsDir));
}
const { Client, Pool } = require('pg')
const config = require('../config');
const client = new Pool(config.pg);



class PG {
  constructor() {
    this.connectP = client.connect();
    this.client = client;
  }

  async query(query, args=[]) {
    await this.connectP;
    return client.query(query, args);
  }
}

module.exports = new PG();
const { Pool } = require('pg')
const config = require('../config');

if (process.env.INSTANCE_CONNECTION_NAME && process.env.NODE_ENV === 'production') {
  config.pg.host = `/cloudsql/${process.env.INSTANCE_CONNECTION_NAME}`;
  console.log('Connecting pg via proxy: '+config.pg.host);
}

const client = new Pool(config.pg);

class PG {
  constructor() {
    this.connectP = client.connect();
    this.client = client;

    client.on('error', (e) => {
      console.error('pg connection error', e);
      this.connectP = null;
    })
  }

  async query(query, args=[]) {
    if( !this.connectP ) {
      this.connectP = client.connect();
    }

    await this.connectP;
    return client.query(query, args);
  }
}

module.exports = new PG();
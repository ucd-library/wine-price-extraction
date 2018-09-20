const pg = require('./pg');
const config = require('../config');

class ArksModel {
  
  async all() {
    let result = await pg.query(`select 
      page_id,ark,rotation,page,ST_AsGeoJSON(bbox) as geojson 
      from ${config.pg.schema}.pages`);
    return result.rows.map(row => {
      row.geojson = JSON.parse(row.geojson);
      delete row.st_asgeojson;
      return row;
    });
  }

}

module.exports = new ArksModel();
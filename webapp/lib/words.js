const pg = require('./pg');
const config = require('../config');

class WordsModel {
  
  async getWords(ark) {
    let result = await pg.query(`select 
      word_id,page_id,carea_id,par_id,line_id,ark,rotation,word,text,x_wconf,ST_AsGeoJSON(bbox) 
      from ${config.pg.schema}.words where ark = '${ark}'`);
    return result.rows.map(row => {
      row.geojson = JSON.parse(row.st_asgeojson);
      delete row.st_asgeojson;
      return row;
    });
  }

}

module.exports = new WordsModel();
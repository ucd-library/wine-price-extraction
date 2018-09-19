const pg = require('./lib/pg');
const config = require('./config');
const path = require('path');
const fs = require('fs');

const CHUNK = 10000;
const total = 5464769;

const data = {};

process();

async function process() {
  let data = require('./count-results.json');

  for( let ark in data ) {
    let rotated = 0;
    let unrotated = 0;

    for( let value in data[ark].rotated ) {
      rotated += data[ark].rotated[value];
    }
    for( let value in data[ark].unrotated ) {
      unrotated += data[ark].unrotated[value];
    }

    let rotation = (await pg.query(`select rotation from ${config.pg.schema}.pages where ark = '${ark}'`))
        .rows.map(row => row.rotation);

    if( rotated > 0 ) {
      console.log(ark, '%'+Math.floor(100*(unrotated/rotated)), 'unrotated='+unrotated, 'rotated='+rotated, rotation[0]);
    } else {
      
      console.log(ark, unrotated, 0);
    }
    
  }
}

async function writeCounts(){
  let arks = (await pg.query(`select ark from ${config.pg.schema}.pages`))
    .rows.map(row => row.ark);

  let c = 0;
  for( let ark of arks ) {
    c++;
    let time = Date.now(); 

    let result = await pg.query(`select 
    word_id,page_id,carea_id,par_id,line_id,ark,rotation,word,text,x_wconf,ST_AsGeoJSON(bbox) 
    from ${config.pg.schema}.words where ark = '${ark}'`);

    if( c > 0 ) {
      process.stdout.clearLine();  // clear current text
      process.stdout.cursorTo(0);
    }

    result.rows.forEach(row => {
      let rotated = row.rotation > 0 ? 'rotated' : 'unrotated';
      let text = row.text.toLowerCase().trim();

      if( !data[row.ark] ) data[row.ark] = {rotated: {}, unrotated: {}};
      if( !data[row.ark][rotated][text] ) data[row.ark][rotated][text] = 1;
      else data[row.ark][rotated][text]++;
    });

    process.stdout.write("Running ... %" + Math.floor((c / arks.length)*100) + ' ('+c+'/'+arks.length+') ('+(Date.now()-time)+'ms) '+result.rows.length );


  }

  console.log('\nwriting results..');
  fs.writeFileSync(
    path.join(__dirname, 'count-results.json'),
    JSON.stringify(data, '  ', '  ')
  )

  pg.client.end();
}
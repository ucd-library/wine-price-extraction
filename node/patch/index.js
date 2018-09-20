const words = require('../../webapp/lib/words');
const pg = require('../../webapp/lib/pg');
const fs = require('fs-extra');
const path = require('path');
const request = require('request');
const jsdom = require("jsdom");
const { JSDOM } = jsdom;

const MIN_CONF = 90;
const ROTATION = 0;
const COLOR = 'gray';
// const COLOR = 'bitonal';
const BUFFER = 10;

const ROOT_DIR = path.join(__dirname, 'tmp');

if( fs.existsSync(ROOT_DIR) ) fs.removeSync(ROOT_DIR);
fs.mkdirpSync(ROOT_DIR);

async function patch(ark) {
  let arkwords = await words.getWords(ark);

  let diffs = [];
  let unrotated = 0;

  for( let word of arkwords ) {
    if( word.x_wconf > MIN_CONF ) continue;
    unrotated++;
    if( word.rotation !== 0 ) continue;
    if( word.length === 2 && word.match(/e/) ) continue;

    let url = getImageUrl(word.ark, word);
    await download(
      path.join(ROOT_DIR, word.word_id+'.png'),
      url,
    );

    url = getImageUrl(word.ark, word, 'tesseract');

    let htmlFile = path.join(ROOT_DIR, word.word_id+'.html');
    await download(
      htmlFile,
      url,
      {
        headers : {
          accept : 'application/hocr+xml'
        }
      }
    );

    let html = fs.readFileSync(htmlFile);
    const { document } = (new JSDOM(html)).window;

    let eles = document.querySelectorAll('.ocrx_word') || [];
    if( eles.length > 0 ) {
      let ele = eles[0];

      word.patch = {
        text : ele.textContent,
        x_wconf : parseInt(ele.getAttribute('title').split(';')[1].replace('x_wconf', '').trim())
      }

      
    } else { 
      word.patch = {
        text : '',
        x_wconf : 0
      }
      diffs.push(0);
    }

    if( word.patch.x_wconf - word.x_wconf > 0) {
      diffs.push(word.patch.x_wconf-word.x_wconf);
      console.log(word.word_id);
      console.log(getImageUrl(word.ark, word));
      console.log(`before: "${word.text}" ${word.x_wconf}` );
      console.log(`after: "${word.patch.text}" ${word.patch.x_wconf}` );
      console.log('\n----\n');
    }

  }

  let t = 0;
  diffs.forEach(v => t+=v);
  console.log('improved', 'avg=', t / diffs.length, 'total=', diffs.length);
  console.log('ignored', unrotated-diffs.length);
  
  pg.client.end();
  process.exit(1);
}

function getImageUrl(ark, word, svc='iiif') {
  let base = ark.split('-')[0];

  let c = word.geojson.coordinates[0];
  c.forEach(v => {
    if( v[1] > 0 ) return;
    v[1] = v[1] * -1;
  });

  let x = c[0][0]-BUFFER;
  let y = c[1][1]-BUFFER;
  let w = c[2][0]-c[1][0]+(2*BUFFER);
  let h = c[0][1]-c[1][1]+(2*BUFFER);

  return `https://sandbox.dams.library.ucdavis.edu/fcrepo/rest/collection/sherry-lehmann/catalogs/${base}/media/images/${ark}/svc:${svc}/${x},${y},${w},${h}/full/${ROTATION}/${COLOR}.png`
}

function download(file, url, options={}) {
  return new Promise((resolve, reject) => {
    let ws = fs.createWriteStream(file);
    ws.on('close', () => resolve())
    request(url, options).pipe(ws)
    
  });
}

patch('d72p44-032');
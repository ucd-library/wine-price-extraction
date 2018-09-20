const puppeteer = require('puppeteer');
const fs = require('fs-extra');
const path = require('path');
const arks = require('../webapp/lib/arks');

let ROOT_DIR = path.join(__dirname, 'screenshots');
// if( fs.existsSync(ROOT_DIR) ) fs.removeSync(ROOT_DIR);
if( !fs.existsSync(ROOT_DIR) ) fs.mkdirpSync(ROOT_DIR);

async function run() {
  const browser = await puppeteer.launch();

  console.log('Loading pages from db...')
  let pages = await arks.all();
  let c = 0;

  for( page of pages ) {
    let time = new Date();

    await screenshot(browser, page.ark);


    if( c > 0 ) {
      process.stdout.clearLine();  // clear current text
      process.stdout.cursorTo(0);
    }
    process.stdout.write("Running ... %" + Math.floor((c / pages.length)*100) + ' ('+page.ark+') ('+(Date.now()-time)+'ms) ');
    c++;
  }

  await browser.close();
}

async function screenshot(browser, ark) { 
  let png = path.join(ROOT_DIR, ark+'.png');
  if( fs.existsSync(path) ) return;

  const page = await browser.newPage();
  await page.setViewport({
    height: 800,
    width: 700
  });
  
  await page.goto('http://localhost:3000/ark/'+ark, {waitUntil: 'networkidle0'});
  
  await page.evaluate(() => {
    document.querySelector('datafest-app')
      .hideInfoPanel();
  });
  await page.screenshot({path: png});
}

run();
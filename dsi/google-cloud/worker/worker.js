const Worker = require('@ucd-lib/sloan-ocr-autoscaler-worker/model/worker');
const cloudStorage = require('@ucd-lib/sloan-ocr-autoscaler-worker/lib/cloud-storage');
const path = require('path');
const fs = require('fs-extra');
const request = require('request');
const {exec} = require('child_process');

const ROOT_DIR = '/storage';

class WorkerImpl extends Worker {
  constructor(name) {
    super(name);
    this.processed = 0;
  }
  
  async run() {
    console.log(this.segment);
    await fs.emptyDir(ROOT_DIR);
    await fs.mkdirp(path.join(ROOT_DIR,'input'));
    await fs.mkdirp(path.join(ROOT_DIR,'output'));

    if( this.segment.ocr ) {
      await this.runOCR();
    }
    if( this.segment.processOcr ) {
      await this.processOCR();
    }

    this.onSuccess();
  }


  async runOCR() {
    await this.downloadImage(this.segment.imageUrl, this.segment.filename);

    // Rscript for_justin_run_wine_price_tables.R /io/input /io/output TRUE 
    let {stdout, stderr} = await this.exec(
      `Rscript for_justin_run_wine_price_tables.R ${ROOT_DIR}/input ${ROOT_DIR}/output TRUE`
    )
    await this._uploadFiles(stdout, stderr, 'ocr');
  }

  async processOCR() {
    // Rscript for_justin_run_wine_price_tables.R /io/input /io/output FALSE /io/output 
    let ocrFile = path.join(ROOT_DIR,'output',path.parse(this.segment.filename).name);
    ocrFile += '_data1.RDS';

    if( !fs.existsSync(ocrFile) ) {
      let gcsFile = path.join(this.segment.pathId, path.parse(this.segment.filename).name+'_data1.RDS');
      await cloudStorage.getFile(ocrFile, gcsFile);
    }

    let {stdout, stderr} = await this.exec(
      `Rscript for_justin_run_wine_price_tables.R ${ROOT_DIR}/input ${ROOT_DIR}/output FALSE ${ROOT_DIR}/output`
    );

    await this._uploadFiles(stdout, stderr, 'process');
  }

  async _uploadFiles(stdout, stderr, suffix='') {
    // write stdout/stderr
    let ioRoot = path.join(ROOT_DIR,'output');
    await fs.writeFile(path.join(ioRoot, `stdout_${suffix}.txt`), stdout);
    await fs.writeFile(path.join(ioRoot, `stderr_${suffix}.txt`), stderr);

    // upload files to cloud storage
    let files = await fs.readdir(path.join(ROOT_DIR,'output'));
    Promise.all(
      files.map(file => cloudStorage.addFile(
        path.join(ioRoot, file),
        path.join(this.segment.pathId, file)
      ))
    );
  }

  exec(cmd, options={}) {
    options.cwd = '/opt/dsi/scripts';

    return new Promise((resolve, reject) => {
      exec(cmd, options, (error, stdout, stderr) => {
        if( error ) reject(error);
        else resolve({stdout, stderr});
      });
    });
  }

  downloadImage(url, filename) {
    let file = path.join(ROOT_DIR, 'input', filename);
    return new Promise((resolve, reject) => {
      let ws = fs.createWriteStream(file);
      ws.on('close', () => resolve(file));
      request(url)
        .pipe(ws)
        .on('error', e => reject(e));
    });
  }
}

let worker = new WorkerImpl('sloan-ocr');
worker.listen();
const {Worker, cloudStorage} = require('@ucd-lib/job-autoscaler-commons');
const logger = require('@ucd-lib/job-autoscaler-commons/lib/logger');
const path = require('path');
const fs = require('fs-extra');
const request = require('request');
const {exec} = require('child_process');

const ROOT_DIR = '/storage';

class WorkerImpl extends Worker {
  constructor(name) {
    super(name);
    this.processed = 0;
    this.exitOnComplete = process.env.KEEPALIVE ? false : true;

    this.R_CMD = 'Rscript --vanilla --quiet';
  }
  
  async run() {
    logger.info('starting segment', this.segment);
    logger.info('exitOnComplete', this.exitOnComplete);

    await fs.emptyDir(ROOT_DIR);
    await fs.mkdirp(path.join(ROOT_DIR,'input'));
    await fs.mkdirp(path.join(ROOT_DIR,'output'));

    try {
      if( this.segment.ocr ) {
        await this.runOCR();
      }
      if( this.segment.processOcr ) {
        await this.processOCR();
      }
      if( this.segment.parseItems ) {
        await this.parseItems();
      }
      if( this.segment.exportCsv ) {
        await this.exportCsv();
      }
    } catch(e) {
      logger.error('error running segment', e, this.segment);
    }

    logger.info('segment complete', this.segment);

    this.onSuccess();
  }


  async runOCR() {
    if( !this.segment.force && !this.segment.forceOcr ) {
      let ocrCloudFile = path.join(this.segment.pathId, path.parse(this.segment.filename).name+'_data1.RDS');
      let exists = await this._gcsFileExists(ocrCloudFile);
      if( exists ) {
        logger.info('Ignoring IMAGE OCR, product exists and no force flag set', this.segment.imageUrl);
        return;
      }
    }
    logger.info('Running image ocr for ', this.segment.imageUrl);

    await this.downloadImage(this.segment.imageUrl, this.segment.filename);

    // Rscript for_justin_run_wine_price_tables.R /io/input /io/output TRUE 
    let {stdout, stderr} = await this.exec(
      `${this.R_CMD} run_wine_price_tables.R FILESET=${ROOT_DIR}/input/${this.segment.filename} DATA.OUTPUT.DIR=${ROOT_DIR}/output OCR.ONLY=TRUE`
    )
    await this._uploadFiles(stdout, stderr, 'ocr');
  }

  async processOCR() {
    if( !this.segment.force && !this.segment.forceProcessOCR ) {
      let ocrCloudFile = path.join(this.segment.pathId, path.parse(this.segment.filename).name+'.RDS');
      if( await this._gcsFileExists(ocrCloudFile) ) {
        logger.info('Ignoring OCR PROCESSING, product exists and no force flag set', this.segment.imageUrl);
        return;
      }
    }
    logger.info('Running ocr processing for ', this.segment.imageUrl);

    // download ocr r data file
    let ocrFile = path.join(ROOT_DIR,'output',path.parse(this.segment.filename).name+'_data1.RDS');
    if( !fs.existsSync(ocrFile) ) {
      let gcsFile = path.join(this.segment.pathId, path.parse(this.segment.filename).name+'_data1.RDS');
      await cloudStorage.getFile(gcsFile, ocrFile);
    }

    // image is required as well
    await this.downloadImage(this.segment.imageUrl, this.segment.filename);

    let {stdout, stderr} = await this.exec(
      `${this.R_CMD} run_wine_price_tables.R FILESET=${ROOT_DIR}/input OUTPUT.DIR=${ROOT_DIR}/output DATA.INPUT.DIR=${ROOT_DIR}/output`
    );

    await this._uploadFiles(stdout, stderr, 'process');
  }

  async parseItems() {
    if( !this.segment.force && !this.segment.forceParseItems ) {
      let ocrCloudFile = path.join(this.segment.pathId, 'parsed_items.RDS');
      if( await this._gcsFileExists(ocrCloudFile) ) {
        logger.info('Ignoring PARSE ITEMS, product exists and no force flag set', this.segment.imageUrl);
        return;
      }
    }
    logger.info('Running parse items for ', this.segment.imageUrl);

    let processedFile = path.join(ROOT_DIR,'output',path.parse(this.segment.filename).name+'.RDS');
    if( !fs.existsSync(processedFile) ) {
      let gcsFile = path.join(this.segment.pathId, path.parse(this.segment.filename).name+'.RDS');

      if( !(await this._gcsFileExists(gcsFile)) ) {
        logger.info(`Ignoring PARSE ITEMS, ${gcsFile} does not exist`, this.segment.imageUrl);
        return;
      }

      await cloudStorage.getFile(gcsFile, processedFile);
    }

    let {stdout, stderr} = await this.exec(
      `${this.R_CMD} run_parse_items.R name.input.dir=${ROOT_DIR}/output name.output.dir=${ROOT_DIR}/output`
    );

    await this._uploadFiles(stdout, stderr, 'parse_items');
  }

  async exportCsv() {
    if( !this.segment.force && !this.segment.forceExportCsv ) {
      let ocrCloudFile = path.join(this.segment.pathId, 'PRICE_NAME.csv');
      if( await this._gcsFileExists(ocrCloudFile) ) {
        logger.info('Ignoring EXPORT CSV, product exists and no force flag set', this.segment.imageUrl);
        return;
      }
    }
    logger.info('Running export csv for ', this.segment.imageUrl);

    let parsedItemsFile = path.join(ROOT_DIR,'output','parsed_items.RDS');
    if( !fs.existsSync(parsedItemsFile) ) {
      let gcsFile = path.join(this.segment.pathId, 'parsed_items.RDS');

      if( !(await this._gcsFileExists(gcsFile)) ) {
        logger.info(`Ignoring EXPORT CSV, ${gcsFile} does not exist`, this.segment.imageUrl);
        return;
      }

      await cloudStorage.getFile(gcsFile, parsedItemsFile);
    }

    let {stdout, stderr} = await this.exec(
      `${this.R_CMD} run_wine_database_one_page.R truth.dir=/opt/dsi/Data in=${ROOT_DIR}/output/parsed_items.RDS`
    );

    await this._uploadFiles(stdout, stderr, 'export_csv');
  }

  async _uploadFiles(stdout, stderr, suffix='') {
    // write stdout/stderr
    let ioRoot = path.join(ROOT_DIR,'output');
    await fs.writeFile(path.join(ioRoot, `stdout_${suffix}.txt`), stdout);
    await fs.writeFile(path.join(ioRoot, `stderr_${suffix}.txt`), stderr);

    // upload files to cloud storage
    let files = await fs.readdir(path.join(ROOT_DIR,'output'));

    await Promise.all(
      files.map(file => cloudStorage.addFile(
        path.join(ioRoot, file),
        path.join(this.segment.pathId, file)
      ))
    );
  }

  async _gcsFileExists(gcsFile) {
    let resp = await cloudStorage.getRootBucket().file(gcsFile).exists();
    return resp[0];
  }

  exec(cmd, options={}) {
    options.cwd = '/opt/dsi/scripts';

    logger.info('Worker running', cmd, options);
    return new Promise((resolve, reject) => {
      exec(cmd, options, (error, stdout, stderr) => {
        if( error ) reject(error);
        else resolve({stdout, stderr});
      });
    });
  }

  downloadImage(url, filename) {
    let file = path.join(ROOT_DIR, 'input', filename);
    if( fs.existsSync(file) ) return;

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
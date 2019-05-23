let Crawler = require('./lib/crawler');

const Master = require('@ucd-lib/sloan-ocr-autoscaler-worker/model/master');
const Job = require('@ucd-lib/sloan-ocr-autoscaler-worker/model/job');
const cloudStorage = require('@ucd-lib/sloan-ocr-autoscaler-worker/lib/cloud-storage');

(async function() {
  console.log('ensuring bucket');
  await cloudStorage.createRootBucketIfNotExists();

  console.log('crawling');
  let crawler = new Crawler('sherry-lehmann');
  let images = await crawler.getImageUrls({
    max: 2,
    root: '/catalogs/d7q30n'
  });

  let job = new Job();
  job.addProcesses('sloan-ocr');

  for( let image of images ) {
    let task = job.addTask({
      title: 'sherry-lehmann ocr test'
    });

    image.ocr = true;
    image.processOcr = true;

    job.addTaskSegment(task.id, image);
  }

  console.log('starting');
  await Master.init('sloan-ocr');
  await Master.startJob(job);

})();


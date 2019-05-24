let Crawler = require('./lib/crawler');

const {Master, Job, cloudStorage, config} = require('@ucd-lib/job-autoscaler-commons');

config.cloudStorage.rootBucketName = 'sloan-ocr';
config.pubsub.topic = 'sloan-ocr';
config.pubsub.subscription = 'sloan-ocr-sub';
config.firebase.collection = 'sloan-ocr';

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
      url : image.imageUrl,
      filename: image.filename
    });

    image.ocr = true;
    image.processOcr = true;
    image.force = true;

    job.addTaskSegment(task.id, image);
  }

  console.log('starting');
  await Master.init('sloan-ocr');
  await Master.startJob(job);
})();


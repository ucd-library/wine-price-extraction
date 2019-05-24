let Crawler = require('./lib/crawler');

const {Master, Job, cloudStorage, config} = require('@ucd-lib/job-autoscaler-commons');

config.cloudStorage.rootBucketName = 'sloan-ocr';
config.pubsub.topic = 'sloan-ocr';
config.pubsub.subscription = 'sloan-ocr-sub';
config.firebase.collection = 'sloan-ocr';

(async function() {
  console.log('ensuring bucket');
  await cloudStorage.createRootBucketIfNotExists();

  let crawler = new Crawler('sherry-lehmann');
  let images = await crawler.getImageUrls({
//    max: 100,
    root: [
      "/catalogs/d79w2m",
      "/catalogs/d7ps3c", 
      "/catalogs/d72013",
      "/catalogs/d7z30t", 
      "/catalogs/d7v88n", 
      "/catalogs/d7b01k", 
      "/catalogs/d77p4p",
      "/catalogs/d73s33", 
      "/catalogs/d7mw2d", 
      "/catalogs/d7j01f", 
      "/catalogs/d7k01r", 
      "/catalogs/d7sg6b", 
      "/catalogs/d73w2r", 
      "/catalogs/d7101s", 
      "/catalogs/d76k5q", 
      "/catalogs/d70597", 
      "/catalogs/d7f598", 
      "/catalogs/d78g6p", 
      "/catalogs/d7c889", 
      "/catalogs/d7ms3r", 
      "/catalogs/d7vc79", 
      "/catalogs/d7qp41", 
      "/catalogs/d7kw23", 
      "/catalogs/d7w88z", 
      "/catalogs/d7g59k", 
      "/catalogs/d7pp4q", 
      "/catalogs/d7s881", 
      "/catalogs/d7bc7n", 
      "/catalogs/d7g01t", 
      "/catalogs/d72s3s", 
      "/catalogs/d76p4c", 
      "/catalogs/d7tg6n", 
      "/catalogs/d7d59z", 
      "/catalogs/d7h014", 
      "/catalogs/d74s3d", 
      "/catalogs/d7b880", 
      "/catalogs/d7qs3p", 
      "/catalogs/d7001g", 
      "/catalogs/d7ds3w", 
      "/catalogs/d7ws37", 
      "/catalogs/d7pk52", 
      "/catalogs/d7388t", 
      "/catalogs/d7988p", 
      "/catalogs/d75k5d", 
      "/catalogs/d7h59w", 
      "/catalogs/d7js34", 
      "/catalogs/d71s3g", 
      "/catalogs/d7x30h", 
      "/catalogs/d75p42", 
      "/catalogs/d7nk5r"
    ]
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
    // image.force = true;

    job.addTaskSegment(task.id, image);
  }

  console.log('starting', images.length);
  await Master.init('sloan-ocr');
  await Master.startJob(job);
})();


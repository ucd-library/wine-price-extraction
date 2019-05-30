let Crawler = require('./lib/crawler');
const path = require('path');

process.env.SERVICE_ACCOUNT_PATH = path.join(__dirname, '..', 'service-account.json');

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
      "/D-005/d79w2m",
      "/D-005/d7ps3c",
      "/D-005/d72013",
      "/D-005/d7z30t",
      "/D-005/d7v88n",
      "/D-005/d7b01k",
      "/D-005/d77p4p",
      "/D-005/d73s33",
      "/D-005/d7mw2d",
      "/D-005/d7j01f",
      "/D-005/d7k01r",
      "/D-005/d7sg6b",
      "/D-005/d73w2r",
      "/D-005/d7101s",
      "/D-005/d76k5q",
      "/D-005/d70597",
      "/D-005/d7f598",
      "/D-005/d78g6p",
      "/D-005/d7c889",
      "/D-005/d7ms3r",
      "/D-005/d7vc79",
      "/D-005/d7qp41",
      "/D-005/d7kw23",
      "/D-005/d7w88z",
      "/D-005/d7g59k",
      "/D-005/d7pp4q",
      "/D-005/d7s881",
      "/D-005/d7bc7n",
      "/D-005/d7g01t",
      "/D-005/d72s3s",
      "/D-005/d76p4c",
      "/D-005/d7tg6n",
      "/D-005/d7d59z",
      "/D-005/d7h014",
      "/D-005/d74s3d",
      "/D-005/d7b880",
      "/D-202/d7qs3p",
      "/D-202/d7001g",
      "/D-202/d7ds3w",
      "/D-202/d7ws37",
      "/D-202/d7pk52",
      "/D-202/d7388t",
      "/D-202/d7988p",
      "/D-202/d75k5d",
      "/D-202/d7h59w",
      "/D-202/d7js34",
      "/D-202/d71s3g",
      "/D-202/d7x30h",
      "/D-202/d75p42",
      "/D-202/d7nk5r"
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


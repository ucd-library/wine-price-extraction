var router = require('express').Router();

router.use('/words', require('./words'));

module.exports = router;
const router = require('express').Router();
const model = require('../../lib/words');

router.get('/:ark', async (req, res) => {
  let ark = req.params.ark;

  try {
    res.json(await model.getWords(ark));
  } catch(e) {
    res.status(400).json({
      error: true,
      message: e.message,
      stack: e.stack
    });
  }
});

module.exports = router;
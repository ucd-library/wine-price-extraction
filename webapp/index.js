const express = require('express');
const app = express();

require('./controllers/static')(app);
app.use(require('./controllers'));

app.listen(3000, () => {
  console.log('App listening on port 3000');
});
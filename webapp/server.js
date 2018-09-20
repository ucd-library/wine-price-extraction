const express = require('express');
const app = express();

require('./controllers/static')(app);
app.use(require('./controllers'));

app.listen(process.env.PORT || 4000, () => {
  console.log('App listening on port '+(process.env.PORT || 4000));
});
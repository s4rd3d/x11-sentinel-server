const util = require('util')
const express = require('express')
const fs = require('fs')
const path = require('path')

const app = express()
const port = process.env.EVALUATION_PORT;
const responseDir = process.env.EVALUATION_RESPONSE_DIR;

app.use(express.json({limit: '50mb'}));

app.post('/profile', (req, res) => {
  console.log('profile build request');
  filePath = path.join(responseDir, 'build_profile.json');
  fs.readFile(filePath, (err, data) => {
    if (!err) {
      res.send(data);
    }
    else {
      console.log(err)
    }
  });
})

app.post('/verify', (req, res) => {
  console.log('verification request');
  filePath = path.join(responseDir, 'verify.json');
  fs.readFile(filePath, (err, data) => {
    if (!err) {
      res.send(data);
    }
    else {
      console.log(err)
    }
  });
})

app.listen(port, () => {
  console.log(`App listening on port ${port}`)
})

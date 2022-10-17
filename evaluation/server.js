const util = require('util')
const express = require('express')
const fs = require('fs')
const path = require('path')

const app = express()
const port = process.env.EVALUATION_PORT;
const responseDir = process.env.EVALUATION_RESPONSE_DIR;
const profileBuilddWaitTime = process.env.EVALUATION_PROFILE_BUILD_WAIT_TIME;
const verifyWaitTime = process.env.EVALUATION_VERIFY_WAIT_TIME;

app.use(express.json({limit: '50mb'}));

app.post('/profile', (req, res) => {
  console.log('profile build request');
  filePath = path.join(responseDir, 'build_profile.json');
  fs.readFile(filePath, (err, data) => {
    if (!err) {
      setTimeout(() => {
        console.log('profile build request succeeded');
        res.send(data);
      }, profileBuilddWaitTime)
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
      setTimeout(() => {
        console.log('verify request succeeded');
        res.send(data);
      }, verifyWaitTime)
    }
    else {
      console.log(err)
    }
  });
})

app.listen(port, () => {
  console.log(`App listening on port ${port}`)
})

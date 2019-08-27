var fs = require('fs');
var glob = require('glob');
var path = require('path');
const { exec } = require('child_process');

//const { Elm }  = require('./Top.elm'); -- do it this way if using webpack-loader
const {
  Elm
} = require('./elm.js');

const app = Elm.Top.init();

glob("examples/*.elm", function(er, files) {
  files.forEach(function(file) {
    fs.readFile(file, 'utf8', function(err, contents) {
      var filename = path.basename(file);
      app.ports.modelInPort.send([filename, contents]);
    });
  });
});

app.ports.codeOutPort.subscribe(request => {
  fs.writeFile('pre/' + request[0], request[1], (err) => {
    if (err) throw err;
  })
});

glob("pre/*.elm", function(er, files) {
  files.forEach(function(file) {
    fs.readFile(file, 'utf8', function(err, contents) {
      var filename = path.basename(file);

      exec('"elm-format" pre/' + filename + '  --output post/' + filename);
    });
  });
});

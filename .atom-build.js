
var start = undefined;

var T =
  { cmd: "make ARG={FILE_ACTIVE} atom-build-any"
  , name: "make atom-build-any"
  , cwd: "{PROJECT_PATH}"
  , keymap: "cmd-U"
  , atomCommandName: "user:build"
  , preBuild: function() { start=new Date(); atom.notifications.addInfo('Build started', {dismissable: false}); }
  , postBuild: function(res) {
      if (res) { atom.notifications.addSuccess('Build successifull / '+((new Date() - start) / 1000).toString() + 'sec', {dismissable: true}); }
      // else { atom.notifications.addError('Build failed', {dismissable: true}); }
    }
  , functionMatch: function(termout) {
      var matches = [];
      var stripProjectRoot = function(s){
        var x = s.replace(/^ */, '');

        var editor = atom.workspace.getActivePaneItem();
        var file = editor.buffer.file;
        var mode;

        var filePath = file.path;
        if (filePath.indexOf('frontend') >= 0) {
          mode = 'frontend';
        } else {
          mode = 'backend';
        }

        if (x[0] == "/") {
          /* abs path - compiler error */
          var pr = __dirname;
          var y = x.replace(pr, '');
          if (y[0] == '/') {
            return y.substr(1);
          } else {
            return y;
          }
        } else {
          /* rel path - import error */
          if (mode == 'frontend') {
            return "frontend/" + x;
          } else {
            return "backend/" + x;
          }
        }
      }
      termout.split(/\n/).forEach(function (rawline, rawlinenum, rawlines) {
        // var match = rawline.match(/(.+\.hs):(\d+):(\d+):/);
        var match = rawline.match(/(.+\.hs):(\d+):(\d+):\s*$/); // exclude warnings
        if (match) {
          var ctx = rawlines[rawlinenum];
          var ctx2 = ["show ->"]; // rawlines.slice(rawlinenum+1, rawlinenum + 6);
          var prefixLen = function(s) { if (s) {return s.match(/^ */)[0].length } else {return 0}};
          for (var i = rawlinenum+1; i<rawlinenum+55; i++) {
            if(prefixLen(rawlines[i]) > 4) { ctx2.push(rawlines[i]); }
          }

          matches.push({
              file: stripProjectRoot(match[1])
            , line: match[2]
            , col: match[3]
            , message: ctx
            , trace: [{ file: stripProjectRoot(match[1])
                      , line: match[2]
                      , col: match[3]
                      , message: ctx2.join("\n")
                      }]
          });
        }
      });
      return matches;
    }
  }

module.exports = T

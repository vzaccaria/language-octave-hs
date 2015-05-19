global.jQuery = global.$ = require('jquery')
require('jq-console')
require('./client.less')
require('./materialize.css')

var jqconsole = global.$('#console').jqconsole('MiniOctave — (c) Vittorio Zaccaria, 2015\n\n', '> ');

global.cliAsk = function (cb) {
  jqconsole.Prompt(true, cb)
}

global.cliPrint = function (data) {
  jqconsole.Write(data + '\n', 'jqconsole-output');
}

global.jQuery(function () {
  global.main()
})
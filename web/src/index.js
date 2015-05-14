global.jQuery = global.$ = require('jquery')
require('jq-console')
require('./client.less')
require('./materialize.css')

var jqconsole = global.$('#console').jqconsole('MiniOctave — (c) Vittorio Zaccaria, 2015\n\n', '> ');

var startPrompt = function () {
  // Start the prompt with history enabled.
  jqconsole.Prompt(true, function (input) {
    // Output input with the class jqconsole-output.
    jqconsole.Write(input + '\n', 'jqconsole-output');
    // Restart the prompt.
    startPrompt();
  });
};
startPrompt();
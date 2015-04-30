var {
  generateProject
} = require('diy-build')

var path = require('path')

generateProject(_ => {

  _.ghc = (dir, ...deps) => {
    var command = (_) => `ghc ${_.source} -o ${_.product}`
    var product = (_) => `./${path.basename(_.source, '.hs')}.x`
    _.compileFiles(...([command, product, dir].concat(deps)))
  }

  _.collectSeq("all", _ => {
    _.collect("build", _ => {
      _.ghc("OctaveGrammar.hs", "*.hs")
    })
    _.cmd("./OctaveGrammar.x")
  })

  _.collect("update", _ => {
    _.cmd("make clean && ../node_modules/.bin/babel configure.js | node")
  });

})
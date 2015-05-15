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

  _.ghcjs = (dir, ...deps) => {
    var command = (_) =>
      `ghcjs -DGHCJS_BROWSER ${_.source} -o HS.x
\techo "(function(global) {" > ${_.product}
\tcat HS.x/{rts,lib,out}.js ./Exports.js >> ${_.product}
\techo "})(window);" >> ${_.product}
\trm -rf HS.x
`
    var product = (_) => `./${path.basename(_.source, '.hs')}.js`
    _.compileFiles(...([command, product, dir].concat(deps).concat('Exports.js')))
  }

  _.collectSeq("all", _ => {
    _.collect("build", _ => {
        _.ghcjs("Octave.hs", "*.hs")
      })
      // _.cmd("./Octave.x")
  })

  _.collect("update", _ => {
    _.cmd("make clean && ../node_modules/.bin/babel configure.js | node")
  });

  _.collect("c", _ => {
    _.cmd("rm -rf Octave.x *.hi *.o *.x *.js_hi *.js_o")
    _.cmd("make clean")
  })

})
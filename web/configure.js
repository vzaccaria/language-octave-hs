var {
  generateProject
} = require('diy-build');

generateProject(_ => {
  "use strict"

  _.browserify = (dir, ...deps) => {
    var command = (_) => `../node_modules/.bin/browserify -t babelify -t node-lessify  ${_.source} -o ${_.product}`
    var product = (_) => `${_.source.replace(/\..*/, '.bfd.js')}`
    _.compileFiles(...([command, product, dir].concat(deps)))
  }

  _.jadeify = (dir, ...deps) => {
    var command = (_) => `../node_modules/.bin/jade ${_.source}`
    var product = (_) => `${_.source.replace(/\..*/, '.html')}`
    _.compileFiles(...([command, product, dir].concat(deps)))
  }

  _.copy = (dir, ...deps) => {
    var command = (_) => `cat ${_.source} > ${_.product}`
    var product = (_) => `${_.source}.cpy.js`
    _.compileFiles(...([command, product, dir].concat(deps)))
  }

  _.collect("all", _ => {

    _.toFile("_site/index.html", _ => {
      _.jadeify("src/index.jade")
    })

    _.toFile("_site/client.js", _ => {
      _.concat(_ => {
        _.browserify("src/index.js", "src/*.less")
      })
      _.copy("./lib/Octave.js")
    })
  })

  _.collectSeq("update", _ => {
    _.cmd("../node_modules/.bin/babel ./configure.js | node")
  })

  _.collect("start", _ => {
    _.startWatch("_site/**/*")
    _.startServe("_site")
  })

  _.collect("stop", _ => {
    _.stopWatch()
    _.stopServe()
  })
})
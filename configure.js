var {
  generateProject
} = require('diy-build')

var path = require('path')

var initializeHooks = _ => {
  "use strict"

  _.babel = (dir, ...deps) => {
    var command = (_) => `./node_modules/.bin/babel ${_.source} -o ${_.product}`
    var product = (_) => `./lib/${path.basename(_.source)}`
    _.compileFiles(...([command, product, dir].concat(deps)))
  }

  _.verb = (verbfile, deps) => {
    var command = () => `./node_modules/.bin/verb`
    var product = () => `./readme.md`
    _.compileFiles(...([command, product, verbfile].concat(deps)))
  }

  _.ghcjs = (dir, ...deps) => {
    var command = (_) =>
      `ghcjs -DGHCJS_BROWSER -ihs ${_.source} -o HS.x
\techo "(function(global) {" > ${_.product}
\tcat HS.x/{rts,lib,out}.js ./hs/Exports.js >> ${_.product}
\techo "})(window);" >> ${_.product}
\trm -rf HS.x
`
    var product = (_) => `./${path.basename(_.source, '.hs')}.js`
    _.compileFiles(...([command, product, dir].concat(deps).concat('./hs/Exports.js')))
  }

  _.browserify = (dir, ...deps) => {
    var command = (_) => `./node_modules/.bin/browserify -t babelify -t node-lessify  ${_.source} -o ${_.product}`
    var product = (_) => `${_.source.replace(/\..*/, '.bfd.js')}`
    _.compileFiles(...([command, product, dir].concat(deps)))
  }

  _.jadeify = (dir, ...deps) => {
    var command = (_) => `./node_modules/.bin/jade ${_.source}`
    var product = (_) => `${_.source.replace(/\..*/, '.html')}`
    _.compileFiles(...([command, product, dir].concat(deps)))
  }
}

var produceWeb = _ => {
  "use strict"
  _.collect("build-web", _ => {
    _.toFile("_site/index.html", _ => {
      _.jadeify("web/src/index.jade")
    })

    _.toFile("_site/client.js", _ => {
      _.concat(_ => {
        _.browserify("web/src/index.js", "web/src/*.less", "web/src/*.css")
      })
      _.ghcjs("./hs/Octave.hs")
    })
  })
}

var produceHaskell = _ => {
  "use strict"
  _.collect("build-haskell", _ => {
    _.toFile("./lib/Octave.js", _ => {
      _.ghcjs("./hs/Octave.hs", "./hs/*.hs")
    })
  })
}

var produceDocs = _ => {
  "use strict"
  _.collectSeq("docs", _ => {
    // _.cmd("./node_modules/.bin/markdox ./index.js -o docs/api.md")
    _.verb("./verbfile.js", "docs/*.md")
  })
}

var cleanTargets = _ => {
  _.collect("clean-dev", _ => {
    _.cmd("make clean")
    _.cmd("rm -rf hs/*.js_o")
    _.cmd("rm -rf hs/*.js_hi")
  })
}



generateProject(_ => {
  "use strict"

  initializeHooks(_)

  _.collect("all", _ => {
    _.collectSeq("build", _ => {
      produceHaskell(_)
      produceWeb(_)
    })
    produceDocs(_)
  })

  _.collect("test", _ => {
    _.cmd("make all")
    _.cmd("./node_modules/.bin/mocha ./lib/test.js")
  })

  _.collect("update", _ => {
    _.cmd("make clean && ./node_modules/.bin/babel configure.js | node")
  });

  ["major", "minor", "patch"].map(it => {
    _.collect(it, _ => {
      _.cmd(`make all`)
      _.cmd(`./node_modules/.bin/xyz -i ${it}`)
    })

  })

  cleanTargets(_)

  _.collect("start", _ => {
    _.startWatch("_site/**/*")
    _.startServe("_site")
  })

  _.collect("stop", _ => {
    _.stopWatch()
    _.stopServe()
  })
})
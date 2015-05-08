/// <reference path="../typings/lodash/lodash.d.ts"/>

var _ = require('lodash');
var math = require('mathjs');

var _global = {

}

function isMatrix(m) {
  "use strict"
  return (m instanceof math.type.Matrix)
}

function copyOf(_env) {
  "use strict"
  return _.cloneDeep(_env)
}

function evalSym(_curenv, symbol, ...indexes) {
  "use strict"
  var sym = _curenv[symbol]

  if (indexes.length > 2) {
    throw "Sorry, cant use more than two indexes at the moment"
  }
  if (indexes.length === 0) {
    return copyOf(_curenv[symbol])
  } else {
    if (indexes.length === 1) {
      if (!isMatrix(sym)) {
        throw "Sorry, unexpected index."
      } else {
        return 2122;
      }
    }
  }
}

function mcreate(_curenv, symbol, value) {
  "use strict"
  _curenv[symbol] = value
}


function meval(_curenv, symbol, ...indexes) {
  "use strict";

  if (_.isUndefined(_curenv[symbol]) || _.isNull(_curenv[symbol])) {
    throw "Symbol is not defined at this level"
  }

  if (_.isFunction(_curenv[symbol])) {
    /**
     * Invoke the function with a copy of the local environment
     */
    return _curenv[symbol](copyOf(_global), ...indexes)
  } else {
    /**
     * We now know that it is a variable.
     */
    evalSym(_curenv, symbol, ...indexes)
  }
}

module.exports = {
  meval,
  mcreate
}
var chai = require('chai')
chai.use(require('chai-as-promised'))
var should = chai.should()

/*global describe, it, before, beforeEach, after, afterEach */
var mod = require('./octave')

var statementTests = [
  // ["foo;", "It works! just lexed: (foo)"],
  // ["'foo';", "It works! just lexed: (foo)"],
  // ["121;", "It works! just lexed: (121)"],
  // ["121; x;", "It works! just lexed: (121, x)"],
  // ["121; x ;", "It works! just lexed: (121, x)"]
]

var exprTests = [
  ["1+1", "ok: BinOp \"+\" (ConstI 1) (ConstI 1)"],
  ["-1+1", "ok: BinOp \"+\" (Unop \"-\" (ConstI 1)) (ConstI 1)"],
  ["[1 2 3]", "ok: Matrix [Row [ConstI 1,ConstI 2,ConstI 3]]"],
  ["[1 2; 3 4]", "ok: Matrix [Row [ConstI 1,ConstI 2],Row [ConstI 3,ConstI 4]]"],
  ["1+1:2:3", "ok: Range (Range (BinOp \"+\" (ConstI 1) (ConstI 1)) (ConstI 2)) (ConstI 3)"],
  [":", "ok: Default"],
  ["'a'", "ok: Str \"a\""],
  ["a:b", "ok: Range (Eval \"a\" []) (Eval \"b\" [])"],
  ["(1+1*(3-2))", "ok: BinOp \"+\" (ConstI 1) (BinOp \"*\" (ConstI 1) (BinOp \"-\" (ConstI 3) (ConstI 2)))"],
  ["a(1,1)", "ok: Eval \"a\" [ConstI 1,ConstI 1]"],
  ["a(:)", "ok: Eval \"a\" [Default]"],
  ["['a' 'b']", "ok: Matrix [Row [Str \"a\",Str \"b\"]]"],
  ["['a','b']", "ok: Matrix [Row [Str \"a\",Str \"b\"]]"],
  ["[a'  'b']", "ok: Matrix [Row [Tran (Eval \"a\" []),Str \"b\"]]"],
  ["[1+2 3]", "ok: Matrix [Row [BinOp \"+\" (ConstI 1) (ConstI 2),ConstI 3]]"]
]

var trTests = [
  // ["a(1,1)", "ok: meval(ce, \"a\", 1, 1)"],
  // ["a(:)", "ok: meval(ce, \"a\", __oct__default__)"],
  // ["bi", "ok: meval(ce, \"bi\")"],
  // ["a(1:3)", "ok: meval(ce, \"a\", range(1, 3))"],
  // ["a(1:2:3)", "ok: meval(ce, \"a\", range(1, 2, 3))"],
  // ["1+1", "ok: bop(\"+\", 1, 1)"],
  // ["'1'", "ok: \"1\""]
]


describe('#module', () => {
  "use strict"
  it('should load the module', () => {
    should.exist(mod.alive)
    mod.alive().should.equal("Hei, I am alive")
  })
  statementTests.map(_ => {
    it(`Should parse ${_[0]}`, () => {
      mod.justParseStatements(_[0]).should.equal(_[1])
    })
  })
  exprTests.map(_ => {
    it(`Should parse ${_[0]}`, () => {
      mod.justParseExpression(_[0]).should.equal(_[1])
    })
  })
  trTests.map(_ => {
    it(`Should parse ${_[0]}`, () => {
      mod.justTranslateExpression(_[0]).should.equal(_[1])
    })
  })
})
var chai = require('chai')
chai.use(require('chai-as-promised'))
var should = chai.should()

/*global describe, it, before, beforeEach, after, afterEach */
var mod = require('./octave')

var statementTests = [
  ["foo;", "It works! just lexed: (foo)"],
  ["'foo';", "It works! just lexed: (foo)"],
  ["121;", "It works! just lexed: (121)"],
  ["121; x;", "It works! just lexed: (121, x)"],
  ["121; x ;", "It works! just lexed: (121, x)"]
]

var exprTests = [
  ["1+1", "ok: BinOp \"+\" (ConstI 1) (ConstI 1)"],
  ["-1+1", "ok: BinOp \"+\" (Unop \"-\" (ConstI 1)) (ConstI 1)"],
  ["[1 2 3]", "ok: Matrix [Row [ConstI 1,ConstI 2,ConstI 3]]"],
  ["[1 2; 3 4]", "ok: Matrix [Row [ConstI 1,ConstI 2],Row [ConstI 3,ConstI 4]]"],
  ["1+1:2:3", "ok: Cons (Cons (BinOp \"+\" (ConstI 1) (ConstI 1)) (ConstI 2)) (ConstI 3)"],
  ["[1+2 3]", "ok: Matrix [Row [BinOp \"+\" (ConstI 1) (ConstI 2),ConstI 3]]"]
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
})
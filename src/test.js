var chai = require('chai')
chai.use(require('chai-as-promised'))
var should = chai.should()

/*global describe, it, before, beforeEach, after, afterEach */
var mod = require('./octave')

describe('#module', () => {
  "use strict"
  it('should load the module', () => {
    should.exist(mod.alive)
    mod.alive().should.equal("Hei, I am alive")
  })
  it('should load the lexer', () => {
    should.exist(mod.justLex)
    mod.justLex("foo").should.equal("It works! just lexed: foo")
  })
  it('should load the lexer', () => {
    should.exist(mod.justLex)
    mod.justLex("'foo'").should.equal("It works! just lexed: foo")
  })
  it('should be able parse a number', () => {
    should.exist(mod.justLex)
    mod.justLex("121").should.equal("It works! just lexed: 121")
  })
  it('should be able parse a complex string', () => {
    should.exist(mod.justLex)
    mod.justLex("121, 'x',123").should.equal("It works! just lexed: 121x123")
  })
})
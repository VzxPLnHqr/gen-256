package vzxplnhqr.examples

import cats.effect._
import cats.effect.syntax.all._
import cats.implicits._
import cats.syntax.all._

import scoin._
import scodec.bits._

import spire.math._

trait RandomBitcoinScriptUtils {

    val randomIO: IO[std.Random[IO]]

    def randomBytes(length: Int): IO[ByteVector] = 
        randomIO
        .flatMap(_.nextBytes(length))
        .flatMap(bs => IO(ByteVector(bs)))

    // all valid mainnet opcodes from https://en.bitcoin.it/wiki/Script
    val allowableScriptElts = 
        (ScriptElt.elt2code 
            -- List(OP_CAT, OP_SUBSTR, OP_LEFT, OP_RIGHT, 
            OP_INVERT, OP_AND, OP_OR, OP_XOR, OP_2MUL, OP_2DIV, OP_MUL, 
            OP_DIV, OP_MOD, OP_LSHIFT, OP_RSHIFT)
            -- List(OP_CODESEPARATOR, OP_CHECKSIG, OP_CHECKSIGVERIFY, OP_CHECKMULTISIG, OP_CHECKMULTISIGVERIFY)
            -- List(OP_INVALIDOPCODE, OP_RESERVED, OP_VER, OP_VERIF, OP_VERNOTIF, OP_RESERVED1, OP_RESERVED2)
            -- List(OP_RETURN, OP_CHECKLOCKTIMEVERIFY, OP_CHECKSEQUENCEVERIFY)
            -- List(OP_RIPEMD160, OP_SHA1, OP_SHA256, OP_HASH160, OP_HASH256)
        ).map((e,v) => e).toList

    // if we introduce a pseudo opcode that, if selected meant "end the script here",
    // what probability should it be selected?
    val probOfTerminating: Double = 1.0 / (allowableScriptElts.size + 1).toDouble

    def randomScriptElt: IO[ScriptElt] = 
        randomIO.flatMap(_.betweenInt(0,allowableScriptElts.size))
                .map(allowableScriptElts(_))
    
    def randomPushData: IO[List[ScriptElt]] = for {
        // max push is 520 bytes (https://github.com/bitcoin/bips/blob/master/bip-0342.mediawiki#Resource_limits)
        length <- randomIO.flatMap(_.betweenInt(0,521))
        bytes <- randomBytes(length).map(bs => List(OP_PUSHDATA(bs)))
    } yield bytes

    /**
      * Naively generate a random script which should parse ok on first-pass.
      * 1. Start from random allowable script element.
        2. if element is data push, push random data
        3. ensure script length is not greater than max allowed 
      *
      * */
    def randomScriptfromRandomBytes: IO[List[ScriptElt]] = for {
        elem <- randomScriptElt
        thenTerminate <- randomIO.flatMap(_.nextDouble).map(_ <= probOfTerminating)
        script <- (elem, thenTerminate) match {
                    case (OP_PUSHDATA1 | OP_PUSHDATA2 | OP_PUSHDATA4,false) =>
                        randomPushData.flatMap(xs => randomScriptfromRandomBytes.map(ys => xs ++ ys))
                    case (OP_PUSHDATA1 | OP_PUSHDATA2 | OP_PUSHDATA4,true) =>
                        randomPushData
                    case (e, false)=>
                        randomScriptfromRandomBytes.map(xs => xs.appended(e))
                    case (_, true) => 
                        IO(List.empty)
                }
    } yield script


    def randomScript(length: Int) = 
        (0 until length).toList.traverse(i => randomScriptElt)

    val prog0 = IO.println(s"${allowableScriptElts.size} allowable op codes:\n $allowableScriptElts") >> 
        (for {
        _ <- IO.print("gen random script?")
        _ <- IO.readLine
        script <- randomScriptfromRandomBytes
        _ <- IO.println(s"random ${script.length} element script: " + script.mkString(" "))
        scriptBytes <- IO(Script.write(script))
        parsedScript <- IO(Script.parse(scriptBytes)).handleErrorWith(e => IO.println("parse error: "+ e.getMessage()))
    } yield ()).foreverM
}
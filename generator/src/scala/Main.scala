package vzxplnhqr

import cats.effect._
import cats.effect.syntax.all._
import cats.implicits._
import cats.syntax.all._

import scoin._
import scodec.bits._

object Main extends IOApp.Simple {

    val randomIO = std.Random.scalaUtilRandom[IO]

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
            -- List(OP_RETURN)
        ).map((e,v) => e).toList

    def randomScriptElt: IO[ScriptElt] = 
        randomIO.flatMap(_.betweenInt(0,allowableScriptElts.size))
                .map(allowableScriptElts(_))


    def randomScript(length: Int) = 
        (0 until length).toList.traverse(i => randomScriptElt)
    
    val run = IO.println(s"allowable op codes: $allowableScriptElts") >> 
        (for {
        _ <- IO.print("how many bytes of random script?")
        numBytes <- IO.readLine.map(_.toInt)
        script <- randomScript(numBytes)
        _ <- IO.println(s"random $numBytes script: " + script.mkString(" "))
        scriptBytes <- IO(Script.write(script))
        parsedScript <- IO(Script.parse(scriptBytes)).handleErrorWith(e => IO.println("parse error: "+ e.getMessage()))
    } yield ()).foreverM
}


package vzxplnhqr

import cats.effect._
import cats.effect.syntax.all._
import cats.implicits._
import cats.syntax.all._

import scoin._
import scodec.bits._

import spire.math._
object StringSearch extends examples.StringSearchExample with IOApp.Simple {

    val randomIO = std.Random.scalaUtilRandom[IO]  
    val run = evolveN(List(mary,fox),5000)
                .flatMap(_.parTraverse(bs => IO(geneticString.fromBytes(bs)).flatMap(s => IO.println("========> " + s)))).as(())
    

}

object RandomBitcoinScripts extends examples.RandomBitcoinScriptUtils with IOApp.Simple {
    val randomIO = std.Random.scalaUtilRandom[IO]
    val run = prog0
}
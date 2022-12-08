package vzxplnhqr.examples

import vzxplnhqr._

import cats.effect._
import cats.effect.syntax.all._
import cats.implicits._
import cats.syntax.all._

import scoin._
import scodec.bits._

import spire.math._

trait StringSearchExample {
    val randomIO: IO[std.Random[IO]]

    val mary = ByteVector("the quick brown fox jumped over the sleeping dog".getBytes)
    val fox = ByteVector("mary had a little lamb whose fleece was white as snow".getBytes)
    val target = ByteVector("My father had a small estate in Nottinghamshire; I was the third of five sons. He sent me to Emanuel College in Cambridge, at fourteen years old, where I resided three years, and applied myself close to my studies; but the charge of maintaining me, although I had a very scanty allowance, being too great for a narrow fortune, I was bound apprentice to Mr. James Bates.".getBytes)
    val targetBits = target.bits
    val targetString = String(target.toArray)
    val geneticString = new Genetic[String] {
        def fromBytes(bytes: ByteVector): String = String(bytes.toArray)
    }

    // naive fitness function for strings: 
    //    number of bits which are the 
    //    same as the target string with bonus if strings are similar in length
    val simpleFitnessFn: String => String => IO[BigInt] = 
        target => candidate => for {
        _ <- IO.unit
        candidate_bits = ByteVector(candidate.getBytes)
        target_bits = ByteVector(target.getBytes)
        length_dif = math.abs(target_bits.size - candidate_bits.size) 
        maxLength = math.min(candidate_bits.length.toInt, target_bits.length.toInt)
        lengthPenalty = math.max(candidate_bits.length.toInt, target_bits.length.toInt) - length_dif
        // inefficient bitwise comparison up to whichever ends first
        score <- (0 until maxLength).toList.traverse{
                    i => IO(target_bits(i) == candidate_bits(i))
                }.map(_.count(_ == true)).map(score => BigInt(score + lengthPenalty))
    } yield score

    def evolve(numGenerations: Int, printEvery: Int) = Range(0,numGenerations).toList.foldM(List(mary,fox)){ 
        case(pop, i)=> if( i % printEvery == 0 ) {
            IO.println(s"Generation $i ===============================")
            >> randomIO.flatMap(_.betweenInt(0,pop.size)).flatMap(r => IO(pop(r)))
            .flatMap(b => simpleFitnessFn(targetString)(geneticString.fromBytes(b)).map(score => (b,score)))
            .flatMap((b,s) => IO.println(s"--$s-->" + String(b.toArray)))
            >> randomIO.flatMap(rand => Evolver.iterate(simpleFitnessFn(targetString))(pop,100)(geneticString,rand))
        } else {
             randomIO.flatMap(rand => Evolver.iterate(simpleFitnessFn(targetString))(pop,100)(geneticString,rand))
        }
    }
}

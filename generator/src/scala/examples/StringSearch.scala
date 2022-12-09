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
    val simpleFitnessFn: String => String => IO[BigInt] = 
        target => candidate => for {
        candidate_bytes <- IO(ByteVector(candidate.getBytes))
        target_bytes <- IO(ByteVector(target.getBytes))
        longestPossiblePrefix <- IO(math.min(target_bytes.size,candidate_bytes.size))
        score <- ???
    } yield (score)

    def evolveN(startingPop: List[ByteVector], numGenerations: Int, printEvery: Int = 10): IO[List[ByteVector]] = for {
        randIO <- randomIO
        fitnessFn <- IO(simpleFitnessFn(targetString))
        evolveOnce = (cur_pop:List[ByteVector]) => Evolver.iterateOnce(fitnessFn)(cur_pop, newPopSize = 100)(geneticString,randIO)
        finalPop <-(1 to numGenerations).toList.foldLeftM(startingPop){
            case (newPop, i) => if( i % printEvery == 0 ) {
                evolveOnce(newPop).flatTap(_ => printGenerationSummary(newPop, i).start)
            } else evolveOnce(newPop)
        }
    } yield finalPop

    def printGenerationSummary(pop: List[ByteVector], generationNumber: Int): IO[Unit] = for {
        _ <- IO.println(s"==== Generation $generationNumber ===============")
        _ <- IO.println(s"-------population size: ${pop.size}")
        lengthSortedPop <- pop.parTraverse(c => IO(c.size)).map(_.sorted)
        medLength <- IO(lengthSortedPop(pop.size / 2))
        fitnessSortedPop <- pop.parTraverse(c => IO(geneticString.fromBytes(c)).flatMap(s => simpleFitnessFn(targetString)(s))).map(_.sorted)
        medFitness <- IO(fitnessSortedPop(pop.size / 2))
        targetFitness <- simpleFitnessFn(targetString)(targetString)
        _ <- IO.println(s"-------target length: ${target.size} bytes")
        _ <- IO.println(s"-------median length: $medLength bytes")
        _ <- IO.println("--------------------------------------")
        _ <- IO.println(s"-------median fitness: $medFitness")
        _ <- IO.println(s"-------target fitness: $targetFitness")
        i <- std.Random.scalaUtilRandom[IO].flatMap(_.betweenInt(0,pop.size))
        candidate = pop(i)
        candidate_repr <- IO(geneticString.fromBytes(candidate))
        candidate_score <- simpleFitnessFn(targetString)(candidate_repr)
        _ <- IO.println(s"------candidate $i, score $candidate_score, size ${candidate.size} bytes ----> ")
        _ <- IO.println(candidate_repr)
    } yield ()
}

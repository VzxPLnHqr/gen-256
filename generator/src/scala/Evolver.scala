package vzxplnhqr

import cats.effect._
import cats.effect.syntax.all._
import cats.implicits._
import cats.syntax.all._

import scoin._
import scodec.bits._

import spire.math._

trait Genetic[A]{
    /**
      * fixed length genome representation only
      *
      * @param bytes
      * @return
      */
    def fromBytes(bytes: ByteVector): A
}

object Evolver {
    def iterateOnce[A](fitness: A => IO[BigInt])
                  (currentPop: List[ByteVector], newPopSize: Int)
                  (implicit genetic: Genetic[A], randomIO: std.Random[IO]): IO[List[ByteVector]] = for {
        
        // apply fitness function to each member of population
        // returns List[A,BigInt]
        scoredPop <- currentPop.parTraverse{ 
                        candidate_bytes => for {
                            repr <- IO(genetic.fromBytes(candidate_bytes))
                            score <- fitness(repr)
                        } yield (candidate_bytes,score)
                    }

        // now build a new population by sampling from the old one
        // predetermined/fixed population size
        newPop <- (1 to newPopSize).toList.parTraverse{ 
                    i => for {
                        candidates <- sampleFromWeightedList(scoredPop)(randomIO)
                                        .both(sampleFromWeightedList(scoredPop)(randomIO))
                        crossed <- crossover(candidates._1._1,candidates._2._1)(randomIO)
                        mutated <- mutate(crossed)(randomIO)
                        score <- IO(genetic.fromBytes(mutated)).flatMap(repr => fitness(repr))
                        // select the fittest between the mutant and the parents
                        //selected <- IO(List((candidates._1._1, candidates._1._2), (candidates._2._1, candidates._2._2), (mutated,score))).map(_.maxBy(_._2))
                    } yield mutated //selected._1
                }
    } yield newPop

    /**
      * select a random big integer
      * (currently uses scala.util.Random)
      *
      * @param minInclusive
      * @param maxExclusive
      * @param randomIO
      * @return
      */
    def betweenBigInt(minInclusive: BigInt, maxExclusive: BigInt)
        (implicit randomIO: std.Random[IO]): IO[BigInt] = for {
            diff <- IO(maxExclusive - minInclusive)
            bitlength <- IO(diff.bitLength)
            r <- IO(BigInt(bitlength,scala.util.Random)).iterateUntil(_ < diff)
        } yield minInclusive + r

    /**
     *  Random selection from weighted list.
      * A solution that runs in O(n) would be to start out with selecting the first element. 
      * Then for each following element either keep the element you have or replace 
      * it with the next one. Let w be the sum of all weights for elements considered 
      * so far. Then keep the old one with probability w/(w+x) and choose the new 
      * one with p=x/(w+x), where x is the weight of the next element.
      * Source: https://stackoverflow.com/questions/4511331/randomly-selecting-an-element-from-a-weighted-list
      *
      * @param pop
      * @return
      */
    def sampleFromWeightedList[A]( pop: List[(A,BigInt)])
                                 (implicit randomIO: std.Random[IO]): IO[(A,BigInt)] =
        pop.foldLeftM((BigInt(0),Option.empty[(A,BigInt)])){ 
            case ((accum_score, incumbent), (candidate, candidate_score)) => 
                for {
                    new_accum_score <- IO(accum_score + candidate_score)
                    winner <- if(new_accum_score == 0)
                                IO(Some((candidate,candidate_score)))
                              else
                                betweenBigInt(0, new_accum_score)
                                .map(_ < candidate_score)
                                .ifM(
                                    ifTrue = IO(Some((candidate,candidate_score))),
                                    ifFalse = IO(incumbent)
                                )
                } yield (new_accum_score, winner)
        }.flatMap(r => (IO.fromOption(r._2)(new RuntimeException("No candidate selected!"))))
        
    /**
     * perform random naive crossover between two byte vectors:
        1. for each individual pick a crossover point
        3. take the head of lhs and tail of rhs
     * */
    def crossover(lhs: ByteVector, rhs: ByteVector)
        (implicit random: std.Random[IO]): IO[ByteVector] = for {
            i <- random.betweenInt(0,lhs.size.toInt + 1)
            j <- random.betweenInt(0,rhs.size.toInt + 1)
            bytes <- IO(lhs.take(i)).map(_ ++ rhs.drop(j))
            //_ <- IO.println(bytes.size - lhs.size)
        } yield bytes

    /**
     * naive mutation where probability of a byte mutating is proportional
     * to the length of the gnome */
    def mutate(input: ByteVector)(implicit random: std.Random[IO]): IO[ByteVector] = {
        val probOfMutation = if(input.size == 0) 0 else 1.0 / input.size.toDouble
        // note: this does not allow for a mutation to "delete" a byte, but it probably should
        IO(input).map(_.toArray.toList).flatMap(_.parTraverse(b => mutateByte(probOfMutation,b)).map(ByteVector(_)))
    }

    def mutateByte(p: Double, b: Byte)(implicit random: std.Random[IO]): IO[Byte] = 
        random.nextDouble.map(_ <= p).flatMap{ 
                case true => random.nextBytes(1).map(_.head)
                case false => IO(b)
    }
}



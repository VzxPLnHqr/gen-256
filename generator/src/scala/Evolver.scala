package vzxplnhqr

import cats.effect._
import cats.effect.syntax.all._
import cats.implicits._
import cats.syntax.all._

import scoin._
import scodec.bits._

import spire.math._

trait Genetic[A]{
    def fromBytes(bytes: ByteVector): A
}

object Evolver {
    def iterate[A](fitness: A => IO[BigInt])
                  (pop: List[ByteVector], newPopSize: Int)
                  (implicit genetic: Genetic[A], randomIO: std.Random[IO]): IO[List[ByteVector]] = for {
        
        // apply fitness function to each member of population
        // returns List[A,BigInt]
        scoredPop <- pop.parTraverse{ 
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
                        crossed <- crossover(candidates._1,candidates._2)(randomIO)
                        mutated <- mutate(crossed)(randomIO)
                    } yield mutated
                }
    } yield newPop

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
                                 (implicit randomIO: std.Random[IO]): IO[A] =
        pop.foldLeftM((BigInt(0),Option.empty[A])){ 
            case ((accum_score, incumbent), (candidate, candidate_score)) => 
                for {
                    new_accum_score <- IO(accum_score + candidate_score)
                    p <- IO(Real(candidate_score) / Real(new_accum_score)).map(_.toDouble)
                    winner <- randomIO.nextDouble.map(_ <= p)
                                .ifM(
                                    ifTrue = IO(Some(candidate)),
                                    ifFalse = IO(incumbent)
                                )
                } yield (new_accum_score, winner)
        }.flatMap(r => IO.fromOption(r._2)(new RuntimeException("No candidate selected!")))

    /**
     * perform random naive crossover between two byte vectors:
        1. pick a point on the lhs
        2. pick a point on the rhs
        3. take the tail of rhs and head of lhs
     * */
    def crossover(lhs: ByteVector, rhs: ByteVector)
        (implicit random: std.Random[IO]): IO[ByteVector] = for {
            r <- random.betweenInt(0,lhs.size.toInt + 1)
                    .both(random.betweenInt(0,rhs.size.toInt + 1))
            bytes <- IO(lhs.take(r._1)).map(_ ++ rhs.drop(r._2))
        } yield bytes

    /**
     * naive mutation where probability of a byte mutating is proportional
     * to the length of the gnome */
    def mutate(input: ByteVector)(implicit random: std.Random[IO]): IO[ByteVector] = {
        val probOfMutation = if(input.size == 0) 0 else 1.0 / input.size.toDouble

        // note: this code does not currently ever "delete" a byte from the genome
        // but it probably should occassionally do that
        def mutateByte(p: Double, b: Byte): IO[Byte] = 
            random.nextDouble.map(_ <= p)
            .flatMap{ 
                case true => random.nextBytes(1).map(_.head)
                case false => IO(b)
            }
        input.toArray.toList.parTraverse(b => mutateByte(probOfMutation,b)).map(ByteVector(_))
    }
}



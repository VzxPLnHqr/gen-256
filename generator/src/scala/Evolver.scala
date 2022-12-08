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
                  (implicit g: Genetic[A], random: std.Random[IO]): IO[List[ByteVector]] = {

        val scoredPop = pop.traverse{ 
            bs => for {
                b <- IO(bs)
                repr <- IO(g.fromBytes(bs))
                score <- fitness(repr)
            } yield (b,score)
        }

        val wheel = scoredPop.flatMap(rouletteWheel(_))

        //fixed population size
        (1 to newPopSize).toList.parTraverse{_ => 
            for {
                lhs_r <- random.nextDouble
                rhs_r <- random.nextDouble
                lhs <- wheel.flatMap(w => IO.fromOption(w.apply(lhs_r))(new RuntimeException("lhs candidate does not exist!")))
                rhs <- wheel.flatMap(w => IO.fromOption(w.apply(lhs_r))(new RuntimeException("rhs candidate does not exist!")))
                crossed <- crossover(lhs,rhs)(random)
                mutated <- mutate(crossed)(random)
            } yield mutated
        }
    }

    type RouletteWheel = Double => Option[ByteVector]
    
    /**
     * take a scored population, make a roulette wheel with odds for a
     * candidate proportional to what it contributes to the total fitness
     * of the population */
    def rouletteWheel(pop: List[(ByteVector, BigInt)]): IO[RouletteWheel] = for {
        totalFitness <- IO(pop.map(_._2).foldLeft(BigInt(0))(_ + _))
        popPmf <- IO(pop.map((candidate, score) => (candidate, (Real(score)/Real(totalFitness)).toDouble)).sortBy(_._2))
        popCdf <- IO(popPmf.map(_._2).scanLeft(0.0)(_ + _).drop(1))
        thewheel <- IO(
            (p: Double) => popCdf.zip(popPmf).dropWhile{
                case (accum_p, (c_bytes, c_p)) => accum_p < p
            }.map(_._2._1).headOption
        )
    } yield thewheel

    /**
     * perform randome crossover between two byte vectors:
        1. pick a point on the lhs
        2. pick a point on the rhs
        3. take the tail of rhs and head of lhs
     * */
    def crossover(lhs: ByteVector, rhs: ByteVector)
        (implicit random: std.Random[IO]): IO[ByteVector] = for {
            a <- random.betweenInt(0,lhs.size.toInt + 1)
            b <- random.betweenInt(0,rhs.size.toInt + 1)
            bytes = lhs.take(a) ++ rhs.drop(b)
        } yield bytes

    /**
     * naive mutation where probability of a byte mutating is proportional
     * to the length of the gnome */
    def mutate(input: ByteVector)(implicit random: std.Random[IO]): IO[ByteVector] = {
        val probOfMutation = if(input.size == 0) 0 else 1.0 / input.size.toDouble
        
        def mutateByte(p: Double, b: Byte): IO[Byte] = 
            random.nextDouble.map(_ <= p)
            .flatMap{ 
                case true => random.nextBytes(1).map(_.head)
                case false => IO(b)
            }
        input.toArray.toList.parTraverse(b => mutateByte(probOfMutation,b)).map(ByteVector(_))
    }
}



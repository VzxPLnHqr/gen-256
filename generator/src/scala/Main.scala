package vzxplnhqr

import cats.effect._
import cats.effect.syntax.all._
import cats.implicits._
import cats.syntax.all._

import scoin._
import scodec.bits._

import spire.math._
object StringSearch extends StringSearchExample with IOApp.Simple {

    val randomIO = std.Random.scalaUtilRandom[IO]  
    val run = evolve(5000,1).as(())
    

}

object RandomBitcoinScripts extends RandomBitcoinScriptUtils with IOApp.Simple {
    val randomIO = std.Random.scalaUtilRandom[IO]
    val run = prog0
}

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
    val probOfTerminating: Double = 1 / (allowableScriptElts.size + 1).toDouble

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
        target => candidate => {
        val candidate_bits = ByteVector(candidate.getBytes).bits
        val target_bits = ByteVector(target.getBytes).bits
        val length_dif = math.abs(target_bits.size - candidate_bits.size) 
        val maxLength = math.min(candidate_bits.length.toInt, target_bits.length.toInt)
        val lengthPenalty = math.max(candidate_bits.length.toInt, target_bits.length.toInt) - length_dif
        // inefficient bitwise comparison up to whichever ends first
        (0 until maxLength).toList.parTraverse{
            i => IO(target_bits(i) == candidate_bits(i))
        }.map(_.count(_ == true)).map(score => BigInt(score + lengthPenalty))
    }

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
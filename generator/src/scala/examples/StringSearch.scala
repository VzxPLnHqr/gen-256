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
    val mary_string = "mary had a little lamb whose fleece was white as snow"
    val fox_string = "the quick brown fox jumped over the sleeping dog"
    val target_string = """A purely peer-to-peer version of electronic cash would allow online
payments to be sent directly from one party to another without going through a
financial institution. Digital signatures provide part of the solution, but the main
benefits are lost if a trusted third party is still required to prevent double-spending.
We propose a solution to the double-spending problem using a peer-to-peer network.
The network timestamps transactions by hashing them into an ongoing chain of
hash-based proof-of-work, forming a record that cannot be changed without redoing
the proof-of-work. The longest chain not only serves as proof of the sequence of
events witnessed, but proof that it came from the largest pool of CPU power. As
long as a majority of CPU power is controlled by nodes that are not cooperating to
attack the network, they'll generate the longest chain and outpace attackers. The
network itself requires minimal structure. Messages are broadcast on a best effort
basis, and nodes can leave and rejoin the network at will, accepting the longest
proof-of-work chain as proof of what happened while they were gone."""
    val mary = ByteVector(mary_string.getBytes)
    val fox = ByteVector("mary had a little lamb whose fleece was white as snow".getBytes)
    val target = ByteVector(target_string.getBytes)
    val initialNonRandomPop = (251 to 350).map(ByteVector.empty.padTo(_)).toList
    //val targetBits = target.bits
    //val targetString = String(target.toArray)
    val geneticString = new Genetic[String] {
        //prepend length of bytevector (in hex) to string
        def fromBytes(bytes: ByteVector): String = 
            String(bytes.toArray) //String(bytes.drop(3).toArray).padTo(bytes.take(3).toInt(),'-')
    }

    // naive fitness function for strings:
    val simpleFitnessFn: String => String => IO[BigInt] = 
        target => 
            candidate => for {
            //a naive fitness score based on length first not content
            //unsuitable for strings (targets) longer than approx 1MB in size
            //the 1MB is the "ceiling" and must always be larger than both the candidate 
            target_size <- IO(target.size)
            candidate_size <- IO(candidate.size)
            abs_size_diff <- IO(math.abs(target_size - candidate_size))
            score <- IO(BigInt(1000000 - abs_size_diff))
            /*xorred_bits <- IO(abs_size_diff).map(_.toBinaryString.reverse.padTo(20,'0').reverse)
            score <- (0 until 20).toList.parTraverse(i => xorred_bits(i) match {
                case '0' => IO(BigInt(2).pow(i))
                case _ => IO(BigInt(0))
            }).map(_.sum)*/
            /*target_bits <- IO(ByteVector(target.getBytes))
            candidate_bits <- IO(ByteVector(candidate.getBytes))
            xorred <- IO(target_bits.xor(candidate_bits))
            score <- (0 until xorred.size.toInt)
                        .toList.foldLeftM(BigInt(0)){
                        case (accum, i) => (accum << 8, xorred(i)) match {
                            case (coeff, b) if b != 0 => IO(coeff+b.toInt)
                            case (coeff, b) => IO(coeff)
                        }
            }*/
    } yield (score)

    def evolveN(startingPop: List[ByteVector], numGenerations: Int, printEvery: Int = 10): IO[List[ByteVector]] = for {
        randIO <- randomIO
        fitnessFn <- IO(simpleFitnessFn(geneticString.fromBytes(target)))
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
        target_string<- IO(target).map(geneticString.fromBytes(_))
        lengthSortedPop <- pop.parTraverse(c => IO(c.size)).map(_.sorted)
        medLength <- IO(lengthSortedPop(pop.size / 2))
        fitnessSortedPop <- pop.parTraverse(c => IO(geneticString.fromBytes(c)).flatMap(s => simpleFitnessFn(target_string)(s))).map(_.sorted)
        medFitness <- IO(fitnessSortedPop(pop.size / 2))
        targetFitness <- simpleFitnessFn(target_string)(target_string)
        _ <- IO.println(s"-------target length: ${target.size} bytes")
        _ <- IO.println(s"-------median length: $medLength bytes")
        _ <- IO.println("--------------------------------------")
        _ <- IO.println(s"-------median fitness: $medFitness")
        _ <- IO.println(s"-------target fitness: $targetFitness")
        i <- std.Random.scalaUtilRandom[IO].flatMap(_.betweenInt(0,pop.size))
        candidate = pop(i)
        candidate_repr <- IO(geneticString.fromBytes(candidate))
        candidate_score <- simpleFitnessFn(target_string)(candidate_repr)
        _ <- IO.println(s"------candidate $i, score $candidate_score, size ${candidate.size} bytes ----> ")
        _ <- IO.println(candidate_repr)
    } yield ()
}

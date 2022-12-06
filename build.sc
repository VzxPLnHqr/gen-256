import mill._, scalalib._

object `generator` extends ScalaModule {
   def scalaVersion = "3.2.0"
   def ivyDeps = Agg(
      ivy"org.typelevel::cats-effect:3.3.12", // for purely functional programming
      ivy"com.fiatjaf::scoin:0.4.0", // for btc data structures
      ivy"org.typelevel::spire:0.18.0" // for number types
      //ivy"org.slf4j:slf4j-nop:1.7.12" // to get rid of a weird logging warning
    )
}
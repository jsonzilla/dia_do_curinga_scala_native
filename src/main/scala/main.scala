
import ddc.Date
import ddc.FrodeCal

object Main extends App {
  println("Entre com dia mes e ano (separados por espaco):")

  val input: _root_.scala.Predef.String = scala.io.StdIn.readLine()
  val lines: Array[String] = input.split(" ")

  if (lines.length == 3) {
    val d = new Date(lines.apply(2).toInt,
                     lines.apply(1).toInt,
                     lines.apply(0).toInt)

    println(FrodeCal.Complete(d))
    println(FrodeCal.Compact(d))
  }
  else {
    println("Digitou errado")
  }
}

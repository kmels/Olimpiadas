import scala.io.Source

/**
 * Created by kmels on 15/08/14.
 */
object Problema1 {

  def ejecutar = {
    println("Ejecutando problema 1 ...")

    val (_, lines) = readLinesFrom("Entrada-P1.txt")

    val mappedLines = lines.map(bits => showIP(bits))

    writeLinesTo("Salida-P2.txt", mappedLines)
  }

  def showIP(bits: String): String = {
    def showOctect(octet : String):Int = octet.reverse.zipWithIndex.map({ case (n,i) => (Integer.parseInt(n.toString) * Math.pow(2,i)) }).sum.toInt

    val oct1 = bits.take(8)
    val oct2 = bits.drop(8).take(8)
    val oct3 = bits.drop(16).take(8)
    val oct4 = bits.drop(24).take(8)

    return showOctect(oct1) + "." + showOctect(oct2) + "." + showOctect(oct3) + "." + showOctect(oct4)
  }

  def writeLinesTo(fname: String, lines: List[String]) = {
    val p = new java.io.PrintWriter(fname)
    lines.foreach(l => p.write(l+"\n"))
    p.close()
  }

  def readLinesFrom(fname: String): (Int, List[String]) = {
    val ls: List[String] = Source.fromFile(fname).getLines().toList
    return (ls.head.toInt, ls.tail)
  }
}

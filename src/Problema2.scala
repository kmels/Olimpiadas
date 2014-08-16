/**
 * Created by kmels on 15/08/14.
 */

import java.util
import java.util.Date

import scala.collection.JavaConversions._
import scala.io.Source

object Problema2 {

  type Soduku = Array[Array[Int]]

  def ejecutar = {
    println("Ejecutando problema 2...")

    val (n_casos, casos) = readSodukusFrom("Entrada-P2.txt")

    val solvedSudokus = casos.map(soduku => solve(soduku))

    writeSodukusTo("Salida-P2.txt", solvedSudokus)
  }

  def solve(sudoku: Soduku): Soduku = {
    println("Solving soduku of size " + sudoku.length + "...")

    def benchmark[E](e: => E): E = {
      val now = new Date().getTime
      val r = e
      val after = new Date().getTime

      //println(" took... "+ (after - now).toString + " ms")
      r
    }

    def isSolved(s: Soduku): Boolean = {
      val firstLineSum: Int = s.head.sum
      val equalLineSum = s.forall(line => line.sum == firstLineSum)

      val firstColSum: Int = s.map(line => line.head).sum

      //for (c <- 1 to 2){
        val colItems = s.map(l => l(8))

        if (colItems.sum != firstColSum)
          return false;
      //}

      return equalLineSum
    }

    val opts: Seq[Int] = (1 to 9).toList
    val perms: Seq[Seq[Int]] = opts.permutations.toList

    // makes the possible perms for a line
    def possibleLinePerms(line: Seq[Int]): Seq[Seq[Int]] = {
      val fixedPositions = line.zipWithIndex.filter({case (n,pos) => (n > 0)})
      def hasFixedNumPos(l: Seq[Int], fixedNumPos: (Int, Int)) = l.get(fixedNumPos._2) == fixedNumPos._1
      perms.filter(perm => fixedPositions.forall(fixedPos => hasFixedNumPos(perm, fixedPos)))
    }

    val possibleLines = sudoku.map( line =>  possibleLinePerms(line) )

    for (l1 <- possibleLines(0)){
      for (l2 <- possibleLines(1)){
        for (l3 <- possibleLines(2)){
          for (l4 <- possibleLines(3)){
            for (l5 <- possibleLines(4)){
              for (l6 <- possibleLines(5)){
                for (l7 <- possibleLines(6)){
                  for (l8 <- possibleLines(7)){
                    for (l9 <- possibleLines(8)){
                      // if the union is solved, return this, else nothing.

                      //print("Checking ...")
                      val sudoku: Soduku = Array(l1.toArray,l2.toArray,l3.toArray,l4.toArray,l5.toArray,l6.toArray,l7.toArray,l8.toArray,l9.toArray)
                      val solved = benchmark(isSolved(sudoku))
                      //println(solved)

                      if (solved) {
                        println("Solved!")
                        return sudoku
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }

    return sudoku
  }

  def writeSodukusTo(fname: String, soduku: Seq[Soduku]) = {
    val p = new java.io.PrintWriter(fname)

    soduku.foreach(soduku_line => {

      soduku_line.foreach(component => p.write(component.mkString("")+"\n"))
      p.write("\n")
    })
    p.close()
  }

  def readSodukusFrom(fname: String): (Int, Seq[Soduku]) = {
    val ls: List[String] = Source.fromFile(fname).getLines().toList

    val n_cases = ls.head.toInt

    val sodukus: util.LinkedList[Soduku] = new util.LinkedList[Soduku]()
    var sodukuLines: List[String] = ls.tail

    def readSoduku(lines: List[String]): Soduku = {
      lines.foreach(l => println(l.mkString(",")))

      def mkLine(xs: String): Array[Int] = xs.map(n => if (n != '.') Integer.parseInt(n.toString) else -1 ).toArray
      lines.map(l => mkLine(l)).toArray
    }

    while(sodukuLines.size > 0){
      while (sodukuLines.head.trim.length == 0)
        sodukuLines = sodukuLines.drop(1);

      sodukus.add(readSoduku(sodukuLines.take(9)))
      sodukuLines = sodukuLines.drop(9)
    }

    return (n_cases, sodukus)
  }
}


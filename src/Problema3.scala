/**
 * Created by kmels on 15/08/14.
 */

import scala.collection.mutable.ListBuffer
import scala.io.Source

object Problema3 {

  case class PushCommand(player: Char, position: Int)
  case class CommandCase(buttons_to_push: Int, command_list: Seq[PushCommand]){
    var now = 0;
    var positions = collection.mutable.Map[(Char,Int), Int]( ('O', 1) -> 1, ('B',1) -> 1)
    var player_pushes = collection.mutable.Map[(Char,Int), Int]()

    var latest_time = 1;

    override def toString = {
      var _s = command_list.mkString(" — ")+"\n"
      _s += "\t\tTime\t+^+^+^\t\t\tOrange\t\t\t\t\t+^+^+^\t\t\tBlue\t\t\t\t+^+^+^\n"
      _s += "+^+^+^+^+^+^+^+^+^+^+^+^+^+^+^+^+^+^+^+^+^+^+^+^+^+^+^+^+^+^+^+^+^+^+^+^+^+^+^+^+^+^+^+^+^+^+^+^+^\n"

      for (t <- (1 to latest_time)){
        //push or move
        val orange_pos = if (player_pushes.get(('O', t)).isDefined) "Push button " + player_pushes.get(('O', t)).get + "\t"
          else {
            val previous_pos = positions.getOrElse(('O', t -1), 1)
            val pos = positions.get(('O', t)).get
            if (previous_pos == pos)
              "Stay at button " + pos
            else
              "Move to button " + pos
          }

        val blue_pos = if (player_pushes.get(('B', t)).isDefined) "Push button " + player_pushes.get(('B',t)).get + "\t"
        else {
          val previous_pos = positions.getOrElse(('B', t - 1), 1)
          val pos = positions.get(('B', t)).get
          if (previous_pos == pos)
            "Stay at button " + pos
          else
            "Move to button " + pos
        }

        _s += "\t\t" + t + "\t\t+^+^+^\t\t" + orange_pos + "\t\t\t+^+^+^\t\t" + blue_pos + "\t\t+^+^+^\n"
      }

      _s;
    }

    def solve: Int = {
      println("Solving case with "+command_list.length + " push commands")

      var commandQueue = command_list;
      val players = List('O','B')

      while (commandQueue.nonEmpty) {
        now = now + 1;

        println("Doing command at t = " + now + ", missing: "+ commandQueue)
        val p@PushCommand(player, pos) = commandQueue.head
        //println("Player is at position "+ positions.ge)
        // if player wants to push, and is in position .. then push
        if (positions.get((player, now)).get == pos) {
          //push button, command completed, stay
          commandQueue = commandQueue.drop(1);
          positions.put((player, now), pos)
          player_pushes.put((player, now), pos)
          println(player + " pushes the button at " + pos)

          //what's the other doing
          val other: Char = players.filter(_ != player).head
          //should he stay or should does he go?
          val other_cmd = commandQueue.find({ case PushCommand(p, pos) => p == other})
          val others_pos = positions.get((player, now)).get
          if (!other_cmd.isDefined) {
            //does not have command attached
            positions.put((other, now), others_pos) //stay
            println(player + " stays at " + pos)
          }else {
            //should he stay to push or should he move?
            val goal = other_cmd.get.position
            if (others_pos == goal) {
              positions.put((other, now), others_pos) //stay
              println(other + " stays at " + pos)
            }else {
              //move towards his goal
              positions.put((other, now), if (goal > others_pos) pos + 1 else others_pos - 1)
              println(other + " moves to " + (if (goal > others_pos) pos + 1 else others_pos - 1))
            }
          }
        } else{
          //player does not want to push here, he moves.
          val goal_pos: Int = pos
          val current_pos: Int = positions.get((player,now)).get
          positions.put((player, now), if (goal_pos > current_pos) current_pos + 1 else current_pos - 1)
          println(player + " moves to " + (if (goal_pos > current_pos) current_pos + 1 else current_pos - 1))

          //should the other move or stay?
          val other: Char = players.filter(_ != player).head
          //should he stay or should does he go?
          val others_cmd = commandQueue.find({ case PushCommand(p, pos) => p == other})
          val others_pos = positions.get((other, now)).get
          player_pushes.put((player, now), pos)

          if (!others_cmd.isDefined) {
            //no command!
            positions.put((other, now), others_pos) //stay there
            println(other+ " stays at " + others_pos)
          } else{
            //either move or push
            val goal = others_cmd.get.position
            if (goal == others_pos){
              //push, command completed, stay
              //commandQueue = commandQueue.drop(1)
              positions.put((other, now), others_pos) //stay
              println(other+ " stays at " + others_pos)
            }else{
              //move towards his goal
              println("AAHA.")
              positions.put((other, now), if (goal > others_pos) others_pos + 1 else others_pos- 1)
              println(other+ " moves to " + (if (goal > others_pos) others_pos + 1 else others_pos- 1))
            }
          }
        }
      }

      latest_time = now
      latest_time
    }
  }

  def ejecutar = {
    println("Ejecutando problema 3...")

    val (n_commands, commandCases) = readCaseCommands("Entrada-P3.txt")
    assert(n_commands == commandCases.length, " Number of command cases at the beginning should be equal to the number of command cases ");
    val solvedCases: Seq[(Int,Int)] = commandCases.zipWithIndex.map({ case (cmd, idx) => (cmd.solve, idx + 1)})

    commandCases.foreach(cc => println(cc.toString))
    writeCaseSimulations("Salida-P3.txt", solvedCases)
  }

  def writeCaseSimulations(fname: String, cases: Seq[(Int, Int)]) = {
    val p = new java.io.PrintWriter(fname)
    cases.foreach(c => p.write("#" + c._2 +": "+ c._1 + "\n"))
    p.close()
  }

  def readCaseCommands(fname: String): (Int, Seq[CommandCase]) = {
    val ls: List[String] = Source.fromFile(fname).getLines().toList

    val n_cases = ls.head.toInt

    def readCommandCase(x: String): CommandCase = {
      val cmps: Array[String] = x.split(' ');
      println(cmps.mkString(".."))

      val n = cmps.head.toInt
      var rest: Array[String] = cmps.tail;
      val commands = new ListBuffer[PushCommand]()

      while(commands.size < n){
        val command_cmps = rest.take(2)
        val command = new PushCommand(command_cmps.head.head, command_cmps.tail.head.toInt)
        rest = rest.drop(2);
        commands += command
      }

      new CommandCase(n, commands)
    }

    val commands = ls.tail.map(c => readCommandCase(c))

    return (n_cases, commands)
  }
}


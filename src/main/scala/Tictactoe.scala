package calico.tictactoe

import org.newdawn.slick.AppGameContainer
import org.newdawn.slick.{BasicGame, GameContainer, Graphics, Color, Input}
import org.newdawn.slick.geom.{Rectangle, Transform, Circle}

/**
 * Example Slick/LWJGL SBT plugin prototype
 * Works like a charm
 */
object TicTacToe {
  def main(args: Array[String]) = {
    val container = new AppGameContainer(new TicTacToe, 800, 600, false);

    container.setTargetFrameRate(60)
    container.start
  }
}

class TicTacToe extends BasicGame("Tic Tac Toe") {
  // The actual player
  var player = "X"

  // Boolean to decide whose turn it is
  var clicked = false

  val xcoord = List(70, 300, 530)
  val ycoord = List(50, 220, 390)

  // A mutable collection holding scores
  var players = scala.collection.mutable.Map("X" -> 0, "O" -> 0, "Cat" -> 0)

  // Nine clickables squares
  var clickables = build 
  
  // Over Message ex: So and So wins!
  var message = ""

  // Game Over
  var gameover = false

  def build = {
    message = ""
    gameover = false
    for (x <- xcoord; y <- ycoord) yield(new Open(x, y))
  }

  override def mouseClicked(button: Int, x: Int, y: Int, clickCount: Int) {
    if(!gameover) {
      clickables.find(c => c.bounds.contains(x, y) && c.owner == "") match {
        case Some(cell) => {
          cell.owner = player
          clicked = true
        }
        case None =>
      }
    }
  }

  override def keyPressed(key: Int, c: Char) {
    // Pressing ESC will start the game over
    if(gameover && key == Input.KEY_ESCAPE) {
      clickables = build
    }
  }

  override def init(gc: GameContainer) {
  }

  override def update(gc: GameContainer, delta: Int) {
    if(clicked) {
      player = if(player == "X") "O" else "X"
      clicked = false
    }

    // Check for Win/Lose condition
    if(!gameover) {
      for(person <- List("X", "O")) {
        // Horizontal Win
        for (x <- xcoord) {
          checkThree((c => c.x == x && c.owner != ""), person)
        }

        // Vertical Win
        for (y <- ycoord) {
          checkThree((c => c.y == y && c.owner != ""), person)
        }

        // Diagonal Win
        checkThree((c => (c.x == xcoord(0) && c.y == ycoord(0) && c.owner != "") ||
                         (c.x == xcoord(1) && c.y == ycoord(1) && c.owner != "") ||
                         (c.x == xcoord(2) && c.y == ycoord(2) && c.owner != "")), person)

        checkThree((c => (c.x == xcoord(2) && c.y == ycoord(0) && c.owner != "") ||
                         (c.x == xcoord(1) && c.y == ycoord(1) && c.owner != "") ||
                         (c.x == xcoord(0) && c.y == ycoord(2) && c.owner != "")), person)
      }
      
      // If no one wins and all filled, then cat
      if(message == "" && clickables.filter(c => c.owner != "").size == 9) {
        message = "Cat"
        gameover = true
        players.put("Cat", players("Cat") + 1)
      }
    }
  }

  def checkThree(cond: Open => Boolean, person: String) = {
    val line = clickables.filter(cond)
    val same = line.foldLeft(true)((in, c) => c.owner == person && in)
    if(line.size == 3 && same) {
      gameover = true
      message = person + " Wins!"
      players.put(person, players(person) + 1)
    }
  }

  override def render(gc: GameContainer, g: Graphics) {
    g.setColor(Color.white)
    g.drawString("Player " + player + " turn", 200, 10)
    g.drawString(message, 400, 10)

    // Print scores
    for(((person, wins), index) <- players.zipWithIndex) {
      g.drawString(person + ": " + wins, 730, 20 * index)
    }

    // Vertical Lines
    g.setColor(Color.yellow)
    for(position <- List(280, 510)) {
      val line = new Rectangle(position, 50, 20, 490)
      g.draw(line)
      g.fill(line)
    }

    // Horizontal Lines
    for (position <- List(200, 370)) {
      val line = new Rectangle(70, position, 673, 20)
      g.draw(line)
      g.fill(line)
    }

    //Render clickables with or without owners
    clickables.foreach(c => c.render(g))
  }
}

/**
 * A simple clickable square who may or may not have an owner yet
 */
class Open(val x: Int, val y: Int) {
  var owner = ""
  val (width, height) = (209, 149)
  val bounds = new Rectangle(x, y, width, height)

  /**
   * Render nothing, an X or an O depending on the owner of the square
   */
  def render(g: Graphics) {
    owner match {
      case "" => 
      case "X" => {
        g.setColor(Color.red)
        // Have to fudge the x,y coord before the rotation
        val line = new Rectangle(x + 10, y + (height/2) + 26, height, 20)
        val line2 = new Rectangle(x, y + (height/2) - 30, height, 20)
        val first = line transform(Transform.createRotateTransform(0.8.toFloat, x + (width/ 2), y + (height /2)))
        val second = line2 transform(Transform.createRotateTransform(5.4.toFloat, x + (width /2), y + (height /2)))
        g.draw(first)
        g.fill(first)
        g.draw(second)
        g.fill(second)
      }
      case "O" => {
        g.setColor(Color.blue)
        val circle = new Circle(x + (height/2) + 23, y + (width/2) - 25, width/3)
        val circle2 = new Circle(x + (height/2) + 23, y + (width/2) - 25, width/4)
        g.draw(circle)
        g.fill(circle)
        g.draw(circle2)
        g.setColor(Color.black)
        g.fill(circle2)
      }
    }
  }
}
// vim: set ts=4 sw=4 et:

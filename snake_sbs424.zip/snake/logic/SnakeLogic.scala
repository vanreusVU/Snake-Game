package snake.logic

import engine.random.{RandomGenerator, ScalaRandomGen}
import snake.game.{Apple, Direction, East, Empty, GridType, North, SnakeBody, SnakeHead, South, West}
import snake.logic.SnakeLogic._

class StepState(val pSnake : Array[Array[Int]], val pHeadDirection : Direction, val pBodyDirection : Direction, val pAppleLocation : Array[Int], val pTimesGrow : Int){
  var snake : Array[Array[Int]] = pSnake
  var headDirection : Direction = pHeadDirection
  var bodyDirection : Direction = pBodyDirection
  var appleLocation : Array[Int] = pAppleLocation
  var timesGrow : Int = pTimesGrow
}

class SnakeLogic(val randomGen: RandomGenerator,
                 val nrColumns: Int,
                 val nrRows: Int) {

  def this() = this(new ScalaRandomGen(), DefaultRows, DefaultColumns)

  val growAmount : Int = 3
  var reverseMode : Boolean = false
  var movingDirection : Direction = West()

  //Init Step
  var crState : Array[StepState] = Array(new StepState(Array(Array(2,0),Array(1,0),Array(0,0)),
    East(), West(), Array(-1,-1), 0))

  //We spawn our first apple
  currentState.appleLocation = randomAppleLocation()

  def isBodyPart(x : Int, y: Int): Boolean = {
    for(i <- currentState.snake.indices if currentState.snake(i)(0) == x && currentState.snake(i)(1) == y){
      return true
    }
    false
  }

  def numEmptySpace : Int = {
    (nrRows * nrColumns) - currentState.snake.length
  }

  def indexToXY(index : Int): (Int,Int) = {
    (index % nrColumns,(index / nrColumns).floor.toInt)
  }

  def randomAppleLocation(): Array[Int] = {
    if(numEmptySpace != 0) {
      val appleLocationIndex = randomGen.randomInt(numEmptySpace)
      var curIndex = 0
      for (i <- 0 to (nrColumns * nrRows) if !isBodyPart(indexToXY(i)._1, indexToXY(i)._2) ) {
        if (curIndex == appleLocationIndex) {
          return Array(indexToXY(i)._1, indexToXY(i)._2)
        }
        curIndex += 1
      }
    }
    Array(-1,-1) // Means error state
  }

  def eatApple() : Unit = {
    if(isCollidingApple) {
      if ((nrRows * nrColumns) - currentState.snake.length != 0)
        currentState.appleLocation = randomAppleLocation()
      currentState.timesGrow += growAmount
    }
  }

  def isGameOver: Boolean = {
    for(i <- currentState.snake.indices if currentState.snake(i)(0) == currentState.snake.head(0) && currentState.snake(i)(1) == currentState.snake.head(1) && i > 0){
      return true
    }
    false
  }

  def growSnake() : Unit = {
    if(currentState.timesGrow > 0){
      currentState.snake = currentState.snake :+ Array(currentState.snake.last(0), currentState.snake.last(1))
      currentState.timesGrow -= 1
    }
  }

  def currentState : StepState = {
    crState.last
  }

  def moveSnake(): Unit = {
    for (index <- currentState.snake.indices.reverse if (index > 0)) {
      currentState.snake(index)(0) = currentState.snake(index - 1)(0)
      currentState.snake(index)(1) = currentState.snake(index - 1)(1)
    }
    currentState.headDirection match {
      case North() => currentState.snake.head(1) += -1
      case South() => currentState.snake.head(1) += 1
      case West()  => currentState.snake.head(0) += -1
      case East()  => currentState.snake.head(0) += 1
    }

    currentState.snake.head(0) = (currentState.snake.head(0) % nrColumns + nrColumns) % nrColumns
    currentState.snake.head(1) = (currentState.snake.head(1) % nrRows + nrRows) % nrRows
  }

  def isCollidingApple : Boolean = {
    if(currentState.snake.head(0) == currentState.appleLocation.head && currentState.snake.head(1)  == currentState.appleLocation.last){
      return true
    }
    false
  }

  def saveCurrentState() : Unit = {
    crState = crState :+ new StepState(currentState.snake.map(_.clone()),currentState.headDirection, currentState.bodyDirection, currentState.appleLocation.clone(), currentState.timesGrow)
  }

  def step(): Unit = {
    if(!isGameOver && !reverseMode){
      saveCurrentState()
      growSnake()
      moveSnake()
      eatApple()
      currentState.bodyDirection = currentState.headDirection.opposite
    }else if(reverseMode){
      if(crState.length > 1) {
        crState = crState.init
        currentState.headDirection = currentState.bodyDirection.opposite
      }
    }
  }

  def setReverseTime(reverse: Boolean): Unit = {
    reverseMode = reverse
  }

  def changeDir(d: Direction): Unit = {
    if(!isGameOver && d != currentState.bodyDirection){
      currentState.headDirection = d
    }
  }

  def getGridTypeAt(x: Int, y: Int): GridType = {
    for (i <- currentState.snake.indices) {
      if(i == 0 && x == currentState.snake(i)(0) && y == currentState.snake(i)(1)) {
        return SnakeHead(currentState.headDirection)
      }else if (x == currentState.snake(i)(0) && y == currentState.snake(i)(1)) {
        return SnakeBody((i - 1.0f) / 10.0f)
      }
    }
    if(currentState.appleLocation.head == x && currentState.appleLocation.last == y){
      return Apple()
    }
    Empty()
  }
}

object SnakeLogic {
  val DefaultColumns = 5
  val DefaultRows = 5
}
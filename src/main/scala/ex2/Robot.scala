package ex2

type Position = (Int, Int)
enum Direction:
  case North, East, South, West
  def turnRight: Direction = this match
    case Direction.North => Direction.East
    case Direction.East => Direction.South
    case Direction.South => Direction.West
    case Direction.West => Direction.North

  def turnLeft: Direction = this match
    case Direction.North => Direction.West
    case Direction.West => Direction.South
    case Direction.South => Direction.East
    case Direction.East => Direction.North

trait Robot:
  def position: Position
  def direction: Direction
  def turn(dir: Direction): Unit
  def act(): Unit

class SimpleRobot(var position: Position, var direction: Direction) extends Robot:
  def turn(dir: Direction): Unit = direction = dir
  def act(): Unit = position = direction match
    case Direction.North => (position._1, position._2 + 1)
    case Direction.East => (position._1 + 1, position._2)
    case Direction.South => (position._1, position._2 - 1)
    case Direction.West => (position._1 - 1, position._2)

  override def toString: String = s"robot at $position facing $direction"

class DumbRobot(val robot: Robot) extends Robot:
  export robot.{position, direction, act}
  override def turn(dir: Direction): Unit = {}
  override def toString: String = s"${robot.toString} (Dump)"

class LoggingRobot(val robot: Robot) extends Robot:
  export robot.{position, direction, turn}
  override def act(): Unit =
    robot.act()
    println(robot.toString)

class RobotWithBattery(val robot: Robot, var batteryLevel: Int, val turnCost: Int, val actCost: Int) extends Robot:
  export robot.{position, direction}
  override def turn(dir: Direction): Unit =
    if batteryLevel >= turnCost then
      robot.turn(dir)
      batteryLevel -= turnCost

  override def act():Unit =
    if batteryLevel >= actCost then
      robot.act()
      batteryLevel -= actCost

import scala.util.Random
class RobotCanFail(val robot: Robot, val failingProbability: Double) extends Robot:
  private val random = Random()
  export robot.{position, direction}
  private def shouldFail(): Boolean = random.nextDouble() < failingProbability
  override def turn(dir: Direction): Unit =
    if !shouldFail() then
      robot.turn(dir)
  override def act(): Unit =
    if !shouldFail() then
      robot.act()

class RobotRepeated(val robot: Robot, var repetitions: Int) extends Robot:
  export robot.{position, direction}
  override def turn(dir: Direction): Unit =
    for i <- 1 to repetitions do
      robot.turn(dir)
  override def act(): Unit =
    for i <- 1 to repetitions do
      robot.act()



@main def testRobot(): Unit =
  val robot = LoggingRobot(SimpleRobot((0, 0), Direction.North))
  robot.act() // robot at (0, 1) facing North
  robot.turn(robot.direction.turnRight) // robot at (0, 1) facing East
  robot.act() // robot at (1, 1) facing East
  robot.act() // robot at (2, 1) facing East

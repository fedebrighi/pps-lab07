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

class RobotWithBattery(val robot: SimpleRobot, var batteryLevel: Int, val turnCost: Int, val actCost: Int) extends Robot:
  export robot.{position, direction}
  override def turn(dir: Direction): Unit =
    if batteryLevel >= turnCost then
      robot.turn(dir)
      batteryLevel -= turnCost

  override def act():Unit =
    if batteryLevel >= actCost then
      robot.act()
      batteryLevel -= actCost

import ex2.Direction.East

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
  override def toString: String = robot.toString

class RobotRepeated(val robot: Robot, var repetitions: Int) extends Robot:
  export robot.{position, direction}
  override def turn(dir: Direction): Unit =
    for i <- 1 to repetitions do
      robot.turn(dir)
  override def act(): Unit =
    for i <- 1 to repetitions do
      robot.act()
  override def toString: String = robot.toString


@main def testRobot(): Unit =
  val batteryRobot = RobotWithBattery(SimpleRobot((0, 0), Direction.North), batteryLevel = 5, turnCost = 2, actCost = 1)
  println(s"1: ${batteryRobot.robot}, Battery Level: ${batteryRobot.batteryLevel}")
  batteryRobot.act()
  println(s"2: ${batteryRobot.robot}, Battery Level: ${batteryRobot.batteryLevel}")
  batteryRobot.turn(batteryRobot.direction.turnRight)
  println(s"3: ${batteryRobot.robot}, Battery Level: ${batteryRobot.batteryLevel}")
  batteryRobot.turn(batteryRobot.direction.turnRight)
  println(s"4: ${batteryRobot.robot}, Battery Level: ${batteryRobot.batteryLevel}")
  batteryRobot.act()
  println(s"5: ${batteryRobot.robot}, Battery Level: ${batteryRobot.batteryLevel}")

  println("\n")
  val robotCanFail = RobotCanFail(SimpleRobot((0,0), Direction.North), failingProbability = 0.8)
  for _ <- 1 to 5 do
    robotCanFail.act()
    println(s"${robotCanFail.robot.toString}")

  println("\n")
  val repeatedRobot = RobotRepeated(SimpleRobot((0, 0), Direction.North), repetitions = 2)
  println(s"1: ${repeatedRobot.robot}")
  repeatedRobot.act()
  println(s"2: ${repeatedRobot.robot}")
  repeatedRobot.turn(repeatedRobot.direction.turnRight)
  println(s"3: ${repeatedRobot.robot}")
  repeatedRobot.turn(repeatedRobot.direction.turnRight)
  println(s"4: ${repeatedRobot.robot}")
  repeatedRobot.act()
  println(s"5: ${repeatedRobot.robot}")


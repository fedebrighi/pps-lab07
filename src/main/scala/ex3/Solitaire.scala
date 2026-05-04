package ex3
object Solitaire extends App:
  private def render(solution: Seq[(Int, Int)], width: Int, height: Int): String =
    val reversed = solution.reverse
    val rows =
      for y <- 0 until height
          row = for x <- 0 until width
          number = reversed.indexOf((x, y)) + 1
          yield if number > 0 then "%-2d ".format(number) else "X  "
      yield row.mkString
    rows.mkString("\n")

  private val possibleMoves = Seq(
    (3, 0), (-3, 0), (0, 3), (0, -3),
    (2, 2), (2, -2), (-2, 2), (-2, -2)
  )

  private def isInsideBoard(width: Int, height: Int, position: (Int, Int)): Boolean =
    val (x, y) = position
    x >= 0 && y >= 0 && x < width && y < height

  private def placeMarks(board: (Int, Int), currentPosition: (Int, Int), usedPositions: Seq[(Int, Int)]): Seq[Seq[(Int, Int)]] =
    val (width, height) = board
    if usedPositions.size == width * height then
      return Seq(usedPositions)
    val usedSet = usedPositions.toSet
    val candidates =
      for
        (dx, dy) <- possibleMoves
        next = (currentPosition._1 + dx, currentPosition._2 + dy)
        if isInsideBoard(width, height, next)
        if !usedSet.contains(next)
      yield next
    def onwardCount(pos: (Int, Int)) =
      possibleMoves.count { case (dx, dy) =>
        val n = (pos._1 + dx, pos._2 + dy)
        isInsideBoard(width, height, n) && !usedSet.contains(n)
      }
    val ordered = candidates.sortBy(onwardCount)
    for
      nextPos <- ordered
      solution <- placeMarks(board, nextPos, usedPositions :+ nextPos)
    yield solution

  private val width = 5
  private val height = 5
  private val start = (width / 2, height / 2)
  private val solutions = placeMarks((width, height), start, Seq(start))

  solutions.foreach { sol =>
    println(render(sol, width, height))
    println("---")
  }
trait Groupable {
  def groupFilter[A, B](inGroup: (A, Int) => Boolean)(items: Seq[B])(group: A) = 
    items.zipWithIndex.filter {
      case (_, index) => inGroup(group, index)
    }.map {
      case (item, _) => item
    }
}

trait EqualityTester {
  def allEqual[A](group: Seq[A]) : Boolean = group.distinct.size == 1
}

class Board[A](columns: Int, rows: Int, initValue: A) extends Groupable {
  var pieces : Seq[A] = IndexedSeq.fill(rows * columns)(initValue)

  def update(row: Int, column: Int, piece: A) {
    checkBounds(row, column)
    pieces = pieces.updated(row * rows + column, piece)
  }

  def apply(row: Int, column: Int) = {
    checkBounds(row, column)
    pieces(row * rows + column)
  }

  private def checkBounds(row: Int, column: Int) {
    require(column >= 0 && column < columns && row >= 0 && row < rows,
      "Invalid location row: %d, column: %d".format(row, column))
  }

  def groupByColumns : Traversable[Seq[A]] = {
    val columnMapper = groupFilter { (group: Int, index) => column(index) == group }(pieces) _

    0.until(columns).map(columnMapper) 
  }

  def groupByRows : Traversable[Seq[A]] = {
    val rowMapper = groupFilter { (group: Int, index) => row(index) == group }(pieces) _

    0.until(rows).map(rowMapper)
  }

  protected def column(index: Int) = index % rows

  protected def row(index: Int) = index / rows

  override def toString = groupByRows.map(_.mkString("[", ",", "]")).mkString("\n")
}

class SquareBoard[A](size: Int, initValue: A) extends Board[A](size, size, initValue) {
  sealed abstract class Diagonal
  case object TopLeft extends Diagonal
  case object TopRight extends Diagonal

  def groupByDiagonal : Traversable[Seq[A]] = {
    val diagonalMapper = groupFilter { (group: Diagonal, index) =>
      group match {
        case TopLeft => column(index) == row(index)
        case TopRight => row(index) == size - 1 - column(index)
      }
    }(pieces) _

    Set(TopLeft, TopRight).map(diagonalMapper)
  }
}

sealed abstract class BoardPiece
sealed abstract class PlayerPiece extends BoardPiece
case object Blank extends BoardPiece
case object X     extends PlayerPiece
case object O     extends PlayerPiece

class TicTacToeGame extends EqualityTester {
  private val board = new SquareBoard[BoardPiece](3, Blank)

  def update(row: Int, column: Int, piece: PlayerPiece) {
    require(board(row, column) == Blank,
      "A piece has already been placed at row: %d, column: %d".format(row, column))
    board(row, column) = piece
  }

  def winner : Option[PlayerPiece] = collectWinners(board.groupByColumns ++ board.groupByRows ++ board.groupByDiagonal) match {
    case (w: PlayerPiece) :: Nil => Some(w)
    case Nil => None
    case ws => throw new IllegalStateException("Multiple winners: %s".format(ws))
  }

  private def collectWinners(groups: Traversable[Seq[BoardPiece]]) = {
    groups.foldLeft(Seq[BoardPiece]()) { (winners, group) => 
      if (allEqual(group)) group.head +: winners else winners
    }.distinct.filterNot(_ == Blank)
  }

  def boardAsString = board.toString
}

class InteractiveTicTacToe {
  private def input(player: PlayerPiece, group: String) : Int = {
    printf("Enter %s for player %s: ", group, player)
    readInt
  }

  private def inputTurn(player: PlayerPiece) : (Int, Int) =
    (input(player, "row"), input(player, "column"))

  private def turn(player: PlayerPiece, game: TicTacToeGame) {
    println(game.boardAsString)

    try {
      val (row, column) = inputTurn(player)
      game(row, column) = player

      game.winner match {
        case Some(w) => printf("%s\nPlayer %s won\n", game.boardAsString, w)
        case None => turn(if (player == X) O else X, game)
      }
    } catch  {
      case e: Exception => {
        println(e.getMessage)
        turn(player, game)
      }
    }
  }

  def go {
    turn(X, new TicTacToeGame())
  }
}

new InteractiveTicTacToe().go

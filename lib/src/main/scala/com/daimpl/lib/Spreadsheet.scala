package com.daimpl.lib

import rdts.base.{Bottom, Lattice, LocalUid, Uid}
import rdts.datatypes.LastWriterWins
import rdts.datatypes.contextual.ReplicatedList
import rdts.dotted.{Dotted, HasDots}
import rdts.time.{CausalTime, Dot, Dots}

case class Spreadsheet(
  columns: ReplicatedList[Dot] = ReplicatedList.empty,
  rows: ReplicatedList[Dot] = ReplicatedList.empty,
  content: Map[Dot, Map[Dot, LastWriterWins[String | Null]]] = Map.empty
)
{
  def addRow()(using LocalUid)(using context: Dots): Dotted[Spreadsheet] = {
    val nextDot = context.nextDot(LocalUid.replicaId)
    Dotted(Spreadsheet(rows = rows.append(nextDot).data), Dots.single(nextDot))
  }

  def addColumn()(using LocalUid)(using context: Dots): Dotted[Spreadsheet] = {
    val nextDot = context.nextDot(LocalUid.replicaId)
    Dotted(Spreadsheet(columns = columns.append(nextDot).data), Dots.single(nextDot))
  }

  def removeRow(rowIdx: Int)(using LocalUid): Dotted[Spreadsheet] = {
    Dotted(Spreadsheet(), rows.delete(rowIdx).context)
  }

  def removeColumn(colIdx: Int)(using LocalUid): Dotted[Spreadsheet] = {
    Dotted(Spreadsheet(), columns.delete(colIdx).context)
  }

  def insertRow(rowIdx: Int)(using LocalUid)(using context: Dots): Dotted[Spreadsheet] = {
    val nextDot = context.nextDot(LocalUid.replicaId)
    Dotted(Spreadsheet(rows = rows.insert(rowIdx, nextDot).data), Dots.single(nextDot))
  }

  def insertColumn(colIdx: Int)(using LocalUid)(using context: Dots): Dotted[Spreadsheet] = {
    val nextDot = context.nextDot(LocalUid.replicaId)
    Dotted(Spreadsheet(columns = columns.insert(colIdx, nextDot).data), Dots.single(nextDot))
  }

  def editCell(rowIdx: Int, colIdx: Int, content: String | Null) (using LocalUid) : Dotted[Spreadsheet] =
  {
    val dot1 = columns.read(colIdx).get
    val dot2 = rows.read(rowIdx).get

    Dotted(
      Spreadsheet(rows = rows, columns = columns, content = Map(dot1 -> Map(dot2 -> LastWriterWins(CausalTime.now(), content)))),
      Dots.from(Array(dot1, dot2))
    )
  }

  def printToConsole(): Unit = {

    val maxStringLength = content.values.flatMap(_.values).map(_.value.length()).maxOption().getOrElse(1)

    println(s"${columns.toList.size}x${rows.toList.size}")
    for (r <- rows.toList) {
      val line = columns.toList
        .map(c => ("%" + maxStringLength + "s").format(content.get(c)
          .flatMap(_.get(r))
          .map(_.value)
          .getOrElse("·")
        ))
        .mkString("| ", " | ", " |")
      println(line)
    }
  }

  def purgeTombstones()(using LocalUid, Dots): Dotted[Spreadsheet] = {
    val colDelta = columns.purgeTombstones()
    val rowDelta = rows.purgeTombstones()

    Dotted(
      Spreadsheet(colDelta.data, rowDelta.data),
      colDelta.context `union` rowDelta.context
    )
  }
}

object Spreadsheet {
  given Lattice[Spreadsheet] = {
    Lattice.derived
  }

  given hasDots[E]: HasDots[Spreadsheet] = HasDots.derived

  given bottom[E]: Bottom[Spreadsheet] = Bottom.provide(Spreadsheet())
}
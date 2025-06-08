package com.daimpl.lib

import rdts.base.LocalUid
import rdts.time.Dots

trait SpreadsheetLike[D]:
  def numRows: Int

  def numColumns: Int

  def read(colIdx: Int, rowIdx: Int): Option[String]

  def toList: List[List[Option[String]]]

  def addRow()(using LocalUid)(using ctx: Dots): D
  def addColumn()(using LocalUid)(using ctx: Dots): D

  def insertRow(rowIdx: Int)(using LocalUid)(using ctx: Dots): D
  def insertColumn(colIdx: Int)(using LocalUid)(using ctx: Dots): D

  def removeRow(rowIdx: Int)(using LocalUid)(using context: Dots): D
  def removeColumn(colIdx: Int)(using LocalUid)(using context: Dots): D

  def editCell(rowIdx: Int, colIdx: Int, content: String | Null)(using LocalUid)(using context: Dots): D

  def purgeTombstones()(using LocalUid, Dots): D

  def printToConsole(): Unit

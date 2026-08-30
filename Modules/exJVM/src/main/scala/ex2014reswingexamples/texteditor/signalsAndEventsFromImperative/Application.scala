package ex2014reswingexamples.texteditor.signalsAndEventsFromImperative

import reactives.default.*

import scala.swing.BorderPanel.Position
import scala.swing.{BorderPanel, Dimension, Frame, GridPanel, MainFrame, ScrollPane, SimpleSwingApplication}

object Application extends SimpleSwingApplication {
  // reactive components
  val textArea = new TextArea("Lorem ipsum dolor sit amet\nconsectetur adipisicing elit\nsed do eiusmod")

  val positionLabel = new ReactiveLabel(Signal {
    val pos = textArea.caret.position.value
    "Ln " + (pos.row + 1) + " : " + textArea.lineCount.value + "    Col " + (pos.col + 1)
  })

  val selectionLabel = new ReactiveLabel(
    Signal { "Sel " + textArea.selected.value.size }
  )

  val charCountLabel = new ReactiveLabel(Signal { "Ch " + textArea.charCount.value })

  val wordCountLabel = new ReactiveLabel(Signal { "Words " + textArea.wordCount.value })

  val selectAllButton = new ReactiveButton("Select All")
  selectAllButton.clicked observe { _ =>
    textArea.selectAll(); textArea.requestFocus()
  }

  val copyButton = new ReactiveButton("Copy")
  copyButton.clicked observe { _ =>
    textArea.copy(); textArea.requestFocus()
  }

  val pasteButton = new ReactiveButton("Paste")
  pasteButton.clicked observe { _ =>
    textArea.paste(); textArea.requestFocus()
  }

  // layout
  def top: Frame =
    new MainFrame {
      title = "TextEditor (signals0)"
      preferredSize = new Dimension(500, 500)
      contents = new BorderPanel {
        layout(new ScrollPane(textArea)) = Position.Center
        layout(new GridPanel(1, 0) {
          contents += selectAllButton
          contents += copyButton
          contents += pasteButton
        }) = Position.North
        layout(new GridPanel(1, 0) {
          contents += positionLabel
          contents += selectionLabel
          contents += charCountLabel
          contents += wordCountLabel
        }) = Position.South
      }
    }
}

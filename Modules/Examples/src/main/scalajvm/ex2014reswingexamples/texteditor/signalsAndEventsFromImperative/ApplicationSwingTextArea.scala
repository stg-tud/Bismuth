package ex2014reswingexamples.texteditor.signalsAndEventsFromImperative

import reactives.default.*

import scala.math.min
import scala.swing.BorderPanel.Position
import scala.swing.{BorderPanel, Dimension, Frame, GridPanel, MainFrame, ScrollPane, SimpleSwingApplication}

object ApplicationSwingTextArea extends SimpleSwingApplication {
  // reactive components
  val textArea = new ReactiveTextArea("Lorem ipsum dolor sit amet\nconsectetur adipisicing elit\nsed do eiusmod")

  val positionLabel = new ReactiveLabel(Signal {
    val pos  = min(textArea.caretPosSignal.value, textArea.textSignal.value.length)
    val line = textArea.peer.getLineOfOffset(pos);
    val col  = pos - textArea.peer.getLineStartOffset(line);
    "Ln " + (line + 1) + " : " + textArea.lineCountSignal.value + "    Col " + (col + 1)
  })

  val selectionLabel = new ReactiveLabel(
    Signal { "Sel " + (if textArea.selected != null then textArea.selected.length else 0) }
  )

  val charCountLabel = new ReactiveLabel(Signal { "Ch " + textArea.textSignal.value.length })

  val wordCountLabel = new ReactiveLabel(Signal { "Words " + textArea.textSignal.value.length })

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

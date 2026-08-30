package ex2013reswing

import scala.swing.SequentialContainer
import scala.swing.event.{ComponentAdded, ComponentRemoved}

trait ReSequentialContainer extends ReUIElement {
  protected def peer: SequentialContainer

  private def peerContents: CompList = peer.contents.toSeq: CompList

  private def peerContents_=(components: CompList): Unit = {
    peer.contents.clear()
    peer.contents ++= components
    peer.repaint()
    peer.peer.validate
  }

  def contents: ReSwingValue[CompList]

  contents.using(() => peerContents, peerContents_=, classOf[ComponentAdded], classOf[ComponentRemoved])

}


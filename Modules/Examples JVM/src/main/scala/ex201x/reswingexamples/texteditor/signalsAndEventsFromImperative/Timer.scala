package ex201x.reswingexamples.texteditor.signalsAndEventsFromImperative

import reactives.default.*

import javax.swing

class Timer(delay0: Int) {
  val peer: swing.Timer = new swing.Timer(delay0, null) {
    override def fireActionPerformed(e: java.awt.event.ActionEvent): Unit = {
      Timer.this.isRunning `set` this.isRunning(); fired.fire()
    }
  }

  def this(delay: Int, repeating: Boolean) = {
    this(delay)
    this.repeating = repeating
  }

  private val isRunning = Var(true)

  val running: Signal[Boolean]                               = Signal { isRunning.value }
  val fired: Evt[Unit]                                 = Evt[Unit]()
  def delay                                 = peer.getDelay
  def delay_=(delay: Int): Unit                   = peer.setDelay(delay)
  def repeating                             = peer.isRepeats
  def repeating_=(repeating: Boolean): Unit = { peer.setRepeats(repeating); isRunning `set` peer.isRunning() }

  def restart: Timer = { peer.restart(); isRunning `set` peer.isRunning(); this }
  def start: Timer   = { peer.start(); isRunning `set` peer.isRunning(); this }
  def stop: Timer    = { peer.stop(); isRunning `set` peer.isRunning(); this }
}

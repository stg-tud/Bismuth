package ex201x.reswingexamples.texteditor.signalsAndEventsFromImperative

import ex201x.reswingexamples.texteditor.{JScrollableComponent, LineIterator, LineOffset, Position}
import reactives.default.*

import java.awt.datatransfer.{DataFlavor, StringSelection}
import java.awt.{Dimension, Graphics2D, Point, Rectangle, SystemColor, Toolkit}
import scala.language.postfixOps
import scala.math.{max, min}
import scala.swing.*
import scala.swing.event.*

/** A custom text editor component, written directly against Swing and reactives.
  *
  * This replaces the old version that used to extend `ReComponent` from the
  * `reswing` library. It is a plain `scala.swing.Component` that exposes the
  * relevant document state (caret, selection, character/line/word counts) as
  * reactive signals and listens to the raw `Swing` key / mouse events.
  */
class TextArea extends Component with Reactor {
  override lazy val peer: JScrollableComponent & SuperMixin = new JScrollableComponent with SuperMixin
  focusable = true

  protected def stringWidth = peer.metrics.stringWidth
  protected def lineHeight  = peer.unitHeight

  protected val padding   = 5
  protected val clipboard = Toolkit.getDefaultToolkit.getSystemClipboard

  protected lazy val buffer = new GapBuffer

  def this(text: String) = {
    this()
    buffer.insert(text)
    updatePreferredSize()
  }

  private val focusVar: Var[Boolean] = Var(false)
  val focusSignal: Signal[Boolean]   = Signal { focusVar.value }

  private def updatePreferredSize(): Unit = {
    def it = LineIterator(buffer.iterable.readValueOnce)
    preferredSize = new Dimension(2 * padding + it.map(stringWidth(_)).max, (it.size + 1) * lineHeight)
    peer.revalidate()
  }

  val charCount: Signal[Int] = Signal { buffer.length.value } // #SIG

  val lineCount: Signal[Int] = Signal { LineIterator(buffer.iterable.value).size } // #SIG

  val wordCount: Signal[Int] = Signal { // #SIG
    buffer.iterable.value.iterator.foldLeft((0, false)) { (c, ch) =>
      val alphanum = Character.isLetterOrDigit(ch)
      (if alphanum && !c._2 then c._1 + 1 else c._1, alphanum)
    }._1
  }

  val selected: Signal[Iterable[Char]] = Signal { // #SIG
    val (it, dot, mark) = (buffer.iterable.value, caret.dot.value, caret.mark.value)
    val (start, end)    = (min(dot, mark), max(dot, mark))
    new Iterable[Char] { def iterator: Iterator[Char] = it.iterator.slice(start, end) }: Iterable[Char]
  }

  def selectAll(): Unit = {
    caret.dot = charCount.readValueOnce
    caret.mark = 0
  }

  def paste(): Unit = {
    removeSelection()
    val c = clipboard.getContents(null);
    if c.isDataFlavorSupported(DataFlavor.stringFlavor) then {
      val str = c.getTransferData(DataFlavor.stringFlavor).asInstanceOf[String]
      buffer.insert(str)
      caret.offset = caret.offset.readValueOnce + str.length
    }
  }

  def copy(): Unit =
    if selected.readValueOnce.nonEmpty then {
      val s = new StringSelection(selected.readValueOnce.mkString)
      clipboard.setContents(s, s)
    }

  // A caret has a position in the document referred to as a dot.
  // The dot is where the caret is currently located in the model.
  // There is a second position maintained by the caret that represents
  // the other end of a selection called mark.
  // If there is no selection the dot and mark will be equal.
  // [same semantics as for: javax.swing.text.Caret]
  object caret {
    def dot                     = buffer.caret
    def dot_=(value: Int): Unit = buffer.caretChanged.fire(value)

    // dot as position (row and column)
    private val dotPosSignal            = Signal { LineOffset.position(buffer.iterable.value, dot.value) }
    def dotPos                          = dotPosSignal
    def dotPos_=(value: Position): Unit = dot = LineOffset.offset(buffer.iterable.readValueOnce, value)

    private val markVar = Var(0)

    // mark as offset
    private val markSignal       = Signal { markVar.value }
    def mark                     = markSignal
    def mark_=(value: Int): Unit = if value >= 0 && value <= buffer.length.readValueOnce then markVar `set` value

    // mark as position (row and column)
    private val markPosSignal            = Signal { LineOffset.position(buffer.iterable.value, mark.value) }
    def markPos                          = markPosSignal
    def markPos_=(value: Position): Unit = mark = LineOffset.offset(buffer.iterable.readValueOnce, value)

    // caret location as offset
    def offset                     = dot
    def offset_=(value: Int): Unit = {
      dot = value
      mark = value
    }

    // caret location as position (row and column)
    def position                          = dotPos
    def position_=(value: Position): Unit = offset = LineOffset.offset(buffer.iterable.readValueOnce, value)

    protected[TextArea] val blink: Timer             = new Timer(500) start
    protected[TextArea] val steady                   = new Timer(500, false)
    protected[TextArea] val visible: Signal[Boolean] = blink.fired.toggle(
      Signal { focusSignal.value },
      Signal { focusSignal.value && steady.running.value }
    )
  }

  protected def posInLinebreak(p: Int): Boolean =
    p > 0 && p < buffer.length.readValueOnce &&
    buffer(p - 1) == '\r' && buffer(p) == '\n'

  protected def removeSelection(): Unit = {
    val selStart = min(caret.dot.readValueOnce, caret.mark.readValueOnce)
    val selEnd   = max(caret.dot.readValueOnce, caret.mark.readValueOnce)
    caret.offset = selStart
    buffer.remove(selEnd - selStart)
  }

  protected def pointFromPosition(position: Position): Point = {
    val line = LineIterator(buffer.iterable.readValueOnce).drop(position.row).next()
    val y    = position.row * lineHeight
    val x    = stringWidth(line.substring(0, math.min(position.col, line.length)))
    new Point(x + padding, y)
  }

  protected def positionFromPoint(point: Point): Position = {
    val row = point.y / lineHeight
    val it  = LineIterator(buffer.iterable.readValueOnce).drop(row)
    val col =
      if it.hasNext then {
        var prefix = ""
        var col    = 0
        it.next().takeWhile { ch =>
          if stringWidth(prefix + ch) < point.x then { prefix += ch; col += 1; true }
          else false
        }
        col
      } else 0
    Position(row, col)
  }

  listenTo(this)
  reactions += {
    case e: KeyPressed =>
      handleKeyPressed(e)
    case e: KeyTyped =>
      handleKeyTyped(e)
    case MousePressed(_, point, _, _, _) =>
      this.requestFocusInWindow()
      caret.position = positionFromPoint(point)
    case MouseDragged(_, point, _) =>
      caret.dotPos = positionFromPoint(point)
    case _: FocusGained => focusVar.set(true)
    case _: FocusLost   => focusVar.set(false)
  }

  private def handleKeyPressed(e: KeyPressed): Unit = {
    def shift = e.modifiers == Key.Modifier.Shift
    if e.modifiers == Key.Modifier.Control then
        e.key match {
          case Key.V => paste()
          case Key.C => copy()
          case Key.A => selectAll()
          case _     =>
        }
    else
        e.key match {
          case Key.Left =>
            val offset = caret.offset.readValueOnce - (if posInLinebreak(caret.offset.readValueOnce - 1) then 2 else 1)
            if shift then caret.dot = offset else caret.offset = offset
          case Key.Right =>
            val offset = caret.offset.readValueOnce + (if posInLinebreak(caret.offset.readValueOnce + 1) then 2 else 1)
            if shift then caret.dot = offset else caret.offset = offset
          case Key.Up =>
            val position = Position(max(0, caret.position.readValueOnce.row - 1), caret.position.readValueOnce.col)
            if shift then caret.dotPos = position else caret.position = position
          case Key.Down =>
            val position = Position(
              min(lineCount.readValueOnce - 1, caret.position.readValueOnce.row + 1),
              caret.position.readValueOnce.col
            )
            if shift then caret.dotPos = position else caret.position = position
          case Key.Home =>
            var offset = 0
            for (ch, i) <- buffer.iterable.readValueOnce.iterator.zipWithIndex do
                if i < caret.offset.readValueOnce && (ch == '\r' || ch == '\n') then
                    offset = i + 1;
            if shift then caret.dot = offset else caret.offset = offset
          case Key.End =>
            val offset =
              caret.offset.readValueOnce +
              buffer.iterable.readValueOnce.iterator.drop(caret.offset.readValueOnce).takeWhile {
                ch => ch != '\r' && ch != '\n'
              }.size
            if shift then caret.dot = offset else caret.offset = offset
          case _ =>
        }
  }

  private def handleKeyTyped(e: KeyTyped): Unit =
    if e.modifiers != Key.Modifier.Control then
        e.char match {
          case '\u007f' => // Del key
            if selected.readValueOnce.isEmpty then {
              val count = if posInLinebreak(caret.dot.readValueOnce + 1) then 2 else 1
              buffer.remove(count);
            } else removeSelection()
          case '\b' => // Backspace key
            if selected.readValueOnce.isEmpty then {
              val count = min(if posInLinebreak(caret.dot.readValueOnce - 1) then 2 else 1, caret.dot.readValueOnce)
              caret.offset = caret.offset.readValueOnce - count
              buffer.remove(count);
            } else removeSelection()
          case c => // character input
            removeSelection()
            buffer.insert(c.toString)
            caret.offset = caret.offset.readValueOnce + 1
        }

  // handle scroll and paint updates
  caret.position.changed observe { _ =>
    val point = pointFromPosition(caret.position.readValueOnce)
    peer.scrollRectToVisible(new Rectangle(point.x - 8, point.y, 16, 2 * lineHeight))
    caret.steady.restart
    ()
  }

  buffer.length.changed || caret.visible.changed ||
  caret.dot.changed || caret.mark.changed observe { _ =>
    updatePreferredSize()
    this.repaint()
  }

  override def paintComponent(g: Graphics2D): Unit = {
    super.paintComponent(g)
    g.setRenderingHint(java.awt.RenderingHints.KEY_ANTIALIASING, java.awt.RenderingHints.VALUE_ANTIALIAS_ON)
    g.setColor(SystemColor.text)
    g.fillRect(0, 0, size.width, size.height + lineHeight)

    val selStart = min(caret.dot.readValueOnce, caret.mark.readValueOnce)
    val selEnd   = max(caret.dot.readValueOnce, caret.mark.readValueOnce)

    var lineIndex = 0
    var charIndex = 0
    for line <- LineIterator(buffer.iterable.readValueOnce) do {
      var start, middle, end = ""
      var middleX, endX      = 0

      if selStart < charIndex + line.length && selEnd > charIndex then {
        val startIndex = if selStart > charIndex then selStart - charIndex else 0
        val endIndex   = if selEnd < charIndex + line.length then selEnd - charIndex else line.length

        start = line.substring(0, startIndex)
        middle = line.substring(startIndex, endIndex)
        end = line.substring(endIndex)

        middleX = padding + stringWidth(start)
        endX = padding + stringWidth(start + middle)

        g.setColor(SystemColor.textHighlight)
        g.fillRect(
          middleX,
          lineIndex * lineHeight + lineHeight - font.getSize,
          endX - middleX,
          lineHeight
        )
      } else
          start = line

      lineIndex += 1
      charIndex += line.length

      g.setColor(SystemColor.textText)
      g.drawString(start, padding, lineIndex * lineHeight)
      g.drawString(end, endX, lineIndex * lineHeight)

      g.setColor(SystemColor.textHighlightText)
      g.drawString(middle, middleX, lineIndex * lineHeight)
    }

    if caret.visible.readValueOnce then {
      def point = pointFromPosition(caret.position.readValueOnce)
      g.setColor(SystemColor.textText)
      g.drawLine(point.x, point.y + lineHeight - font.getSize, point.x, point.y + lineHeight)
    }
  }
}
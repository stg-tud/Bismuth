package ex201x.reswingexamples.reader.gui

import ex201x.reswingexamples.reader.data.{FeedStore, RSSChannel, RSSItem}
import reactives.default.*

import java.awt.{Dimension, Point, Toolkit}
import javax.swing.ImageIcon
import scala.swing.*
import scala.swing.event.*

/** A `Button` that exposes its clicks as a reactive event. */
@scala.annotation.nowarn("msg=shadows field")
class ReactiveButton(text: String) extends Button(text) with Reactor {
  val clicked: Evt[ButtonClicked] = Evt[ButtonClicked]()
  listenTo(this)
  reactions += { case c @ ButtonClicked(_) => clicked.fire(c) }
}

/** A `CheckBox` whose checked state is exposed as a reactive signal. */
@scala.annotation.nowarn("msg=shadows field")
class ReactiveCheckBox(text: String, selectedInit: Boolean) extends CheckBox(text) with Reactor {
  selected = selectedInit
  private val selectedVar: Var[Boolean] = Var(selectedInit)
  val selectedSignal: Signal[Boolean]   = Signal { selectedVar.value }
  listenTo(this)
  reactions += { case ButtonClicked(_) => selectedVar.set(selected) }
}

/** A `Label` whose text can be bound to a reactive signal. */
@scala.annotation.nowarn("msg=shadows field")
class ReactiveLabel(text: String) extends Label(text) {
  def text_=(value: Signal[String]): Unit = {
    text = value.now
    value.changed observe { s => if text != s then text = s }
    ()
  }
}

/** Responsible for displaying the content of the given FeedStore
  * The connections between the displayed content is mainly coordinated
  * by an initialized content mediator
  */
class GUI(
    store: FeedStore,
    notifications: Signal[String] = Signal { "" },
    itemStatus: Signal[String] = Signal { "" },
    fetcherState: Signal[String] = Signal { "" }
) extends SimpleSwingApplication {
  val refreshButton = new ReactiveButton("Refresh")
  val refresh       = refreshButton.clicked

  val requestURLAddition: Evt[String] = Evt[String]() // #EVT

  val refreshCheckbox = new ReactiveCheckBox("auto refresh", selectedInit = true)
  def refreshAllowed  = refreshCheckbox.selectedSignal

  def top: Frame =
    new MainFrame {
      val quitAction: Action      = swing.Action("Quit") { quit() }
      val urlDialogAction: Action = swing.Action("Add url") {
        val input = Dialog.showInput(
          null,
          "Please enter a feed url:",
          "Add URL",
          Dialog.Message.Question,
          Swing.EmptyIcon,
          Nil,
          ""
        )
        input.foreach { requestURLAddition.fire(_) }
      }

      menuBar = new MenuBar {
        contents += new Menu("File") {
          contents += new MenuItem(quitAction)
        }
        contents += new Menu("Edit") {
          contents += new MenuItem(urlDialogAction)
        }
      }

      val (framewidth, frameheight) = (840, 480)
      configure()

      val channelList: ReListViewEx[RSSChannel] = new ReListViewEx[RSSChannel](3) {
        renderer = ListView.Renderer(_.title)
      }
      channelList.bind(Signal { store.channels.value.keys.toSeq })

      val selectedChannelItems: Signal[Seq[RSSItem]] = Signal.dynamic { // #SIG
        channelList.selectedItem.value match {
          case Some(channel) => store.channels.value.get(channel) match {
              case Some(items) => items.value.toSeq
              case _           => Seq.empty
            }
          case _ => Seq.empty
        }
      }

      val itemList: ReListViewEx[RSSItem] = new ReListViewEx[RSSItem](8) {
        renderer = ListView.Renderer(_.title)
      }
      itemList.bind(selectedChannelItems)

      val renderArea = new RssItemRenderPane(itemList.selectedItem)

      val statusBar = new ReactiveLabel("") {
        preferredSize = new Dimension(framewidth / 3, 15)
        horizontalAlignment = Alignment.Left
      }
      statusBar.text = notifications

      val itemCountStatus = new ReactiveLabel("") {
        preferredSize = new Dimension(framewidth / 3, 15)
        horizontalAlignment = Alignment.Left
      }
      itemCountStatus.text = itemStatus

      val fetcherStatus = new ReactiveLabel("") {
        preferredSize = new Dimension(framewidth / 3, 15)
        horizontalAlignment = Alignment.Left
      }
      fetcherStatus.text = fetcherState

      contents = new BorderPanel {
        val topPane: GridPanel = new GridPanel(1, 1) {
          contents += new BorderPanel {
            add(new Label("Choose Channel: "), BorderPanel.Position.West)
            add(new ScrollPane(channelList), BorderPanel.Position.Center)
            add(
              new GridPanel(2, 1) {
                contents += refreshButton
                contents += refreshCheckbox
              },
              BorderPanel.Position.East
            )
          }
        }

        val splitPane = new SplitPane(Orientation.Vertical, new ScrollPane(itemList), new ScrollPane(renderArea))

        val mainPane = new SplitPane(Orientation.Horizontal, topPane, splitPane)

        add(mainPane, BorderPanel.Position.Center)
        add(
          new GridPanel(1, 3) {
            contents += statusBar
            contents += itemCountStatus
            contents += fetcherStatus
          },
          BorderPanel.Position.South
        )
      }

      private def configure(): Unit = {
        title = "RSS Reader"
        iconImage = new ImageIcon("res/icon.png").getImage

        minimumSize = new Dimension(framewidth, frameheight)

        val screenSize = Toolkit.getDefaultToolkit.getScreenSize
        location = new Point((screenSize.width - framewidth) / 2, (screenSize.height - frameheight) / 2)
      }
    }
}
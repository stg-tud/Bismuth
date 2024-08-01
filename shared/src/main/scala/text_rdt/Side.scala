package text_rdt
import upickle.default._

enum Side derives CanEqual, ReadWriter {
  case Left, Right
}

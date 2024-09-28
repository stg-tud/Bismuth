package text_rdt
import upickle.default._

enum Side derives ReadWriter {
  case Left, Right
}

package lofi_acl.sync.monotonic

enum Operation extends Ordered[Operation] {
  override def compare(that: Operation): Int = {
    (this, that) match
        case (READ, READ)   => 0
        case (READ, WRITE)  => -1
        case (WRITE, WRITE) => 0
        case (WRITE, READ)  => 1
  }

  case READ
  case WRITE
}

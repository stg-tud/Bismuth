package channels.broadcast.authz

enum CausalOrder:
    case BEFORE
    case AFTER
    case CONCURRENT
    case EQUAL
    case UNKNOWN

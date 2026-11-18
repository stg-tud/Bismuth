package channels.authz

enum CausalOrder:
    case BEFORE
    case AFTER
    case CONCURRENT
    case EQUAL
    case UNKNOWN

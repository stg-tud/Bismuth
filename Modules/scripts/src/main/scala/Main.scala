package scripts

import de.rmgk.options.*

@main def main(args: String*): Unit =
    val result = parseArguments(args.toList) {
        alternatives(
          subcommand("select-scheduler", "select the default scheduler for reactives") {
              val selection = positional[String]("scheduler", "default scheduler for all").value
              rejectUnknown()
              () => selectScheduler(selection)
          },
        )
    }
    result match
        case Result.Ok(run, _) => run()
        case _: Result.Err     => sys.exit(1)

/** Fail on leftover arguments instead of silently ignoring them. */
inline def rejectUnknown(): Unit =
    val unknown = ReadRemaining.value
    if unknown.nonEmpty then throw ParseException(Result.Err("unknown arguments", remaining = Some(unknown)))

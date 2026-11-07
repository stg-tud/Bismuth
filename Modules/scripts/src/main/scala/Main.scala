package scripts

import de.rmgk.options.*

@main def main(args: String*): Unit =
    val result = parseArguments(args.toList) {
        alternatives(
          subcommand("fix-unused-imports", "compile via BSP and remove all imports reported as unused") {
              val clean  = flag("--clean", "clean all targets first, otherwise only recompiled files report warnings").value
              val dryRun = flag("--dry-run", "only report what would change").value
              rejectUnknown()
              () => fixUnusedImports(clean, dryRun)
          },
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

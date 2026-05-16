package de.tu_darmstadt.informatik.st.reform.components

import de.tu_darmstadt.informatik.st.reform.JSImplicits
import de.tu_darmstadt.informatik.st.reform.*
import de.tu_darmstadt.informatik.st.reform.given
import de.tu_darmstadt.informatik.st.reform.given_ExecutionContext
import de.tu_darmstadt.informatik.st.reform.services.AvailableConnection
import outwatch.*
import outwatch.dsl.*
import reactives.default.*

// NOTE: scala-loci RemoteRef removed – connection rows use String alias/name instead
def connectionRow(name: String, source: String, uuid: String, displayId: String, tpe: String, ref: String)(using
    jsImplicits: JSImplicits,
) = {
  if source == "discovery" then {
    val own = jsImplicits.discovery.decodeToken(jsImplicits.discovery.token.now.get)
    div(
      cls := "flex items-center justify-between p-2 hover:bg-slate-100 dark:hover:bg-gray-700/50 rounded-md",
      div(
        cls := "flex flex-col text-sm",
        div(
          div(
            name,
            if tpe == "SSO" then Some(icons.CheckCircle(cls := "w-4 h-4 text-blue-600")) else None,
            cls := "flex gap-2 items-center",
          ),
          cls := "font-bold",
        ),
        i(
          span(
            "ID: ",
            cls := "text-slate-400 dark:text-gray-400",
          ),
          displayId,
          cls := "text-slate-500 text-xs dark:text-gray-200",
        ),
        i(
          span(
            "Source: ",
            cls := "text-slate-400 dark:text-gray-400",
          ),
          source,
          cls := "text-slate-500 text-xs dark:text-gray-200",
        ),
        i(
          span(
            "Connection: ",
            cls := "text-slate-400 dark:text-gray-400",
          ),
          // NOTE: getConnectionMode previously used scala-loci RemoteRef – now returns constant
          Signal.fromFuture(jsImplicits.webrtc.getConnectionMode(ref)),
          cls := "text-slate-500 text-xs dark:text-gray-200",
        ),
      ),
      div(
        cls := "flex flex-row gap-1",
        if own.uuid != uuid then
            Some(
              div(
                icons.Forbidden(cls := "text-red-600 w-3 h-3"),
                cls      := "tooltip tooltip-left hover:bg-red-200 rounded-md p-1 h-fit w-fit cursor-pointer",
                data.tip := "Remove from Whitelist",
                onClick.foreach { _ =>
                  jsImplicits.discovery.deleteFromWhitelist(uuid)
                },
              ),
            )
        else None,
      ),
    )
  } else
      div(
        cls := "flex items-center justify-between p-2 hover:bg-slate-100 dark:hover:bg-gray-700/50 rounded-md",
        div(
          cls := "flex flex-col text-sm",
          div(
            div(
              name,
              if tpe == "SSO" then Some(icons.CheckCircle(cls := "w-4 h-4 text-blue-600")) else None,
              cls := "flex gap-2 items center",
            ),
            cls := "font-bold",
          ),
          i(
            span(
              "Source: ",
              cls := "text-slate-400 dark:text-gray-400",
            ),
            source,
            cls := "text-slate-500 text-xs dark:text-gray-200",
          ),
          i(
            span(
              "Connection: ",
              cls := "text-slate-400 dark:text-gray-400",
            ),
            Signal.fromFuture(jsImplicits.webrtc.getConnectionMode(ref)),
            cls := "text-slate-500 text-xs dark:text-gray-200",
          ),
        ),
      )
}

def availableConnectionRow(
    connection: AvailableConnection,
)(using jsImplicits: JSImplicits) = {
  div(
    cls := "flex items-center justify-between p-2 hover:bg-slate-100 dark:hover:bg-gray-700/50 rounded-md mt-2",
    div(
      cls := "flex flex-col text-sm",
      div(
        connection.name,
        if connection.tpe == "SSO" then Some(icons.CheckCircle(cls := "w-4 h-4 text-blue-600")) else None,
        cls := "flex gap-2 font-bold items-center",
      ),
      i(
        span(
          "ID: ",
          cls := "text-slate-400 dark:text-gray-400",
        ),
        connection.displayId,
        cls := "text-slate-500 text-xs dark:text-gray-200",
      ),
      i(
        span(
          "Trust: ",
          cls := "text-slate-400 dark:text-gray-400",
        ),
        if connection.trusted && !connection.mutualTrust then s"wait for ${connection.name} to trust you"
        else if !connection.trusted then s"you do not trust ${connection.name} "
        else "you trust each other",
        cls := "text-slate-500 text-xs dark:text-gray-200",
      ),
    ),
    if !connection.trusted then {
      div(
        icons.Check(cls := "w-4 h-4 text-green-600"),
        cls      := "tooltip tooltip-left hover:bg-green-200 rounded-md p-0.5 h-fit w-fit cursor-pointer",
        data.tip := "Add to Whitelist",
        onClick.foreach(_ => jsImplicits.discovery.addToWhitelist(connection.uuid)),
      )
    } else None,
    if connection.trusted && connection.mutualTrust then {
      div(
        icons.Check(cls := "w-4 h-4 text-green-600"),
        cls      := "tooltip tooltip-left hover:bg-green-200 rounded-md p-0.5 h-fit w-fit cursor-pointer",
        data.tip := "Connect",
        onClick.foreach(_ => jsImplicits.discovery.connectTo(connection.uuid)),
      )
    } else None,
  )
}

package de.tu_darmstadt.informatik.st.reform

import scala.concurrent.ExecutionContext

// macrotask executor breaks indexeddb
given ExecutionContext = scala.concurrent.ExecutionContext.global

import de.tu_darmstadt.informatik.st.reform.npm.IIndexedDB
import de.tu_darmstadt.informatik.st.reform.services.{DiscoveryService, MailService, RoutingService, Toaster}
import de.tu_darmstadt.informatik.st.reform.webrtc.WebRTCService

abstract case class JSImplicits() {
  lazy val toaster: Toaster
  lazy val mailing: MailService
  lazy val routing: RoutingService
  lazy val indexeddb: IIndexedDB
  lazy val repositories: Repositories
  lazy val discovery: DiscoveryService
  lazy val webrtc: WebRTCService
}

object Globals {
  lazy val VITE_SELENIUM: Boolean = false

  lazy val VITE_DATABASE_VERSION: String = "2"

  lazy val APP_VERSION: String = "unknown"

  // Hardcoded defaults for stubbed WebRTC/peer services
  lazy val VITE_TURN_SERVER_HOST: String = "localhost"
  lazy val VITE_TURN_SERVER_PORT: String = "3478"

  lazy val VITE_DEKANAT_MAIL: String      = ""
  lazy val VITE_CONTRACT_PDF_PATH: String = "/contract_unlocked.pdf"
  lazy val VITE_LETTER_PDF_PATH: String   = "/letter_editable.pdf"
}

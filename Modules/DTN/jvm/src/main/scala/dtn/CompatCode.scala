package dtn

import sttp.capabilities.WebSockets
import sttp.client4.*
import sttp.client4.httpclient.HttpClientFutureBackend
import sttp.model.Uri

import java.io.PrintStream
import java.nio.file.{Files, Paths}
import java.time.{ZoneId, ZonedDateTime}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.{Try, Using}

object CompatCode {
  val backend: GenericBackend[Future, WebSockets] = HttpClientFutureBackend()

  def uget(uri: Uri): Future[String] = backend.send(basicRequest.get(uri).response(asStringAlways)).map(x => x.body)
}

extension [U >: Unit](fut: Future[U])
  def recoverAndLog(): Future[U] = {
    fut.recover { e =>
      Using(Files.newOutputStream(Paths.get(s"/shared/err-${ZonedDateTime.now(ZoneId.of("UTC"))}"))) { out =>
        Using(PrintStream(out)) { outPrinter =>
          e.printStackTrace(outPrinter)
        }
        out.flush()
      }
      e.printStackTrace()
    }
  }

extension [U >: Unit](x: Try[U])
  def recoverAndLog(): Try[U] = {
    x.recover { e =>
      Using(Files.newOutputStream(Paths.get(s"/shared/err-${ZonedDateTime.now(ZoneId.of("UTC"))}"))) { out =>
        Using(PrintStream(out)) { outPrinter =>
          e.printStackTrace(outPrinter)
        }
        out.flush()
      }
      e.printStackTrace()
    }
  }

extension [E <: Exception](e: E)
  def log(): Unit = {
    Using(Files.newOutputStream(Paths.get(s"/shared/err-${ZonedDateTime.now(ZoneId.of("UTC"))}"))) { out =>
      Using(PrintStream(out)) { outPrinter =>
        e.printStackTrace(outPrinter)
      }
      out.flush()
    }
    e.printStackTrace()
  }

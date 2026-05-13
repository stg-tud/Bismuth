package de.tu_darmstadt.informatik.st.reform

import de.tu_darmstadt.informatik.st.reform.entity.*
import de.tu_darmstadt.informatik.st.reform.given_ExecutionContext
import de.tu_darmstadt.informatik.st.reform.npm.IIndexedDB
import de.tu_darmstadt.informatik.st.reform.npm.MemoryIndexedDB
import de.tu_darmstadt.informatik.st.reform.repo.Repository
import de.tu_darmstadt.informatik.st.reform.repo.Synced
import de.tu_darmstadt.informatik.st.reform.utils.Seqnal.*
import loci.communicator.ws.jetty.WS
import loci.registry.Registry
import munit.FunSuite
import org.eclipse.jetty.server.Server
import org.eclipse.jetty.server.ServerConnector
import org.eclipse.jetty.servlet.ServletContextHandler

import scala.concurrent.Future

class MainJVMTest extends FunSuite:

  private def testEntity[T <: Entity[T]](value: Synced[T]): T =
    val now = value.signal.now
    now.identifier.getAll
    assert(!now.withExists(false).exists)
    now.default

  private def testRepository[T <: Entity[T]](repository: Repository[T]): Future[Unit] =
    for
      _ <- repository.all.waitUntil(_.isEmpty)
      _ <- repository.create(repository.defaultValue).map(testEntity)
      _ <- repository.all.waitUntil(_.length == 1)
    yield ()

  private def freshRepositories() =
    given Registry = Registry()
    given IIndexedDB = MemoryIndexedDB()
    Repositories()

  private def testSyncing[T <: Entity[T]](fun: Repositories => Repository[T], port: Int): Future[Unit] =
    val registry0 = Registry()
    val registry1 = Registry()
    val indexedDb0: IIndexedDB = MemoryIndexedDB()
    val indexedDb1: IIndexedDB = MemoryIndexedDB()
    val repositories0 = Repositories()(using registry0, indexedDb0)
    val repositories1 = Repositories()(using registry1, indexedDb1)
    val server = Server()
    val connector = ServerConnector(server)
    connector.setPort(port)
    val context = ServletContextHandler(ServletContextHandler.SESSIONS)
    server.setHandler(context)
    server.addConnector(connector)
    for
      _ <- testRepository(fun(repositories0))
      _ <- fun(repositories0).all.waitUntil(_.length == 1)
      _ <- fun(repositories1).all.waitUntil(_.isEmpty)
      _ = registry0.listen(WS(context, "/registry/*"))
      _ = server.start()
      _ <- registry1.connect(WS(s"ws://localhost:$port/registry/"))
      _ <- fun(repositories1).all.waitUntil(_.length == 1)
      _ = registry0.terminate()
      _ = registry1.terminate()
    yield ()

  test("projects repository") {
    testRepository(freshRepositories().projects)
  }

  test("users repository") {
    testRepository(freshRepositories().users)
  }

  test("hiwis repository") {
    testRepository(freshRepositories().hiwis)
  }

  test("supervisors repository") {
    testRepository(freshRepositories().supervisors)
  }

  test("contractSchemas repository") {
    testRepository(freshRepositories().contractSchemas)
  }

  test("paymentLevels repository") {
    testRepository(freshRepositories().paymentLevels)
  }

  test("salaryChanges repository") {
    testRepository(freshRepositories().salaryChanges)
  }

  test("syncing projects") {
    testSyncing(_.projects, 1337)
  }

  test("syncing users") {
    testSyncing(_.users, 1338)
  }

  test("syncing hiwis") {
    testSyncing(_.hiwis, 1339)
  }

  test("syncing supervisors") {
    testSyncing(_.supervisors, 1340)
  }

  test("syncing contractSchemas") {
    testSyncing(_.contractSchemas, 1341)
  }

  test("syncing paymentLevels") {
    testSyncing(_.paymentLevels, 1342)
  }

  test("syncing salaryChanges") {
    testSyncing(_.salaryChanges, 1343)
  }

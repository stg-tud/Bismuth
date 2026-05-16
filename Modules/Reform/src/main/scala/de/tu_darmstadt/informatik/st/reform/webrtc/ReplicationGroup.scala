/*
Copyright 2022 https://github.com/rescala-lang/REScala

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.

--- NOTE: scala-loci peer-to-peer sync has been removed. This class now works
--- with local IndexedDB storage only. Remote sync no longer happens.
 */
package de.tu_darmstadt.informatik.st.reform.webrtc

import de.tu_darmstadt.informatik.st.reform.given_ExecutionContext
import de.tu_darmstadt.informatik.st.reform.repo.{Storage, Synced}
import rdts.base.*
import reactives.default.*

import scala.concurrent.Future

/** Local-only wrapper around a named value.
  *
  * Originally used scala-loci to replicate state between peers.
  * scala-loci has been removed, so this only stores/loads from IndexedDB.
  */
class ReplicationGroup[A](name: String)(using
    bottom: Bottom[A],
    storage: Storage[A],
) {

  println(s"[ReplicationGroup] scala-loci sync removed – '$name' is local-only")

  @volatile
  private var cache: Map[String, Future[Synced[A]]] = Map.empty

  def createAndSync(id: String, initialValue: A): Future[Synced[A]] = {
    synchronized {
      if cache.contains(id) then {
        throw new Exception("This is not a new entity!")
      } else {
        val synced = storage
          .getOrDefault(id, initialValue)
          .map { value =>
            Synced(storage, id, Var(value))
          }
        cache += (id -> synced)
        synced
      }
    }
  }

  def getOrCreateAndSync(id: String): Future[Synced[A]] =
    cache.getOrElse(id, createAndSync(id, bottom.empty))
}

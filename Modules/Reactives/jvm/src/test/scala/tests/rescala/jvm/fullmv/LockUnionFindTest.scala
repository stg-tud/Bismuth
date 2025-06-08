package tests.rescala.fullmv

import reactives.fullmv.*
import reactives.fullmv.sgt.synchronization.*

import scala.concurrent.Await
import scala.concurrent.duration.Duration

class LockUnionFindTest extends munit.FunSuite {
  val engine = new FullMVEngine(Duration.Zero, "LockUnionFindTest")

  test("single lock gc works") {
    val turn = engine.newTurn()
    turn.beginFraming()
    val lock = turn.subsumableLock.get

    if SubsumableLockImpl.DEBUG then println(s"single lock gc with $turn using $lock")

    assertEquals(lock.refCount.get, 1)
    assertEquals(engine.getInstance(turn.guid), Some(turn))
    assertEquals(engine.lockHost.getInstance(lock.guid), Some(lock))

    turn.completeFraming()

    assertEquals(lock.refCount.get, 1)
    assertEquals(engine.getInstance(turn.guid), Some(turn))
    assertEquals(engine.lockHost.getInstance(lock.guid), Some(lock))

    turn.completeExecuting()

    assert(lock.refCount.get <= 0)
    assertEquals(engine.getInstance(turn.guid), None)
    assertEquals(engine.lockHost.getInstance(lock.guid), None)
  }

  test("lock/unlock holds temporary reference") {
    val turn = engine.newTurn()
    turn.beginFraming()
    val lock = turn.subsumableLock.get

    assertEquals(lock.refCount.get, 1)

    val l = Await.result(turn.tryLock(), Duration.Zero).asInstanceOf[Locked].lock

    assertEquals(lock.refCount.get, 2)

    l.asyncUnlock()

    assertEquals(lock.refCount.get, 1)
  }

  test("single subsumed gc works") {
    val turn1 = engine.newTurn()
    turn1.beginExecuting()
    val lock1 = turn1.subsumableLock.get()

    val turn2 = engine.newTurn()
    turn2.beginExecuting()
    val lock2 = turn2.subsumableLock.get()

    if SubsumableLockImpl.DEBUG then println(s"single subsumed gc with $turn1 using $lock1 and $turn2 using $lock2")

    val l1 = Await.result(turn1.tryLock(), Duration.Zero).asInstanceOf[Locked].lock

    assertEquals(lock1.refCount.get, 2) // turn1 and thread
    assertEquals(engine.getInstance(turn1.guid), Some(turn1))
    assertEquals(engine.lockHost.getInstance(lock1.guid), Some(lock1))
    assert(lock2.refCount.get == 1)
    assertEquals(engine.getInstance(turn2.guid), Some(turn2))
    assertEquals(engine.lockHost.getInstance(lock2.guid), Some(lock2))

    assertEquals(Await.result(turn2.trySubsume(l1), Duration.Zero), Successful)

    assertEquals(lock1.refCount.get, 3) // turn2, turn1 and thread
    assertEquals(engine.getInstance(turn1.guid), Some(turn1))
    assertEquals(engine.lockHost.getInstance(lock1.guid), Some(lock1))
    assert(lock2.refCount.get <= 0)
    assertEquals(engine.getInstance(turn2.guid), Some(turn2))
    assertEquals(engine.lockHost.getInstance(lock2.guid), None)

    l1.asyncUnlock()

    assertEquals(lock1.refCount.get, 2) // turn2 and turn1
    assertEquals(engine.getInstance(turn1.guid), Some(turn1))
    assertEquals(engine.lockHost.getInstance(lock1.guid), Some(lock1))
    assert(lock2.refCount.get <= 0)
    assertEquals(engine.getInstance(turn2.guid), Some(turn2))
    assertEquals(engine.lockHost.getInstance(lock2.guid), None)

    turn1.completeExecuting()

    assertEquals(lock1.refCount.get, 1) // turn2
    assertEquals(engine.getInstance(turn1.guid), None)
    assertEquals(engine.lockHost.getInstance(lock1.guid), Some(lock1))
    assertEquals(engine.getInstance(turn2.guid), Some(turn2))

    turn2.completeExecuting()

    assert(lock1.refCount.get <= 0)
    assertEquals(engine.lockHost.getInstance(lock1.guid), None)
    assertEquals(engine.getInstance(turn2.guid), None)
  }

  test("multiple subsumed gc works") {
    val maxIdx = 10
    val turns  = Array.fill(maxIdx + 1) {
      val turn = engine.newTurn()
      turn.beginExecuting()
      turn
    }
    val locks = turns.map(_.subsumableLock.get)

    turns.reduce { (t1, t2) =>
      val l = Await.result(t2.tryLock(), Duration.Zero).asInstanceOf[Locked].lock
      assertEquals(Await.result(t1.trySubsume(l), Duration.Zero), Successful)
      l.asyncUnlock()
      t2
    }

    assert(locks(0).refCount.get <= 0)     // gc'd
    assertEquals(locks(1).refCount.get, 1) // turn 0
    for i <- 2 until maxIdx do {
      assertEquals(locks(i).refCount.get, 2) // lock(i-1), turn(i-1)
    }
    assertEquals(locks(maxIdx).refCount.get, 3) // lock(count-1), turn(count-1), turn(count)

    turns(1).completeExecuting()
    turns(2).completeExecuting()
    turns(4).completeExecuting()

    assert(locks(0).refCount.get <= 0)     // gc'd
    assertEquals(locks(1).refCount.get, 1) // turn 0
    assertEquals(locks(2).refCount.get, 1) // lock 1
    assertEquals(locks(3).refCount.get, 1) // lock 2
    assertEquals(locks(4).refCount.get, 2) // turn 3, lock 3
    assertEquals(locks(5).refCount.get, 1) // lock 4
    for i <- 6 until maxIdx do {
      assertEquals(locks(i).refCount.get, 2) // lock(i-1), turn(i-1)
    }
    assertEquals(locks(maxIdx).refCount.get, 3) // lock(count-1), turn(count-1), turn(count)

    Await.result(turns(0).tryLock(), Duration.Zero).asInstanceOf[Locked].lock.asyncUnlock()

    assert(locks(0).refCount.get <= 0)     // gc'd
    assert(locks(1).refCount.get <= 0)     // gc'd
    assert(locks(2).refCount.get <= 0)     // gc'd
    assert(locks(3).refCount.get <= 0)     // gc'd
    assertEquals(locks(4).refCount.get, 1) // turn 3
    assert(locks(5).refCount.get <= 0)     // gc'd
    for i <- 6 until maxIdx do {
      assertEquals(locks(i).refCount.get, 1) // turn(i-1)
    }
    assertEquals(
      locks(maxIdx).refCount.get,
      maxIdx - 6 + 4
    ) // lock(4), lock(6) to lock(maxIdx - 1), turn(0), turn(count-1), turn(count)
  }

  test("underLock works") {
    val turn1 = engine.newTurn()
    turn1.beginExecuting()
    val lock1 = turn1.subsumableLock.get()

    val turn2 = engine.newTurn()
    turn2.beginExecuting()
    val lock2 = turn2.subsumableLock.get()

    assertEquals(lock1.refCount.get, 1)
    assertEquals(lock2.refCount.get, 1)

    val locked = SerializationGraphTracking.tryLock(turn1, turn2, UnlockedUnknown).asInstanceOf[LockedSameSCC]
    locked.unlock()

    assert(
      (lock1.refCount.get == 2 && lock2.refCount.get <= 0)
      ||
      (lock1.refCount.get <= 0 && lock2.refCount.get == 2)
    )
  }

  test("lock is exclusive and failed locks retain reference counts") {
    // we can lock
    val turn = engine.newTurn()
    val lock = turn.subsumableLock.get
    turn.beginExecuting()
    assertEquals(lock.refCount.get, 1)

    val l = Await.result(turn.tryLock(), Duration.Zero).asInstanceOf[Locked].lock
    assertEquals(lock.refCount.get, 2)

    // lock is exclusive
    assert(Await.result(turn.tryLock(), Duration.Zero) == Blocked)
    assertEquals(lock.refCount.get, 2)

    l.asyncUnlock()
    assertEquals(lock.refCount.get, 1)

    // unlock unblocks
    val l2 = Await.result(turn.tryLock(), Duration.Zero).asInstanceOf[Locked].lock
    assertEquals(lock.refCount.get, 2)

    l2.asyncUnlock()
    assertEquals(lock.refCount.get, 1)
    turn.completeExecuting()
    assert(lock.refCount.get <= 0)
  }

  test("union works") {
    val a = engine.newTurn()
    a.beginExecuting()
    val b = engine.newTurn()
    b.beginExecuting()

    val l1 = Await.result(b.tryLock(), Duration.Zero).asInstanceOf[Locked].lock

    assertEquals(Await.result(a.trySubsume(l1), Duration.Zero), Successful)

    assertEquals(Await.result(a.getLockedRoot, Duration.Zero), LockedState(l1.guid))
    assertEquals(Await.result(b.getLockedRoot, Duration.Zero), LockedState(l1.guid))
    assert(Await.result(a.tryLock(), Duration.Zero) == Blocked)
    assert(Await.result(b.tryLock(), Duration.Zero) == Blocked)

    l1.asyncUnlock()

    val l2 = Await.result(a.tryLock(), Duration.Zero).asInstanceOf[Locked].lock
    assert(Await.result(b.tryLock(), Duration.Zero) == Blocked)

    l2.asyncUnlock()

    val l3 = Await.result(b.tryLock(), Duration.Zero).asInstanceOf[Locked].lock
    assert(Await.result(a.tryLock(), Duration.Zero) == Blocked)

    l3.asyncUnlock()
  }

  test("blocked union retains reference counts") {
    val turn1 = engine.newTurn()
    turn1.beginExecuting()
    val lock1 = turn1.subsumableLock.get()

    val turn2 = engine.newTurn()
    turn2.beginExecuting()
    val lock2 = turn2.subsumableLock.get()

    if SubsumableLockImpl.DEBUG then
      println(s"single subsume blocked gc with $turn1 using $lock1 and $turn2 using $lock2")

    val l1 = Await.result(turn1.tryLock(), Duration.Zero).asInstanceOf[Locked].lock
    val l2 = Await.result(turn2.tryLock(), Duration.Zero).asInstanceOf[Locked].lock

    assertEquals(lock1.refCount.get, 2) // turn1 and thread
    assertEquals(lock2.refCount.get, 2) // turn2 and thread

    assertEquals(Await.result(turn2.trySubsume(l1), Duration.Zero), Blocked)

    assertEquals(lock1.refCount.get, 2) // turn1 and thread
    assertEquals(lock2.refCount.get, 2) // turn2 and thread

    l2.asyncUnlock()

    assertEquals(lock1.refCount.get, 2) // turn1 and thread
    assertEquals(lock2.refCount.get, 1) // turn2

    assertEquals(Await.result(turn2.trySubsume(l1), Duration.Zero), Successful)

    assertEquals(lock1.refCount.get, 3) // turn2, turn1 and thread
    assert(lock2.refCount.get <= 0)     // none

    l1.asyncUnlock()

    assertEquals(lock1.refCount.get, 2) // turn2 and turn1
    assert(lock2.refCount.get <= 0)

    turn1.completeExecuting()

    assertEquals(lock1.refCount.get, 1) // turn2

    turn2.completeExecuting()

    assert(lock1.refCount.get <= 0)
  }

  test("reentrant tryLock works") {
    val turn1 = engine.newTurn()
    turn1.beginExecuting()
    val lock1 = turn1.subsumableLock.get()

    assertEquals(lock1.refCount.get, 1)

    val l1 = Await.result(turn1.tryLock(), Duration.Zero).asInstanceOf[Locked].lock

    assertEquals(lock1.refCount.get, 2)

    assertEquals(Await.result(turn1.trySubsume(l1), Duration.Zero), Successful)

    assertEquals(lock1.refCount.get, 2)

    l1.asyncUnlock()
    turn1.completeExecuting()

    assert(lock1.refCount.get <= 1)
  }

  test("blocked tryLock works") {
    val turn1 = engine.newTurn()
    turn1.beginExecuting()
    val lock1 = turn1.subsumableLock.get()

    val turn2 = engine.newTurn()
    turn2.beginExecuting()
    val lock2 = turn2.subsumableLock.get()

    assertEquals(lock1.refCount.get, 1)
    assertEquals(lock2.refCount.get, 1)

    val l1 = Await.result(turn1.tryLock(), Duration.Zero).asInstanceOf[Locked].lock
    val l2 = Await.result(turn2.tryLock(), Duration.Zero).asInstanceOf[Locked].lock

    assertEquals(lock1.refCount.get, 2)
    assertEquals(lock2.refCount.get, 2)

    assertEquals(Await.result(turn2.trySubsume(l1), Duration.Zero), Blocked)

    assertEquals(lock1.refCount.get, 2)
    assertEquals(lock2.refCount.get, 2)

    l1.asyncUnlock()
    l2.asyncUnlock()

    turn1.completeExecuting()
    turn2.completeExecuting()
  }
}

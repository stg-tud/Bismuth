package rdts.time

type Time = Long

object Time:
  def currentWallMillis(): Time = System.currentTimeMillis

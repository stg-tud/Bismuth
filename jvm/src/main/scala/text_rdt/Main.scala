package text_rdt

import java.lang.management.ManagementFactory
import java.io.BufferedReader
import java.io.InputStreamReader
import javax.management.ObjectName
import java.time.Duration
import java.time.temporal.ChronoUnit

@main
def peakMemory(): Unit = {
  // local
  val test = MyRealWorldBenchmarkLargeLocal()
  test.factoryConstructor = sys.props.get("factoryConstructor").get
  test.count = 100
  val replica = test.localMethod()
  /*{
    // remote
    val test = MyRealWorldBenchmarkLargeRemote()
    test.factoryConstructor = sys.props.get("factoryConstructor").get
    test.count = 100
    test.setupReplicaWithOperationsApplied()
    val replica = test.remoteMethod().get
    test.replicaWithOperationsApplied = null.asInstanceOf[text_rdt.Replica[
      text_rdt.SimpleFugueFactory.simpleFugueFactory.type |
        text_rdt.ComplexFugueFactory.complexFugueFactory.type |
        text_rdt.SimpleAVLFugueFactory.simpleAVLFugueFactory.type |
        text_rdt.ComplexAVLFugueFactory.complexAVLFugueFactory.type
    ]]
    print(replica.state.rootTreeNode)
  } */
  println(
    ManagementFactory
      .getPlatformMBeanServer()
      .nn
      .invoke(
        new ObjectName("com.sun.management:type=DiagnosticCommand"),
        "gcClassHistogram",
        Array[Object | Null](null),
        Array("[Ljava.lang.String;")
      )
      .asInstanceOf[String]
  );
  val _ = replica.hashCode();
  Thread.sleep(420)
}

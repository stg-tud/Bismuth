package ex2024textRDT

import java.lang.management.ManagementFactory
import javax.management.ObjectName

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
    test.replicaWithOperationsApplied = null.asInstanceOf[ex2024textRDT.Replica[
      ex2024textRDT.fugue.simple.SimpleFugueFactory.simpleFugueFactory.type |
        ex2024textRDT.fugue.complex.ComplexFugueFactory.complexFugueFactory.type |
        ex2024textRDT.fugue.simpleavl.SimpleAVLFugueFactory.simpleAVLFugueFactory.type |
        ex2024textRDT.fugue.complexavl.ComplexAVLFugueFactory.complexAVLFugueFactory.type
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

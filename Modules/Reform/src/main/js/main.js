// Match the exWeb setup: Vite only bundles plain JS, sbt produces Scala.js output ahead of time.
const useFullopt = import.meta.env?.PROD;
const modulePromise = useFullopt
  ? import("../../../target/reform-opt/main.js")
  : import("../../../target/reform-fastopt/main.js");

modulePromise.catch((error) => {
  console.error("Failed to load Scala.js bundle. Run the sbt link task first.", error);
  throw error;
});

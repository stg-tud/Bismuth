@main
def main(): Unit =
  (1 to 5).map(println)

  for (i <- 1 to 5) do
    println(s"i = $i")
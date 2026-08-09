package test.rdts.deltalens

import rdts.base.{Bottom, LocalUid}
import rdts.datatypes.*
import rdts.syntax.deltalens.*
import rdts.time.{CausalTime, Dots}

/** Tests for [[com.softwaremill.deltalens.deltaModify]].
  *
  * Unlike plain `modify`, `deltaModify` does not rebuild the whole value: it produces a *delta*, i.e. only the
  * modification, with every field that is not on the modification path replaced by its `Bottom.empty` value (the
  * identity of the corresponding `Lattice` merge).
  */
class DeltaModifyTest extends munit.FunSuite {

  given Bottom[Int]    = Bottom.provide(42)
  given Bottom[String] = Bottom.provide("BOTTOM")
  given LocalUid       = LocalUid.predefined("delta-modify-test")

  case class Address(street: String, zip: Int) derives Bottom
  case class Person(name: String, age: Int, address: Address) derives Bottom
  given Bottom[List[Person]] = Bottom.provide(Nil)
  case class WithMiddle(middleName: Option[String], other: Int) derives Bottom
  case class Team(name: String, members: List[Person]) derives Bottom
  case class Registry(people: Map[String, Person]) derives Bottom

  case class Doc(title: LastWriterWins[String], body: String) derives Bottom
  case class RecipeBook(recipes: ObserveRemoveMap[String, Int], version: Int) derives Bottom
  case class ListDoc(owner: String, items: ReplicatedList[String]) derives Bottom
  case class MvrDoc(owner: String, reg: MultiVersionRegister[Int]) derives Bottom
  case class FlagDoc(owner: String, flag: EnableWinsFlag) derives Bottom
  case class EpochDoc(owner: String, epoch: Epoch[Int]) derives Bottom

  case class Recipe(title: LastWriterWins[String], servings: Int) derives Bottom
  case class CookBook(owner: String, recipes: Map[String, Recipe]) derives Bottom

  val alice: Person          = Person("Alice", 30, Address("Main St", 12345))
  val bob: Person            = Person("Bob", 25, Address("2nd St", 11111))
  val missing: Person        = Person("Missing", 0, Address("Nowhere", 0))
  val withMiddle: WithMiddle = WithMiddle(Some("Marie"), 1)

  val team: Team         = Team("Team1", List(alice, bob))
  val registry: Registry =
    Registry(Map("key1" -> alice, "key2" -> bob))

  val doc: Doc = Doc(LastWriterWins.now("Hello"), "body")

  test("blanks out untouched sibling fields at the top level") {
    val delta = alice.deltaModify(_.age).using(_ + 1)

    assertEquals(delta, Person("BOTTOM", 31, Address("BOTTOM", 42)))
  }

  test("blanks out everything not on a deeply nested path") {
    val delta = alice.deltaModify(_.address.zip).using(_ + 1)

    // Everything except the modified zip is bottom: `name`/`age` at the root and `street` in address.
    assertEquals(delta, Person("BOTTOM", 42, Address("BOTTOM", 12346)))
  }

  test("setTo a fixed constant keeps everything else bottom") {
    val delta = alice.deltaModify(_.name).setTo("NewName")

    assertEquals(delta, Person("NewName", 42, Address("BOTTOM", 42)))
  }

  test("each over a list produces a delta per element") {
    val delta = team.deltaModify(_.members.each.name).using(_.toUpperCase)

    assertEquals(
      delta,
      Team(
        "BOTTOM",
        List(
          Person("ALICE", 42, Address("BOTTOM", 42)),
          Person("BOB", 42, Address("BOTTOM", 42))
        )
      )
    )
  }

  test("each over an Option navigates into the option's content") {
    val delta = withMiddle.deltaModify(_.middleName.each).using(_.toUpperCase)

    assertEquals(delta, WithMiddle(Some("MARIE"), 42))
  }

  test("at an index of a list targets only that element, keeping others intact") {
    val delta = team.deltaModify(_.members.at(1).name).using(_.toUpperCase)

    // Element 0 is not navigated, so it stays as the *original* value (not a delta).
    // Element 1 becomes a delta with its untouched fields blanked out.
    assertEquals(
      delta,
      Team(
        "BOTTOM",
        List(
          alice,
          Person("BOB", 42, Address("BOTTOM", 42))
        )
      )
    )
  }

  test("index over a list targets only that element where present, keeping others intact") {
    val delta = team.deltaModify(_.members.index(1).name).using(_.toUpperCase)

    assertEquals(
      delta,
      Team(
        "BOTTOM",
        List(
          alice,
          Person("BOB", 42, Address("BOTTOM", 42))
        )
      )
    )
  }

  test("atOrElse appends a default delta when the index is missing") {
    val delta = team.deltaModify(_.members.atOrElse(5, missing).name).using(_.toUpperCase)

    assertEquals(
      delta,
      Team(
        "BOTTOM",
        List(alice, bob, Person("MISSING", 42, Address("BOTTOM", 42)))
      )
    )
  }

  test("modify a value in a Map via at") {
    val delta = registry.deltaModify(_.people.at("key2").name).using(_.toUpperCase)

    assertEquals(
      delta,
      Registry(
        Map(
          "key1" -> alice,
          "key2" -> Person("BOB", 42, Address("BOTTOM", 42))
        )
      )
    )
  }

  test("index over a Map is a no-op on the map when the key is missing") {
    val delta = registry.deltaModify(_.people.index("nope").name).using(_.toUpperCase)

    // index only modifies when present, so the untouched map keeps its original contents
    assertEquals(delta, Registry(Map("key1" -> alice, "key2" -> bob)))
  }

  test("modify a LastWriterWins payload, neighbouring fields become bottom") {
    val res = doc.deltaModify(_.title).using(_.write("New title"))

    assertEquals(res.title.read, "New title")
    assertEquals(res.body, "BOTTOM")
  }

  test("modify nested inside a LastWriterWins payload") {
    val res = doc.deltaModify(_.title.payload).using(_.toUpperCase)

    assertEquals(res.title.read, "HELLO")
    assertEquals(res.body, "BOTTOM")
  }

  test("modify an ObserveRemoveMap field applying a CRDT operation, neighbouring fields bottom") {
    val recipeBook: RecipeBook =
      RecipeBook(
        ObserveRemoveMap(Map("pasta" -> ObserveRemoveMap.Entry(Dots.empty, 2)), Dots.empty),
        3
      )
    val res = recipeBook.deltaModify(_.recipes).using(_.update("pasta", 3))

    assertEquals(res.recipes.get("pasta"), Some(3))
    // `version` is untouched, so it becomes the Int bottom (42)
    assertEquals(res.version, 42)
  }

  test("modify a ReplicatedList applying an append produces a mergeable delta") {
    // a full ReplicatedList is obtained by merging the append deltas onto the empty list
    var full: ReplicatedList[String] = ReplicatedList.empty[String]
    full = full `merge` full.append("a")
    full = full `merge` full.append("b")
    assertEquals(full.toList, List("a", "b")) // sanity check on the fixture

    val listDoc: ListDoc = ListDoc("owner", full)
    val res              = listDoc.deltaModify(_.items).using(_.append("c"))

    // `res.items` is the *delta* produced by append("c"); merging it onto the original
    // items yields the intended target list. The neighbouring `owner` field is bottom.
    assertEquals((full `merge` res.items).toList, List("a", "b", "c"))
    assertEquals(res.owner, "BOTTOM")
  }

  test("setTo on an RDT-typed field keeps the set constant and blanks siblings") {
    val recipeBook: RecipeBook =
      RecipeBook(
        ObserveRemoveMap(Map("pasta" -> ObserveRemoveMap.Entry(Dots.empty, 2)), Dots.empty),
        3
      )
    val res = recipeBook.deltaModify(_.recipes).setTo(ObserveRemoveMap.empty)

    assertEquals(res.recipes, ObserveRemoveMap.empty)
    assertEquals(res.version, 42)
  }

  test("modify a MultiVersionRegister by writing concurrently yields a write delta") {
    val mvrDoc: MvrDoc = MvrDoc("owner", rdts.datatypes.MultiVersionRegister.empty[Int])

    val res = mvrDoc.deltaModify(_.reg).using(_.writeConcurrent(5))

    assertEquals(res.reg.read, Set(5))
    assertEquals(res.owner, "BOTTOM")
  }

  test("modify an EnableWinsFlag by enabling yields an enable delta") {
    val flagDoc: FlagDoc = FlagDoc("owner", rdts.datatypes.EnableWinsFlag.empty)

    val res = flagDoc.deltaModify(_.flag).using(_.enable())

    assert(res.flag.read)
    assertEquals(res.owner, "BOTTOM")
  }

  test("modify an Epoch value keeps the untouched counter bottom-derived sibling") {
    val epochDoc: EpochDoc = EpochDoc("owner", Epoch(0, 7))

    val res = epochDoc.deltaModify(_.epoch).using(_.write(9))

    assertEquals(res.epoch.read, 9)
    assertEquals(res.owner, "BOTTOM")
  }

  test("navigate into a Map value and modify a nested RDT payload") {
    val cookbook: CookBook = CookBook(
      "owner",
      Map(
        "main" -> Recipe(LastWriterWins.now("bolognese"), 4),
        "side" -> Recipe(LastWriterWins.now("salad"), 2)
      )
    )

    val res = cookbook.deltaModify(_.recipes.at("main").title.payload).using(_.toUpperCase)

    // the navigated map value becomes a delta: its `title` keeps the modified payload but the
    // untouched `timestamp` inside the LWW becomes the CausalTime bottom, and `servings` is bottom.
    // the un-navigated "side" entry keeps its original value, and `owner` is bottom.
    assertEquals(
      res,
      CookBook(
        "BOTTOM",
        Map(
          "main" -> Recipe(LastWriterWins(CausalTime.empty, "BOLOGNESE"), 42),
          "side" -> cookbook.recipes("side")
        )
      )
    )
  }

  test("eachWhere only transforms matching elements, others keep their original value") {
    val delta = team.deltaModify(_.members.eachWhere(_.age > 27).name).using(_.toUpperCase)

    assertEquals(
      delta,
      Team(
        "BOTTOM",
        List(
          Person("ALICE", 42, Address("BOTTOM", 42)),
          bob // age 25 does not match the predicate, so it is left as the original value
        )
      )
    )
  }

  // ── deltaModifyAll ──────────────────────────────────────────────────────────────

  test("deltaModifyAll blanks out fields not on any given path") {
    val delta = alice.deltaModifyAll(_.name, _.address.street).using(_.toUpperCase)

    // Both navigated fields get the modification; `age` and `address.zip` are bottom.
    assertEquals(delta, Person("ALICE", 42, Address("MAIN ST", 42)))
  }

  test("deltaModifyAll on a single path behaves like deltaModify") {
    val d1 = alice.deltaModify(_.name).using(_.toUpperCase)
    val d2 = alice.deltaModifyAll(_.name).using(_.toUpperCase)

    assertEquals(d1, d2)
  }

  test("deltaModifyAll on nested paths through a collection targets all elements") {
    val delta = team.deltaModifyAll(
      _.members.each.name,
      _.members.each.address.street
    ).using(_.toUpperCase)

    assertEquals(
      delta,
      Team(
        "BOTTOM",
        List(
          Person("ALICE", 42, Address("MAIN ST", 42)),
          Person("BOB", 42, Address("2ND ST", 42))
        )
      )
    )
  }

  // ── deltaModifyLens ─────────────────────────────────────────────────────────────

  test("deltaModifyLens produces a PathLazyModify with delta semantics") {
    val lens = modifyLens[Person].delta(_.address.street)
    val res  = lens.using(_.toUpperCase)(alice)

    assertEquals(res, Person("BOTTOM", 42, Address("MAIN ST", 42)))
  }

  test("deltaModifyLens can be composed with andThenModify") {
    // Compose two delta lenses: first navigate into address, then into street of that address
    val addressLens = modifyLens[Person].delta(_.address)
    val streetLens  = modifyLens[Address].apply(_.street)
    val composed    = addressLens.andThenModify(streetLens)

    val res = composed.using(_.toUpperCase)(alice)

    // The outer delta blanks out Person fields not on the path, so `name`/`age` become bool.
    // The inner (non-delta) modify targets `street` inside the already-navigated address;
    // it preserves the other address fields (zip unchanged).
    assertEquals(res, Person("BOTTOM", 42, Address("MAIN ST", 12345)))
  }

  // ── deltaModifyAllLens ──────────────────────────────────────────────────────────

  test("deltaModifyAllLens produces a multi-path delta lens") {
    val lens = modifyAllLens[Person].delta(_.name, _.address.street)
    val res  = lens.using(_.toUpperCase)(alice)

    assertEquals(res, Person("ALICE", 42, Address("MAIN ST", 42)))
  }
}

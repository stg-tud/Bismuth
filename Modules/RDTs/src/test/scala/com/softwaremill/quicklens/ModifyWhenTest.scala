package com.softwaremill.deltalens

import rdts.syntax.deltalens.*

object ModifyWhenTestData {
  trait Animal
  case class Dog(age: Int)        extends Animal
  case class Cat(ages: List[Int]) extends Animal
  case class Zoo(animals: List[Animal])

  val dog: Animal      = Dog(4)
  val olderDog: Animal = Dog(5)

  val cat: Animal      = Cat(List(3, 12, 13))
  val olderCat: Animal = Cat(List(4, 12, 13))

  val zoo: Zoo      = Zoo(List(dog, cat))
  val olderZoo: Zoo = Zoo(List(olderDog, olderCat))

  trait MyOption[+A]
  case class MySome[+A](value: A) extends MyOption[A]
  case object MyNone              extends MyOption[Nothing]

  val someDog: MyOption[Dog]      = MySome(Dog(4))
  val someOlderDog: MyOption[Dog] = MySome(Dog(5))
  val noDog: MyOption[Dog]        = MyNone
}

class ModifyWhenTest extends munit.FunSuite {
  import ModifyWhenTestData.*

  test("modify a field in a subtype") {
    assertEquals(dog.modify(_.when[Dog].age).using(_ + 1), olderDog)
  }

  test("ignore subtypes other than the selected one") {
    assertEquals(cat.modify(_.when[Dog].age).using(_ + 1), cat)
  }

  test("modify a Functor field in a subtype") {
    assertEquals(cat.modify(_.when[Cat].ages.at(0)).using(_ + 1), olderCat)
  }

  test("modify a field in a subtype through a Functor") {
    assertEquals(
      zoo
        .modifyAll(
          _.animals.each.when[Dog].age,
          _.animals.each.when[Cat].ages.at(0)
        )
        .using(_ + 1),
      olderZoo
    )
  }

  test("modify a field in a subtypes (parameterized)") {
    assertEquals(someDog.modify(_.when[MySome[Dog]].value.age).using(_ + 1), someOlderDog)
  }

  test("ignore subtypes other than the selected one (parameterized)") {
    assertEquals(noDog.modify(_.when[MySome[Dog]].value.age).using(_ + 1), MyNone)
  }
}

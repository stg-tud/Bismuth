package tests.rescala

import munit.FunSuite
import org.scalajs.dom
import org.scalajs.dom.Element
import org.scalajs.dom.html.Span
import reactives.default.*
import reactives.extra.Tags
import reactives.extra.Tags.*
import scalatags.JsDom.all.*
import scalatags.generic.StylePair

import scala.util.chaining.scalaUtilChainingOps

class RescalatagsTest extends FunSuite {

  test("put var into dom") {

    val v        = Var.empty[Element]
    val rendered = div().render.reattach(v)
    assertEquals(rendered.textContent, "", "empty var gives empty frag")

    assertEquals(rendered.innerHTML, "<!--reattach start--><!--reattach end-->", "empty var into dom is empty")

    v.set(span("hallo welt").render)
    assertEquals(
      rendered.innerHTML,
      "<!--reattach start--><span>hallo welt</span><!--reattach end-->",
      "setting var changes rendered outer tag"
    )

    v.set(div("hallo div").render)
    assertEquals(
      rendered.innerHTML,
      "<!--reattach start--><div>hallo div</div><!--reattach end-->",
      "resetting var changes rendered outer tag"
    )

  }

  given RangeSplice[dom.Element, Modifier] with {
    override def splice(anchor: dom.Element, range: dom.Range, value: Modifier): Unit =
        val parent = range.commonAncestorContainer
        parent match
            case elem: dom.Element => value.applyTo(elem)
  }

  test("put style into dom") {
    val v: Var[String] = Var.empty[String]

    val ourTag: Span = span.render.reattach(v.map(backgroundColor := _))

    assertEquals(ourTag.style.getPropertyValue(backgroundColor.cssName), "", "empty color does not render")

    v.set("red")
    assertEquals(ourTag.style.getPropertyValue(backgroundColor.cssName), "red", "changing var changes color")

    v.set("blue")
    assertEquals(ourTag.style.getPropertyValue(backgroundColor.cssName), "blue", "changing var changes color again")
  }

  test("put attribute into dom") {
    val v = Var.empty[String]

    val ourTag = a().render.reattach(v.map(href := _))

    assertEquals(ourTag.outerHTML, "<a><!--reattach start--><!--reattach end--></a>", "empty href does not render")

    v.set("www.rescala-lang.com")
    assertEquals(
      ourTag.outerHTML,
      "<a href=\"www.rescala-lang.com\"><!--reattach start--><!--reattach end--></a>",
      "changing var changes href"
    )

    v.set("index.html")
    assertEquals(
      ourTag.outerHTML,
      "<a href=\"index.html\"><!--reattach start--><!--reattach end--></a>",
      "changing var changes href again"
    )

  }

  test("work with multiple childern") {

    val v = Var(Seq(span("hey"), span("ho")))

    def vrend = v.map(_.map(_.render))

    val outerR                 = div().render.reattach(vrend)
    val outerWithOtherChildren = div(span("before")).render.reattach(vrend).tap(_.append(span("after").render))

    assertEquals(
      outerR.innerHTML,
      "<!--reattach start--><span>hey</span><span>ho</span><!--reattach end-->",
      "render fragments"
    )
    assertEquals(
      outerWithOtherChildren.innerHTML,
      "<span>before</span><!--reattach start--><span>hey</span><span>ho</span><!--reattach end--><span>after</span>",
      "render fragments2"
    )

    v.set(Seq(span("hallo welt")))
    assertEquals(
      outerR.innerHTML,
      "<!--reattach start--><span>hallo welt</span><!--reattach end-->",
      "setting to less elements works"
    )
    assertEquals(
      outerWithOtherChildren.innerHTML,
      "<span>before</span><!--reattach start--><span>hallo welt</span><!--reattach end--><span>after</span>",
      "setting to less elements works2"
    )

    v.set(Seq(span("hey2"), span("ho2")))
    assertEquals(
      outerR.innerHTML,
      "<!--reattach start--><span>hey2</span><span>ho2</span><!--reattach end-->",
      "increasing works"
    )
    assertEquals(
      outerWithOtherChildren.innerHTML,
      "<span>before</span><!--reattach start--><span>hey2</span><span>ho2</span><!--reattach end--><span>after</span>",
      "increasing works2"
    )

  }
}

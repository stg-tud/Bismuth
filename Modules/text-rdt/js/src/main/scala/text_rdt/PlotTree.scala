package text_rdt

import org.scalajs.dom.document
import org.scalajs.dom.SVGTextElement
import scala.scalajs.js

case class LayoutReturn(
    leftWidth: Double,
    rightWidth: Double,
    var height: Double
)

def layout(
    selector: String,
    node: MyD3TreeNode,
    x: Double,
    y: Double
): LayoutReturn = {
  val verticalDistance = 40
  val horizontalDistance = 10

  var leftWidth = 0.0
  var rightWidth = 0.0
  var maxHeight = y

  node.leftChildren.foreach(d3Child => {
    val childLayout =
      layout(
        selector,
        d3Child,
        x + leftWidth,
        y + verticalDistance * node.leftChildren.length
      )
    leftWidth += childLayout.leftWidth + childLayout.rightWidth
    leftWidth += horizontalDistance

    maxHeight = Math.max(maxHeight, childLayout.height)
  })

  val element = document.querySelector(
    selector +
      " #x" +
      node.value.toList
        .map(_.toInt.toHexString)
        .mkString
  )
  element match {
    case element: SVGTextElement => {
      val bbox = element.getBBox()
      leftWidth += bbox.width / 2
      rightWidth += bbox.width / 2
      node.width = bbox.width
      node.height = bbox.height
    }
    case default =>
      leftWidth += (node.value.length * 10f).round
      rightWidth += (node.value.length * 10f).round
      node.height = 40
  }

  node.x = x + leftWidth
  node.y = y

  node.rightChildren.foreach(d3Child => {
    rightWidth += horizontalDistance
    val childLayout =
      layout(
        selector,
        d3Child,
        x + leftWidth + rightWidth,
        y + verticalDistance * node.rightChildren.length
      )
    rightWidth += childLayout.leftWidth + childLayout.rightWidth

    maxHeight = Math.max(maxHeight, childLayout.height)
  })

  LayoutReturn(
    leftWidth,
    rightWidth,
    maxHeight
  )
}

def plotTreeNode(node: MyD3TreeNode): String = {
  (node.leftChildren ++ node.rightChildren)
    .map(child => {
      s"""<path shape-rendering="geometricPrecision" stroke="black" stroke-width="1.5" d="M${node.x},${node.y + node.height}L${child.x},${child.y - 5}"></path>""" +
        plotTreeNode(child)
    })
    .mkString
    + s"""<text font-size="smaller" font-family="'Noto Sans Mono', 'Noto Sans Symbols 2'" id="x${node.value.toList
        .map(_.toInt.toHexString)
        .mkString}" shape-rendering="geometricPrecision" x="${node.x}" y="${node.y}" dominant-baseline="hanging" text-anchor="middle">${node.value}</text>
        """

}

def plotTreeInternal(selector: String, myRoot: MyD3TreeNode) = {
  val layoutResult = layout(selector, myRoot, 0, 0)
  val result =
    s"""<svg version="1.1" xmlns="http://www.w3.org/2000/svg" width="${layoutResult.leftWidth + layoutResult.rightWidth}" height="${14 + layoutResult.height}">"""
      +
        plotTreeNode(myRoot)
        + "</svg>"
  js.Dynamic.global.document
    .querySelector(selector)
    .innerHTML = result;
}

def plotTree(selector: String, myRoot: MyD3TreeNode): Unit = {
  plotTreeInternal(selector, myRoot)
  plotTreeInternal(selector, myRoot)
}

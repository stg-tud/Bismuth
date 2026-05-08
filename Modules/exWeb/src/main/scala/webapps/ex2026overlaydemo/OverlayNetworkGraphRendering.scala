package webapps.ex2026overlaydemo

import org.scalajs.dom
import org.scalajs.dom.{CanvasRenderingContext2D, HTMLCanvasElement}
import webapps.ex2026overlaydemo.OverlayNetworkGraphModel.{EdgeKind, GraphEdge, GraphNode}

import scala.scalajs.js

object OverlayNetworkGraphRendering {

  private def rgba(r: Int, g: Int, b: Int, a: Double): String =
    s"rgba($r, $g, $b, ${math.max(0.0, math.min(1.0, a))})"

  def syncCanvasResolution(canvas: HTMLCanvasElement, ctx: CanvasRenderingContext2D): (Double, Double) = {
    val dpr           = math.max(1.0, dom.window.devicePixelRatio)
    val logicalWidth  = math.max(300.0, canvas.clientWidth.toDouble)
    val logicalHeight = math.max(300.0, canvas.clientHeight.toDouble)
    val pixelWidth    = math.round(logicalWidth * dpr).toInt
    val pixelHeight   = math.round(logicalHeight * dpr).toInt
    if canvas.width != pixelWidth || canvas.height != pixelHeight then {
      canvas.width = pixelWidth
      canvas.height = pixelHeight
    }
    ctx.setTransform(dpr, 0, 0, dpr, 0, 0)
    (logicalWidth, logicalHeight)
  }

  def renderGraph(
      ctx: CanvasRenderingContext2D,
      width: Double,
      height: Double,
      nodes: Vector[GraphNode],
      edges: Vector[GraphEdge]
  ): Unit = {
    ctx.fillStyle = "#0b1020"
    ctx.fillRect(0, 0, width, height)

    val byUid = nodes.map(n => n.uid -> n).toMap

    edges.foreach { edge =>
      edge.kind match
          case EdgeKind.EagerOverlay =>
            ctx.strokeStyle = rgba(34, 197, 94, 0.95 * edge.opacity)
            ctx.setLineDash(js.Array())
            ctx.lineWidth = 3.0
          case EdgeKind.ActiveOverlay =>
            ctx.strokeStyle = rgba(96, 165, 250, 0.85 * edge.opacity)
            ctx.setLineDash(js.Array())
            ctx.lineWidth = 2.0
          case EdgeKind.PassiveOverlay =>
            ctx.strokeStyle = rgba(148, 163, 184, 0.55 * edge.opacity)
            ctx.fillStyle = rgba(148, 163, 184, 0.75 * edge.opacity)
            ctx.setLineDash(js.Array(6, 6))
            ctx.lineWidth = 1.25
      for
          a <- byUid.get(edge.from)
          b <- byUid.get(edge.to)
      do
          edge.kind match
              case EdgeKind.PassiveOverlay => renderPassiveArrow(ctx, a.x, a.y, b.x, b.y)
              case EdgeKind.ActiveOverlay  => renderHalfLine(ctx, a.x, a.y, b.x, b.y)
              case EdgeKind.EagerOverlay   => renderHalfLine(ctx, a.x, a.y, b.x, b.y)
    }
    ctx.setLineDash(js.Array())

    nodes.foreach { node =>
      ctx.beginPath()
      ctx.fillStyle =
        if node.highlighted then rgba(253, 230, 138, node.opacity)
        else rgba(96, 165, 250, node.opacity)
      ctx.arc(node.x, node.y, 8, 0, math.Pi * 2)
      ctx.fill()

      ctx.fillStyle = rgba(226, 232, 240, node.opacity)
      ctx.font = "10px sans-serif"
      ctx.fillText(node.label.take(12), node.x + 10, node.y - 2)
      if node.details.nonEmpty then {
        ctx.fillStyle = rgba(148, 163, 184, node.opacity)
        ctx.font = "9px sans-serif"
        ctx.fillText(node.details.take(72), node.x + 10, node.y + 9)
      }
    }
  }

  private def renderHalfLine(
      ctx: CanvasRenderingContext2D,
      fromX: Double,
      fromY: Double,
      toX: Double,
      toY: Double
  ): Unit = {
    val dx          = toX - fromX
    val dy          = toY - fromY
    val distance    = math.max(1.0, math.sqrt(dx * dx + dy * dy))
    val ux          = dx / distance
    val uy          = dy / distance
    val startOffset = 10.0
    val endDistance = math.max(startOffset + 8.0, distance * 0.5)
    val startX      = fromX + ux * startOffset
    val startY      = fromY + uy * startOffset
    val endX        = fromX + ux * endDistance
    val endY        = fromY + uy * endDistance

    ctx.beginPath()
    ctx.moveTo(startX, startY)
    ctx.lineTo(endX, endY)
    ctx.stroke()
  }

  private def renderPassiveArrow(
      ctx: CanvasRenderingContext2D,
      fromX: Double,
      fromY: Double,
      toX: Double,
      toY: Double
  ): Unit = {
    val dx          = toX - fromX
    val dy          = toY - fromY
    val distance    = math.max(1.0, math.sqrt(dx * dx + dy * dy))
    val ux          = dx / distance
    val uy          = dy / distance
    val startOffset = 10.0
    val endDistance = math.max(startOffset + 8.0, distance * 0.5)
    val startX      = fromX + ux * startOffset
    val startY      = fromY + uy * startOffset
    val endX        = fromX + ux * endDistance
    val endY        = fromY + uy * endDistance

    renderHalfLine(ctx, fromX, fromY, toX, toY)

    val arrowSize = 6.0
    val leftX     = endX - ux * arrowSize - uy * arrowSize * 0.6
    val leftY     = endY - uy * arrowSize + ux * arrowSize * 0.6
    val rightX    = endX - ux * arrowSize + uy * arrowSize * 0.6
    val rightY    = endY - uy * arrowSize - ux * arrowSize * 0.6

    ctx.beginPath()
    ctx.moveTo(endX, endY)
    ctx.lineTo(leftX, leftY)
    ctx.lineTo(rightX, rightY)
    ctx.closePath()
    ctx.fill()
  }
}

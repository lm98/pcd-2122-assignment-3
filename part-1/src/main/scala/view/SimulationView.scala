package view

import view.SimulationView
import view.ViewUtils
import com.sun.java.accessibility.util.AWTEventMonitor.{addKeyListener, addWindowListener}
import model.Body
import model.Boundary
import model.Objects2d.P2d

import java.awt.event.{ActionListener, KeyEvent, KeyListener, WindowAdapter, WindowEvent}
import java.awt.{BorderLayout, Dimension, Graphics2D, RenderingHints}
import javax.swing.SwingUtilities
import scala.language.postfixOps
import scala.swing.*
import scala.swing.BorderPanel.Position.*
import scala.swing.event.ButtonClicked

class SimulationView(view: ViewUtils, w: Int, h: Int) extends Frame:
  val visualiserPanel = new VisualiserPanel(w,h, view.bounds)
  size = Dimension(w + 100, h + 100)
  title = "Bodies simulation"
  resizable = false
  contents = new BorderPanel{
    layout(ControlPanel(view)) = North
    layout(visualiserPanel) = Center
  }
  visible = true
  addWindowListener( new WindowAdapter {
    override def windowClosing(ev: WindowEvent): Unit = System.exit(-1)
    override def windowClosed(ev: WindowEvent): Unit = System.exit(-1)
  })
  def display(bodies: List[Body], vt: Double, iter: Long, bounds: Boundary): Unit = //todo add here actor?
    SwingUtilities.invokeLater(() =>
      visualiserPanel display(bodies, vt, iter)
      repaint()
    )
end SimulationView

sealed class ControlPanel(view: ViewUtils) extends FlowPanel:
  val start: Button = new Button("start"){
    reactions += {
      case event.ButtonClicked(_) =>
        enabled = false
        stop.enabled = true
        view.startSimulation()
    }
  }
  val stop: Button = new Button("stop"){
    reactions += {
      case event.ButtonClicked(_) =>
        enabled = false
        start.enabled = true
        view.stopSimulation()
    }
  }
  contents += start
  contents += stop
end ControlPanel


sealed class VisualiserPanel(w: Int, h: Int, bound: Boundary) extends Panel, KeyListener:
  var bodies: List[Body] = List()
  var nIter: Int = 0
  var vt: Double = 0
  var scale: Double = 1
  val dx: Long = w/2 -20
  val dy: Long = h/2 -20

  preferredSize = Dimension(w,h)

  override def paint(g: Graphics2D): Unit =
    if bodies.nonEmpty then
      val g2: Graphics2D = g
      g2 setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
      g2 setRenderingHint(RenderingHints.KEY_RENDERING, RenderingHints.VALUE_RENDER_QUALITY)
      g2 clearRect(0, 0, w, h)

      val x0 = getXCoord(bound.x0)
      val y0 = getYCoord(bound.y0)
      val wd = getXCoord(bound.x1) - x0
      val ht = y0 - getYCoord(bound.y1)

      g2 drawRect(x0, y0 - ht, wd, ht)

      bodies.foreach( b =>
        val p: P2d = b.pos
        var radius: Int = (10 * scale).toInt
        if radius < 1 then radius = 1
        g2 drawOval(getXCoord(p.x), getYCoord(p.y), radius, radius)
      )
      val time: String = String format("%.2f", vt)
      g2 drawString("Bodies: " + bodies.length + " - vt: " + time + " - nIter: " + nIter + " (UP for zoom in, DOWN for zoom out)", 2, 20)

  def getXCoord(x: Double): Int = (dx + (x * dx * scale)).toInt
  def getYCoord(y: Double): Int = (dy - (y * dy * scale)).toInt

  def display(b: List[Body], v: Double, iter: Long): Unit =
    this.bodies = b
    this.vt = v
    this.nIter = iter.toInt

  override def keyPressed(e: KeyEvent): Unit = e.getKeyCode match
    case KeyEvent.VK_UP => updateScale(1.1)
    case KeyEvent.VK_DOWN => updateScale(0.9)

  override def keyTyped(e: KeyEvent): Unit = {}
  override def keyReleased(e: KeyEvent): Unit = {}
  private def updateScale(k: Double): Unit = scale *= k

end VisualiserPanel

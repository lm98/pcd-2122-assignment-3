package view

import model.{Costants, PluviometerState, RectangleBounds, Zone, ZoneState}

import scala.util.Random
import com.sun.java.accessibility.util.AWTEventMonitor.{addActionListener, addWindowListener}

import java.awt.event.{WindowAdapter, WindowEvent}
import java.awt.{Dimension, Graphics2D, RenderingHints, TextArea}
import javax.swing.{BorderFactory, SwingUtilities}
import scala.::
import scala.language.postfixOps
import scala.swing.*
import scala.swing.Action.NoAction.enabled
import scala.swing.BorderPanel.Position.*


class AppView(val zones: List[Zone], width: Int = 820, height: Int = 520) extends Frame:
  val cityPanel: CityPanel = new CityPanel
  val buttonsPanel: ManagePanel = new ManagePanel
  size = Dimension(width + 100, height + 100)
  title = "Zones with rain detector"
  contents = new BorderPanel{
    layout(cityPanel) = North
    layout(buttonsPanel) = Center
  }
  resizable = false
  visible = true
  addWindowListener( new WindowAdapter {
    override def windowClosing(ev: WindowEvent): Unit = System.exit(-1)
    override def windowClosed(ev: WindowEvent): Unit = System.exit(-1)
  })
  def display(): Unit = ??? //todo change area status

  class CityPanel extends Panel:
    preferredSize = Dimension(600,300)

    override def paint(g: Graphics2D): Unit =
      val g2: Graphics2D = g
      g2 setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
      g2 setRenderingHint(RenderingHints.KEY_RENDERING, RenderingHints.VALUE_RENDER_QUALITY)
//      g2 drawRect (0, 0, 700, 700)
      g2 setColor java.awt.Color.BLACK
      zones.foreach(zone => {
        zone.state match
          case ZoneState.Ok => g2.setColor(java.awt.Color.GREEN)
          case ZoneState.Alarm => g2.setColor(java.awt.Color.RED)
          case ZoneState.Managing => g2.setColor(java.awt.Color.CYAN)
        g2 fillRect(zone.bounds.x0, zone.bounds.y0, zone.bounds.width, zone.bounds.height)
        g2.setColor(java.awt.Color.BLACK)
        g2 drawString(s"ZONE ${zone.id}", zone.bounds.x0 + 5, zone.bounds.y0 + 15)
//        g2 drawString (s"Rain Gauges: ${zone.numDevices}", zone.bounds.x0 + 10, zone.bounds.y0 + 30)
        zone.pluviometers.foreach(p => {
          g2.fillOval(p.x, p.y, 10, 10)
          p.state match
            case PluviometerState.Ok => g2.setColor(java.awt.Color.BLACK)
            case PluviometerState.Alarm => g2.setColor(java.awt.Color.BLUE)
        })
        addActionListener(l => {

        })
        g2 drawRect(zone.bounds.x0, zone.bounds.y0, zone.bounds.width, zone.bounds.height)
      })

  class ManagePanel extends BoxPanel(Orientation.Vertical):
    zones.foreach(z => {
      val textArea = new TextField(s"\tZone ${z.id}\tRain gauges = ${z.pluviometers.size}\tStatus: ${z.state} ")
      textArea.preferredSize = Dimension(50,30)
      textArea.editable = false
      val buttonManage: Button = new Button {
        text = s"Manage Zone"
        action = new Action(s"Manage Zone ${z.id}"):
          override def apply(): Unit =
            enabled = false
      }
      z.state match
        case ZoneState.Ok =>
          buttonManage.enabled = false
          buttonManage.visible = false
        case ZoneState.Managing =>
          buttonManage.enabled = false
          buttonManage.visible = true
        case ZoneState.Alarm =>
          buttonManage.enabled = true
          buttonManage.visible = true
      contents += textArea
      contents += buttonManage
    })
    /*zones.foreach(z => {
      z.state match
        case ZoneState.Alarm => manage.visible = true
        case ZoneState.Managing => manage.enabled = false
        case ZoneState.Ok => manage.visible = false
    })*/

import Costants.*
@main def testMain(): Unit =
  val rand = new Random()
  val rows: Int = 2
  val cols: Int = 3
  var x: Int = 0
  val zones = for
    r <- 0 until rows
    c <- 0 until cols
  yield
    x = x + 1
    new Zone(x, ZoneState.Managing, rand.between(1,4), new RectangleBounds(c *  Costants.defalutWidth, r * Costants.defaultHeight))

  new AppView(zones.toList)


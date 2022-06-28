package view

import model.Zone
import model.ZoneState

import scala.util.Random
import com.sun.java.accessibility.util.AWTEventMonitor.addWindowListener

import java.awt.event.{WindowAdapter, WindowEvent}
import java.awt.{Dimension, Graphics2D, RenderingHints}
import javax.swing.{BorderFactory, SwingUtilities}
import scala.::
import scala.language.postfixOps
import scala.swing.*
import scala.swing.BorderPanel.Position.*


class AppView(zones: Int = 6, width: Int = 720, height: Int = 720) extends Frame:
  val cityPanel: CityPanel = new CityPanel()
  size = Dimension(width + 100, height + 100)
  title = "Zones with rain detector"
  contents = new BorderPanel{
    layout(cityPanel) = Center
  }
  resizable = false
  visible = true
  addWindowListener( new WindowAdapter {
    override def windowClosing(ev: WindowEvent): Unit = System.exit(-1)
    override def windowClosed(ev: WindowEvent): Unit = System.exit(-1)
  })
  def display(): Unit = ??? //todo change area status

  class CityPanel(numZones: Int = 6) extends Panel:
    val rand: Int = new Random().between(1,4)
    val zones = for x <- 1 until (numZones + 1) yield
      Zone(x, ZoneState.Ok, rand)

    preferredSize = Dimension(600,600)

    override def paint(g: Graphics2D): Unit =
      val g2: Graphics2D = g
      g2 setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
      g2 setRenderingHint(RenderingHints.KEY_RENDERING, RenderingHints.VALUE_RENDER_QUALITY)
      g2 drawRect (0, 0, 700, 700)
      g2 setColor java.awt.Color.BLACK
      zones.foreach(zone => {
        println(zone.id + " " + zone.state)
        zone.state match
          case ZoneState.Ok => g2.setColor(java.awt.Color.GREEN)
          case ZoneState.Alarm => g2.setColor(java.awt.Color.RED)
          case ZoneState.Managing => g2.setColor(java.awt.Color.CYAN)
        g2 fillRect((zone.id + 1) * 50, (zone.id + 1) * 30, zone.width, zone.height)
        g2.setColor(java.awt.Color.BLACK)
        g2 drawString(s"ZONE ${zone.id} - status: ${zone.state.toString}", 5 + 5, 7 + 15)
        g2 drawString (s"Pluviometers: ${zone.numDevices}", 5 + 5, 23 + 15)
        val rand = new Random().between(50, 150)
        for i <- 0 until zone.numDevices yield
          g2.fillOval(rand, rand, 10, 10)
        g2 drawRect(5, 7, 200 - 1, 150 - 1)
      })


  class ZonePanel(zone: Zone) extends Panel: //todo status will be enum with ok/alarm/management
    val manage: Button = new Button("Manage") {
      enabled = false
      action = new Action("Manage Zone") :
        def apply(): Unit = ??? //todo send manage action
    }
    override def paint(g: Graphics2D): Unit =
      val g2: Graphics2D = g
      g2 setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
      g2 setRenderingHint(RenderingHints.KEY_RENDERING, RenderingHints.VALUE_RENDER_QUALITY)


@main def testMain(): Unit =
  new AppView()

package view

import actors.ViewActor
import akka.actor.typed.{ActorRef, ActorSystem, Behavior}
import model.{Costants, Pluviometer, PluviometerState, RectangleBounds, Zone, ZoneState}

import scala.util.Random
import com.sun.java.accessibility.util.AWTEventMonitor.{addActionListener, addWindowListener}

import java.awt.event.{WindowAdapter, WindowEvent}
import java.awt.{Dimension, Graphics2D, RenderingHints, TextArea}
import javax.swing.{BorderFactory, SwingUtilities}
import scala.language.postfixOps
import scala.swing.*
import scala.swing.Action.NoAction.enabled
import scala.swing.BorderPanel.Position.*

trait ViewFunctions:
  def updateGui(newZone: Zone): Unit

class AppView(val zone: Zone, viewActor: ActorRef[ViewActor.Event], width: Int = 820, height: Int = 520) extends Frame with ViewFunctions:
  val cityPanel: CityPanel = new CityPanel
  val buttonsPanel: ManagePanel = new ManagePanel
  size = Dimension(width + 100, height + 100)
  title = "Zones with rain detector"
  contents = new BorderPanel{
    layout(cityPanel) = North
    layout(buttonsPanel) = Center
  }
  resizable = true
  visible = true
  addWindowListener( new WindowAdapter {
    override def windowClosing(ev: WindowEvent): Unit = System.exit(-1)
    override def windowClosed(ev: WindowEvent): Unit = System.exit(-1)
  })
  override def updateGui(newZone: Zone): Unit =
    SwingUtilities.invokeLater(() =>
      /*zones.filter(z => zone.id.equals(z.id)).foreach(u =>
        u.changeState(zone.state)
      )*/
      zone.changeState(newZone.state)
      buttonsPanel.display()
      repaint()
    )

  class CityPanel extends Panel:
    preferredSize = Dimension(600,300)

    override def paint(g: Graphics2D): Unit =
      val g2: Graphics2D = g
      g2 setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
      g2 setRenderingHint(RenderingHints.KEY_RENDERING, RenderingHints.VALUE_RENDER_QUALITY)
      g2 setColor java.awt.Color.BLACK
      zone.state match
        case ZoneState.Ok => g2.setColor(java.awt.Color.GREEN)
        case ZoneState.Alarm => g2.setColor(java.awt.Color.RED)
        case ZoneState.Managing => g2.setColor(java.awt.Color.CYAN)
      g2 fillRect(zone.bounds.x0, zone.bounds.y0, zone.bounds.width, zone.bounds.height)
      g2.setColor(java.awt.Color.BLACK)
      g2 drawString(s"ZONE ${zone.id} - ${zone.state.toString}", zone.bounds.x0 + 5, zone.bounds.y0 + 15)
      zone.pluviometers.foreach(p => {
        g2.fillOval(p.x, p.y, 10, 10)
        p.state match
          case PluviometerState.Ok => g2.setColor(java.awt.Color.BLACK)
          case PluviometerState.Alarm => g2.setColor(java.awt.Color.BLUE)
      })
      g2 drawRect(zone.bounds.x0, zone.bounds.y0, zone.bounds.width, zone.bounds.height)
      /*zones.foreach(zone => {
        zone.state match
          case ZoneState.Ok => g2.setColor(java.awt.Color.GREEN)
          case ZoneState.Alarm => g2.setColor(java.awt.Color.RED)
          case ZoneState.Managing => g2.setColor(java.awt.Color.CYAN)
        g2 fillRect(zone.bounds.x0, zone.bounds.y0, zone.bounds.width, zone.bounds.height)
        g2.setColor(java.awt.Color.BLACK)
        g2 drawString(s"ZONE ${zone.id} - ${zone.state.toString}", zone.bounds.x0 + 5, zone.bounds.y0 + 15)
//        g2 drawString (s"Rain Gauges: ${zone.numDevices}", zone.bounds.x0 + 10, zone.bounds.y0 + 30)
        zone.pluviometers.foreach(p => {
          g2.fillOval(p.x, p.y, 10, 10)
          p.state match
            case PluviometerState.Ok => g2.setColor(java.awt.Color.BLACK)
            case PluviometerState.Alarm => g2.setColor(java.awt.Color.BLUE)
        })
        g2 drawRect(zone.bounds.x0, zone.bounds.y0, zone.bounds.width, zone.bounds.height)
      })*/

  sealed class ManagePanel extends BoxPanel(Orientation.Vertical):
    var buttons: Map[Int, Button] = Map()
    var textAreas: Map[Int, TextField] = Map()
    preferredSize = Dimension(600,400)
//    zones.foreach(z => {
      textAreas = textAreas.+((zone.id, new TextField(){
        text = s"\tZone ${zone.id}\tRain gauges = ${zone.pluviometers.size}\tStatus: ${zone.state.toString} "
        editable = false
        preferredSize = Dimension(50,50)
      }))
      buttons = buttons.+((zone.id, new Button{
        visible = false
        action = new Action(s"Manage Zone ${zone.id}"):
          override def apply(): Unit =
//            enabled = false
            viewActor ! ViewActor.ManageAlarm()
        })
      )
      contents += textAreas(zone.id)
      contents += buttons(zone.id)
//    })

    def display(): Unit =
//      zones.foreach(z =>
      textAreas(zone.id).text = s"\tZone ${zone.id}\tRain gauges = ${zone.pluviometers.size}\tStatus: ${zone.state.toString} "
      zone.state match
          case ZoneState.Ok =>
            buttons(zone.id).enabled = false;
            buttons(zone.id).visible = false
          case ZoneState.Managing =>
            buttons(zone.id).enabled = false;
            buttons(zone.id).visible = true
          case ZoneState.Alarm =>
            buttons(zone.id).enabled = true;
            buttons(zone.id).visible = true
//      )
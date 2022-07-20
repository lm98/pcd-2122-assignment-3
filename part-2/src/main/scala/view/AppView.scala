package view

import akka.actor.typed.{ActorRef, ActorSystem, Behavior}
import cluster.view.ViewActor
import model.*

import scala.util.Random
import com.sun.java.accessibility.util.AWTEventMonitor.{addActionListener, addWindowListener}

import java.awt.event.{WindowAdapter, WindowEvent}
import java.awt.{ComponentOrientation, Dimension, Graphics2D, RenderingHints, TextArea}
import javax.swing.{BorderFactory, JScrollPane, SwingUtilities}
import scala.language.postfixOps
import scala.swing.*
import scala.swing.Action.NoAction.enabled
import scala.swing.BorderPanel.Position.*

class AppView(var zones: List[Zone], var fireStations: List[FireStation], var rainGauges: List[RainGauge], viewActor: ActorRef[ViewActor.Event], val rows: Int, val cols: Int, width: Int = 820, height: Int = 520) extends Frame:
  val cityPanel: CityPanel = new CityPanel
  val buttonsPanel: ManagePanel = new ManagePanel
  val scrollArea: ScrollPane = new ScrollPane(buttonsPanel)
  size = Dimension(width + 100, height + 100)
  title = "Zones with rain detector"
  contents = new BorderPanel{
    layout(cityPanel) = North
    layout(scrollArea) = Center
  }
  resizable = true
  visible = true
  addWindowListener( new WindowAdapter {
    override def windowClosing(ev: WindowEvent): Unit = System.exit(-1)
    override def windowClosed(ev: WindowEvent): Unit = System.exit(-1)
  })
  def updateStations(fs: FireStation): Unit =
    SwingUtilities.invokeLater( () =>
      fireStations = fireStations :+ fs
      buttonsPanel.display()
      repaint()
    )
  def updateFireStationsState(fs: FireStation): Unit =
    fireStations = fireStations.filterNot( f => f.pos.equals(fs.pos))
    updateStations(fs)
  def updateRainGauges(rainGauge: RainGauge): Unit =
    SwingUtilities.invokeLater( () =>
      rainGauges = rainGauges :+ rainGauge
      buttonsPanel.display()
      repaint()
    )
  def updateGui(zs: List[Zone]): Unit =
    SwingUtilities.invokeLater(() =>
      zones = zs
      buttonsPanel.display()
      repaint()
    )

  class CityPanel extends Panel:
    preferredSize = Dimension(cols * Costants.defalutWidth,rows * Costants.defaultHeight)

    override def paint(g: Graphics2D): Unit =
      val g2: Graphics2D = g
      g2 setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
      g2 setRenderingHint(RenderingHints.KEY_RENDERING, RenderingHints.VALUE_RENDER_QUALITY)
      g2 setColor java.awt.Color.BLACK
      zones.foreach(zone => {
        zone.state match
          case ZoneState.Ok => g2.setColor(java.awt.Color.GREEN)
          case ZoneState.Alarm => g2.setColor(java.awt.Color.RED)
          case ZoneState.Managing => g2.setColor(java.awt.Color.CYAN)
        g2 fillRect(zone.bounds.topLeft.x, zone.bounds.topLeft.y, zone.bounds.width, zone.bounds.height)
        g2 setColor java.awt.Color.BLACK
        g2 drawRect(zone.bounds.topLeft.x, zone.bounds.topLeft.y, zone.bounds.width, zone.bounds.height)
        g2 drawString(s"ZONE ${zone.id} - ${zone.state.toString}", zone.bounds.topLeft.x + 5, zone.bounds.topLeft.y + 15)
      })
      rainGauges.foreach(r => {
        g2.fillOval(r.pos.x, r.pos.y, 10, 10)
        g2.setColor(java.awt.Color.BLACK)
      })
      fireStations.foreach( f => {
        g2 fillRect(f.pos.x, f.pos.y, 10, 10)
        g2 setColor java.awt.Color.BLUE
      })

  sealed class ManagePanel extends BoxPanel(Orientation.Vertical):
    var buttons: Map[Int, Button] = Map()
    var textAreas: Map[Int, TextField] = Map()
//    preferredSize = Dimension(cols * Costants.defalutWidth, rows * cols * 50)
    zones.foreach(zone => {
      val rainGaugesNumber: Int = rainGauges.count( _.zoneID == zone.id)
      val fireStationsStates = fireStations.filter( f => f.zoneID.equals(zone.id)).map(f => f.state)
      textAreas = textAreas.+((zone.id, new TextField(){
        text = s"Zone ${zone.id} -- Rain gauges = $rainGaugesNumber -- Status: ${zone.state.toString} -- Fire Station: $fireStationsStates"
        editable = false
        preferredSize = Dimension(50,50)
      }))
      buttons = buttons.+((zone.id, new Button{
        visible = false
        action = new Action(s"Manage Zone ${zone.id}"):
          override def apply(): Unit =
            viewActor ! ViewActor.ManageAlarm(zone.id)
        })
      )
      contents += textAreas(zone.id)
      contents += buttons(zone.id)
    })

    def display(): Unit =
      zones.foreach(zone =>
        val fireStationsStates = fireStations.filter( f => f.zoneID.equals(zone.id)).map(f => f.state)
        val rainGaugesNumber: Int = rainGauges.count( _.zoneID == zone.id)
        textAreas(zone.id).text = s"Zone ${zone.id} -- Rain gauges = $rainGaugesNumber -- Status: ${zone.state.toString} -- Fire Station: $fireStationsStates"
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
      )
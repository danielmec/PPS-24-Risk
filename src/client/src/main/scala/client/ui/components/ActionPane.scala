package client.ui.components

import scalafx.Includes._
import scalafx.geometry.{Insets, Pos}
import scalafx.scene.control.{Button, Tooltip}
import scalafx.scene.layout.HBox

/**
 * Pannello con i pulsanti di azione del gioco
 */
class ActionPane(onShowTerritories: => Unit) extends HBox(15) {
  
  padding = Insets(10)
  alignment = Pos.Center
  style = "-fx-background-color: #d0d0d0; -fx-border-color: #aaaaaa; -fx-border-width: 1 0 0 0;"
  
  // Rendi i pulsanti accessibili pubblicamente
  val attackButton: Button = createActionButton("Attacca", "Attacca un territorio nemico confinante")
  val reinforceButton: Button = createActionButton("Sposta", "Sposta armate tra territori confinanti")
  val cardsButton: Button = createActionButton("Mostra Carte", "Visualizza e gestisci le tue carte territorio")
  
  private val showTerritoriesButton = new Button("Mostra territori") {
    onAction = _ => onShowTerritories
  }
  
  val endTurnButton: Button = new Button("Fine Turno") {
    prefWidth = 120
    style = "-fx-font-weight: bold; -fx-font-size: 13px;"
    tooltip = new Tooltip("Termina il tuo turno")
    disable = false
  }

  children = Seq(
    attackButton,
    reinforceButton,
    cardsButton,
    showTerritoriesButton,
    endTurnButton
  )
  
  // metodo di supporto 
  private def createActionButton(text: String, tooltipText: String): Button = {
    val button = new Button(text) {
      prefWidth = 120
      style = "-fx-font-weight: bold; -fx-font-size: 13px;"
    }
    button.tooltip = new Tooltip(tooltipText)
    
    //per ora disabilitati
    button.disable = true
    
    button
  }
  
  /**
   * Abilita o disabilita i bottoni di azione
   */
  def setActionsEnabled(enabled: Boolean): Unit = {
    import scalafx.scene.control.Button
    children.foreach { node =>
      if (node.delegate.isInstanceOf[javafx.scene.control.Button]) {
        val button = Button(node.delegate.asInstanceOf[javafx.scene.control.Button])
        button.disable = !enabled
      }
    }
  }
}


package client.ui.components

import scalafx.Includes._
import scalafx.geometry.Insets
import scalafx.geometry.Pos
import scalafx.scene.control.Tooltip
import scalafx.scene.control.Button
import scalafx.scene.layout.HBox

/**
 * ActionPanel is a horizontal container holding action buttons
 * for controlling the main interactions during a player's turn
 * in the game (e.g., attacking, reinforcing, managing cards).
 * 
 * It includes buttons for:
 * - Attacking a neighboring enemy territory
 * - Moving troops between owned territories
 * - Viewing and trading cards
 * - Showing the full territory map
 * - Ending the turn
 * 
 * Buttons are styled uniformly and can be enabled/disabled together
 * using [[setActionsEnabled]].
 *
 * @param onShowTerritories Callback invoked when the "Show Territories" button is clicked.
 */
class ActionPane(onShowTerritories: => Unit) extends HBox(15) {

  /** UI configuration */
  padding = Insets(10)
  alignment = Pos.Center
  style = "-fx-font-weight: bold; -fx-font-size: 13px;"

  /**
   * Button for initiating an attack on a neighboring enemy territory.
   * Disabled by default.
   */
  val attackButton: Button = createActionButton("Attacca", "Attacca un territorio nemico confinante")

  /**
   * Button for reinforcing a friendly territory by moving troops.
   * Disabled by default.
   */
  val reinforceButton: Button = createActionButton("Sposta", "Sposta armate tra territori confinanti")

  /**
   * Button for showing and managing the player's territory cards.
   * Disabled by default.
   */
  val cardsButton: Button = createActionButton("Mostra Carte", "Visualizza e gestisci le tue carte territorio")

  /**
   * Button for displaying the full territory map.
   * Clicking this button triggers the [[onShowTerritories]] callback.
   */
  private val showTerritoriesButton = new Button("Mostra Territori") {
    onAction = _ => onShowTerritories
  }

  /**
   * Button for ending the current player's turn.
   * Enabled by default.
   */
  val endTurnButton: Button = new Button("Fine Turno") {
    prefWidth = 120
    style = "-fx-font-weight: bold; -fx-font-size: 13px;"
    tooltip = new Tooltip("Termina il tuo turno")
    disable = false
  }

  /** Adds all action buttons to the panel */
  children = Seq(
    attackButton,
    reinforceButton,
    cardsButton,
    showTerritoriesButton,
    endTurnButton
  )

  /**
   * Creates a standard action button with a given label and tooltip.
   * The button is styled and disabled by default.
   *
   * @param text The label of the button.
   * @param tooltipText The tooltip description shown on hover.
   * @return A styled and disabled Button.
   */
  private def createActionButton(text: String, tooltipText: String): Button = {
    val button = new Button(text) {
      prefWidth = 120
      style = "-fx-font-weight: bold; -fx-font-size: 13px;"
    }
    button.tooltip = new Tooltip(tooltipText)
    button.disable = true
    button
  }

  /**
   * Enables or disables all buttons (except "Show Territories") at once.
   * 
   * @param enabled If true, enables all buttons; otherwise disables them.
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

  /**
   * Gets the "Attack" button.
   *
   * @return The attackButton instance.
   */
  def getAttackButton: Button = attackButton

  /**
   * Gets the "Reinforce" button.
   *
   * @return The reinforceButton instance.
   */
  def getReinforceButton: Button = reinforceButton

  /**
   * Gets the "End Turn" button.
   *
   * @return The endTurnButton instance.
   */
  def getEndTurnButton: Button = endTurnButton

  /**
   * Gets the "Cards" button.
   *
   * @return The cardsButton instance.
   */
  def getCardsButton: Button = cardsButton
}

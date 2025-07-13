package client.ui.dialogs

import scalafx.scene.control._
import scalafx.scene.layout._
import scalafx.stage.Modality
import scalafx.stage.Stage
import scalafx.geometry.Insets
import scalafx.scene.Scene
import scalafx.geometry.Pos
import client.ui.GameWindow

/**
 * Represents a data structure containing the name of the territory and the type of card.
 *
 * @param territoryName the name of the territory on the card
 * @param cardType the type of the card (e.g., infantry, cavalry, artillery)
 */
case class CardInfo(territoryName: String, cardType: String)

/**
 * A modal dialog that displays the player's current territory cards.
 * If the player has no cards, a message is shown instead.
 *
 * @param owner the main game window that owns this dialog
 * @param cards a sequence of cards owned by the player
 */
class ShowCardsDialog(owner: GameWindow, cards: Seq[CardInfo]) extends Stage {
  initOwner(owner)
  initModality(Modality.APPLICATION_MODAL)
  title = "Le tue carte"
  width = 400
  height = 300

  /** The list view displaying the cards, or a message if the list is empty. */
  val cardsList = if (cards.nonEmpty) {
    new ListView[String](cards.map(c => s"${c.territoryName} (${c.cardType})"))
  } else {
    new Label("Non possiedi carte territorio.")
  }

  val closeButton = new Button("Chiudi") {
    onAction = _ => close()
  }

  val layout = new VBox(20, new Label("Carte territorio possedute:"), cardsList, closeButton) {
    padding = Insets(20)
    alignment = Pos.Center
  }

  /** Scene setup for the dialog. */
  scene = new Scene(layout)
}

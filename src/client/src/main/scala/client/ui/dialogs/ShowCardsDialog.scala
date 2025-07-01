package client.ui.dialogs

import scalafx.scene.control._
import scalafx.scene.layout._
import scalafx.stage.{Modality, Stage}
import scalafx.geometry.Insets
import scalafx.scene.Scene
import scalafx.geometry.Pos
import client.ui.GameWindow

case class CardInfo(territoryName: String, cardType: String)

class ShowCardsDialog(owner: GameWindow, cards: Seq[CardInfo]) extends Stage {
  initOwner(owner)
  initModality(Modality.APPLICATION_MODAL)
  title = "Le tue carte"
  width = 400
  height = 300

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

  scene = new Scene(layout)
}
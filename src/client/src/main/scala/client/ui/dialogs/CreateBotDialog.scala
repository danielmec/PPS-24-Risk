package client.ui.dialogs

import scalafx.Includes._
import scalafx.geometry.{Insets, Pos}
import scalafx.scene.Scene
import scalafx.scene.control.{Button, ComboBox, Label, TextField}
import scalafx.scene.layout.{GridPane, HBox, VBox}
import scalafx.scene.text.{Font, FontWeight}
import scalafx.stage.{Modality, Stage}
import scalafx.collections.ObservableBuffer

/**
 * Dialog per selezionare il tipo e il nome di ciascun bot
 */
class CreateBotDialog(parentStage: Stage, numBots: Int) extends Stage {
    
    title = "Crea bots"
    initModality(Modality.APPLICATION_MODAL)
    initOwner(parentStage)
    width = 400
    height = 200 + numBots * 40 
    
    val botTypes = ObservableBuffer("Offensivo", "Difensivo")
    
    case class BotConfig(strategySelector: ComboBox[String], nameField: TextField)
    
    val botConfigs = Array.tabulate(numBots) { i =>
        val comboBox = new ComboBox(botTypes)
        comboBox.selectionModel().selectFirst() // Default: Offensivo
        val nameField = new TextField {
        text = s"Bot ${i+1}"
        prefWidth = 150
        }
        BotConfig(comboBox, nameField)
    }
    
    val grid = new GridPane:
        hgap = 10
        vgap = 10
        padding = Insets(20)
        add(new Label("Nome") { style = "-fx-font-weight: bold" }, 0, 0)
        add(new Label("Strategia") { style = "-fx-font-weight: bold" }, 1, 0) 
        for (i <- 0 until numBots) 
            add(botConfigs(i).nameField, 0, i + 1)
            add(botConfigs(i).strategySelector, 1, i + 1)
         
    val confirmButton = new Button("Conferma")
    val cancelButton = new Button("Annulla")
    val buttonBar = new HBox(10, confirmButton, cancelButton){ alignment = Pos.Center }
    
    val layout = new VBox(20) {
        children = Seq(
        new Label("Crea i bot") {
            font = Font.font("Arial", FontWeight.Bold, 14)
            alignmentInParent = Pos.Center
        },
        grid,
        buttonBar
        )
        padding = Insets(20)
        alignment = Pos.Center
    }
    
    scene = new Scene {
        root = layout
    }
    
    private var confirmed = false
    
    case class BotConfiguration(name: String, botStrategy: String)
    
    def getBotConfigurations: Array[BotConfiguration] = 
        botConfigs.map(config => BotConfiguration(
        name = config.nameField.text.value.trim.nonEmpty match {
            case true => config.nameField.text.value.trim
            case false => s"Bot ${botConfigs.indexOf(config) + 1}"  
        },
        botStrategy = config.strategySelector.value.value
        ))
    
    def wasConfirmed: Boolean = confirmed
    
    confirmButton.onAction = _ => 
        confirmed = true
        close()

    cancelButton.onAction = _ => close()
}
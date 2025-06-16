package client.ui

import scalafx.Includes._
import scalafx.application.Platform
import scalafx.geometry.{Insets, Pos}
import scalafx.scene.Scene
import scalafx.scene.control.{Button, Label, ListView}
import scalafx.scene.layout.{BorderPane, HBox, VBox}
import scalafx.scene.text.{Font, FontWeight}
import scalafx.stage.{Modality, Stage}
import scalafx.collections.ObservableBuffer
import client.ClientNetworkManager
import client.ClientJsonSupport._
import client.ClientJsonSupport

/**
 * Finestra che mostra le lobby di gioco disponibili
 */
class LobbyWindow(networkManager: ClientNetworkManager, playerId: String) extends Stage {
  
  title = "Risk - Lobby"
  initModality(Modality.ApplicationModal)
  width = 500
  height = 400
  
  // Modello dati
  val availableGames = ObservableBuffer[GameInfo]()
  
  // Componenti UI
  val titleLabel = new Label("Lobby di gioco")
  titleLabel.font = Font.font("Arial", FontWeight.Bold, 18)
  
  val playerIdLabel = new Label(s"Giocatore: $playerId")
  
  val gameListView = new ListView[GameInfo]()
  gameListView.items = availableGames

  
  import javafx.util.Callback
  import javafx.scene.control.{ListView => JFXListView, ListCell => JFXListCell}

  gameListView.delegate.setCellFactory(new Callback[JFXListView[GameInfo], JFXListCell[GameInfo]] {
    override def call(listView: JFXListView[GameInfo]): JFXListCell[GameInfo] = {
      new GameListCell().delegate
    }
  })
    
  
  val refreshButton = new Button("Aggiorna")
  val createGameButton = new Button("Crea partita")
  val joinGameButton = new Button("Partecipa")
  
  // inizialmente disabilitata
  joinGameButton.disable = true
  
  // Layout
  val buttonBar = new HBox(10, refreshButton, createGameButton, joinGameButton)
  buttonBar.alignment = Pos.Center
  
  val mainLayout = new BorderPane {
    top = new VBox(10, titleLabel, playerIdLabel) {
      alignment = Pos.Center
      padding = Insets(10)
    }
    center = gameListView
    bottom = new VBox(10, buttonBar) {
      alignment = Pos.Center
      padding = Insets(10)
    }
    padding = Insets(10)
  }
  
  scene = new Scene {
    root = mainLayout
  }
  
  // Gestione eventi
  gameListView.selectionModel().selectedItemProperty().addListener { (_, _, newValue) =>
    joinGameButton.disable = newValue == null
  }
  
  refreshButton.onAction = _ => {
    refreshLobby()
  }
  
  createGameButton.onAction = _ => {
    //apre una finestra di dialogo per creare una nuova partita
    val dialog = new CreateGameDialog(networkManager)
    dialog.showAndWait()
  }
  
  joinGameButton.onAction = _ => {
    val selectedGame = gameListView.selectionModel().getSelectedItem
    if (selectedGame != null) {
      networkManager.sendMessage(JoinGameMessage(selectedGame.id)).foreach { success =>
        if (success) {
          // Il server risponderà con gameJoined, che verrà gestito dal callback
        }
      }(networkManager.executionContext)
    }
  }
  
  //inizializza aggiornando la lobby
  refreshLobby()
  
  /**
   * Aggiorna la lista delle partite disponibili
   */
  private def refreshLobby(): Unit = {
    // Mostriamo un "caricamento in corso" prima di inviare la richiesta
    Platform.runLater {
      availableGames.clear()
      availableGames += GameInfo("loading", "Caricamento in corso...", 0, 0)
    }
    
    // Invia la richiesta getAllGames invece di joinLobby
    networkManager.sendMessage(GetAllGamesMessage()).foreach { success =>
      if (success) {
        println("Richiesta getAllGames inviata con successo")
      } else {
        Platform.runLater {
          availableGames.clear()
          availableGames += GameInfo("error", "Errore nella richiesta", 0, 0)
        }
      }
    }(networkManager.executionContext)
  }
  
  //campo per memorizzare la finestra di gioco corrente
  private var currentGameWindow: Option[GameWindow] = None

  //nel costruttore, registra i callback per i messaggi di gioco nel network manager
  networkManager.registerCallback("gameJoined", {
    case msg: GameJoinedMessage =>
      Platform.runLater {
        // Verifica se esiste già una finestra di gioco per questo gameId
        val shouldOpenNewWindow = currentGameWindow match {
          case Some(existingWindow) if existingWindow.gameId == msg.gameId =>
            // Aggiorna la finestra esistente con i nuovi dati dei giocatori
            existingWindow.updatePlayers(msg.players)
            false
          case _ =>
            // Nessuna finestra esistente per questo gioco, o è una partita diversa
            true
        }
        
        if (shouldOpenNewWindow) {
          // Chiudi la finestra corrente della lobby
          this.hide()
          
          // Se esiste già una finestra di gioco (per un'altra partita), chiudila
          currentGameWindow.foreach(_.close())
          
          val gameName = msg.gameName
          
          // Apri la finestra di gioco
          val gameWindow = new GameWindow(networkManager, msg.gameId, gameName, msg.players)
          gameWindow.show()
          
          // memorizza il riferimento alla finestra
          currentGameWindow = Some(gameWindow)
          
          //quando la finestra di gioco viene chiusa, riapri la lobby
          gameWindow.onCloseRequest = _ => {
            this.show()
            currentGameWindow = None
          }
        }
      }
  })

  // registra un callback per ricevere la lista dei games disponibili
  networkManager.registerCallback("gameList", {
    case msg: GameListMessage =>
      Platform.runLater {
        availableGames.clear()
        
        if (msg.games.isEmpty) {
          //nessuna partita disponibile
          availableGames += GameInfo("", "Nessuna partita disponibile", 0, 0)
        } else {
          //aggiungi le partite ricevute dal server
          msg.games.foreach { gameId =>
            availableGames += GameInfo(gameId, s"Partita: $gameId", 0, 4)
          }
        }
      }
  })

  
  networkManager.registerCallback("gameCreated", {
    case msg: GameCreatedMessage =>
      //dopo la creazione di una partita, aggiorna l'elenco
      refreshLobby()
  })
  
  // Classi di supporto
  
  case class GameInfo(id: String, name: String, players: Int, maxPlayers: Int) {
    override def toString: String = s"$name ($players/$maxPlayers)"
  }
  
  class GameListCell extends scalafx.scene.control.ListCell[GameInfo] {
    item.onChange { (_, _, newValue) => 
      if (newValue != null) {
        text = s"${newValue.name} (${newValue.players}/${newValue.maxPlayers})"
      } else {
        text = ""
      }
    }
  }
}


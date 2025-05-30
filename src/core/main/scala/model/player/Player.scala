enum PlayerType:
    case Human,
    case Bot

enum PlayerColor:
    case Red, 
    case Blue,
    case Green,
    case Yellow,
    case Black,
    case White



/**
    * Represents a contrait for a player in the game.
    * @param id Unique identifier for the player.
    * @param name Name of the player.
    * @param color Color associated with the player.
    * @param playerType Type of the player (Human or Bot).
 */
trait Player:
    def id: String
    def name: String
    def color: PlayerColor
    def playerType: PlayerType


/**
    * Implementation of the Player trait.
    * Apply() method is implicit.
    */
case class PlayerImpl(
    id: String,
    name: String,
    color: PlayerColor,
    playerType: PlayerType) extends Player:
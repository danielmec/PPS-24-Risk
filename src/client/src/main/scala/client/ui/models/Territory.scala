package client.ui.models

import scalafx.beans.property.{StringProperty, IntegerProperty}

/**
 * Rappresenta un territorio del gioco Risk
 */
class Territory(val name: String, val continentName: String) {
  val owner = StringProperty("")
  val armies = IntegerProperty(0)
  
  def this(name: String, continentName: String, initialOwner: String, initialArmies: Int) = {
    this(name, continentName)
    owner.value = initialOwner
    armies.value = initialArmies
  }
}
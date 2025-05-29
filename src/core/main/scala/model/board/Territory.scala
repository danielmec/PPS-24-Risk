case class Territory(
                      name: String,
                      neighbors: Set[String],
                      owner: Option[Player],
                      troops: Int
                    )
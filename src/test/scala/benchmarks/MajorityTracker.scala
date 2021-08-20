package benchmarks

case class MajorityTracker(var peersCount: Int = 2, var round: Int = 0, var acksReceived: Int = 1, var acksSameValue: Int = 1, var value: Any, var retries: Int = 3) {
  private def incrAcksReceived: Int = {
    acksReceived += 1
    acksReceived
  }

  def nextRound(): Int = {
    round += 1
    acksReceived = 1
    acksSameValue = 1
    round
  }

  def updateAcksAccordingToValue(round: Int, value: Any): Int = {
    if (round == this.round) {
      incrAcksReceived
      if (value == this.value) acksSameValue += 1
    }
    acksSameValue
  }

  def reachedMajority: Boolean = acksSameValue > acksReceived / 2

  def useARetry(): Boolean = {
    if (retries > 0) {
      retries -= 1
      true
    }
    else false
  }

  //  def reachedEndOfRound: Boolean = (peersCount == acksReceived && acksSameValue <= acksReceived/2) || acksSameValue > acksReceived/2
  def reachedNonFruitfulEndOfRound: Boolean = peersCount == acksReceived && acksSameValue <= acksReceived / 2
}


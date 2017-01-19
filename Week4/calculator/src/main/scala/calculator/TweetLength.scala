package calculator

object TweetLength {
  final val MaxTweetLength = 140

  def tweetRemainingCharsCount(tweetText: Signal[String]): Signal[Int] = {
    // we don;t use tweetText().length() as it does not take into account the supplementary characters
    // and hence might give incorrect results. Instead we use the below teweetLength function, and give
    // it the argument which is the value of the tweetText i.e. tweetText()
    Signal(MaxTweetLength - tweetLength(tweetText()))
  }

  def colorForRemainingCharsCount(remainingCharsCount: Signal[Int]): Signal[String] = {
    // Note: This does not work if you return a separate signal for each case, as the whole point is to
    // have a single signal which can check to get the current state.
    Signal {
      if (remainingCharsCount() < 0) "red"
      else if (0 <= remainingCharsCount() && remainingCharsCount() <= 14) "orange"
      else "green"
    }
  }
  
  /**
   * Computes the length of a tweet, given its text string.
   *  This is not equivalent to text.length, as tweet lengths count the number
   *  of Unicode *code points* in the string.
   *  Note that this is still a simplified view of the reality. Full details
   *  can be found at
   *  https://dev.twitter.com/overview/api/counting-characters
   */
  private def tweetLength(text: String): Int = {
    /* This should be simply text.codePointCount(0, text.length), but it
     * is not implemented in Scala.js 0.6.2.
     */
    if (text.isEmpty) 0
    else {
      text.length - text.init.zip(text.tail).count(
        (Character.isSurrogatePair _).tupled)
    }
  }
}

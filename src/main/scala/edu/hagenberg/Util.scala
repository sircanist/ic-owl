package edu.hagenberg

import java.util.{Collection, Optional, Random}

object Util {

  private val random: Random = new Random()
  def getRandomElement[E](c: Collection[E]): Optional[E] = {
    if (c.isEmpty())
      return Optional.empty()
    return c.stream().skip(random.nextInt(c.size())).findFirst()
  }
}

package monocle.refined.internal

private[monocle] trait Chars {
  def testChar(c: Char): Boolean
}

private[monocle] object Chars extends CharsInstances

private[monocle] trait CharsInstances {
  implicit val lowerCaseChar = new Chars {
    override def testChar(c: Char): Boolean = c.isLower
  }

  implicit val upperCaseChar = new Chars {
    override def testChar(c: Char): Boolean = c.isUpper
  }
}

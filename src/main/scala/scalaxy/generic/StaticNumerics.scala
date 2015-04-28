package scalaxy.generic

trait ExtremeValues[A] {
  def MaxValue: A
  def MinValue: A
}

trait FloatingPointValues[A] {
  def MinPositiveValue: A
  def PositiveInfinity: A
  def NegativeInfinity: A
  def NaN: A
}

private[generic] trait CommonValues {

  object IntValues extends ExtremeValues[Int] {
    override def MaxValue = Int.MaxValue
    override def MinValue = Int.MinValue
  }

  object LongValues extends ExtremeValues[Long] {
    override def MaxValue = Long.MaxValue
    override def MinValue = Long.MinValue
  }

  object ShortValues extends ExtremeValues[Short] {
    override def MaxValue = Short.MaxValue
    override def MinValue = Short.MinValue
  }

  object ByteValues extends ExtremeValues[Byte] {
    override def MaxValue = Byte.MaxValue
    override def MinValue = Byte.MinValue
  }

  object FloatValues extends FloatingPointValues[Float] with ExtremeValues[Float] {
    override def MaxValue = Float.MaxValue
    override def MinValue = Float.MinValue
    override def MinPositiveValue = Float.MinPositiveValue
    override def PositiveInfinity = Float.PositiveInfinity
    override def NegativeInfinity = Float.NegativeInfinity
    override def NaN = Float.NaN
  }

  object DoubleValues extends FloatingPointValues[Double] with ExtremeValues[Double] {
    override def MaxValue = Double.MaxValue
    override def MinValue = Double.MinValue
    override def MinPositiveValue = Double.MinPositiveValue
    override def PositiveInfinity = Double.PositiveInfinity
    override def NegativeInfinity = Double.NegativeInfinity
    override def NaN = Double.NaN
  }

}

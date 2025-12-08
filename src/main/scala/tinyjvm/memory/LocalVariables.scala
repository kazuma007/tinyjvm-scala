package tinyjvm.memory

/**
 * Local variable array for storing method parameters and local variables
 */
class LocalVariables(maxLocals: Int):
  private val variables = new Array[Any](maxLocals)

  /**
   * Set a local variable at the given index
   */
  def set(index: Int, value: Any): Unit =
    if index < 0 || index >= maxLocals then
      throw new IndexOutOfBoundsException(
        s"Local variable index out of bounds: $index (max: ${maxLocals - 1})"
      )
    variables(index) = value

  /**
   * Get a local variable at the given index
   */
  def get(index: Int): Any =
    if index < 0 || index >= maxLocals then
      throw new IndexOutOfBoundsException(
        s"Local variable index out of bounds: $index (max: ${maxLocals - 1})"
      )
    variables(index)

  // Type-safe getters
  def getInt(index: Int): Int = get(index).asInstanceOf[Int]
  def getLong(index: Int): Long = get(index).asInstanceOf[Long]
  def getFloat(index: Int): Float = get(index).asInstanceOf[Float]
  def getDouble(index: Int): Double = get(index).asInstanceOf[Double]
  def getRef(index: Int): AnyRef = get(index).asInstanceOf[AnyRef]

  /**
   * Get the maximum number of local variables
   */
  def size: Int = maxLocals

  override def toString: String =
    val nonNull = variables.zipWithIndex.filter(_._1 != null)
    s"[${nonNull.map { case (v, i) => s"$i=$v" }.mkString(", ")}]"

end LocalVariables

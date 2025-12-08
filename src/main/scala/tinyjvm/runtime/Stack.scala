package tinyjvm.runtime

import scala.collection.mutable.ArrayBuffer

/**
 * Thread stack that holds frames for method invocations
 */
class Stack(maxSize: Int = 1000):
  private val frames = ArrayBuffer[Frame]()

  /**
   * Push a new frame onto the stack
   */
  def push(frame: Frame): Unit =
    if frames.size >= maxSize then
      throw new StackOverflowError(s"Thread stack overflow: max size = $maxSize")
    frames.append(frame)

  /**
   * Pop the top frame from the stack
   */
  def pop(): Frame =
    if frames.isEmpty then
      throw new RuntimeException("Cannot pop from empty thread stack")
    frames.remove(frames.size - 1)

  /**
   * Peek at the top frame without removing it
   */
  def peek(): Frame =
    if frames.isEmpty then
      throw new RuntimeException("Cannot peek empty thread stack")
    frames.last

  /**
   * Check if stack is empty
   */
  def isEmpty: Boolean = frames.isEmpty

  /**
   * Get current stack depth
   */
  def size: Int = frames.size

  /**
   * Clear all frames (used on exception)
   */
  def clear(): Unit = frames.clear()

  override def toString: String =
    s"Stack(${frames.map(f => s"${f.className}.${f.methodName}").mkString(" -> ")})"

end Stack

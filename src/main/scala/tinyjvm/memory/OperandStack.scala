package tinyjvm.memory

import scala.collection.mutable.ArrayBuffer

/** Operand stack for storing intermediate computation values
  */
class OperandStack(maxSize: Int):
  private val stack = ArrayBuffer[Any]()

  /** Push a value onto the stack
    */
  def push(value: Any): Unit =
    if stack.size >= maxSize then
      throw new StackOverflowError(
        s"Operand stack overflow: max size = $maxSize, current = ${stack.size}"
      )
    stack.append(value)

  /** Pop a value from the stack
    */
  def pop(): Any =
    if stack.isEmpty then throw new RuntimeException("Operand stack underflow")
    stack.remove(stack.size - 1)

  /** Peek at the top value without removing it
    */
  def peek(): Any =
    if stack.isEmpty then throw new RuntimeException("Cannot peek empty operand stack")
    stack.last

  /** Check if stack is empty
    */
  def isEmpty: Boolean = stack.isEmpty

  /** Get current stack size
    */
  def size: Int = stack.size

  /** Clear all values from the stack
    */
  def clear(): Unit = stack.clear()

  override def toString: String = s"[${stack.mkString(", ")}]"

end OperandStack

package tinyjvm.runtime

import tinyjvm.classloader.ClassEntry
import tinyjvm.classloader.ConstantPoolEntry
import tinyjvm.classloader.NameAndTypeEntry
import tinyjvm.classloader.Utf8Entry
import tinyjvm.memory.LocalVariables
import tinyjvm.memory.OperandStack

/** A frame represents a method invocation on the JVM stack Contains: operand stack, local
  * variables, program counter, and constant pool reference
  */
class Frame(
    val maxStack: Int,
    val maxLocals: Int,
    val code: Array[Byte],
    val className: String,
    val methodName: String,
    val constantPool: Array[ConstantPoolEntry] = Array()
):
  val operandStack = new OperandStack(maxStack)
  val localVariables = new LocalVariables(maxLocals)
  var pc: Int = 0 // Program counter

  /** Read the next byte from the code array and increment PC
    */
  def nextByte(): Byte =
    if pc >= code.length then
      throw new RuntimeException(
        s"Program counter out of bounds: pc=$pc, code.length=${code.length}"
      )
    val byte = code(pc)
    pc += 1
    byte

  /** Read the next short (2 bytes) from the code array in big-endian format
    */
  def nextShort(): Short =
    val byte1 = nextByte() & 0xff
    val byte2 = nextByte() & 0xff
    ((byte1 << 8) | byte2).toShort

  /** Check if there's more code to execute
    */
  def hasMoreCode: Boolean = pc < code.length

  /** Move program counter back by one byte (rarely used)
    */
  def revertByte(): Unit =
    if pc > 0 then pc -= 1

  /** Resolve a class name from the constant pool
    */
  def resolveClassName(classIndex: Int): String =
    if classIndex < 0 || classIndex >= constantPool.length then
      throw new RuntimeException(s"Invalid constant pool index: $classIndex")

    constantPool(classIndex) match
      case ClassEntry(nameIndex) =>
        constantPool(nameIndex) match
          case Utf8Entry(name) => name
          case other =>
            throw new RuntimeException(
              s"Expected Utf8Entry at index $nameIndex, found: ${other.getClass.getSimpleName}"
            )
      case other =>
        throw new RuntimeException(
          s"Expected ClassEntry at index $classIndex, found: ${other.getClass.getSimpleName}"
        )

  /** Resolve a method/field name and descriptor from the constant pool
    */
  def resolveNameAndType(nameAndTypeIndex: Int): (String, String) =
    if nameAndTypeIndex < 0 || nameAndTypeIndex >= constantPool.length then
      throw new RuntimeException(s"Invalid constant pool index: $nameAndTypeIndex")

    constantPool(nameAndTypeIndex) match
      case NameAndTypeEntry(nameIndex, descriptorIndex) =>
        val name = constantPool(nameIndex) match
          case Utf8Entry(n) => n
          case other =>
            throw new RuntimeException(
              s"Expected Utf8Entry at index $nameIndex, found: ${other.getClass.getSimpleName}"
            )

        val descriptor = constantPool(descriptorIndex) match
          case Utf8Entry(d) => d
          case other =>
            throw new RuntimeException(
              s"Expected Utf8Entry at index $descriptorIndex, found: ${other.getClass.getSimpleName}"
            )

        (name, descriptor)
      case other =>
        throw new RuntimeException(
          s"Expected NameAndTypeEntry at index $nameAndTypeIndex, found: ${other.getClass.getSimpleName}"
        )

  override def toString: String =
    s"Frame($className.$methodName, pc=$pc, stack=$operandStack, locals=$localVariables)"

end Frame

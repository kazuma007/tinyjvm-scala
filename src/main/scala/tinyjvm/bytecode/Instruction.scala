package tinyjvm.bytecode

import org.objectweb.asm.Opcodes._
import tinyjvm.bytecode.ArithmeticOperations._
import tinyjvm.bytecode.ArrayOperations._
import tinyjvm.bytecode.ExtendedOpcodes._
import tinyjvm.runtime.Frame

/** Base trait for all JVM bytecode instructions
  */
sealed trait Instruction:
  def execute(frame: Frame): Unit
  def opcode: Int

/** Helper functions for array operations
  */
object ArrayOperations:

  /** Generic array load operation with bounds and null checking
    * @param frame
    *   The execution frame
    * @param converter
    *   Optional conversion function for narrow types (byte, char, short)
    */
  def loadFromArray[T](frame: Frame, converter: T => Any = identity[T]): Unit =
    val index = frame.operandStack.pop().asInstanceOf[Int]
    val arrayRef = frame.operandStack.pop()

    if arrayRef == null then throw new NullPointerException("Array is null")

    val array = arrayRef.asInstanceOf[Array[T]]

    if index < 0 || index >= array.length then
      throw new ArrayIndexOutOfBoundsException(
        s"Index $index out of bounds for length ${array.length}"
      )

    frame.operandStack.push(converter(array(index)))

  /** Generic array store operation with bounds and null checking
    * @param frame
    *   The execution frame
    * @param converter
    *   Optional conversion function for narrow types (byte, char, short)
    */
  def storeToArray[T](frame: Frame, converter: Any => T = identity[Any](_).asInstanceOf[T]): Unit =
    val value = frame.operandStack.pop()
    val index = frame.operandStack.pop().asInstanceOf[Int]
    val arrayRef = frame.operandStack.pop()

    if arrayRef == null then throw new NullPointerException("Array is null")

    val array = arrayRef.asInstanceOf[Array[T]]

    if index < 0 || index >= array.length then
      throw new ArrayIndexOutOfBoundsException(
        s"Index $index out of bounds for length ${array.length}"
      )

    array(index) = converter(value)

  /** Get length of any array type with null checking
    */
  def getArrayLength(arrayRef: Any): Int =
    if arrayRef == null then throw new NullPointerException("Array is null")

    arrayRef match
      case arr: Array[Int]    => arr.length
      case arr: Array[Long]   => arr.length
      case arr: Array[Float]  => arr.length
      case arr: Array[Double] => arr.length
      case arr: Array[Byte]   => arr.length
      case arr: Array[Char]   => arr.length
      case arr: Array[Short]  => arr.length
      case arr: Array[AnyRef] => arr.length
      case _ => throw new InternalError(s"Unknown array type: ${arrayRef.getClass}")

end ArrayOperations

/** Helper functions for arithmetic operations
  */
object ArithmeticOperations:

  /** Generic binary arithmetic operation
    */
  def binaryOp[T](frame: Frame, op: (T, T) => T): Unit =
    val v2 = frame.operandStack.pop().asInstanceOf[T]
    val v1 = frame.operandStack.pop().asInstanceOf[T]
    frame.operandStack.push(op(v1, v2))

  /** Binary operation with division by zero check
    */
  def divisionOp[T](frame: Frame, op: (T, T) => T, isZero: T => Boolean): Unit =
    val v2 = frame.operandStack.pop().asInstanceOf[T]
    val v1 = frame.operandStack.pop().asInstanceOf[T]
    if isZero(v2) then throw new ArithmeticException("Division by zero")
    frame.operandStack.push(op(v1, v2))

  /** Unary arithmetic operation
    */
  def unaryOp[T](frame: Frame, op: T => T): Unit =
    val v = frame.operandStack.pop().asInstanceOf[T]
    frame.operandStack.push(op(v))

end ArithmeticOperations

/** Instructions with no operands (e.g., IADD, ISUB, ICONST_1)
  */
case class NoOperandInstruction(opcode: Int) extends Instruction:
  override def execute(frame: Frame): Unit =
    opcode match
      case NOP => // Do nothing
      // Constants
      case ACONST_NULL => frame.operandStack.push(null)
      case ICONST_M1   => frame.operandStack.push(-1)
      case ICONST_0    => frame.operandStack.push(0)
      case ICONST_1    => frame.operandStack.push(1)
      case ICONST_2    => frame.operandStack.push(2)
      case ICONST_3    => frame.operandStack.push(3)
      case ICONST_4    => frame.operandStack.push(4)
      case ICONST_5    => frame.operandStack.push(5)

      case LCONST_0 => frame.operandStack.push(0L)
      case LCONST_1 => frame.operandStack.push(1L)

      case FCONST_0 => frame.operandStack.push(0.0f)
      case FCONST_1 => frame.operandStack.push(1.0f)
      case FCONST_2 => frame.operandStack.push(2.0f)

      case DCONST_0 => frame.operandStack.push(0.0)
      case DCONST_1 => frame.operandStack.push(1.0)

      // Integer arithmetic operations
      case IADD => binaryOp[Int](frame, _ + _)
      case ISUB => binaryOp[Int](frame, _ - _)
      case IMUL => binaryOp[Int](frame, _ * _)
      case IDIV => divisionOp[Int](frame, _ / _, _ == 0)
      case IREM => divisionOp[Int](frame, _ % _, _ == 0)
      case INEG => unaryOp[Int](frame, -_)

      // Long arithmetic operations
      case LADD => binaryOp[Long](frame, _ + _)
      case LSUB => binaryOp[Long](frame, _ - _)
      case LMUL => binaryOp[Long](frame, _ * _)
      case LDIV => divisionOp[Long](frame, _ / _, _ == 0L)

      // Array load operations
      case IALOAD => loadFromArray[Int](frame)
      case LALOAD => loadFromArray[Long](frame)
      case FALOAD => loadFromArray[Float](frame)
      case DALOAD => loadFromArray[Double](frame)
      case AALOAD => loadFromArray[AnyRef](frame)
      case BALOAD => loadFromArray[Byte](frame, _.toInt)
      case CALOAD => loadFromArray[Char](frame, _.toInt)
      case SALOAD => loadFromArray[Short](frame, _.toInt)

      // Array store operations
      case IASTORE => storeToArray[Int](frame)
      case LASTORE => storeToArray[Long](frame)
      case FASTORE => storeToArray[Float](frame)
      case DASTORE => storeToArray[Double](frame)
      case AASTORE => storeToArray[AnyRef](frame, _.asInstanceOf[AnyRef])
      case BASTORE => storeToArray[Byte](frame, v => v.asInstanceOf[Int].toByte)
      case CASTORE => storeToArray[Char](frame, v => v.asInstanceOf[Int].toChar)
      case SASTORE => storeToArray[Short](frame, v => v.asInstanceOf[Int].toShort)

      // Array length
      case ARRAYLENGTH =>
        val arrayRef = frame.operandStack.pop()
        frame.operandStack.push(getArrayLength(arrayRef))

      // Load operations with implicit indices
      case ILOAD_0 | LLOAD_0 | ALOAD_0 => frame.operandStack.push(frame.localVariables.get(0))
      case ILOAD_1 | LLOAD_1 | ALOAD_1 => frame.operandStack.push(frame.localVariables.get(1))
      case ILOAD_2 | LLOAD_2 | ALOAD_2 => frame.operandStack.push(frame.localVariables.get(2))
      case ILOAD_3 | LLOAD_3 | ALOAD_3 => frame.operandStack.push(frame.localVariables.get(3))

      // Store operations with implicit indices
      case ISTORE_0 | LSTORE_0 | ASTORE_0 => frame.localVariables.set(0, frame.operandStack.pop())
      case ISTORE_1 | LSTORE_1 | ASTORE_1 => frame.localVariables.set(1, frame.operandStack.pop())
      case ISTORE_2 | LSTORE_2 | ASTORE_2 => frame.localVariables.set(2, frame.operandStack.pop())
      case ISTORE_3 | LSTORE_3 | ASTORE_3 => frame.localVariables.set(3, frame.operandStack.pop())

      // Stack operations
      case DUP =>
        val value = frame.operandStack.peek()
        frame.operandStack.push(value)

      case DUP_X1 =>
        val v1 = frame.operandStack.pop()
        val v2 = frame.operandStack.pop()
        frame.operandStack.push(v1)
        frame.operandStack.push(v2)
        frame.operandStack.push(v1)

      case DUP2 =>
        val v1 = frame.operandStack.pop()
        val v2 = frame.operandStack.pop()
        frame.operandStack.push(v2)
        frame.operandStack.push(v1)
        frame.operandStack.push(v2)
        frame.operandStack.push(v1)

      case POP => frame.operandStack.pop()

      case POP2 =>
        frame.operandStack.pop()
        frame.operandStack.pop()

      case SWAP =>
        val v1 = frame.operandStack.pop()
        val v2 = frame.operandStack.pop()
        frame.operandStack.push(v1)
        frame.operandStack.push(v2)

      case _ =>
        throw new UnsupportedOperationException(
          s"Unsupported opcode: 0x${opcode.toHexString} at pc=${frame.pc - 1}"
        )

end NoOperandInstruction

/** Array type constants for NEWARRAY instruction
  */
object ArrayType:
  val T_BOOLEAN: Int = 4
  val T_CHAR: Int = 5
  val T_FLOAT: Int = 6
  val T_DOUBLE: Int = 7
  val T_BYTE: Int = 8
  val T_SHORT: Int = 9
  val T_INT: Int = 10
  val T_LONG: Int = 11

/** NEWARRAY instruction - create primitive array
  */
case class NewArrayInstruction(arrayType: Int) extends Instruction:
  override val opcode: Int = NEWARRAY

  override def execute(frame: Frame): Unit =
    val count = frame.operandStack.pop().asInstanceOf[Int]
    if count < 0 then throw new NegativeArraySizeException(s"Array size cannot be negative: $count")

    val array = arrayType match
      case ArrayType.T_BOOLEAN => new Array[Byte](count) // boolean[] stored as byte[]
      case ArrayType.T_CHAR    => new Array[Char](count)
      case ArrayType.T_FLOAT   => new Array[Float](count)
      case ArrayType.T_DOUBLE  => new Array[Double](count)
      case ArrayType.T_BYTE    => new Array[Byte](count)
      case ArrayType.T_SHORT   => new Array[Short](count)
      case ArrayType.T_INT     => new Array[Int](count)
      case ArrayType.T_LONG    => new Array[Long](count)
      case _                   => throw new InternalError(s"Invalid array type: $arrayType")

    frame.operandStack.push(array)

end NewArrayInstruction

/** ANEWARRAY instruction - create reference array
  */
case class ANewArrayInstruction(classIndex: Int) extends Instruction:
  override val opcode: Int = ANEWARRAY

  override def execute(frame: Frame): Unit =
    val count = frame.operandStack.pop().asInstanceOf[Int]
    if count < 0 then throw new NegativeArraySizeException(s"Array size cannot be negative: $count")

    // Create object array (simplified - not checking actual class type)
    val array = new Array[AnyRef](count)
    frame.operandStack.push(array)

end ANewArrayInstruction

/** Instructions with an index operand (e.g., ILOAD, ISTORE)
  */
case class IndexInstruction(opcode: Int, index: Int) extends Instruction:
  override def execute(frame: Frame): Unit =
    opcode match
      case ILOAD | LLOAD | FLOAD | DLOAD | ALOAD =>
        frame.operandStack.push(frame.localVariables.get(index))

      case ISTORE | LSTORE | FSTORE | DSTORE | ASTORE =>
        frame.localVariables.set(index, frame.operandStack.pop())

      case _ =>
        throw new UnsupportedOperationException(
          s"Unsupported indexed opcode: 0x${opcode.toHexString} with index $index"
        )

end IndexInstruction

/** Instructions with a byte operand (e.g., BIPUSH)
  */
case class ByteInstruction(opcode: Int, operand: Byte) extends Instruction:
  override def execute(frame: Frame): Unit =
    opcode match
      case BIPUSH => frame.operandStack.push(operand.toInt)
      case _ =>
        throw new UnsupportedOperationException(s"Unsupported byte opcode: 0x${opcode.toHexString}")

end ByteInstruction

/** Instructions with a short operand (e.g., SIPUSH)
  */
case class ShortInstruction(opcode: Int, operand: Short) extends Instruction:
  override def execute(frame: Frame): Unit =
    opcode match
      case SIPUSH => frame.operandStack.push(operand.toInt)
      case _ =>
        throw new UnsupportedOperationException(
          s"Unsupported short opcode: 0x${opcode.toHexString}"
        )

end ShortInstruction

/** Branch instructions (e.g., GOTO, IF_ICMPEQ)
  */
case class BranchInstruction(opcode: Int, offset: Short) extends Instruction:
  override def execute(frame: Frame): Unit =
    val branchPC = frame.pc - 3 + offset

    opcode match
      case GOTO =>
        frame.pc = branchPC

      case IFEQ =>
        val value = frame.operandStack.pop().asInstanceOf[Int]
        if value == 0 then frame.pc = branchPC

      case IFNE =>
        val value = frame.operandStack.pop().asInstanceOf[Int]
        if value != 0 then frame.pc = branchPC

      case IFLT =>
        val value = frame.operandStack.pop().asInstanceOf[Int]
        if value < 0 then frame.pc = branchPC

      case IFGE =>
        val value = frame.operandStack.pop().asInstanceOf[Int]
        if value >= 0 then frame.pc = branchPC

      case IFGT =>
        val value = frame.operandStack.pop().asInstanceOf[Int]
        if value > 0 then frame.pc = branchPC

      case IFLE =>
        val value = frame.operandStack.pop().asInstanceOf[Int]
        if value <= 0 then frame.pc = branchPC

      case IF_ICMPEQ =>
        val v2 = frame.operandStack.pop().asInstanceOf[Int]
        val v1 = frame.operandStack.pop().asInstanceOf[Int]
        if v1 == v2 then frame.pc = branchPC

      case IF_ICMPNE =>
        val v2 = frame.operandStack.pop().asInstanceOf[Int]
        val v1 = frame.operandStack.pop().asInstanceOf[Int]
        if v1 != v2 then frame.pc = branchPC

      case IF_ICMPLT =>
        val v2 = frame.operandStack.pop().asInstanceOf[Int]
        val v1 = frame.operandStack.pop().asInstanceOf[Int]
        if v1 < v2 then frame.pc = branchPC

      case IF_ICMPGE =>
        val v2 = frame.operandStack.pop().asInstanceOf[Int]
        val v1 = frame.operandStack.pop().asInstanceOf[Int]
        if v1 >= v2 then frame.pc = branchPC

      case IF_ICMPGT =>
        val v2 = frame.operandStack.pop().asInstanceOf[Int]
        val v1 = frame.operandStack.pop().asInstanceOf[Int]
        if v1 > v2 then frame.pc = branchPC

      case IF_ICMPLE =>
        val v2 = frame.operandStack.pop().asInstanceOf[Int]
        val v1 = frame.operandStack.pop().asInstanceOf[Int]
        if v1 <= v2 then frame.pc = branchPC

      case _ =>
        throw new UnsupportedOperationException(
          s"Unsupported branch opcode: 0x${opcode.toHexString}"
        )

end BranchInstruction

/** IINC instruction - increment local variable
  */
case class IincInstruction(index: Int, const: Int) extends Instruction:
  override val opcode: Int = IINC

  override def execute(frame: Frame): Unit =
    val value = frame.localVariables.get(index).asInstanceOf[Int]
    frame.localVariables.set(index, value + const)

end IincInstruction

/** Method invocation instructions (handled specially by interpreter)
  */
case class InvokeInstruction(opcode: Int, methodIndex: Int) extends Instruction:
  override def execute(frame: Frame): Unit =
    throw new UnsupportedOperationException(
      "InvokeInstruction should be handled by interpreter directly"
    )

end InvokeInstruction

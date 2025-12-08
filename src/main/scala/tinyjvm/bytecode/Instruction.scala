package tinyjvm.bytecode

import org.objectweb.asm.Opcodes._
import tinyjvm.bytecode.ExtendedOpcodes._
import tinyjvm.runtime.Frame

/** Base trait for all JVM bytecode instructions
  */
sealed trait Instruction:
  def execute(frame: Frame): Unit
  def opcode: Int

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

      // Arithmetic operations
      case IADD =>
        val v2 = frame.operandStack.pop().asInstanceOf[Int]
        val v1 = frame.operandStack.pop().asInstanceOf[Int]
        frame.operandStack.push(v1 + v2)

      case ISUB =>
        val v2 = frame.operandStack.pop().asInstanceOf[Int]
        val v1 = frame.operandStack.pop().asInstanceOf[Int]
        frame.operandStack.push(v1 - v2)

      case IMUL =>
        val v2 = frame.operandStack.pop().asInstanceOf[Int]
        val v1 = frame.operandStack.pop().asInstanceOf[Int]
        frame.operandStack.push(v1 * v2)

      case IDIV =>
        val v2 = frame.operandStack.pop().asInstanceOf[Int]
        val v1 = frame.operandStack.pop().asInstanceOf[Int]
        if v2 == 0 then throw new ArithmeticException("Division by zero")
        frame.operandStack.push(v1 / v2)

      case IREM =>
        val v2 = frame.operandStack.pop().asInstanceOf[Int]
        val v1 = frame.operandStack.pop().asInstanceOf[Int]
        if v2 == 0 then throw new ArithmeticException("Division by zero")
        frame.operandStack.push(v1 % v2)

      case INEG =>
        val v = frame.operandStack.pop().asInstanceOf[Int]
        frame.operandStack.push(-v)

      // Long arithmetic
      case LADD =>
        val v2 = frame.operandStack.pop().asInstanceOf[Long]
        val v1 = frame.operandStack.pop().asInstanceOf[Long]
        frame.operandStack.push(v1 + v2)

      case LSUB =>
        val v2 = frame.operandStack.pop().asInstanceOf[Long]
        val v1 = frame.operandStack.pop().asInstanceOf[Long]
        frame.operandStack.push(v1 - v2)

      case LMUL =>
        val v2 = frame.operandStack.pop().asInstanceOf[Long]
        val v1 = frame.operandStack.pop().asInstanceOf[Long]
        frame.operandStack.push(v1 * v2)

      case LDIV =>
        val v2 = frame.operandStack.pop().asInstanceOf[Long]
        val v1 = frame.operandStack.pop().asInstanceOf[Long]
        if v2 == 0L then throw new ArithmeticException("Division by zero")
        frame.operandStack.push(v1 / v2)

      // Load operations with implicit indices
      case ILOAD_0 => frame.operandStack.push(frame.localVariables.get(0))
      case ILOAD_1 => frame.operandStack.push(frame.localVariables.get(1))
      case ILOAD_2 => frame.operandStack.push(frame.localVariables.get(2))
      case ILOAD_3 => frame.operandStack.push(frame.localVariables.get(3))

      case LLOAD_0 => frame.operandStack.push(frame.localVariables.get(0))
      case LLOAD_1 => frame.operandStack.push(frame.localVariables.get(1))
      case LLOAD_2 => frame.operandStack.push(frame.localVariables.get(2))
      case LLOAD_3 => frame.operandStack.push(frame.localVariables.get(3))

      case ALOAD_0 => frame.operandStack.push(frame.localVariables.get(0))
      case ALOAD_1 => frame.operandStack.push(frame.localVariables.get(1))
      case ALOAD_2 => frame.operandStack.push(frame.localVariables.get(2))
      case ALOAD_3 => frame.operandStack.push(frame.localVariables.get(3))

      // Store operations with implicit indices
      case ISTORE_0 => frame.localVariables.set(0, frame.operandStack.pop())
      case ISTORE_1 => frame.localVariables.set(1, frame.operandStack.pop())
      case ISTORE_2 => frame.localVariables.set(2, frame.operandStack.pop())
      case ISTORE_3 => frame.localVariables.set(3, frame.operandStack.pop())

      case LSTORE_0 => frame.localVariables.set(0, frame.operandStack.pop())
      case LSTORE_1 => frame.localVariables.set(1, frame.operandStack.pop())
      case LSTORE_2 => frame.localVariables.set(2, frame.operandStack.pop())
      case LSTORE_3 => frame.localVariables.set(3, frame.operandStack.pop())

      case ASTORE_0 => frame.localVariables.set(0, frame.operandStack.pop())
      case ASTORE_1 => frame.localVariables.set(1, frame.operandStack.pop())
      case ASTORE_2 => frame.localVariables.set(2, frame.operandStack.pop())
      case ASTORE_3 => frame.localVariables.set(3, frame.operandStack.pop())

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
        throw new UnsupportedOperationException(
          s"Unsupported byte opcode: 0x${opcode.toHexString}"
        )

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

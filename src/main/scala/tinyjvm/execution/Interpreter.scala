package tinyjvm.execution

import org.objectweb.asm.Opcodes._
import tinyjvm.bytecode._
import tinyjvm.classloader.MethodRefEntry
import tinyjvm.runtime.Frame

/** The interpreter executes bytecode instructions
  */
class Interpreter(executionEngine: ExecutionEngine):

  /** Interpret a frame until it returns
    */
  def interpret(frame: Frame): Option[Any] =
    var returnValue: Option[Any] = None

    while frame.hasMoreCode && returnValue.isEmpty do
      val pc = frame.pc
      val opcodeByte = frame.nextByte()
      val opcode = opcodeByte & 0xff

      try
        opcode match
          // Return instructions
          case RETURN =>
            returnValue = Some(())

          case IRETURN | LRETURN | FRETURN | DRETURN | ARETURN =>
            returnValue = Some(frame.operandStack.pop())

          // Branch instructions
          case GOTO | IFEQ | IFNE | IFLT | IFGE | IFGT | IFLE | IF_ICMPEQ | IF_ICMPNE | IF_ICMPLT |
              IF_ICMPGE | IF_ICMPGT | IF_ICMPLE =>
            val offset = frame.nextShort()
            BranchInstruction(opcode, offset).execute(frame)

          // IINC instruction
          case IINC =>
            val index = frame.nextByte() & 0xff
            val const = frame.nextByte()
            IincInstruction(index, const).execute(frame)

          // Method invocation
          case INVOKESTATIC =>
            val methodRefIndex = frame.nextShort() & 0xffff
            handleInvokeStatic(frame, methodRefIndex)

          case INVOKEVIRTUAL =>
            val methodRefIndex = frame.nextShort() & 0xffff
            println(s"[Interpreter] INVOKEVIRTUAL not fully implemented, treating as INVOKESTATIC")
            handleInvokeStatic(frame, methodRefIndex)

          // Variable load/store with index
          case ILOAD | LLOAD | FLOAD | DLOAD | ALOAD | ISTORE | LSTORE | FSTORE | DSTORE | ASTORE =>
            val index = frame.nextByte() & 0xff
            IndexInstruction(opcode, index).execute(frame)

          // Push byte/short
          case BIPUSH =>
            val operand = frame.nextByte()
            ByteInstruction(opcode, operand).execute(frame)

          case SIPUSH =>
            val operand = frame.nextShort()
            ShortInstruction(opcode, operand).execute(frame)

          // LDC instruction (load constant from pool)
          case LDC =>
            val index = frame.nextByte() & 0xff
            loadConstant(frame, index)

          // All other instructions (no operands)
          case _ =>
            NoOperandInstruction(opcode).execute(frame)

      catch
        case e: Exception =>
          println(s"[Interpreter] Error at pc=$pc, opcode=0x${opcode.toHexString}: ${e.getMessage}")
          throw e

    returnValue

  /** Handle INVOKESTATIC bytecode
    */
  private def handleInvokeStatic(frame: Frame, methodRefIndex: Int): Unit =
    val constantPool = frame.constantPool

    if methodRefIndex < 0 || methodRefIndex >= constantPool.length then
      throw new RuntimeException(s"Invalid constant pool index: $methodRefIndex")

    constantPool(methodRefIndex) match
      case MethodRefEntry(classIndex, nameAndTypeIndex) =>
        val className = frame.resolveClassName(classIndex)
        val (methodName, descriptor) = frame.resolveNameAndType(nameAndTypeIndex)

        println(s"[Interpreter] Invoking static method: $className.$methodName$descriptor")

        // Count and pop arguments
        val argCount = countArguments(descriptor)
        val args = (0 until argCount).map { _ => frame.operandStack.pop() }.reverse.toArray

        // Execute the method
        val result = executionEngine.executeMethod(className, methodName, descriptor, args)

        // Push return value if method returns something
        if !descriptor.endsWith(")V") then result.foreach(frame.operandStack.push)

      case other =>
        throw new RuntimeException(
          s"Expected MethodRefEntry at constant pool index $methodRefIndex, found: ${other.getClass.getSimpleName}"
        )

  /** Load a constant from the constant pool
    */
  private def loadConstant(frame: Frame, index: Int): Unit =
    import tinyjvm.classloader.*

    if index < 0 || index >= frame.constantPool.length then
      throw new RuntimeException(s"Invalid constant pool index: $index")

    frame.constantPool(index) match
      case IntegerEntry(value) => frame.operandStack.push(value)
      case FloatEntry(value)   => frame.operandStack.push(value)
      case LongEntry(value)    => frame.operandStack.push(value)
      case DoubleEntry(value)  => frame.operandStack.push(value)
      case StringEntry(stringIndex) =>
        frame.constantPool(stringIndex) match
          case Utf8Entry(str) => frame.operandStack.push(str)
          case _ => throw new RuntimeException(s"Invalid string constant at index $stringIndex")
      case other =>
        throw new RuntimeException(s"Cannot load constant of type ${other.getClass.getSimpleName}")

  /** Count the number of arguments in a method descriptor
    */
  private def countArguments(descriptor: String): Int =
    val params = descriptor.substring(1, descriptor.indexOf(')'))
    var count = 0
    var i = 0

    while i < params.length do
      params.charAt(i) match
        case 'B' | 'C' | 'F' | 'I' | 'S' | 'Z' =>
          count += 1
          i += 1
        case 'D' | 'J' =>
          count += 1
          i += 1
        case 'L' =>
          count += 1
          i = params.indexOf(';', i) + 1
        case '[' =>
          count += 1
          i += 1
          while i < params.length && params.charAt(i) == '[' do i += 1
          if i < params.length && params.charAt(i) == 'L' then i = params.indexOf(';', i) + 1
          else i += 1
        case _ =>
          i += 1

    count

end Interpreter

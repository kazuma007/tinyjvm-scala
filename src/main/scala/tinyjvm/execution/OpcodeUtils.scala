package tinyjvm.execution

import org.objectweb.asm.Opcodes._
import org.objectweb.asm.util.Printer
import tinyjvm.bytecode.ExtendedOpcodes._

/**
 * Utility object for working with JVM opcodes
 */
object OpcodeUtils:

  /**
   * Get the mnemonic name for an opcode (e.g., 0x60 -> "IADD")
   * Uses ASM's built-in Printer utility
   */
  def getMnemonic(opcode: Int): String =
    if opcode >= 0 && opcode < Printer.OPCODES.length then
      Printer.OPCODES(opcode)
    else
      s"UNKNOWN_0x${opcode.toHexString}"

end OpcodeUtils

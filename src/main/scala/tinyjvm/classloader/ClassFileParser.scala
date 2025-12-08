package tinyjvm.classloader

import tinyjvm.runtime.MethodInfo

import java.io.ByteArrayInputStream
import java.io.DataInputStream
import java.nio.file.Files
import java.nio.file.Paths

/** Represents a parsed class file
  */
case class ClassFile(
    magic: Int,
    minorVersion: Int,
    majorVersion: Int,
    constantPool: Array[ConstantPoolEntry],
    accessFlags: Int,
    thisClass: Int,
    superClass: Int,
    methods: Seq[MethodInfo]
)

/** Constant pool entry types
  */
sealed trait ConstantPoolEntry

case object EmptyEntry extends ConstantPoolEntry
case class Utf8Entry(value: String) extends ConstantPoolEntry
case class IntegerEntry(value: Int) extends ConstantPoolEntry
case class FloatEntry(value: Float) extends ConstantPoolEntry
case class LongEntry(value: Long) extends ConstantPoolEntry
case class DoubleEntry(value: Double) extends ConstantPoolEntry
case class ClassEntry(nameIndex: Int) extends ConstantPoolEntry
case class StringEntry(stringIndex: Int) extends ConstantPoolEntry
case class FieldRefEntry(classIndex: Int, nameAndTypeIndex: Int) extends ConstantPoolEntry
case class MethodRefEntry(classIndex: Int, nameAndTypeIndex: Int) extends ConstantPoolEntry
case class InterfaceMethodRefEntry(classIndex: Int, nameAndTypeIndex: Int) extends ConstantPoolEntry
case class NameAndTypeEntry(nameIndex: Int, descriptorIndex: Int) extends ConstantPoolEntry
case class MethodHandleEntry(referenceKind: Int, referenceIndex: Int) extends ConstantPoolEntry
case class MethodTypeEntry(descriptorIndex: Int) extends ConstantPoolEntry
case class InvokeDynamicEntry(bootstrapMethodAttrIndex: Int, nameAndTypeIndex: Int)
    extends ConstantPoolEntry

/** Parses Java class files according to the JVM specification
  */
class ClassFileParser:

  /** Parse a class file from the given file path
    */
  def parse(filePath: String): ClassFile =
    val bytes = Files.readAllBytes(Paths.get(filePath))
    val dis = new DataInputStream(new ByteArrayInputStream(bytes))

    try
      // Magic number (0xCAFEBABE)
      val magic = dis.readInt()
      if magic != 0xcafebabe then
        throw new ClassFormatError(
          s"Invalid magic number: 0x${magic.toHexString}, expected 0xCAFEBABE"
        )

      // Version
      val minorVersion = dis.readUnsignedShort()
      val majorVersion = dis.readUnsignedShort()
      println(s"[Parser] Class file version: $majorVersion.$minorVersion")

      // Constant pool
      val constantPoolCount = dis.readUnsignedShort()
      println(s"[Parser] Constant pool count: $constantPoolCount")
      val constantPool = parseConstantPool(dis, constantPoolCount)

      // Access flags
      val accessFlags = dis.readUnsignedShort()

      // This class, super class
      val thisClass = dis.readUnsignedShort()
      val superClass = dis.readUnsignedShort()

      // Interfaces
      val interfacesCount = dis.readUnsignedShort()
      for _ <- 0 until interfacesCount do dis.readUnsignedShort() // Skip interfaces for now

      // Fields
      val fieldsCount = dis.readUnsignedShort()
      for _ <- 0 until fieldsCount do skipField(dis)

      // Methods
      val methodsCount = dis.readUnsignedShort()
      println(s"[Parser] Methods count: $methodsCount")
      val methods = (0 until methodsCount).map { _ =>
        parseMethod(dis, constantPool)
      }

      // Skip class attributes
      skipAttributes(dis)

      ClassFile(
        magic,
        minorVersion,
        majorVersion,
        constantPool,
        accessFlags,
        thisClass,
        superClass,
        methods
      )
    finally dis.close()

  /** Parse the constant pool
    */
  private def parseConstantPool(dis: DataInputStream, count: Int): Array[ConstantPoolEntry] =
    val pool = new Array[ConstantPoolEntry](count)
    pool(0) = EmptyEntry

    var i = 1
    while i < count do
      val tag = dis.readUnsignedByte()
      pool(i) = tag match
        case 1 => // UTF8
          val length = dis.readUnsignedShort()
          val bytes = new Array[Byte](length)
          dis.readFully(bytes)
          Utf8Entry(new String(bytes, "UTF-8"))

        case 3 => // Integer
          IntegerEntry(dis.readInt())

        case 4 => // Float
          FloatEntry(dis.readFloat())

        case 5 => // Long (takes 2 slots)
          val entry = LongEntry(dis.readLong())
          i += 1
          if i < count then pool(i) = EmptyEntry
          entry

        case 6 => // Double (takes 2 slots)
          val entry = DoubleEntry(dis.readDouble())
          i += 1
          if i < count then pool(i) = EmptyEntry
          entry

        case 7 => // Class
          ClassEntry(dis.readUnsignedShort())

        case 8 => // String
          StringEntry(dis.readUnsignedShort())

        case 9 => // Fieldref
          FieldRefEntry(dis.readUnsignedShort(), dis.readUnsignedShort())

        case 10 => // Methodref
          MethodRefEntry(dis.readUnsignedShort(), dis.readUnsignedShort())

        case 11 => // InterfaceMethodref
          InterfaceMethodRefEntry(dis.readUnsignedShort(), dis.readUnsignedShort())

        case 12 => // NameAndType
          NameAndTypeEntry(dis.readUnsignedShort(), dis.readUnsignedShort())

        case 15 => // MethodHandle
          MethodHandleEntry(dis.readUnsignedByte(), dis.readUnsignedShort())

        case 16 => // MethodType
          MethodTypeEntry(dis.readUnsignedShort())

        case 18 => // InvokeDynamic
          InvokeDynamicEntry(dis.readUnsignedShort(), dis.readUnsignedShort())

        case _ =>
          println(s"[Parser] Warning: Unknown constant pool tag: $tag at index $i")
          EmptyEntry

      i += 1

    pool

  /** Skip a field entry
    */
  private def skipField(dis: DataInputStream): Unit =
    dis.readUnsignedShort() // access_flags
    dis.readUnsignedShort() // name_index
    dis.readUnsignedShort() // descriptor_index
    skipAttributes(dis)

  /** Parse a method
    */
  private def parseMethod(
      dis: DataInputStream,
      constantPool: Array[ConstantPoolEntry]
  ): MethodInfo =
    val accessFlags = dis.readUnsignedShort()
    val nameIndex = dis.readUnsignedShort()
    val descriptorIndex = dis.readUnsignedShort()
    val attributesCount = dis.readUnsignedShort()

    var maxStack = 100
    var maxLocals = 100
    var code = Array.empty[Byte]

    for _ <- 0 until attributesCount do
      val attrNameIndex = dis.readUnsignedShort()
      val attrLength = dis.readInt()

      val attrName = constantPool(attrNameIndex) match
        case Utf8Entry(name) => name
        case _               => ""

      if attrName == "Code" then
        maxStack = dis.readUnsignedShort()
        maxLocals = dis.readUnsignedShort()
        val codeLength = dis.readInt()
        code = new Array[Byte](codeLength)
        dis.readFully(code)

        // Skip exception table
        val exceptionTableLength = dis.readUnsignedShort()
        for _ <- 0 until exceptionTableLength do dis.skipBytes(8)

        // Skip code attributes
        skipAttributes(dis)
      else dis.skipBytes(attrLength)

    val name = constantPool(nameIndex) match
      case Utf8Entry(n) => n
      case _            => "unknown"

    val descriptor = constantPool(descriptorIndex) match
      case Utf8Entry(d) => d
      case _            => ""

    println(
      s"[Parser] Parsed method: $name$descriptor (maxStack=$maxStack, maxLocals=$maxLocals, codeLen=${code.length})"
    )

    MethodInfo(name, descriptor, accessFlags, maxStack, maxLocals, code)

  /** Skip attribute entries
    */
  private def skipAttributes(dis: DataInputStream): Unit =
    val attributesCount = dis.readUnsignedShort()
    for _ <- 0 until attributesCount do
      dis.readUnsignedShort() // attribute_name_index
      val length = dis.readInt()
      dis.skipBytes(length)

end ClassFileParser

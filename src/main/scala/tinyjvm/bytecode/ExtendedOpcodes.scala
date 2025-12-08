package tinyjvm.bytecode

object ExtendedOpcodes:
  // Load instructions
  val ILOAD_0: Int = 26 // 0x1a
  val ILOAD_1: Int = 27 // 0x1b
  val ILOAD_2: Int = 28 // 0x1c
  val ILOAD_3: Int = 29 // 0x1d

  val LLOAD_0: Int = 30 // 0x1e
  val LLOAD_1: Int = 31 // 0x1f
  val LLOAD_2: Int = 32 // 0x20
  val LLOAD_3: Int = 33 // 0x21

  val FLOAD_0: Int = 34 // 0x22
  val FLOAD_1: Int = 35 // 0x23
  val FLOAD_2: Int = 36 // 0x24
  val FLOAD_3: Int = 37 // 0x25

  val DLOAD_0: Int = 38 // 0x26
  val DLOAD_1: Int = 39 // 0x27
  val DLOAD_2: Int = 40 // 0x28
  val DLOAD_3: Int = 41 // 0x29

  val ALOAD_0: Int = 42 // 0x2a
  val ALOAD_1: Int = 43 // 0x2b
  val ALOAD_2: Int = 44 // 0x2c
  val ALOAD_3: Int = 45 // 0x2d

  // Store instructions
  val ISTORE_0: Int = 59 // 0x3b
  val ISTORE_1: Int = 60 // 0x3c
  val ISTORE_2: Int = 61 // 0x3d
  val ISTORE_3: Int = 62 // 0x3e

  val LSTORE_0: Int = 63 // 0x3f
  val LSTORE_1: Int = 64 // 0x40
  val LSTORE_2: Int = 65 // 0x41
  val LSTORE_3: Int = 66 // 0x42

  val FSTORE_0: Int = 67 // 0x43
  val FSTORE_1: Int = 68 // 0x44
  val FSTORE_2: Int = 69 // 0x45
  val FSTORE_3: Int = 70 // 0x46

  val DSTORE_0: Int = 71 // 0x47
  val DSTORE_1: Int = 72 // 0x48
  val DSTORE_2: Int = 73 // 0x49
  val DSTORE_3: Int = 74 // 0x4a

  val ASTORE_0: Int = 75 // 0x4b
  val ASTORE_1: Int = 76 // 0x4c
  val ASTORE_2: Int = 77 // 0x4d
  val ASTORE_3: Int = 78 // 0x4e

end ExtendedOpcodes

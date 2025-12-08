package tinyjvm.runtime

import tinyjvm.classloader.ConstantPoolEntry

import scala.collection.mutable

/** Represents a loaded class with its methods, fields, and constant pool
  */
case class ClassInfo(
    name: String,
    superClass: Option[String],
    methods: Map[String, MethodInfo],
    fields: Map[String, FieldInfo],
    constantPool: Array[ConstantPoolEntry]
)

/** Represents a method with its bytecode and metadata
  */
case class MethodInfo(
    name: String,
    descriptor: String,
    accessFlags: Int,
    maxStack: Int,
    maxLocals: Int,
    code: Array[Byte]
)

/** Represents a field with its metadata
  */
case class FieldInfo(
    name: String,
    descriptor: String,
    accessFlags: Int
)

/** Method Area stores all loaded classes and their metadata This is shared across all threads
  */
class MethodArea:
  private val classes = mutable.Map[String, ClassInfo]()

  /** Load a class into the method area
    */
  def loadClass(classInfo: ClassInfo): Unit =
    if classes.contains(classInfo.name) then
      println(s"[MethodArea] Class ${classInfo.name} already loaded, skipping")
      return

    classes(classInfo.name) = classInfo
    println(s"[MethodArea] Loaded class: ${classInfo.name} with ${classInfo.methods.size} methods")

  /** Get a loaded class by name
    */
  def getClass(className: String): Option[ClassInfo] =
    classes.get(className)

  /** Get a specific method from a class
    */
  def getMethod(className: String, methodName: String, descriptor: String): Option[MethodInfo] =
    classes.get(className).flatMap { classInfo =>
      val key = s"$methodName$descriptor"
      classInfo.methods.get(key)
    }

  /** Check if a class is loaded
    */
  def hasClass(className: String): Boolean =
    classes.contains(className)

  /** List all loaded classes
    */
  def listClasses(): Seq[String] =
    classes.keys.toSeq.sorted

  /** Get statistics about the method area
    */
  def stats(): String =
    val totalMethods = classes.values.map(_.methods.size).sum
    val totalFields = classes.values.map(_.fields.size).sum
    s"Classes: ${classes.size}, Methods: $totalMethods, Fields: $totalFields"

  override def toString: String =
    s"MethodArea(${classes.keys.mkString(", ")})"

end MethodArea

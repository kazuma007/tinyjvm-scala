package tinyjvm.classloader

import tinyjvm.runtime.ClassInfo
import tinyjvm.runtime.MethodArea

import java.nio.file.Paths

/** ClassLoader loads and parses Java class files
  */
class ClassLoader(methodArea: MethodArea):
  private val parser = new ClassFileParser()

  /** Load a class from a file path
    */
  def loadClass(classPath: String): ClassInfo =
    println(s"[ClassLoader] Loading class from: $classPath")

    val className = extractClassName(classPath)
    val classFile = parser.parse(classPath)
    val classInfo = ClassInfo(
      name = className,
      superClass = Some("java/lang/Object"),
      methods = classFile.methods.map(m => s"${m.name}${m.descriptor}" -> m).toMap,
      fields = Map.empty, // TODO: Parse fields
      constantPool = classFile.constantPool
    )

    // Load into method area
    methodArea.loadClass(classInfo)

    println(s"[ClassLoader] Successfully loaded class: $className")
    classInfo

  /** Extract class name from file path
    */
  private def extractClassName(classPath: String): String =
    val fileName = Paths.get(classPath).getFileName.toString
    if fileName.endsWith(".class") then fileName.substring(0, fileName.length - 6)
    else fileName

end ClassLoader

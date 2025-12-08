package tinyjvm

import tinyjvm.classloader.ClassLoader
import tinyjvm.runtime.{MethodArea, Heap}
import tinyjvm.execution.ExecutionEngine

/**
 * Main JVM class that orchestrates class loading and execution
 */
class JVM:
  private val methodArea = new MethodArea()
  private val heap = new Heap()
  private val classLoader = new ClassLoader(methodArea)
  private val executionEngine = new ExecutionEngine(methodArea)

  /**
   * Load a class file
   */
  def loadClass(classFilePath: String): Unit =
    classLoader.loadClass(classFilePath)

  /**
   * Run a method from a loaded class
   */
  def run(
    className: String, 
    methodName: String = "main", 
    descriptor: String = "()I"
  ): Option[Any] =
    println(s"\n========================================")
    println(s"[TinyJVM] Starting execution")
    println(s"[TinyJVM] Class: $className")
    println(s"[TinyJVM] Method: $methodName$descriptor")
    println(s"========================================\n")

    try
      val result = executionEngine.execute(className, methodName, descriptor)

      println(s"\n========================================")
      println(s"[TinyJVM] Execution completed successfully")
      println(s"[TinyJVM] Result: $result")
      println(s"========================================\n")

      result
    catch
      case e: Exception =>
        println(s"\n========================================")
        println(s"[TinyJVM] Execution failed with exception:")
        println(s"[TinyJVM] ${e.getClass.getName}: ${e.getMessage}")
        println(s"========================================\n")
        throw e

  /**
   * Get the method area (for debugging)
   */
  def getMethodArea: MethodArea = methodArea

  /**
   * Get the heap (for debugging)
   */
  def getHeap: Heap = heap

  /**
   * Get the execution engine (for debugging)
   */
  def getExecutionEngine: ExecutionEngine = executionEngine

end JVM

/**
 * Companion object and entry point
 */
object TinyJVM:

  /**
   * Create a new JVM instance
   */
  def apply(): JVM = new JVM()

  /**
   * Main entry point for command-line execution
   */
  def main(args: Array[String]): Unit =
    if args.length < 1 then
      println("Usage: TinyJVM <classFilePath> [className] [methodName] [descriptor]")
      println("Example: TinyJVM Program.class Program main ()I")
      println("         TinyJVM test/Example.class Example compute (II)I")
      System.exit(1)

    val classFilePath = args(0)

    val className = if args.length > 1 then 
      args(1) 
    else
      val fileName = classFilePath.split("/").last
      if fileName.endsWith(".class") then
        fileName.substring(0, fileName.length - 6)
      else
        fileName

    val methodName = if args.length > 2 then args(2) else "main"
    val descriptor = if args.length > 3 then args(3) else "()I"

    // Create JVM and execute
    val jvm = TinyJVM()
    jvm.loadClass(classFilePath)
    jvm.run(className, methodName, descriptor)

end TinyJVM

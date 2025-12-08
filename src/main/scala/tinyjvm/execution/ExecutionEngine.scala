package tinyjvm.execution

import tinyjvm.runtime.Frame
import tinyjvm.runtime.MethodArea
import tinyjvm.runtime.Stack

/** The execution engine manages the execution of methods
  */
class ExecutionEngine(methodArea: MethodArea):
  private val threadStack = new Stack()
  private val interpreter: Interpreter = new Interpreter(this)

  /** Execute a method
    */
  def execute(
      className: String,
      methodName: String,
      descriptor: String,
      args: Array[Any] = Array()
  ): Option[Any] =
    println(s"[ExecutionEngine] Executing $className.$methodName$descriptor")

    // Get method from method area
    val methodInfo = methodArea.getMethod(className, methodName, descriptor).getOrElse {
      throw new NoSuchMethodError(s"Method $className.$methodName$descriptor not found")
    }

    // Get constant pool from class
    val classInfo = methodArea.getClass(className).getOrElse {
      throw new ClassNotFoundException(s"Class $className not found")
    }

    val frame = new Frame(
      maxStack = methodInfo.maxStack,
      maxLocals = methodInfo.maxLocals,
      code = methodInfo.code,
      className = className,
      methodName = methodName,
      constantPool = classInfo.constantPool
    )

    // Initialize local variables with method arguments
    for i <- args.indices do frame.localVariables.set(i, args(i))

    // Push frame onto thread stack
    threadStack.push(frame)

    try
      // Interpret the method
      val result = interpreter.interpret(frame)

      // Pop frame from thread stack
      threadStack.pop()

      println(s"[ExecutionEngine] Method completed with result: $result")
      result
    catch
      case e: Exception =>
        println(s"[ExecutionEngine] Exception during execution: ${e.getMessage}")
        threadStack.clear()
        throw e

  /** Execute a method (alias for execute, used by interpreter)
    */
  def executeMethod(
      className: String,
      methodName: String,
      descriptor: String,
      args: Array[Any]
  ): Option[Any] =
    execute(className, methodName, descriptor, args)

  /** Get the thread stack (for debugging)
    */
  def getThreadStack: Stack = threadStack

end ExecutionEngine

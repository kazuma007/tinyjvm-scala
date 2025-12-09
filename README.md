# tinyjvm-scala

An (educational) implementation of a Java Virtual Machine in Scala 3, demonstrating core JVM concepts including class loading, bytecode interpretation, and runtime data areas.

## Features

- **Class Loading**: Parse and load Java `.class` files
- **Bytecode Interpretation**: Execute JVM bytecode instructions
- **Runtime Data Areas**: Method area, heap, stacks, frames
- **Supported Instructions**: 50+ bytecode instructions including:
    - Arithmetic operations (IADD, ISUB, IMUL, IDIV, etc.)
    - Control flow (GOTO, IF*, branches)
    - Stack operations (DUP, POP, SWAP)
    - Method invocation (INVOKESTATIC)
    - Load/store operations
    - Constants
    - Array Support
        - Creation:
            - `NEWARRAY` for primitive arrays (int, long, float, double, byte, char, short, boolean)
            - `ANEWARRAY` for reference arrays (e.g., `Object[]`)
        - Access:
            - Loads: `IALOAD`, `LALOAD`, `FALOAD`, `DALOAD`, `AALOAD`, `BALOAD`, `CALOAD`, `SALOAD`
            - Stores: `IASTORE`, `LASTORE`, `FASTORE`, `DASTORE`, `AASTORE`, `BASTORE`, `CASTORE`, `SASTORE`
        - Length:
            - `ARRAYLENGTH` to read array length
        - Safety:
            - `NullPointerException` on null arrays
            - `ArrayIndexOutOfBoundsException` on invalid indices
            - `NegativeArraySizeException` on negative sizes

## Project Structure

```
tinyjvm/
├── bytecode/          # Instruction definitions
│   ├── Instruction.scala
│   └── ExtendedOpcodes.scala
├── classloader/       # Class file parsing
│   ├── ClassFileParser.scala
│   └── ClassLoader.scala
├── execution/         # Execution engine
│   ├── ExecutionEngine.scala
│   ├── Interpreter.scala
│   └── OpcodeUtils.scala
├── memory/            # Memory structures
│   ├── LocalVariables.scala
│   └── OperandStack.scala
├── runtime/           # Runtime data areas
│   ├── Frame.scala
│   ├── Stack.scala
│   ├── Heap.scala
│   └── MethodArea.scala
└── JVM.scala          # Main JVM class
```

## Building

Requires: Scala 3.3.7, SBT 1.11+

```bash
sbt compile
```

## Usage

### Command Line

```bash
sbt "run <classFilePath> [className] [methodName] [descriptor]"
```

### Programmatic

```scala
import tinyjvm.TinyJVM

val jvm = TinyJVM()
jvm.loadClass("Program.class")
val result = jvm.run("Program", "main", "()I")
println(s"Result: $result")
```

## Example

Given this Java code:
```java
public class SimpleAdd {
    public static int add() {
        int a = 5;
        int b = 3;
        return a + b;
    }
}
```

Compile to bytecode:
```bash
javac SimpleAdd.java
```

Run with TinyJVM:
```bash
sbt "run testfiles/SimpleAdd.class SimpleAdd add ()I"
```

Output:
```
% sbt "run testfiles/SimpleAdd.class SimpleAdd add ()I"
[info] running tinyjvm.TinyJVM testfiles/SimpleAdd.class SimpleAdd add ()I
[ClassLoader] Loading class from: testfiles/SimpleAdd.class
[Parser] Class file version: 67.0
[Parser] Constant pool count: 15
[Parser] Methods count: 2
[Parser] Parsed method: <init>()V (maxStack=1, maxLocals=1, codeLen=5)
[Parser] Parsed method: add()I (maxStack=2, maxLocals=2, codeLen=8)
[MethodArea] Loaded class: SimpleAdd with 2 methods
[ClassLoader] Successfully loaded class: SimpleAdd

========================================
[TinyJVM] Starting execution
[TinyJVM] Class: SimpleAdd
[TinyJVM] Method: add()I
========================================

[ExecutionEngine] Executing SimpleAdd.add()I
[ExecutionEngine] Method completed with result: Some(8)

========================================
[TinyJVM] Execution completed successfully
[TinyJVM] Result: Some(8)
========================================

[success] Total time: 12 s, completed 6 Dec 2025, 16:43:01
```

## Linting & Formatting

This project uses **Scalafmt** and **Scalafix**.

### Commands

#### Format code with Scalafmt:

```
sbt scalafmtAll
```

#### Run Scalafix to clean up unused imports and locals:

```
sbt scalafixAll
```

## Limitations

This is an educational implementation with several limitations:
- No object creation or instance methods (only static methods)
- No exception handling
- No garbage collection
- Limited standard library support
- No JIT compilation

## Architecture

### Class Loading
1. Parse .class file according to JVM specification
2. Extract constant pool, methods, and metadata
3. Store in method area

### Execution
1. Create frame with operand stack and local variables
2. Fetch instruction at program counter (PC)
3. Decode and execute instruction
4. Update PC and repeat until return instruction

### Memory Areas
- **Method Area**: Stores class metadata and bytecode
- **Heap**: Stores objects (minimal implementation)
- **Stack**: Holds frames for method invocations
- **Frame**: Contains operand stack, local variables, and PC

## References

- [JVM Specification](https://docs.oracle.com/javase/specs/jvms/se21/html/)
- [ASM Library](https://asm.ow2.io/)
- [Understanding the JVM](https://www.artima.com/insidejvm/ed2/)

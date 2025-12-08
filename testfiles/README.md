# TinyJVM Test Suite

## Running Tests

### Compile Java Test Files
```bash
javac -d testfiles testfiles/*.java
```

### Run All Tests
```bash
sbt test
```

### Run Specific Test
```bash
sbt "testOnly tinyjvm.TinyJVMIntegrationTest"
```

### Run Single Test Case
```bash
sbt "testOnly tinyjvm.TinyJVMIntegrationTest -- -z Fibonacci"
```

## Test Categories

### Basic Tests
- **SimpleAdd**: Basic addition (5 + 3 = 8)
- **SimpleLoop**: Loop with accumulation (sum 1 to 5 = 15)
- **MethodCall**: Method invocation (square(5) = 25)
- **Conditional**: If-else branching

### Advanced Arithmetic Tests
- **ComplexArithmetic**: Multiple operations ((10+5)*3-8/2 = 41)
- **DivisionTest**: Integer division (100/4 = 25)
- **NegativeNumbers**: Negative number handling (-5+10-3 = 2)

### Recursive and Loop Tests
- **Fibonacci**: Iterative Fibonacci (fib(7) = 13)
- **Factorial**: Factorial calculation (5! = 120)
- **NestedLoop**: Nested loops with multiplication table (sum = 100)

### Control Flow Tests
- **MultipleReturns**: Early return from method
- **MaxValue**: Finding maximum of two numbers

### Error Handling Tests
- Invalid class file format
- Non-existent class/method
- Stack/frame management

## Test Coverage

The test suite covers:
- ✓ Arithmetic operations (IADD, ISUB, IMUL, IDIV)
- ✓ Control flow (IF, GOTO, branches)
- ✓ Method calls (INVOKESTATIC)
- ✓ Local variables (ILOAD, ISTORE)
- ✓ Stack operations (DUP, POP)
- ✓ Constants (ICONST, BIPUSH, SIPUSH)
- ✓ Comparisons (IF_ICMP*)
- ✓ Error handling

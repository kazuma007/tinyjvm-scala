# Test Files

This directory contains Java source files for testing TinyJVM.

## Compiling Test Files

### On Linux/Mac:
```bash
chmod +x compile.sh
./compile.sh
```

### Manual compilation:
```bash
javac SimpleAdd.java
javac SimpleLoop.java
javac MethodCall.java
javac Conditional.java
```

## Test Files

- **SimpleAdd.java** - Basic arithmetic (5 + 3 = 8)
- **SimpleLoop.java** - While loop, sum 1 to 5 (result: 15)
- **MethodCall.java** - Static method invocation (5² = 25)
- **Conditional.java** - If/else statement (result: 1)

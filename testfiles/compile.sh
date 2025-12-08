#!/bin/bash
# Compile all Java test files

echo "Compiling test files..."
javac SimpleAdd.java
javac SimpleLoop.java
javac MethodCall.java
javac Conditional.java

echo "Done! Generated .class files:"
ls -la *.class

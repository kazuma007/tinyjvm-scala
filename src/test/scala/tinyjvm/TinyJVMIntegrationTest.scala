package tinyjvm

import org.scalatest.BeforeAndAfterAll
import org.scalatest.funsuite.AnyFunSuite

import java.nio.file.Files
import java.nio.file.Paths
import scala.sys.process._

class TinyJVMIntegrationTest extends AnyFunSuite with BeforeAndAfterAll {

  val testFilesDir = "testfiles"
  val javaFiles = Seq(
    "SimpleAdd",
    "SimpleLoop",
    "MethodCall",
    "Conditional",
    "Fibonacci",
    "Factorial",
    "NestedLoop",
    "ComplexArithmetic",
    "MultipleReturns",
    "MaxValue",
    "DivisionTest",
    "NegativeNumbers"
  )

  override def beforeAll(): Unit = {
    println("\n=== Compiling Java test files ===")

    javaFiles.foreach { className =>
      val javaFile = s"$testFilesDir/$className.java"
      s"$testFilesDir/$className.class"

      if (!Files.exists(Paths.get(javaFile))) {
        println(s"WARNING: $javaFile not found, skipping compilation")
      } else {
        println(s"Compiling $javaFile...")
        val result = s"javac -d $testFilesDir $javaFile".!
        if (result == 0) {
          println(s"  ✓ Successfully compiled $className.java")
        } else {
          println(s"  ✗ Failed to compile $className.java")
        }
      }
    }
    println("=== Compilation complete ===\n")
  }

  // ========================================
  // BASIC TESTS
  // ========================================

  test("SimpleAdd: 5 + 3 should equal 8") {
    val classFile = s"$testFilesDir/SimpleAdd.class"

    if (!Files.exists(Paths.get(classFile))) {
      cancel(s"$classFile not found. Run: javac testfiles/SimpleAdd.java")
    }

    val jvm = TinyJVM()
    jvm.loadClass(classFile)
    val result = jvm.run("SimpleAdd", "add", "()I")

    assert(result.isDefined, "Result should be defined")
    assert(result.get == 8, s"Expected 8, got ${result.get}")
  }

  test("SimpleLoop: sum from 1 to 5 should equal 15") {
    val classFile = s"$testFilesDir/SimpleLoop.class"

    if (!Files.exists(Paths.get(classFile))) {
      cancel(s"$classFile not found. Run: javac testfiles/SimpleLoop.java")
    }

    val jvm = TinyJVM()
    jvm.loadClass(classFile)
    val result = jvm.run("SimpleLoop", "sumToN", "()I")

    assert(result.isDefined, "Result should be defined")
    assert(result.get == 15, s"Expected 15 (1+2+3+4+5), got ${result.get}")
  }

  test("MethodCall: square(5) should equal 25") {
    val classFile = s"$testFilesDir/MethodCall.class"

    if (!Files.exists(Paths.get(classFile))) {
      cancel(s"$classFile not found. Run: javac testfiles/MethodCall.java")
    }

    val jvm = TinyJVM()
    jvm.loadClass(classFile)
    val result = jvm.run("MethodCall", "main", "()I")

    assert(result.isDefined, "Result should be defined")
    assert(result.get == 25, s"Expected 25 (5²), got ${result.get}")
  }

  // ========================================
  // ADVANCED ARITHMETIC TESTS
  // ========================================

  test("ComplexArithmetic: (10 + 5) * 3 - 8 / 2 should equal 41") {
    val classFile = s"$testFilesDir/ComplexArithmetic.class"

    if (!Files.exists(Paths.get(classFile))) {
      cancel(s"$classFile not found")
    }

    val jvm = TinyJVM()
    jvm.loadClass(classFile)
    val result = jvm.run("ComplexArithmetic", "compute", "()I")

    assert(result.isDefined, "Result should be defined")
    assert(result.get == 41, s"Expected 41, got ${result.get}")
  }

  test("DivisionTest: 100 / 4 should equal 25") {
    val classFile = s"$testFilesDir/DivisionTest.class"

    if (!Files.exists(Paths.get(classFile))) {
      cancel(s"$classFile not found")
    }

    val jvm = TinyJVM()
    jvm.loadClass(classFile)
    val result = jvm.run("DivisionTest", "divide", "()I")

    assert(result.isDefined, "Result should be defined")
    assert(result.get == 25, s"Expected 25, got ${result.get}")
  }

  test("NegativeNumbers: -5 + 10 - 3 should equal 2") {
    val classFile = s"$testFilesDir/NegativeNumbers.class"

    if (!Files.exists(Paths.get(classFile))) {
      cancel(s"$classFile not found")
    }

    val jvm = TinyJVM()
    jvm.loadClass(classFile)
    val result = jvm.run("NegativeNumbers", "compute", "()I")

    assert(result.isDefined, "Result should be defined")
    assert(result.get == 2, s"Expected 2, got ${result.get}")
  }

  // ========================================
  // RECURSIVE AND LOOP TESTS
  // ========================================

  test("Fibonacci: fib(7) should equal 13") {
    val classFile = s"$testFilesDir/Fibonacci.class"

    if (!Files.exists(Paths.get(classFile))) {
      cancel(s"$classFile not found")
    }

    val jvm = TinyJVM()
    jvm.loadClass(classFile)
    val result = jvm.run("Fibonacci", "compute", "()I")

    assert(result.isDefined, "Result should be defined")
    assert(result.get == 13, s"Expected 13 (7th Fibonacci), got ${result.get}")
  }

  test("Factorial: 5! should equal 120") {
    val classFile = s"$testFilesDir/Factorial.class"

    if (!Files.exists(Paths.get(classFile))) {
      cancel(s"$classFile not found")
    }

    val jvm = TinyJVM()
    jvm.loadClass(classFile)
    val result = jvm.run("Factorial", "compute", "()I")

    assert(result.isDefined, "Result should be defined")
    assert(result.get == 120, s"Expected 120 (5!), got ${result.get}")
  }

  test("NestedLoop: sum of multiplication table (1-4) should equal 100") {
    val classFile = s"$testFilesDir/NestedLoop.class"

    if (!Files.exists(Paths.get(classFile))) {
      cancel(s"$classFile not found")
    }

    val jvm = TinyJVM()
    jvm.loadClass(classFile)
    val result = jvm.run("NestedLoop", "compute", "()I")

    assert(result.isDefined, "Result should be defined")
    assert(result.get == 100, s"Expected 100, got ${result.get}")
  }

  // ========================================
  // CONTROL FLOW TESTS
  // ========================================

  test("MultipleReturns: should return early on condition") {
    val classFile = s"$testFilesDir/MultipleReturns.class"

    if (!Files.exists(Paths.get(classFile))) {
      cancel(s"$classFile not found")
    }

    val jvm = TinyJVM()
    jvm.loadClass(classFile)
    val result = jvm.run("MultipleReturns", "checkCondition", "()I")

    assert(result.isDefined, "Result should be defined")
    assert(result.get == 42, s"Expected 42, got ${result.get}")
  }

  test("MaxValue: max(15, 23) should equal 23") {
    val classFile = s"$testFilesDir/MaxValue.class"

    if (!Files.exists(Paths.get(classFile))) {
      cancel(s"$classFile not found")
    }

    val jvm = TinyJVM()
    jvm.loadClass(classFile)
    val result = jvm.run("MaxValue", "findMax", "()I")

    assert(result.isDefined, "Result should be defined")
    assert(result.get == 23, s"Expected 23, got ${result.get}")
  }

  // ========================================
  // ERROR HANDLING TESTS
  // ========================================

  test("Class file with invalid magic number should throw ClassFormatError") {
    val fakeClassFile = s"$testFilesDir/Fake.class"
    Files.write(Paths.get(fakeClassFile), "public class Fake {}".getBytes)

    val jvm = TinyJVM()

    assertThrows[ClassFormatError] {
      jvm.loadClass(fakeClassFile)
    }

    Files.deleteIfExists(Paths.get(fakeClassFile))
  }

  test("Non-existent class file should throw exception") {
    val jvm = TinyJVM()

    assertThrows[Exception] {
      jvm.loadClass("NonExistent.class")
    }
  }

  test("Non-existent method should throw NoSuchMethodError") {
    val classFile = s"$testFilesDir/SimpleAdd.class"

    if (!Files.exists(Paths.get(classFile))) {
      cancel(s"$classFile not found")
    }

    val jvm = TinyJVM()
    jvm.loadClass(classFile)

    assertThrows[NoSuchMethodError] {
      jvm.run("SimpleAdd", "nonExistentMethod", "()I")
    }
  }

  // ========================================
  // STACK AND FRAME TESTS
  // ========================================

  test("Multiple class loads should use cached version") {
    val classFile = s"$testFilesDir/SimpleAdd.class"

    if (!Files.exists(Paths.get(classFile))) {
      cancel(s"$classFile not found")
    }

    val jvm = TinyJVM()
    jvm.loadClass(classFile)

    // Load again - should use cached version
    jvm.loadClass(classFile)

    val result = jvm.run("SimpleAdd", "add", "()I")
    assert(result.isDefined, "Result should be defined")
    assert(result.get == 8, s"Expected 8, got ${result.get}")
  }

  test("Method area should contain loaded class") {
    val classFile = s"$testFilesDir/SimpleAdd.class"

    if (!Files.exists(Paths.get(classFile))) {
      cancel(s"$classFile not found")
    }

    val jvm = TinyJVM()
    jvm.loadClass(classFile)

    val methodArea = jvm.getMethodArea
    assert(methodArea.hasClass("SimpleAdd"), "SimpleAdd should be in method area")
    assert(methodArea.getClass("SimpleAdd").isDefined, "Should retrieve SimpleAdd class")
  }
}

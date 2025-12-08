package tinyjvm

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.BeforeAndAfterAll
import java.nio.file.{Files, Paths}
import scala.sys.process._
import java.io.File

class TinyJVMIntegrationTest extends AnyFunSuite with BeforeAndAfterAll {

  val testFilesDir = "testfiles"
  val javaFiles = Seq("SimpleAdd", "SimpleLoop", "MethodCall", "Conditional")

  override def beforeAll(): Unit = {
    println("\n=== Compiling Java test files ===")

    javaFiles.foreach { className =>
      val javaFile = s"$testFilesDir/$className.java"
      val classFile = s"$testFilesDir/$className.class"

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

  test("Conditional: x >= 10 should return 1") {
    val classFile = s"$testFilesDir/Conditional.class"

    if (!Files.exists(Paths.get(classFile))) {
      cancel(s"$classFile not found. Run: javac testfiles/Conditional.java")
    }

    val jvm = TinyJVM()
    jvm.loadClass(classFile)
    val result = jvm.run("Conditional", "checkValue", "()I")

    assert(result.isDefined, "Result should be defined")
    assert(result.get == 1, s"Expected 1 (true branch), got ${result.get}")
  }

  test("Class file with invalid magic number should throw ClassFormatError") {
    // Create a fake class file with invalid magic number
    val fakeClassFile = s"$testFilesDir/Fake.class"
    Files.write(Paths.get(fakeClassFile), "public class Fake {}".getBytes)

    val jvm = TinyJVM()

    assertThrows[ClassFormatError] {
      jvm.loadClass(fakeClassFile)
    }

    // Cleanup
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
}

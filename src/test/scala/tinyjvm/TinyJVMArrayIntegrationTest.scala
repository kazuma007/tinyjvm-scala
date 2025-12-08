package tinyjvm

import org.scalatest.BeforeAndAfterAll
import org.scalatest.funsuite.AnyFunSuite

import java.nio.file.Files
import java.nio.file.Paths
import scala.sys.process._

class TinyJVMArrayIntegrationTest extends AnyFunSuite with BeforeAndAfterAll {

  val testFilesDir = "testfiles"
  val arrayTestFile = "ArrayTest"

  override def beforeAll(): Unit = {
    println("\n=== Compiling Array test file ===")

    val javaFile = s"$testFilesDir/$arrayTestFile.java"

    if (!Files.exists(Paths.get(javaFile))) {
      println(s"WARNING: $javaFile not found, skipping compilation")
    } else {
      println(s"Compiling $javaFile...")
      val result = s"javac -d $testFilesDir $javaFile".!
      if (result == 0) {
        println(s"  ✓ Successfully compiled $arrayTestFile.java")
      } else {
        println(s"  ✗ Failed to compile $arrayTestFile.java")
      }
    }
    println("=== Compilation complete ===\n")
  }

  test("ArrayTest: basicArrayTest - create array and access elements") {
    val classFile = s"$testFilesDir/$arrayTestFile.class"

    if (!Files.exists(Paths.get(classFile))) {
      cancel(s"$classFile not found")
    }

    val jvm = TinyJVM()
    jvm.loadClass(classFile)
    val result = jvm.run(arrayTestFile, "basicArrayTest")

    assert(result.isDefined, "Result should be defined")
    assert(result.get == 60, s"Expected 60 (10+20+30), got ${result.get}")
  }

  test("ArrayTest: sumArray - sum all elements in array") {
    val classFile = s"$testFilesDir/$arrayTestFile.class"

    if (!Files.exists(Paths.get(classFile))) {
      cancel(s"$classFile not found")
    }

    val jvm = TinyJVM()
    jvm.loadClass(classFile)
    val result = jvm.run(arrayTestFile, "sumArray")

    assert(result.isDefined, "Result should be defined")
    assert(result.get == 50, s"Expected 50 (5+10+15+20), got ${result.get}")
  }

  test("ArrayTest: arrayLengthTest - get array length") {
    val classFile = s"$testFilesDir/$arrayTestFile.class"

    if (!Files.exists(Paths.get(classFile))) {
      cancel(s"$classFile not found")
    }

    val jvm = TinyJVM()
    jvm.loadClass(classFile)
    val result = jvm.run(arrayTestFile, "arrayLengthTest")

    assert(result.isDefined, "Result should be defined")
    assert(result.get == 7, s"Expected 7, got ${result.get}")
  }

  test("ArrayTest: findMax - find maximum element") {
    val classFile = s"$testFilesDir/$arrayTestFile.class"

    if (!Files.exists(Paths.get(classFile))) {
      cancel(s"$classFile not found")
    }

    val jvm = TinyJVM()
    jvm.loadClass(classFile)
    val result = jvm.run(arrayTestFile, "findMax")

    assert(result.isDefined, "Result should be defined")
    assert(result.get == 25, s"Expected 25, got ${result.get}")
  }

  test("ArrayTest: initializeAndSum - initialize array with loop and sum") {
    val classFile = s"$testFilesDir/$arrayTestFile.class"

    if (!Files.exists(Paths.get(classFile))) {
      cancel(s"$classFile not found")
    }

    val jvm = TinyJVM()
    jvm.loadClass(classFile)
    val result = jvm.run(arrayTestFile, "initializeAndSum")

    assert(result.isDefined, "Result should be defined")
    assert(result.get == 90, s"Expected 90, got ${result.get}")
  }

  test("ArrayTest: reverseArray - reverse array in place") {
    val classFile = s"$testFilesDir/$arrayTestFile.class"

    if (!Files.exists(Paths.get(classFile))) {
      cancel(s"$classFile not found")
    }

    val jvm = TinyJVM()
    jvm.loadClass(classFile)
    val result = jvm.run(arrayTestFile, "reverseArray")

    assert(result.isDefined, "Result should be defined")
    assert(result.get == 51, s"Expected 51, got ${result.get}")
  }
}

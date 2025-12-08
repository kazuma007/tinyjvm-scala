package tinyjvm.runtime

import scala.collection.mutable

/** Heap for storing objects and arrays
  */
class Heap:
  private val objects = mutable.Map[Int, Any]()
  private var nextObjectId = 1

  /** Allocate a new object on the heap
    * @return
    *   Object ID
    */
  def allocate(obj: Any): Int =
    val objectId = nextObjectId
    nextObjectId += 1
    objects(objectId) = obj
    objectId

  /** Get an object from the heap by ID
    */
  def get(objectId: Int): Option[Any] =
    objects.get(objectId)

  /** Update an existing object on the heap
    */
  def update(objectId: Int, obj: Any): Unit =
    if !objects.contains(objectId) then
      throw new RuntimeException(s"Object with id $objectId does not exist in heap")
    objects(objectId) = obj

  /** Remove an object from the heap (manual deallocation)
    */
  def remove(objectId: Int): Unit =
    objects.remove(objectId)

  /** Get the number of objects in the heap
    */
  def size: Int = objects.size

  /** Simulate garbage collection (placeholder)
    */
  def gc(): Unit =
  println(s"[GC] Running garbage collection (${objects.size} objects)")
  // In a real implementation, this would mark and sweep unused objects

  override def toString: String =
    s"Heap(objects=${objects.size}, nextId=$nextObjectId)"

end Heap

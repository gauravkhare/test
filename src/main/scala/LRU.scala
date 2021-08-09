import scala.collection.immutable.HashMap
import scala.collection.mutable.ArrayBuffer

// Mimic a persistent store
object DataStore {
  val data = HashMap("a" -> "abc", "b" -> "bcd", "c" -> "cde", "d"->"def", "e"->"efg", "g" -> "ghi")
}

// this is least recently used cache
class LRU(val size: Int) {

  private var start: Entry = _
  private var end: Entry = _
  private var count: Int = 0
  private val BUCKETS = 2
  case class Entry(key:String,var value: String, var next: Entry, var nextLinked: Entry)

  private val arr = Array.fill[Entry](BUCKETS)(null)

  private def put(key:String,value:String): Unit = {
    val bucket = key.hashCode % BUCKETS
    if(arr(bucket) != null) {
      var temp = arr(bucket)
      var prev:Entry = null
      while(temp != null) {
        if(temp.key == key) {
          temp.value = value
          return
        } else {
          prev = temp
          temp = temp.next
        }
      }
      val newEntry = Entry(key,value,null,null)
      prev.next = newEntry
      end.nextLinked = newEntry
      end = newEntry
      count = count + 1
    } else {
      if(count==0) {
        arr(bucket) = Entry(key,value,null,null)
        start = arr(bucket)
        end = start
      } else {
        val newEntry = Entry(key,value,null,null)
        arr(bucket) = newEntry
        end.nextLinked = newEntry
        end = newEntry
      }
      count = count + 1
    }
    if(count>size){
      deleteStart()
    }
  }

  private def deleteStart(): Unit = {
    print(s"--delete--${start.key}")
    val bucket = start.key.hashCode % BUCKETS
    arr(bucket) = start.next
    start = start.nextLinked
    count = count - 1
  }

  private def reshuffleBasedOnLastAccess(temp: Entry):Option[String] = {
    // no reshuffle needed if the node that is accessed is already placed last in linked list
    if(temp.key == end.key) {
      print("--noreshuffle--")
      return Some(end.value)
    }

    if(temp == start) {
      start = start.nextLinked
      temp.nextLinked = null
      end.nextLinked = temp
      end = temp
      end.nextLinked = null
    } else {
      var t = start
      var prev: Entry = null
      while (t!=temp) {
        prev = t
        t = t.nextLinked
      }
      prev.nextLinked = temp.nextLinked
      temp.nextLinked = null
      end.nextLinked = temp
      end = temp
      end.nextLinked = null
    }
    Some(end.value)
  }

  def get(key: String) : Option[String] = {
    val bucket = key.hashCode % BUCKETS
    if (arr(bucket) == null) {
      print("--bucketempty--")
      load(key)
    } else {
      var temp = arr(bucket)
      while (temp != null) {
        if(temp.key == key) {
          return reshuffleBasedOnLastAccess(temp)
        } else {
          temp = temp.next
        }
      }
      print("--bucketnonempty--")
      load(key)
    }
  }

  private def load(k: String): Option[String] = {
    print("--load--")
    val v = DataStore.data.get(k)
    v.map { str =>
      put(k,str)
      str
    }
  }

  def getKeys():ArrayBuffer[String] = {
    var temp = start
    var a:ArrayBuffer[String] = ArrayBuffer[String]()
    while (temp!=null) {
      a +=  temp.key
      temp = temp.nextLinked
    }
    a
  }
}
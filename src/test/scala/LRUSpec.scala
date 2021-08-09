import org.scalatest.funsuite.AnyFunSuite

import scala.collection.mutable.ArrayBuffer

class LRUSpec extends AnyFunSuite {

  test("Scenario: Insert 4 kv pairs in a cache of size 4") {
    val ob = new LRU(4)
    ob.get("a") // first time insert
    ob.get("b") // first time insert
    ob.get("c") // first time insert
    ob.get("d") // first time insert

    assert(ob.get("a") === Some("abc"))
    assert(ob.get("b") === Some("bcd"))
    assert(ob.get("c") === Some("cde"))
    assert(ob.get("d") === Some("def"))
    assert(ob.size === 4)
  }

  test("Scenario: Insert 4 kv pairs in a cache of size 4 and verify eviction and reshuffling") {
    val ob = new LRU(4)

    ob.get("a") // first time insert
    ob.get("b") // first time insert
    ob.get("c") // first time insert
    ob.get("d") // first time insert

    assert(ob.get("d") === Some("def"))
    assert(ob.get("a") === Some("abc"))
    assert(ob.get("b") === Some("bcd"))
    assert(ob.get("e") === Some("efg"))
    assert(ob.getKeys() === ArrayBuffer("d","a","b","e"),"Seq in cache should match")

    assert(ob.get("g") === Some("ghi")) // new entry for g, d should be evicted as it was oldest entry
    assert(ob.getKeys() === ArrayBuffer("a","b","e","g"),"d should be evicted")
  }

}

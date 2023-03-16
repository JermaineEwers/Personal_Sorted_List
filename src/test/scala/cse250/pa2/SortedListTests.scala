package cse250.pa2
/**
 * cse250.pa2.SortedListTests
 *
 * Copyright 2022 Oliver Kennedy (okennedy@buffalo.edu)
 *           2022 Eric Mikida (epmikida@buffalo.edu)
 *
 * This work is licensed under the Creative Commons
 * Attribution-NonCommercial-ShareAlike 4.0 International License.
 * To view a copy of this license, visit
 * http://creativecommons.org/licenses/by-nc-sa/4.0/.
 *
 */

import org.scalatest.flatspec.AnyFlatSpec
import scala.util.Random

class SortedListTests extends AnyFlatSpec {

  /**
   * A "seeded" random instance that ensures that tests are 
   * deterministic.  The numbers that come out will be pseudorandom;
   * they'll be "random-like", but it'll be the same random sequence
   * each time, unless you change the seed value (The integer 250 
   * in our case).
   */
  val SeededRandom = new Random(250)


  behavior of "SortedList"
  it should "insert reverse-order elements in order" in 
  {
    val list = new SortedList[Int]()  

    // Inserting in reverse order should always insert at the head

    for(i <- Seq.range(start = 9, end = -1, step = -1) )
    { 
      list.insert(i)
    }

    // The for loop below invokes list via its 'iterator' method.
    // .zipWithIndex specifically wraps the list so that you get
    // back a 2-tuple (element, index) when the iterator is 
    // constructed.  Since we've inserted every element in the
    // range (-1, 9] == [0, 9] == [0, 10), every element should
    // be located at its index. 

    for((elem, index) <- list.zipWithIndex)
    {
      assert(elem == index)
    }
  }

  it should "insert in-order elements in order" in 
  {
    val list = new SortedList[Int]()  

    // Inserting in order should always insert at the tail

    for(i <- 0 until 10)
    { 
      list.insert(i)
    }

    // The for loop below invokes list via its 'iterator' method.
    // .zipWithIndex specifically wraps the list so that you get
    // back a 2-tuple (element, index) when the iterator is 
    // constructed.  Since we've inserted every element in the
    // range [0, 10), every element should be located at its index.

    for((elem, index) <- list.zipWithIndex)
    {
      assert(elem == index)
    }
  }

  it should "insert random-order elements in order" in 
  {
    val list = new SortedList[Int]()  

    // Inserting in random order should test all of the other cases of insert

    for(i <- SeededRandom.shuffle(IndexedSeq.range(start = 0, end = 30)) )
    { 
      list.insert(i)
    }

    // The for loop below invokes list via its 'iterator' method.
    // .zipWithIndex specifically wraps the list so that you get
    // back a 2-tuple (element, index) when the iterator is 
    // constructed.  Since we've inserted every element in the
    // range [0, 30), every element should be located at its index.

    for((elem, index) <- list.zipWithIndex)
    {
      assert(elem == index)
    }

    // Pick 10 distinct random indexes in the range [0, 30) and test
    // the apply method by checking to see if the element at that index
    // is equal to the index (following the same logic as the last test).

    for(index <- SeededRandom.shuffle(IndexedSeq.range(start = 0, end = 30)).take(10))
    {
      assert(list(index) == index)
    }
  }

  it should "insert in-order elements with hints" in
  {
    val list = new SortedList[Int]()  

    // The hinted version of the for loop should be quite a bit faster
    // for in-order insertions if we pass in a "hint" pointing at the
    // tail of the list (the last element we inserted).  If this test case
    // is a bit on the slow side, then you may have an O(n) implementation
    // of the hinted insert.

    var tail = list.insert(0)
    for(i <- 1 until 100)
    { 
      tail = list.insert(i, tail)
    }

    // Test using the same logic as above

    for((elem, index) <- list.zipWithIndex)
    {
      assert(elem == index)
    }
  }

  it should "efficiently update elements" in
  {
    val list = new SortedList[Int]()  

    // The hinted version of the for loop should be quite a bit faster
    // for in-order insertions if we pass in a "hint" pointing at the
    // tail of the list (the last element we inserted).  If this test case
    // is a bit on the slow side, then you may have an O(n) implementation
    // of the hinted insert.

    var first = list.insert(0)
    var tail = first
    for(i <- 1 until 2000)
    { 
      tail = list.insert(i*2, tail)
    }

    // Start with the head element of the list and move it forward 1000 spaces
    // If test case is a bit on the slow side, then you may have an O(n) 
    // implementation of either the hinted remove() or the hinted insert.

    for(i <- 0 until 1000){
      first = list.update(first, i * 2 + 1)
    }

    // Check to see if all of the elements are present and in-order
    for((elem, index) <- list.zipWithIndex)
    {
      if(index < 999){
        assert(elem == ((index+1)*2))
      } else if(index == 999) {
        assert(elem == 999*2+1)
      } else {
        assert(elem == index*2)
      }
    }
  }

  it should "be a linked list" in
  {
    val list = new SortedList[Int]()  

    var tail = list.insert(0)
    for(i <- 1 until 10)
    { 
      list.insert(i, tail)
    }

    var current = list.headNode
    assert(current.isDefined)
    for(i <- 0 until 10)
    {
      assert(current.isDefined)
      assert(current.get.value == i)
      current = current.get.next
    }
  }




  it should "be a  findrefbefore" in {
    val list = new SortedList[Int]()
    //list.insert(1)
 //   assert(list.insert(1)==list.getRef(0))
    list.insert(1)
  //  assert(list.getRef(0).count==2)
    list.insert(2)
    list.insert(30)
    list.insert(31)
    assert(list.getRef(1).prev==list.headNode)



    assert(list.findRefBefore(30).get== list.getRef(2))


    assert(list.findRefBefore(34).get== list.getRef(3) )
    assert(list.findRefBefore(0)==None)

    assert(list.findRefBefore(31,list.getRef(2)).get==list.getRef(3))
    assert(list.findRefBefore(34,list.getRef(1)).get== list.getRef(3) )
    assert(list.findRefBefore(0,list.getRef(1))==None)
    //assert(list.findRefBefore(10)== list.findRef(2 ))

  //  assert(list.findRefBefore(0)!= None)
  }

  it should "be a  findref" in {
    val list = new SortedList[Int]()
    list.insert(1)
    list.insert(2)
    list.insert(30)
    list.insert(31)

    assert(list.findRef(2).get==list.getRef(1))
    assert(list.findRef(32) == None)

    assert(list.findRef(2,list.getRef(0)).get==list.getRef(1))
    assert(list.findRef(31,list.getRef(2)).get==list.getRef(3))

  }

  it should "be a  getref" in {
    val list = new SortedList[Int]()
    list.insert(1)
    list.insert(2)
    list.insert(30)
    list.insert(31)
    assert(list.getRef(0)==list.headNode.get)
    assert(list.getRef(0).next.get==list.headNode.get.next.get)

   assertThrows[IndexOutOfBoundsException]{list.getRef(-1)}
    assertThrows[IndexOutOfBoundsException]{list.getRef(6)}
  }

  it should "be a  apply" in {
    val list = new SortedList[Int]()
    list.insert(1)
    list.insert(2)
    list.insert(30)
    list.insert(31)
    assertThrows[IndexOutOfBoundsException] {
      list.getRef(-1)
    }
    assertThrows[IndexOutOfBoundsException] {
      list.getRef(6)
    }
  assert(list.apply(1)==2)
  }

  it should "be a  insertion" in {





    val lissst = new SortedList[Int]()
    lissst.insert(1)
    lissst.insert(2)
    lissst.insert(30)
    lissst.insert(31)
    lissst.insert(31)
    lissst.insert(31)
    println(lissst.length)
    lissst.remove(lissst.getRef(3))
    //assert(lissst.length==3)
   println(lissst.length)



    val lisst = new SortedList[Int]()
    lisst.insert(1)
    lisst.insert(2)
    lisst.insert(30)
    lisst.insert(31)
    lisst.update(lisst.lastNode.get,32)

    assert(lisst.getRef(0) == lisst.getRef(1).prev.get)
    assert(lisst.getRef(1) == lisst.getRef(2).prev.get)
    assert(lisst.getRef(2) == lisst.getRef(3).prev.get)

    assert(lisst.getRef(0).next.get == lisst.getRef(1))
    assert(lisst.getRef(1).next.get == lisst.getRef(2))
    assert(lisst.getRef(2).next.get == lisst.getRef(3))

    lisst.insert(34)
    assert(lisst.getRef(0) == lisst.getRef(1).prev.get)
    assert(lisst.getRef(1) == lisst.getRef(2).prev.get)
    assert(lisst.getRef(2) == lisst.getRef(3).prev.get)
    assert(lisst.getRef(3) == lisst.getRef(4).prev.get)

    assert(lisst.getRef(0).next.get == lisst.getRef(1))
    assert(lisst.getRef(1).next.get == lisst.getRef(2))
    assert(lisst.getRef(2).next.get == lisst.getRef(3))
    assert(lisst.getRef(3).next.get == lisst.getRef(4))

    //println(lisst)


    val list = new SortedList[Int]()
    list.insert(1)
    list.insert(2)
    list.insert(30)
    list.insert(31)

    assert(list.getRef(0) == list.getRef(1).prev.get)
    assert(list.getRef(1) == list.getRef(2).prev.get)
    assert(list.getRef(2) == list.getRef(3).prev.get)

    assert(list.getRef(0).next.get== list.getRef(1))
    assert(list.getRef(1).next.get== list.getRef(2))
    assert(list.getRef(2).next.get== list.getRef(3))

    /////////////////////////////
    val listy = new SortedList[Int]()
    listy.insert(1)
    listy.insert(30)
    listy.insert(1)
    listy.insert(2)
    listy.insert(30)
    listy.insert(30)
    listy.insert(31)

    //println(listy.getRef(2).value)


    val listyY = new SortedList[Int]()
    listyY.insert(17)
    listyY.insert(19)
    listyY.insert(19)
    listyY.insert(19)
    listyY.insert(19)
    listyY.insert(21)
    assert(listyY.length==6)
   /* assert(listy.getRef(0) == listy.getRef(1).prev.get)
    assert(listy.getRef(1) == listy.getRef(2).prev.get)
    assert(listy.getRef(2) == listy.getRef(3).prev.get)

    assert(listy.getRef(0).next.get == listy.getRef(1))
    assert(listy.getRef(1).next.get == listy.getRef(2))
    assert(listy.getRef(2).next.get == listy.getRef(3))*/

    assert(listy.length==7) ///length should be the amount of elements in the sequence

    var tt = 0
    while (tt < 4) {

      if (list.findRef(list(tt)) == list.headNode) {
        assert(list.findRef(list(tt)).get.prev == None)
        assert(list.findRef(list(tt)).get.next == list.findRef(list(1)))
      } else if (list.findRef(list(tt)) == list.lastNode) {
        assert(list.findRef(list(tt)).get.next == None)
        assert(list.findRef(list(tt)).get.prev == list.findRef(list(2)))
      }

      tt += 1
    }



    //check next and prev and count
    assert(list.findRef(31).get.prev.get.value==30)
    assert(list.findRef(31).get.next==None)
    var i=0
    while(i<4){

      if(list.findRef(list(i)) == list.headNode){
        assert(list.findRef(list(i)).get.prev==None)
        assert(list.findRef(list(i)).get.next==list.findRef(list(1)))
      } else if(list.findRef(list(i)) == list.lastNode){
        assert(list.findRef(list(i)).get.next==None)
        assert(list.findRef(list(i)).get.prev==list.findRef(list(2)))
      }else{
        assert(list.findRef(list(i)).get.count==1)
      }


      i+=1
    }

    assert(list.length==4)

    assert(list.headNode.get.value==1)
    assert(list.lastNode.get.value==31)
    assert(list.headNode.get.next.get.value==2)
    assert(list.headNode.get.next.get.prev.get==list.headNode.get)
    assert(list(0)==1)
    assert(list(1)==2)
    assert(list(2)==30)
    assert(list(3)==31)
 assert(list.length==4)

    val lists = new SortedList[Int]()
    lists.insert(2)
    lists.insert(5)
    lists.insert(1)
    lists.insert(30)
    lists.insert(4)
    lists.insert(31)
    lists.insert(31)
    lists.insert(0)


    var t = lists.headNode
    while (t.get.next != None) {
      assert(t.get.value<t.get.next.get.value)
      t=t.get.next
    }

    assert(lists(0)==0)
    assert(lists(6)==31)
    assert(lists.getRef(6).count==2)

    val listt = new SortedList[Int]()
    listt.insert(1)
    listt.insert(5)
    listt.insert(8)
    listt.insert(30)
    listt.insert(33)
    listt.insert(31,listt.getRef(2))

    assert(listt.length==6)
    assert(listt.getRef(4).value==31)


    val listtt = new SortedList[Int]()
    listtt.insert(1)
    assert(listtt.headNode.get.value==1)
    assert(listtt.lastNode.get.value==1)

    //println(listt)
  }

  it should "be a  remove" in {

    val lists5 = new SortedList[Int]()
    lists5.insert(1)
    lists5.insert(1)
    lists5.insert(2)
    lists5.insert(3)
    val check=lists5.headNode.get.next


    lists5.removeAll(lists5.getRef(1))
    assert(check.get.prev==None)
    assert(check.get.next==None)
    //println(Some(check.get.prev))
    //println(Some(check.get.next))

    val list = new SortedList[Int]()

    list.insert(1)
    list.insert(2)
    list.insert(30)
    list.insert(31)
    list.insert(31)

    assertThrows[IllegalArgumentException] {
      list.removeN(list.lastNode.get,10)
    }

    assert(list.lastNode.get.count == 2)
    assert(list.remove(list.lastNode.get) == 31)

    assert(list.lastNode.get.count == 1)
    assert(list.lastNode.get.value == 31)
    assert(list.lastNode.get.value == 31)

   /* assert(list.remove(list.getRef(0))==1)
    assert(list(0)==2)
    assert(list.remove(list.getRef(3))==31)
    assert(list(1)==30)
    assert(list(2)==31) //needs to be fixed*/


    val list2 = new SortedList[Int]()
    list2.insert(1)
    list2.insert(1)
    list2.insert(2)
    list2.insert(30)
    list2.insert(30)
    list2.insert(31)
assert(list2.removeN(list2.headNode.get,2)==1)
    //list2.removeN(list2.headNode.get,2)
    assert(list2.headNode.get.value==2)

    val list22 = new SortedList[Int]()
    list22.insert(1)
    list22.insert(30)
    list22.insert(30)
    list22.insert(31)
    list22.removeN(list22.headNode.get.next.get,2)
    assert(list22.headNode.get.next==list22.lastNode)



    val list3 = new SortedList[Int]()
    list3.insert(1)
    list3.insert(30)
    list3.insert(30)
    list3.insert(31)

    list3.removeAll(list3.headNode.get.next.get)
    assert(list3.length==2)
    assert(list3.headNode.get.next==list3.lastNode)
    assert(list3(1)==31)

  }



}

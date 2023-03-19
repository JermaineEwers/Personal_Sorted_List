package cse250.pa2
/**
 * cse250.pa2.SortedList
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

import scala.collection.mutable


/**
 * A linked list that stores its elements in sorted order.  
 * 
 * When an element is inserted into the list (or updated), it is positioned 
 * such that <tt>node.next</tt> is the next greater value being stored in
 * the list and <tt>node.prev</tt> is the next lesser value being stored in
 * the list. Duplicate values are stored in a single list node, with the node
 * also holding a count of the number of elements with that value.
 * 
 * SortedList "hinted" variants of several methods, where the caller may
 * provide a reference to a value that is close to the search term in the
 * sorted list.  If this is actually the case, the runtime of these methods,
 * which is normally linear <i>in the size of the list</i> will drop to linear
 * in the number of records between the hint and the search term.
 */
class SortedList[T: Ordering] extends mutable.Seq[T] {
  var headNode: Option[SortedListNode[T]] = None
  var lastNode: Option[SortedListNode[T]] = None
  var length = 0

  /**
   * Compare two values of the sequence type
   *
   * @param a The first element to compare
   * @param b The second element to compare
   * @return <tt>0</tt> if a = b, a negative integer if a < b,
   *         a positive integer if a > b
   *
   *         This function is parameterized by a user-provided Ordering[T]
   *         implementation.  Complexity requirements listed below assume that
   *         this function runs in O(1).
   */
  private def compare(a: T, b: T): Int =
    Ordering[T].compare(a, b)

  /**
   * Find a reference to the element or the element that would precede it
   *
   * @param elem The element to find
   * @return The node containing the greatest element equal to
   *         or below elem.
   *
   *         If the list contains elem, this function should return the list node
   *         containing it.
   *
   *         If the list does not contain elem, this function should return a reference
   *         to the element that would precede it if it were in the list, or None if
   *         elem is lower than the lowest element in the list.
   *
   *         This function should run in O(length)
   */
  def findRefBefore(elem: T): Option[SortedListNode[T]] = {

    /* if(compare(elem,headNode.get.value)<0){
      return None
    }*/

    var t = headNode
    if (compare(elem, t.get.value) < 0) {
      return None
    }
    var len: Int = length
    var num: Int = 0
    var ch = 0
   /* while (t!=None) {
      if (t.get.value == elem) {
        ch = ch + 1
        return t
      }
      num += 1
      t = t.get.next

    }*/

    t = headNode
    num = 0
    if (ch == 0) {
      while (t!=None) {
        if (compare(t.get.value,elem)==0) {
          ch = ch + 1
          return t
        }else if (compare(elem, t.get.value) < 0) {
          return t.get.prev
        } else if (t.get.next == None) {
          return t
        }
        num += 1
        t = t.get.next

      }
    }


    return None


  }

  /**
   * Find a reference to the element or the element that would precede it
   *
   * @param elem The element to find
   * @param hint An element "close" to the element to search for.
   * @return The node containing the greatest element equal to
   *         or below elem.
   *
   *         If the list contains elem, this function should return the list node
   *         containing it.
   *
   *         If the list does not contain elem, this function should return a reference
   *         to the element that would precede it if it were in the list, or None if
   *         elem is lower than the lowest element in the list.
   *
   *         If hint is at position i and elem is at position j, then this function
   *         should run in O( |i-j| )
   */
  def findRefBefore(elem: T, hint: SortedListNode[T]): Option[SortedListNode[T]] = {
    if (compare(elem, headNode.get.value) < 0) {
      return None
    }else if(compare(elem,hint.value)==0){
      return Some(hint)
    }

    if (compare(elem, hint.value) > 0) {
      //forward
      var t: Option[SortedListNode[T]] = Some(hint)
      /*if (hint.prev != None) {
        t = hint.prev.get.next
      } else if (hint.next != None) {
        t = hint.next.get.prev
      }*/
      var len: Int = length
      var num: Int = 0
      var ch = 0

      /*while (t != None) {
        if (t.get.value == elem) {
          ch = ch + 1
          return t
        }
        t = t.get.next
        num += 1
      }*/

    /*  if (hint.prev != None) {
        t = hint.prev.get.next
      } else if (hint.next != None) {
        t = hint.next.get.prev
      }*/
      t=Some(hint)



      if (ch == 0) {
        while (t != None) {
          if (compare(t.get.value,elem)==0) {
           // ch = ch + 1
            return t
          }else if (compare(elem, t.get.value) < 0) {
            return t
          } else if (t.get.next == None) {
            return t
          }
          t = t.get.next
          num += 1
        }
      }

    } else if (compare(elem, hint.value) < 0 && hint.prev != None) {
      //going backwards
      var tt: Option[SortedListNode[T]] = Some(hint)
      var ch2 = 0
      /*if (hint.prev != None) {
        tt = hint.prev.get.next
      } else if (hint.next != None) {
        tt = hint.next.get.prev
      }*/

      /*while (tt != None) {
        if (tt.get.value == elem) {
          ch2 = ch2 + 1
          return tt
        }
        tt = tt.get.prev
      }*/

     /* if (hint.prev != None) {
        tt = hint.prev.get.next
      } else if (hint.next != None) {
        tt = hint.next.get.prev
      }*/
      tt=Some(hint)

      if (ch2 == 0) {
        while (tt != None) {
          if (compare(tt.get.value, elem) == 0) {
            // ch = ch + 1
            return tt
          }else if (compare(elem, tt.get.value) > 0) {
            return tt
          } else if (tt.get.next == None) {
            return tt
          }
          tt = tt.get.prev
          //num += 1
        }
      }
    } else if (compare(elem, hint.value) == 0) {
      var ttt: Option[SortedListNode[T]] = Some(hint)
    /*  if (hint.prev != None) {
        ttt = hint.prev.get.next
      } else if (hint.next != None) {
        ttt = hint.next.get.prev
      }*/
      return ttt
    }


    return None

  }

  /**
   * Find a reference to the specified element
   *
   * @param elem The element to find
   * @param hint An element "close" to the element to search for.
   * @return Some(node) of the node containing elem, or None
   *         if elem is not present in the list
   *
   *         This function should run in O(length)
   */
  def findRef(elem: T): Option[SortedListNode[T]] = {
    var t = headNode
    var len: Int = length
    var num: Int = 0
    var ch = 0
    while (t!=None) {
      if (t.get.value == elem) {
        ch = ch + 1
        return t
      }
      num += 1
      t = t.get.next

    }

    return None
  }

  /**
   * Find a reference to the specified element
   *
   * @param elem The element to find
   * @return Some(node) of the node containing elem, or None
   *         if elem is not present in the list
   *
   *         This function should run in O(length)
   */
  def findRef(elem: T, hint: SortedListNode[T]): Option[SortedListNode[T]] = {

    if(compare(elem,hint.value)==0){
      return Some(hint)
    }

    if (compare(elem, hint.value) > 0) {
      //forward
      var t: Option[SortedListNode[T]] = Some(hint)
     /* if (hint.prev != None) {
        t = hint.prev.get.next
      } else if (hint.next != None) {
        t = hint.next.get.prev
      }*/
      var len: Int = length
      var num: Int = 0
      var ch = 0
      while (t != None) {
        if (t.get.value == elem) {
          ch = ch + 1
          return t
        }
        t = t.get.next
        num += 1
      }

    } else if (compare(elem, hint.value) < 0 && hint.prev != None) {
      //going backwards
      var tt: Option[SortedListNode[T]] = Some(hint)
      var ch2 = 0
     /* if (hint.prev != None) {
        tt = hint.prev.get.next
      } else if (hint.next != None) {
        tt = hint.next.get.prev
      }*/

      while (tt != None) {
        if (tt.get.value == elem) {
          ch2 = ch2 + 1
          return tt
        }
        tt = tt.get.prev
      }

    }

    return None
  }

  /**
   * Return a reference to the element at the specified index
   *
   * @param idx The index to look up
   * @return The node <b>currently</b> at the specified index
   * @throw IndexOutOfBoundsException if idx < 0 or idx >= length
   *
   *        If the list changes, references to nodes who's values are unchanged
   *        should remain valid, even if their index changes.
   *
   *        This function should run in O(idx)
   */
  def getRef(idx: Int): SortedListNode[T] = {
    if (idx < 0 || idx >= length) {
      //"Throws"-->Cite: The Scala API
      throw new IndexOutOfBoundsException() //I figured this out by experimenting with  multiple different things

    }
    var ret: SortedListNode[T] = headNode.get
    var t = headNode



    var len: Int = idx
    var num: Int = 0
    var ch = 0
    /*while (num <= len) {
      if (num == len) {

        ret = t.get
        return t.get
      }
      t = t.get.next
      num += 1
    }*/

    ret
    var tt = this
    var y = Some(tt).value.toList

    return findRef(y(idx)).get
  }

  /**
   * Return the value at the specified index
   *
   * @param idx The index to look up
   * @return The value currently at the specified index
   * @throw IndexOutOfBoundsException if idx < 0 or idx >= length
   *
   *        This function should run in O(idx)
   */
  def apply(idx: Int): T = {
    if (idx < 0 || idx >= length) {
      //"Throws"-->Cite: The Scala API
      throw new IndexOutOfBoundsException() //I figured this out by experimenting with  multiple different things

    }
    var i:Int=0
    var tt=this
    var y=Some(tt).value.toList


  return y(idx)

  }

  /**
   * Insert a new value into the list.
   *
   * @param elem The value to insert
   * @return A reference to the inserted value
   *
   *         The value should be placed so that the list remains in sorted order.
   *         After the insertion, the inserted element's <tt>next</tt> method
   *         should return a reference to the next greatest element, and the
   *         <tt>prev</tt> method should return a reference to the next least
   *         element.
   *
   *         If elem is already in the list, the existing node should just be
   *         updated to indicate another instance of that element has been
   *         inserted.
   *
   *         This function should run in O(length)
   */
  def insert(elem: T): SortedListNode[T] = {
    var nelem = Option[SortedListNode[T]](new SortedListNode[T](elem, 1,None,None))
    //(elem, 0, None, None)


    if (headNode != None) {

      var t = headNode


      var t2 = headNode
      var len: Int = length + 1
      var num: Int = 0
      var num2: Int = 0
      var ch = 0
      var ch2 = 0

      /*while (t2!=None) {
        if (t2.get.value == elem) {
          ch2 += 1
          t2.get.count += 1
          length += 1
          //nelem.get.prev=t2
          return t2.get
        }
        num2 += 1
        //if (t2.get.next != None) {
          t2 = t2.get.next
        //}

      }*/


      /* t2=headNode
       num2=0
       while (num2 < len) {
         if(t2.get.next!=None){
         if (t2.get.value == elem && t2.get.next.get.value!=elem) {
           ch2 += 1
           nelem.get.prev=t2
           nelem.get.next=t2.get.next
           t2.get.next.get.prev=nelem
           t2.get.next=nelem
             length+=1
         }
         }else if(t2.get.next==None){
           if (t2.get.value == elem) {
             ch2 += 1
             nelem.get.prev = t2
             nelem.get.next = None
             t2.get.next = nelem
             length += 1
           }
         }
         num2 += 1
         if (t2.get.next != None) {
           t2 = t2.get.next
         }
       } */

      if (ch2 == 0) {
        while (t!=None) {
          if (compare(elem, t.get.value) < 0 && t.get.prev == None) {
            if (compare(elem, headNode.get.value) < 0) {
              headNode = nelem
            }

            nelem.get.next = t
            nelem.get.prev = None
            t.get.prev = nelem
            //t.get.next=None
            length += 1
            return nelem.get
          } else if (compare(elem, t.get.value) < 0 && t.get.prev != None) {

            nelem.get.next = t
            nelem.get.prev = t.get.prev
            t.get.prev.get.next = nelem
            t.get.prev = nelem
            //t.get.next = None
            length += 1
            return nelem.get
          } else if (compare(elem, t.get.value) > 0 && t.get.next == None) {
            if (t.get.next != None) {
              t = t.get.next
            }
            nelem.get.prev = t
            t.get.next = nelem
            nelem.get.next = None
            lastNode = nelem
            length += 1
            return nelem.get

          } else if (compare(t.get.value, elem) == 0) {
            t.get.count += 1
            length += 1
            return t.get
          } /*else if (compare(elem, t.get.value) > 0 && t.get.next!=None){
          nelem.get.prev=t
          t.get.next.get.prev=nelem
          nelem.get.next=t.get.next
          t.get.next=nelem
         // lastNode = nelem
          length += 1
          return nelem.get
        }*/
          t = t.get.next
          num += 1
        }
      }


      /*if (ch2 != 0) {
        while (num <= len) {
          if (t.get.next.get.next.get.next != lastNode) {
            if (t.get.next.get.value == elem && t.get.next.get.next.get.value != elem) {
              ch += 1
              var pp = t.get.next.get.next
              nelem.get.prev = pp.get.prev
              nelem.get.next = pp
              t.get.next = nelem
              pp.get.prev = nelem
              length+=1
              return nelem.get
            }
          }
          t = t.get.next
          num += 1
        }
      }*/
    } else {
      length += 1;
      headNode=nelem
      lastNode=nelem
    }

    return nelem.get

  }




  /**
   * Insert a new value into the list.
   *
   * @param elem The value to insert
   * @param hint An element "close" to the position where the
   *             element is to be inserted.
   * @return A reference to the inserted value
   *
   * The value should be placed so that the list remains in sorted order.
   * After the insertion, the inserted element's <tt>next</tt> method
   * should return a reference to the next greatest element, and the
   * <tt>prev</tt> method should return a reference to the next least
   * element.
   *
   * If elem is already in the list, the existing node should just be
   * updated to indicate another instance of that element has been
   * inserted.
   *
   * If hint is at position i and elem should be inserted at position j,
   * then this function should run in O( |i-j| )
   */
  def insert(elem: T, hint: SortedListNode[T]): SortedListNode[T] = {
    var nelem = Option[SortedListNode[T]](new SortedListNode[T](elem, 1, None, None))
    //(elem, 0, None, None)



    if (headNode != None) {
      if(compare(elem,hint.value)==0){
        hint.count+=1
        length += 1
        return hint
      }

     if(compare(elem, hint.value) > 0) {
       var t: Option[SortedListNode[T]] = Some(hint)

      /* if (hint.prev != None) {
         t = hint.prev.get.next
       } else if (hint.next != None) {
         t = hint.next.get.prev
       }else if(hint.prev == None){
         t=headNode
       }else if(hint.next==None){
         t=lastNode
       }*/


       var t2: Option[SortedListNode[T]] =  Some(hint)
      /* if (hint.prev != None) {
         t2 = hint.prev.get.next
       } else if (hint.next != None) {
         t2 = hint.next.get.prev
       }*/
       var len: Int = length + 1
       var num: Int = 0
       var num2: Int = 0
       var ch = 0
       var ch2 = 0

      /* while (t2 != None) {
         if (t2.get.value == elem) {
           ch2 += 1
           t2.get.count += 1
           length += 1
           //nelem.get.prev=t2
           return t2.get
         }
         num2 += 1

           t2 = t2.get.next

       }*/

       if (ch2 == 0) {

         while (t != None) {
           if(compare(t.get.value,elem)==0){
             t.get.count+=1
             length += 1
             return t.get
           } else if (compare(elem, t.get.value) < 0 && t.get.prev == None) {
             if (compare(elem, headNode.get.value) < 0) {
               headNode = nelem
             }
             nelem.get.next = t
             nelem.get.prev = None
             t.get.prev = nelem
             //t.get.next=None
             length += 1
             return nelem.get
           } else if (compare(elem, t.get.value) < 0 && t.get.prev != None) {

             nelem.get.next = t
             nelem.get.prev = t.get.prev
             t.get.prev.get.next = nelem
             t.get.prev = nelem
             //t.get.next = None
             length += 1
             return nelem.get
           } else if (compare(elem, t.get.value) > 0 && t.get.next == None) {
             if (t.get.next != None) {
               t = t.get.next
             }
             nelem.get.prev = t
             t.get.next = nelem
             nelem.get.next = None
             lastNode = nelem
             length += 1
             return nelem.get
           }
           t = t.get.next
           num += 1
         }
       }
     } else if (compare(elem, hint.value) < 0 && hint.prev != None) {
        //backwards
        var t: Option[SortedListNode[T]] = Some(hint)
       /* if (hint.prev != None) {
          t = hint.prev.get.next
        } else if (hint.next != None) {
          t = hint.next.get.prev
        }*/


        var t2: Option[SortedListNode[T]] =Some(hint)
        /*if (hint.prev != None) {
          t2 = hint.prev.get.next
        } else if (hint.next != None) {
          t2 = hint.next.get.prev
        }*/
        var len: Int = length + 1
        var num: Int = 0
        var num2: Int = 0
        var ch = 0
        var ch2 = 0

        /*while (t2 != None) {
          if (t2.get.value == elem) {
            ch2 += 1
            t2.get.count += 1
            length += 1
            //nelem.get.prev=t2
            return t2.get
          }
          num2 += 1

          t2 = t2.get.prev

        }*/

        if (ch2 == 0) {
          while (t != None) {
            if(t.get.prev!=None){
            if(compare(elem, t.get.value) < 0 && compare(elem, t.get.prev.get.value) > 0){
              nelem.get.next = t
              nelem.get.prev = t.get.prev
              t.get.prev.get.next = nelem
              t.get.prev = nelem
              //t.get.next = None
              length += 1
              return nelem.get
            }}
            else if (compare(elem, t.get.value) < 0 && t.get.prev == None) {
              if (compare(elem, headNode.get.value) < 0) {
                headNode = nelem
              }
              nelem.get.next = t
              nelem.get.prev = None
              t.get.prev = nelem
              //t.get.next=None
              length += 1
              return nelem.get
            }else if (compare(t.get.value, elem) == 0) {
              t.get.count += 1
              length += 1
              return t.get
            }
            /*else if (compare(elem, t.get.value) < 0 && t.get.prev != None) {

              nelem.get.next = t
              nelem.get.prev = t.get.prev
              t.get.prev.get.next = nelem
              t.get.prev = nelem
              //t.get.next = None
              length += 1
              return nelem.get
            } else if (compare(elem, t.get.value) > 0 && t.get.next == None) {
              if (t.get.next != None) {
                t = t.get.next
              }
              nelem.get.prev = t
              t.get.next = nelem
              nelem.get.next = None
              lastNode = nelem
              length += 1
              return nelem.get
            }*/
            t = t.get.prev
            num += 1
          }
        }

      }
    } else {
        headNode = nelem
      lastNode=nelem
        length += 1
      }


      return nelem.get


  }
  /**
   * Remove one instance of the value of the referenced node from the list
   *
   * @param ref The node holding the value to remove (must be part of the list)
   * @return The removed value
   *
   * If the node contains multiple instances of the value, the node itself
   * should remain in the list, as we are only removing a single instance of
   * that value.
   *
   * This function should run in O(1)
   */
  def remove(ref: SortedListNode[T]): T =
  {
    val rem:Int = 1

    var tt = this
    var y = Some(tt).value.toList


    //findRefBefore(ref.value,ref).get.count=findRefBefore(ref.value,ref).get.count -1
    ref.count=ref.count-1
    var ttt = headNode.get.next
    var last = lastNode.get.prev
    val value=ref.value
   // val px=findRefBefore(ref.value,ref)
    val px=Some(ref)
   // val count=findRefBefore(ref.value,ref).get.count
    val count=ref.count
    if(count==0){
      if (ref.next != None && ref.prev != None) {
        ref.next.get.prev = ref.prev
        ref.prev.get.next = ref.next
        px.get.prev = None
        px.get.next = None

        // findRef(ref.value).get.next=None
        //findRef(ref.value).get.prev=None
        length = length - rem
        return value
      } else if (ref.next == None && ref.prev != None) {
        //lastNode=findRef(ref.value).get.prev
        val yu = ref
        ref.prev.get.next = None
        yu.prev = None
        //findRef(ref.value).get.prev=None

        //findRef(ref.value).get.prev=None
        lastNode = last
        length = length - rem
        return value
      } else if (ref.next != None && ref.prev == None) {
        //headNode=findRef(ref.value).get.next
        val yu = ref
        ref.next.get.prev = None
        yu.next = None
        //findRef(ref.value).get.next=None
        headNode = ttt
        length = length - rem
        return value
      } else if (ref.next == None && ref.prev == None) {
        headNode = None
        lastNode = None
        length = length - rem
        return value
      }


    }

   /* if(count==0){
    if(findRefBefore(ref.value,ref).get.next!=None  && findRefBefore(ref.value,ref).get.prev!=None ){
      findRefBefore(ref.value,ref).get.next.get.prev=findRefBefore(ref.value,ref).get.prev
      findRefBefore(ref.value,ref).get.prev.get.next=findRefBefore(ref.value,ref).get.next
      px.get.prev = None
      px.get.next = None

     // findRef(ref.value).get.next=None
      //findRef(ref.value).get.prev=None
      length=length-rem
      return value
    }else if(findRefBefore(ref.value,ref).get.next==None  && findRefBefore(ref.value,ref).get.prev!=None ){
      //lastNode=findRef(ref.value).get.prev
      val yu=findRefBefore(ref.value,ref)
      findRefBefore(ref.value,ref).get.prev.get.next=None
      yu.get.prev=None
      //findRef(ref.value).get.prev=None

      //findRef(ref.value).get.prev=None
      lastNode=last
      length=length-rem
      return value
    }else if(findRefBefore(ref.value,ref).get.next!=None  && findRefBefore(ref.value,ref).get.prev==None ){
      //headNode=findRef(ref.value).get.next
      val yu=findRefBefore(ref.value,ref)
      findRefBefore(ref.value,ref).get.next.get.prev=None
      yu.get.next=None
      //findRef(ref.value).get.next=None
      headNode=ttt
      length=length-rem
      return value
    }else if(findRefBefore(ref.value,ref).get.next==None  && findRefBefore(ref.value,ref).get.prev==None){
      headNode=None
      lastNode=None
      length=length-rem
      return value
    }

    }*/
    length=length-rem
    return value


  }

  /**
   * Remove n instances of the value of the referenced node from the list
   *
   * @param ref The node holding the value to remove (must be part of the list)
   * @param n   The number of times the value should be removed
   * @return The removed value
   * @throw IllegalArgumentException if n > ref.count
   *
   * If the node contains more than n instances of the value, the node itself
   * should remain in the list, as we are only removing n instances of
   * that value.
   *
   * This function should run in O(1)
   */
  def removeN(ref: SortedListNode[T], n: Int): T =
  {
    val rem:Int = n
    if (n>ref.count) {
      //"Throws"-->Cite: The Scala API
      throw new IllegalArgumentException //I figured this out by experimenting with multiple different things

    }




val value=ref.value
     var ttt=headNode.get.next
    var last=lastNode.get.prev
    val px=Some(ref)
    ref.count = ref.count - n

val count= ref.count
    if (count == 0) {
      if (ref.next != None && ref.prev != None) {
        ref.next.get.prev = ref.prev
        ref.prev.get.next = ref.next
        px.get.prev = None
        px.get.next = None

        // findRef(ref.value).get.next=None
        //findRef(ref.value).get.prev=None
        length = length - rem
        return value
      } else if (ref.next == None && ref.prev != None) {
        //lastNode=findRef(ref.value).get.prev
        val yu = ref
        ref.prev.get.next = None
        yu.prev = None
        //findRef(ref.value).get.prev=None

        //findRef(ref.value).get.prev=None
        lastNode = last
        length = length - rem
        return value
      } else if (ref.next != None && ref.prev == None) {
        //headNode=findRef(ref.value).get.next
        val yu = ref
        ref.next.get.prev = None
        yu.next = None
        //findRef(ref.value).get.next=None
        headNode = ttt
        length = length - rem
        return value
      } else if (ref.next == None && ref.prev == None) {
        headNode = None
        lastNode = None
        length = length - rem
        return value
      }


    }





    /*if (findRefBefore(ref.value,ref).get.count == 0) {
      if (findRefBefore(ref.value,ref).get.next != None &&findRefBefore(ref.value,ref).get.prev != None) {
        findRefBefore(ref.value,ref).get.next.get.prev = findRefBefore(ref.value,ref).get.prev
        findRefBefore(ref.value,ref).get.prev.get.next = findRefBefore(ref.value,ref).get.next
        px.get.prev = None
        px.get.next = None
        //findRef(ref.value).get.next.get.prev = findRef(ref.value).get.prev
        //findRef(ref.value).get.next = None
        //findRef(ref.value).get.prev = None
        length=length-rem
        return ref.value
      } else if (findRefBefore(ref.value,ref).get.next == None && findRefBefore(ref.value,ref).get.prev != None) {
        //lastNode = findRef(ref.value).get.prev
        val yu=findRefBefore(ref.value,ref)
        findRefBefore(ref.value,ref).get.prev.get.next = None
yu.get.prev=None
        //findRef(ref.value).get.prev = None
        length=length-rem
        lastNode = last
        return ref.value
      } else if (findRefBefore(ref.value,ref).get.next != None && findRefBefore(ref.value,ref).get.prev == None) {
        //headNode = findRef(ref.value).get.next
        val yu=findRefBefore(ref.value,ref)
        findRefBefore(ref.value,ref).get.next.get.prev = None
yu.get.next=None
       // findRef(ref.value).get.next = None
        length=length-rem
        headNode = ttt
        return ref.value
      } else if (findRefBefore(ref.value,ref).get.next == None && findRefBefore(ref.value,ref).get.prev == None) {
        headNode = None
        lastNode=None
        length=length-rem
        return ref.value
      }

    }*/
    length=length-rem
    return ref.value


  }

  /**
   * Remove all instances of the value of the referenced node from the list
   *
   * @param ref The node to remove (must be part of the list)
   * @return The value of the removed node
   *
   * This function should run in O(1)
   */
  def removeAll(ref: SortedListNode[T]): T =
  {
    val rem:Int = ref.count
    ref.count = 0
    var ttt = headNode.get.next
    var last = lastNode.get.prev
    val px=Some(ref)
    val count= ref.count
    val value =ref.value

    if (count == 0) {
      if (ref.next != None && ref.prev != None) {
        ref.next.get.prev = ref.prev
        ref.prev.get.next = ref.next
        px.get.prev = None
        px.get.next = None

        // findRef(ref.value).get.next=None
        //findRef(ref.value).get.prev=None
        length = length - rem
        return value
      } else if (ref.next == None && ref.prev != None) {
        //lastNode=findRef(ref.value).get.prev
        val yu = ref
        ref.prev.get.next = None
        yu.prev = None
        //findRef(ref.value).get.prev=None

        //findRef(ref.value).get.prev=None
        lastNode = last
        length = length - rem
        return value
      } else if (ref.next != None && ref.prev == None) {
        //headNode=findRef(ref.value).get.next
        val yu = ref
        ref.next.get.prev = None
        yu.next = None
        //findRef(ref.value).get.next=None
        headNode = ttt
        length = length - rem
        return value
      } else if (ref.next == None && ref.prev == None) {
        headNode = None
        lastNode = None
        length = length - rem
        return value
      }


    }





 /*
    if (findRefBefore(ref.value,ref).get.count == 0) {
      if (findRefBefore(ref.value,ref).get.next != None && findRefBefore(ref.value,ref).get.prev != None) {
        findRefBefore(ref.value,ref).get.next.get.prev = findRefBefore(ref.value,ref).get.prev
        findRefBefore(ref.value,ref).get.prev.get.next = findRefBefore(ref.value,ref).get.next
        px.get.prev=None
        px.get.next=None
          length=length-rem
        //findRef(ref.value).get.next = None
        //findRef(ref.value).get.prev = None
        return ref.value
      } else if (findRefBefore(ref.value,ref).get.next == None && findRefBefore(ref.value,ref).get.prev != None) {
        val yu=findRefBefore(ref.value,ref)
        findRefBefore(ref.value,ref).get.prev.get.next = None
       yu.get.prev=None
       // findRef(ref.value).get.prev = None
        lastNode = last
        length=length-rem
        return ref.value
      } else if (findRefBefore(ref.value,ref).get.next != None && findRefBefore(ref.value,ref).get.prev == None) {
        val yu=findRefBefore(ref.value,ref)
        findRefBefore(ref.value,ref).get.next.get.prev = None
        yu.get.next=None
        //findRef(ref.value).get.next = None
        headNode = ttt
        length=length-rem
        return ref.value
      } else if (findRefBefore(ref.value,ref).get.next == None && findRefBefore(ref.value,ref).get.prev == None) {
        headNode = None
        lastNode=None
        length=length-rem
        return ref.value
      }

    }*/
    length=length-rem
    return ref.value
  }

  /**
   * Modify a value presently in the list
   *
   * @param ref  A reference to the node to be updated
   * @param elem The value to update the node
   * @return A reference to the updated node
   *
   * If i is the position of ref before the update and j is the position
   * of ref after the update, then this function should run in O( |i-j| )
   */
  def update(ref: SortedListNode[T], elem: T): SortedListNode[T] = {
    val ret = insert(elem, ref)
    remove(ref)
    return ret
  }

  /**
   * Modify a value presently in the list
   *
   * @param idx The index of the value
   *
   * This function should run in O(length)
   */
  def update(idx: Int, elem: T): Unit = {
    update(getRef(idx), elem)
  }

  /**
   * Return an iterator over the elements of the collection.
   *
   * @return An iterator over the elements of the collection
   *
   * The iterator should return elements in sorted order from least to
   * greatest (according to the [[compare]] method in this class).
   *
   * The iterator's <tt>next</tt> and <tt>hasNext<tt> methods should both
   * run in O(1).
   */
  def iterator: Iterator[T] = {
    return new Iterator[T] {
      var current = headNode
      var count = 0

      def hasNext: Boolean = current.isDefined

      def next(): T = {
        val ret = current.get
        count += 1
        if (count >= ret.count) {
          current = ret.next
          count = 0
        }
        return ret.value
      }
    }
  }

  /**
   * Return the last element of the list
   * @return               The last element of the list
   * 
   * This function should run in O(1)
   */
  override def last = lastNode.get.value
}
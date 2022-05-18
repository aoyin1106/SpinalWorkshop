package workshop.prime

import spinal.core._
import spinal.lib._


object Prime{
  //Pure scala function which return true when the number is prime
  def apply(n : Int) =  ! ((2 until n-1) exists (n % _ == 0))

  //Should return True when the number is prime.
  def apply(n : UInt) : Bool = {

    val valueRange = 0 until (1 << widthOf(n))
    val primeList = valueRange.filter(i => Prime(i))
    val primeHits = primeList.map(i => i === n)
    val hit = primeHits.orR
    return hit
  }
}



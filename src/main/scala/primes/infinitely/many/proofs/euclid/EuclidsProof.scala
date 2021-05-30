package primes.infinitely.many.proofs.euclid

import primes.util.Primes


/*
https://www.math.utah.edu/~pa/math/q2.html
*
* */
object EuclidsProof {

  def main(args: Array[String]): Unit = {

    List.range(3L, 14L).foreach(l =>{

      val numbers = euclidsInfinitelyManyPrimes(l)
      val number = numbers.head
      val result = if(Primes.isPrime(number)) s"$number is prime" else number
      println((numbers.foldLeft(List[Long]())((a,b) => b :: a)).mkString(",")+" ="+result)
    })
  }

  def productPlusOne(finiteListOfPrimes: List[Long], numToAdd: Long = 1): Long
                            = (finiteListOfPrimes.foldLeft(1L)(_ * _)) + numToAdd

  def euclidsInfinitelyManyPrimes(limit: Long): List[Long] = {

    val finiteListOfPrimes = Primes.generatePrimes(limit-2 )
    productPlusOne(finiteListOfPrimes):: finiteListOfPrimes
  }
}

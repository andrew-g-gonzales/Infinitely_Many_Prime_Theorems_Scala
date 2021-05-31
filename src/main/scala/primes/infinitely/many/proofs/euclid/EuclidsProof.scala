package primes.infinitely.many.proofs.euclid

import primes.util.Primes

import scala.annotation.tailrec


/*
https://www.math.utah.edu/~pa/math/q2.html

https://www-users.cs.york.ac.uk/susan/cyc/p/primeprf.htm
*
* */
object EuclidsProof {

  def main(args: Array[String]): Unit = {

    proof(List[Long](2L),13)
  }

  def printProof(numbers:List[Long] )= {
    val number  = (numbers.foldLeft(1L)(_ * _)) + 1
    val result = Primes.isPrime(number) match {
      case true => s"$number, is prime"
      case false =>{
        val tuple = divisibleByGreaterPrime(numbers.head, number)
        s"$number = ${tuple._1}*${tuple._2}"
      }
    }

    println((numbers.foldLeft(List[Long]())((a,b) => b :: a)).mkString(",")+"+1 = "+result)
  }

  def proof(list:List[Long], limit:Long):Unit = {

    limit match  {
      case x if x < 1 =>{
        printProof(list)
      }
      case _ =>{
        printProof(list)
        proof(addNextPrime(list), limit-1)
      }
    }
  }

  def addNextPrime(list:List[Long]):List[Long]=Primes.findNextPrime(list.head+1)::list

  @tailrec
  def divisibleByGreaterPrime(p:Long, number:Long):(Long,Long) = {

    val next = Primes.findNextPrime(p+1)
    (number % p ==0) match {
      case true => (p, number/p)
      case false => divisibleByGreaterPrime(next,number)
    }
  }


}

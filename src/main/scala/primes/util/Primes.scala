package primes.util

object Primes {

    def isPrime(long: Long): Boolean =
      if (long < 4L)
        long > 1L
      else if (((long % 2L) == 0L) || ((long % 3L) == 0L))
        false
      else
        (5L to math.sqrt(long).toLong by 6L).forall(n => ((long % n) != 0L) && ((long % (n + 2L)) != 0L))

    def finiteListOfPrimes(number:Long): List[Long] = List.range(2L, number+1L) filter isPrime

    def generatePrimes(limit:Long):List[Long] = {

      def build(acc:List[Long], limit:Long):List[Long]={

          limit match {
              case x if x < 1 => acc
              case _=> build(Primes.findNextPrime(acc.head+1) :: acc  ,limit-1)
          }
      }

        build(List(2L),limit)
    }

    def findNextPrime(n: Long) : Long = {
        def iterate(m: Long) : Long ={
            if(isPrime(m)) m
            else iterate(m +1)
        }
        iterate(n)
    }

}

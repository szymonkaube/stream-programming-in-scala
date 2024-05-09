/*
This is an implementation of HyperLogLog algorithm.
It uses MurmurHash3 hash function for strings which is the only data type it is viable for.
It can be easily made viable for other data types by changing the hash function.
The list of MurmurHash3 functions for different data types is available here:
https://www.scala-lang.org/api/current/scala/util/hashing/MurmurHash3$.html
*/

import scala.util.hashing.MurmurHash3
import scala.math.log, scala.math.pow, scala.math.exp


/**
* Estimates the distinct cardinality of a multiset using the HyperLogLog algorithm.
*
* @param multiset  The input array of strings representing the multiset.
* @param b         The number of bits to use for buckets (affects accuracy vs. memory tradeoff).
*                  Default value is 12.
* @return          An estimated double value representing the cardinality of the multiset.
*/
def hyperLogLog(multiset: Array[String], b: Int=12): Double = {
    val nOfBuckets = 1 << b      // The number of buckets is 2^b
    val buckets: Array[Int] = Array.fill(nOfBuckets)(0)
    // Set alpha which will be needed later for small/large range correction of the estimate
    val alpha: Double = b match {
        case 4 => 0.673
        case 5 => 0.697
        case 6 => 0.709
        case _ => 0.7213 / (1 + 1.079 / nOfBuckets)
    }

    multiset.foreach(element => {
        val elementHashed: Int = MurmurHash3.stringHash(element)
        // Set bucketIndex to the number corresponding to first b bits of the hash
        val bucketIndex: Int = (elementHashed >>> (32 - b))
        val restBits: Int = elementHashed << b
        val positionOfFirstOne: Int = Integer.numberOfLeadingZeros(restBits) + 1
        // Compare the position of the first one in the rest of the bits with the value in the bucket
        if (positionOfFirstOne > buckets(bucketIndex)) buckets(bucketIndex) = positionOfFirstOne
    }) 
    // An estimate before possible corrections
    val estimate: Double = alpha * pow(nOfBuckets, 2) / (buckets.map(M => pow(2, -M)).sum)
    var estimateImproved: Double = estimate
    val twoToPower32: Long = 1L << 32
    // Small range correction
    if (estimate <= 5 * nOfBuckets / 2) {
        val nOfEmptyBuckets: Double = buckets.filter(_ == 0).size
        if (nOfEmptyBuckets != 0) estimateImproved = nOfBuckets * log(nOfBuckets / nOfEmptyBuckets)
    }
    // Large range correction
    else if (estimate > twoToPower32 / 30) estimateImproved = -twoToPower32 * log(1 - estimate / twoToPower32.toDouble)

    estimateImproved
}

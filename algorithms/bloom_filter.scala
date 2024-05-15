/*
This is an implementation of Bloom filter algorithm.
It uses MurmurHash3 hash function for strings which is the only data type it is viable for.
It can be easily made viable for other data types by changing the hash function.
The list of MurmurHash3 functions for different data types is available here:
https://www.scala-lang.org/api/current/scala/util/hashing/MurmurHash3$.html
*/

import scala.collection.mutable.Buffer
import scala.util.hashing.MurmurHash3
import scala.math.log

/**
* BloomFilter class implements a probabilistic data structure used for membership queries.
* @param baseSet The initial set of strings used to build the Bloom filter.
* @param epsilon The acceptable false positive probability. It should be between 0 and 1.
*/
class BloomFilter(baseSet: Set[String], epsilon: Float) {
    require((0 <= epsilon) && (epsilon <= 1))
    // Estimating the parameters which give false-positive error epsilon
    private val n: Int = baseSet.size
    private val k: Int = (-log(epsilon) / log(2)).toInt
    private val m: Int = (-n * log(epsilon) / (log(2) * log(2))).toInt
    
    // Creating the Bloom filter array
    val bloomFilter: Array[Boolean] = Array.fill(m)(false)
    // Building the Bloom filter from the base set
    baseSet.foreach(element => {
        val hashes: List[Int] = familyHash(element, k)
        val indices: List[Int] = hashes.map(hash => hash % m)
        indices.foreach(index => bloomFilter(index) = true)
    })

    /**
    * Queries the Bloom filter to check for membership of elements in the query set.
    * @param querySet The set of strings to be queried against the Bloom filter.
    * @return A buffer containing elements from the query set that are likely to be in the base set.
    */
    def queryBloomFilter(querySet: Set[String]): Buffer[String] = {
        var output = Buffer[String]()
        querySet.foreach(element => {
            // Get hashes of the element
            val hashes: List[Int] = familyHash(element, k)
            // Get indices in the filter to check
            val indices: List[Int] = hashes.map(hash => hash % m)
            // Check the indices in the filter
            val isInBaseSet: Boolean = indices.forall(index => bloomFilter(index))
            if (isInBaseSet) output += element
        })
        output
    }

    /**
    * Generates a list of numHashFunctions hash values for a given string.
    * @param stringToHash The string to be hashed.
    * @param numHashFunctions The number of hash functions to generate.
    * @return A list of numHashFunctions hash values.
    */
    private def familyHash(stringToHash: String, numHashFunctions: Int): List[Int] = {
        // Returns List(h_1(s), ..., h_numHashFunctions(s)), where h_i(s) is a 16-bit int
        val hashes: Int = MurmurHash3.stringHash(stringToHash)
        val (high, low) = (hashes >>> 16, hashes & ((1 << 16) - 1))
        List.tabulate(numHashFunctions)(j => (high + j * low) & ((1 << 16) - 1))
    }
}
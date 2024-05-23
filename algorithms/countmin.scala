/*
This is an implementation of Count-min algorithm.
It uses MurmurHash3 hash function for strings which is the only data type it is viable for.
It can be easily made viable for other data types by changing the hash function.
The list of MurmurHash3 functions for different data types is available here:
https://www.scala-lang.org/api/current/scala/util/hashing/MurmurHash3$.html
*/

import scala.util.hashing.MurmurHash3
import scala.collection.mutable

/**
 * Class implementing the Count-Min Sketch data structure for approximate frequency estimation.
 * 
 * @constructor Create a new CountMin instance.
 * @param arrayToCount Array of strings for which the frequencies need to be counted.
 * @param bucketExponent The exponent used to determine the number of buckets (numBuckets = 2^bucketExponent).
 * @param numHashes The number of hash functions used in the Count-Min Sketch.
 */
class CountMin(arrayToCount: Array[String], bucketExponent: Int, numHashes: Int) {
    require((arrayToCount.length > 0) && (bucketExponent > 0) && (numHashes > 0))
    
    private val numBuckets: Int = 1 << bucketExponent   // numBuckets is 2^bucketExponent
    // Initializing the 2D array that will store frequencies
    val hashGrid: Array[Array[Int]] = Array.ofDim(numHashes, numBuckets)
    // Creating the count-min grid
    arrayToCount.foreach(element => {
        val hashes: List[Int] = familyHash(element, numHashes)
        val columns: List[Int] = hashes.map(hash => hash % numBuckets)
        val rows = 0 to (numHashes - 1)
        val hashGridPositions: IndexedSeq[(Int, Int)] = rows.zip(columns)
        hashGridPositions.foreach((row, column) => hashGrid(row)(column) += 1)
    })
    
    /**
     * Query the Count-Min Sketch for the estimated frequencies of a set of elements.
     * 
     * @param elemsToCount Set of strings for which the frequencies are to be queried.
     * @param hashGrid 2D array representing the count-min sketch grid.
     * @return A map of strings to their estimated frequencies.
     */
    def queryCountMin(elemsToCount: Set[String], hashGrid: Array[Array[Int]]) = {
        // Recover the number of hashes and the number of buckets from the structure of hashGrid
        val numHashes: Int = hashGrid.size
        val numBuckets: Int = hashGrid(0).size
        // Initializing the map to store words and their estimated frequencies
        val elementFreqs = mutable.Map.empty[String, Int]
        elemsToCount.foreach(element => {
            // Finding the positions of the word in the grid
            val hashes: List[Int] = familyHash(element, numHashes)
            val columns: List[Int] = hashes.map(hash => hash % numBuckets)
            val rows = 0 to (numHashes - 1)
            val hashGridPositions: IndexedSeq[(Int, Int)] = rows.zip(columns)
            // Reading the frequencies in the found positions
            val freqs: IndexedSeq[Int] = hashGridPositions.map((row, column) => hashGrid(row)(column))
            // Assigning the minimal frequency among the fitting positions
            elementFreqs(element) = freqs.min
        })
        elementFreqs
    }
    
    /**
    * Generates a list of numHashFunctions hash values for a given string.
     * @param stringToHash The string to be hashed.
     * @param numHashFunctions The number of hash functions to generate.
     * @return A list of numHashFunctions hash values.
     */
    private def familyHash(s: String, k: Int): List[Int] = {
        // Returns List(h_1(s), ..., h_k(s)), where h_i(s) is a 16-bit int
        val hs: Int = MurmurHash3.stringHash(s)
        val (high, low) = (hs >>> 16, hs & ((1 << 16) - 1))
        List.tabulate(k)(j => (high + j * low) & ((1 << 16) - 1))
    }
}
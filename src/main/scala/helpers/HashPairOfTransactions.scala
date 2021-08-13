package helpers

import helpers.SHA256Hash.sha256Hash

import scala.collection.mutable.ArrayBuffer

/**
 * Hashes a pair of transactions and returns the hashed string value
 */
object HashPairOfTransactions {

  /**
   * Hashes a pair of transaction hashes, returning the resultant hash value
   *
   * If we pass two transactions in the txPair parameter, it will hash them as sha256(tx1+tx2)
   * If we pass one transactions, it directly returns that transaction (does not rehash it)
   *
   * @param txPair An ArrayBuffer[String] of the transactions to hash
   * @return The String value of the two transactions when hashed together
   */
  def hashPairOfTransactions(txPair: ArrayBuffer[String]): String = {
    if(txPair.length==1) {
      return txPair(0)
    }
    val a:String = txPair(0)
    val b:String = txPair(1)
    val hashedValue:String = sha256Hash(a+b)
    hashedValue
  }

}

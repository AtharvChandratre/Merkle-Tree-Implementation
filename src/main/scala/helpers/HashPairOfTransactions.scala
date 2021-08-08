package helpers

import helpers.SHA256Hash.sha256Hash

import scala.collection.mutable.ArrayBuffer

object HashPairOfTransactions {

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

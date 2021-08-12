package helpers

import helpers.HashPairOfTransactions.hashPairOfTransactions

import scala.collection.mutable.ArrayBuffer

class HashPairOfTransactionsTest extends org.scalatest.flatspec.AnyFlatSpec {

  "The Hash Pair function" should "hash two transactions in the order they are given in" in {
    assert(hashPairOfTransactions(ArrayBuffer("ca978112ca1bbdcafac231b39a23dc4da786eff8147c4e72b9807785afee48bb",
      "3e23e8160039594a33894f6564e1b1348bbd7a0088d42c4acb73eeaed59c009d"))
      .equals("62af5c3cb8da3e4f25061e829ebeea5c7513c54949115b1acc225930a90154da"))
    assert(hashPairOfTransactions(ArrayBuffer("3e23e8160039594a33894f6564e1b1348bbd7a0088d42c4acb73eeaed59c009d",
      "ca978112ca1bbdcafac231b39a23dc4da786eff8147c4e72b9807785afee48bb"))
      .equals("ab19ec537f09499b26f0f62eed7aefad46ab9f498e06a7328ce8e8ef90da6d86"))
  }

  it should "return the first element if only one element has been passed to the function" in {
    assert(hashPairOfTransactions(ArrayBuffer("a"))
      .equals("a"))
  }

}

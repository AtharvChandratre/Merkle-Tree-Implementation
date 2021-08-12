import helpers.MerkleProof
import org.scalatest.BeforeAndAfterEach
import org.scalatest.flatspec.AnyFlatSpec

import scala.collection.mutable.ArrayBuffer

class MerkleTreeTest extends AnyFlatSpec with BeforeAndAfterEach {

  behavior of "The Merkle Tree"

  it should "return the correct merkle proof for a given transaction which is in the list of transactions" in {
    val merkleTreeTestObj = new MerkleTree()

    val testTransactionsList = new ArrayBuffer[String]()
    testTransactionsList.append("a")
    testTransactionsList.append("b")
    testTransactionsList.append("c")
    testTransactionsList.append("d")
    merkleTreeTestObj.addBulkTransactions(testTransactionsList)

    val testTxID:Int = 1

    val merkleProofTest:MerkleProof = merkleTreeTestObj.findMerkleProof(testTxID)

    val merkleProofToAssert:MerkleProof = new MerkleProof()
    merkleProofToAssert.position = testTxID
    merkleProofToAssert.nodes.append("ca978112ca1bbdcafac231b39a23dc4da786eff8147c4e72b9807785afee48bb")
    merkleProofToAssert.nodes.append("d3a0f1c792ccf7f1708d5422696263e35755a86917ea76ef9242bd4a8cf4891a")

    assert(merkleProofTest.nodes == merkleProofToAssert.nodes)
    assert(merkleProofTest.position == merkleProofToAssert.position)

  }

  it should "return a blank merkle proof with position=txID for a transaction which is not in the list of transactions" in {
    val merkleTreeTestObj = new MerkleTree()

    val testTransactionsList = new ArrayBuffer[String]()
    testTransactionsList.append("a")
    testTransactionsList.append("b")
    testTransactionsList.append("c")
    testTransactionsList.append("d")
    merkleTreeTestObj.addBulkTransactions(testTransactionsList)

    val testTxID:Int = 5

    val merkleProofTest:MerkleProof = merkleTreeTestObj.findMerkleProof(testTxID)

    val merkleProofToAssert:MerkleProof = new MerkleProof()
    merkleProofToAssert.position = testTxID

    assert(merkleProofTest.nodes == merkleProofToAssert.nodes)
    assert(merkleProofTest.position == merkleProofToAssert.position)
  }

  it should "return a blank merkle proof with position=-1 if there are no transactions at all" in {
    val merkleTreeTestObj = new MerkleTree()
    val testTxID:Int = 3

    val merkleProofTest:MerkleProof = merkleTreeTestObj.findMerkleProof(testTxID)

    val merkleProofToAssert:MerkleProof = new MerkleProof()
    merkleProofToAssert.position = -1

    assert(merkleProofTest.nodes == merkleProofToAssert.nodes)
    assert(merkleProofTest.position == merkleProofToAssert.position)
  }

  it should "get the Merkle Root Correctly" in {
    val merkleTreeTestObj = new MerkleTree()
    val testTransactionsList = new ArrayBuffer[String]()
    testTransactionsList.append("a")
    testTransactionsList.append("b")
    testTransactionsList.append("c")
    testTransactionsList.append("d")
    assert(merkleTreeTestObj.getMerkleRoot(testTransactionsList)
      .equals("12a40550c10c6339bf6f271445270e49b844d6c9e8abc36b9b642be532befe94"))
  }

  it should "return true when the merkle proof provided for a transaction is valid" in {
    val merkleTreeTestObj = new MerkleTree()
    val testTransactionsList = new ArrayBuffer[String]()
    testTransactionsList.append("a")
    testTransactionsList.append("b")
    testTransactionsList.append("c")
    testTransactionsList.append("d")
    merkleTreeTestObj.addBulkTransactions(testTransactionsList)

    val testTxID:Int = 1
    val merkleProofToAssert:MerkleProof = new MerkleProof()
    merkleProofToAssert.position = testTxID
    merkleProofToAssert.nodes.append("ca978112ca1bbdcafac231b39a23dc4da786eff8147c4e72b9807785afee48bb")
    merkleProofToAssert.nodes.append("d3a0f1c792ccf7f1708d5422696263e35755a86917ea76ef9242bd4a8cf4891a")

    val merkleRoot:String = "58c89d709329eb37285837b042ab6ff72c7c8f74de0446b091b6a0131c102cfd"

    assert(merkleTreeTestObj.verifyMerkleProof(merkleProofToAssert,merkleRoot))
  }

  it should "return false when the merkle proof provided for a transaction is invalid" in {
    val merkleTreeTestObj = new MerkleTree()
    val testTransactionsList = new ArrayBuffer[String]()
    testTransactionsList.append("a")
    testTransactionsList.append("b")
    testTransactionsList.append("c")
    testTransactionsList.append("d")
    merkleTreeTestObj.addBulkTransactions(testTransactionsList)

    val testTxID:Int = 1
    val merkleProofToAssert1:MerkleProof = new MerkleProof()
    merkleProofToAssert1.position = testTxID
    merkleProofToAssert1.nodes.append("ca978112ca1bbdcafac231b39a23dc4da786eff8147c4e72b9807785afee48bb")
    merkleProofToAssert1.nodes.append("d3a0f1c792ccf7f1708d5422696263e35755a86917ea76ef9242bd4a8cf4891a")
    val merkleRoot1:String = "a"

    val merkleProofToAssert2:MerkleProof = new MerkleProof()
    merkleProofToAssert2.position = testTxID
    merkleProofToAssert2.nodes.append("a")
    merkleProofToAssert2.nodes.append("d3a0f1c792ccf7f1708d5422696263e35755a86917ea76ef9242bd4a8cf4891a")
    val merkleRoot2:String = "58c89d709329eb37285837b042ab6ff72c7c8f74de0446b091b6a0131c102cfd"

    assert(!merkleTreeTestObj.verifyMerkleProof(merkleProofToAssert1, merkleRoot1))
    assert(!merkleTreeTestObj.verifyMerkleProof(merkleProofToAssert2, merkleRoot2))
  }

  it should "add a new transaction correctly to the list of transactions and transaction hashes" in {
    val merkleTreeTestObj = new MerkleTree()
    merkleTreeTestObj.addTransaction("d")
    merkleTreeTestObj.addTransaction("b")
    merkleTreeTestObj.addTransaction("a")
    merkleTreeTestObj.addTransaction("c")

    val hashOfTx:String = "ca978112ca1bbdcafac231b39a23dc4da786eff8147c4e72b9807785afee48bb"

    assert(merkleTreeTestObj.getTransactionsList.contains("a"))
    assert(merkleTreeTestObj.getTransactionsList.indexOf("a")==2)
    assert(merkleTreeTestObj.getTransactionsHashes.contains(hashOfTx))
    assert(merkleTreeTestObj.getTransactionsHashes.indexOf(hashOfTx)==2)
  }

  it should "get Transaction ID of an added transaction correctly" in {
    val merkleTreeTestObj = new MerkleTree()

    merkleTreeTestObj.addTransaction("d")
    merkleTreeTestObj.addTransaction("b")
    merkleTreeTestObj.addTransaction("a")
    merkleTreeTestObj.addTransaction("c")

    assert(merkleTreeTestObj.getTransactionID("a")==2)
    assert(merkleTreeTestObj.getTransactionID("b")==1)
    assert(merkleTreeTestObj.getTransactionID("c")==3)
    assert(merkleTreeTestObj.getTransactionID("d")==0)
  }

  it should "add multiple transactions in bulk correctly" in {
    val merkleTreeTestObj = new MerkleTree()
    val testTransactionsList = new ArrayBuffer[String]()

    testTransactionsList.append("a")
    testTransactionsList.append("b")
    testTransactionsList.append("c")
    testTransactionsList.append("d")

    merkleTreeTestObj.addBulkTransactions(testTransactionsList)

    assert(merkleTreeTestObj.getTransactionID("a")==0)
    assert(merkleTreeTestObj.getTransactionID("b")==1)
    assert(merkleTreeTestObj.getTransactionID("c")==2)
    assert(merkleTreeTestObj.getTransactionID("d")==3)
  }

}

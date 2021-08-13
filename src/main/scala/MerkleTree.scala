import helpers.HashPairOfTransactions.hashPairOfTransactions
import helpers.MerkleProof
import helpers.SHA256Hash.sha256Hash

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer
import scala.io.StdIn.readLine

/**
 * Contains the functionality of a merkle tree.
 * User can add transactions to the Merkle Tree,
 * and get attributes like the merkle root, merkle proof for a transaction and verify the merkle proof.
 */
class MerkleTree {

  private val transactionsList = new ArrayBuffer[String]()
  private val transactionsHashes = new ArrayBuffer[String]()
  private[this] var _merkleProof: MerkleProof = new MerkleProof()

  private def merkleProof: MerkleProof = _merkleProof

  private def merkleProof_=(value: MerkleProof): Unit = {
    _merkleProof = value
  }

  /**
   * Adds a new transaction to the the ArrayBuffer of transactions.
   * Also hashes the input transaction using SHA256 and adds it to the ArrayBuffer of transaction hashes.
   *
   * @param tx Transaction to be added to the list of transactions
   *
   */
  def addTransaction(tx: String): Unit = {
    transactionsList.append(tx)
    transactionsHashes.append(sha256Hash(tx))
  }

  /**
   * Gets the ArrayBuffer of transactions.
   *
   * @return The ArrayBuffer of Transactions
   */
  def getTransactionsList:ArrayBuffer[String] = {
    transactionsList
  }

  /**
   * Gets the ArrayBuffer of transaction hashes.
   *
   * @return The ArrayBuffer of transaction hashes
   */
  def getTransactionsHashes:ArrayBuffer[String] = {
    transactionsHashes
  }

  /**
   * Gets the transaction ID from the list of transactions using the transaction details.
   *
   * @param tx The transaction details for which transaction ID is to be found
   * @return Transaction ID of the transaction
   */
  def getTransactionID(tx:String): Int = {
    transactionsList.indexOf(tx)
  }

  /**
   * Adds multiple transactions in bulk to the ArrayBuffer of transactions.
   * Also adds each transaction's SHA256 hash value to the ArrayBuffer of transaction hashes.
   *
   * @param transactions ArrayBuffer of transactions to be added
   */
  def addBulkTransactions(transactions:ArrayBuffer[String]): Unit = {
    transactions.foreach(tx => {
      this.addTransaction(tx)
    })
  }

  /**
   * Prints out all the transactions in the ArrayBuffer of transactions to the console
   */
  def listTransactions(): Unit = {
    println("_____________________________________________________")
    println("\n*** List All Transactions ***\n")
    transactionsList.foreach(println)
  }

  /**
   * Computes the Merkle Root of a given ArrayBuffer of transaction hashes.
   *
   * @param txHashList Optional parameter to take the list of transaction hashes to use for computing the Merkle Root.
   *                   If not given, will use the transaction hashes of the object being used to invoke this function.
   * @return The Merkle Root of the given transaction hashes.
   */
  @tailrec
  final def getMerkleRoot(txHashList: ArrayBuffer[String] = this.transactionsHashes): String = {
    if(txHashList.isEmpty) "No transactions to compute Merkle Tree for"
    else if (txHashList.length == 1)
      txHashList(0)
    else {
      val tree = new ArrayBuffer[String]()
      txHashList.grouped(2).foreach(x => tree.append(hashPairOfTransactions(x)))
      getMerkleRoot(tree)
    }
  }

  /**
   * Computes the Merkle Proof for a given transaction ID.
   *
   * @param txID ID of the transaction to compute the Merkle Root for.
   * @param txHashList Optional parameter to take the list of transaction hashes to use for computing the Merkle Proof.
   *                   If not given, will use the transaction hashes of the object being used to invoke this function.
   * @return The Merkle Proof of the given transaction.
   */
  final def findMerkleProof(txID: Int,
                            txHashList: ArrayBuffer[String] = this.transactionsHashes): MerkleProof = {

    val proof = new MerkleProof()
    proof.position = txID
    if (txHashList.isEmpty) {
      proof.position = -1
      return proof
    }
    else if(txID>=transactionsHashes.length) {
      return proof
    }
    val tx:String = transactionsHashes(txID)
    merkleProof = calculateMerkleProof(tx,proof,txHashList)
    merkleProof
  }

  /**
   * Internal function to recursively calculate the Merkle Proof
   *
   * @param tx Hashed value of the transaction to calculate the Merkle Proof for
   * @param proof Merkle Proof being computed
   * @param txHashList Optional parameter to take the list of transaction hashes to use for computing the Merkle Proof.
   *                   If not given, will use the transaction hashes of the object being used to invoke this function.
   * @return The Merkle Proof of the given transaction.
   */
  @tailrec
  private def calculateMerkleProof (tx: String,
                                  proof: MerkleProof,
                                  txHashList: ArrayBuffer[String] = this.transactionsHashes) : MerkleProof= {
    var newTx: String = tx
    val tree = new ArrayBuffer[String]()

    if (txHashList.length == 1) {
      if (proof.nodes.isEmpty) {
        proof.position = -1
      }
      return proof
    }
    else {
      txHashList.grouped(2).foreach(x => {
        val hash: String = hashPairOfTransactions(x)

        if (x.contains(tx)) {
          var side: Int = 0
          if (tx.equals(x(0))) {
            side = 1
          }
          proof.nodes.append(x(side))
          newTx = hash
        }
        tree.append(hash)
      })
    }
    calculateMerkleProof(newTx, proof, tree)
  }


  /**
   * Verifies a given Merkle Proof.
   *
   * @param proof The Merkle Proof to be verified.
   * @param merkleRoot The Merkle Root used to compare to the calculated merkle root by this function.
   * @param txHashList Optional parameter to give the list of transaction hashes to use for Merkle Proof verification.
   *                   If not given, it will use the transaction hashes of the object being used to invoke this function.
   * @return A boolean denoting if the merkle proof is valid (true) or not (false).
   */
  def verifyMerkleProof(proof: MerkleProof,
                        merkleRoot: String,
                        txHashList: ArrayBuffer[String] = this.transactionsHashes): Boolean = {
    var pos: Int = proof.position
    var tempHash:String = txHashList(pos)
    proof.nodes.foreach( node => {
      if(pos%2==0)
        tempHash = hashPairOfTransactions(ArrayBuffer(tempHash,node))
      else
        tempHash = hashPairOfTransactions(ArrayBuffer(node,tempHash))
      pos/=2
    })
    if (tempHash.equals(merkleRoot)) true
    else false
  }
}

/**
 * Companion object of the MerkleTree class.
 * Provides a console I/O to interact with a MerkleTree object.
 */
object MerkleTree {
  def main(args: Array[String]): Unit = {

    val merkleTreeObj = new MerkleTree()
    var merkleProofObj = new MerkleProof()

    do {
      println("_____________________________________________________")
      println("1. Add a transaction to the merkle tree")
      println("2. List transactions")
      println("3. Find merkle root")
      println("4. Check if the transaction is present")
      println("5. Exit")
      val userInput = readLine("Enter your choice: ")
      userInput match {
        case "1" =>
          val tx = readLine("Enter the transaction: ")
          merkleTreeObj.addTransaction(tx)
        case "2" =>
          merkleTreeObj.listTransactions()
        case "3" =>
          println("Merkle Root is : " + merkleTreeObj.getMerkleRoot())
        case "4" =>
          try {
            val txID:Int = readLine("Enter the transaction ID: ").toInt
            merkleProofObj = merkleTreeObj.findMerkleProof(txID)
            println("Verifying merkle proof:")
            val merkleRoot: String = merkleTreeObj.getMerkleRoot()
            println(merkleTreeObj.verifyMerkleProof(merkleTreeObj.merkleProof, merkleRoot))
          }catch {
            case e: NumberFormatException =>
              println("Enter a valid numerical input")
          }
        case "5" => return;
        case _ => println("Please enter a valid input.");
      }
    } while (true)
  }
}

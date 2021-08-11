import helpers.HashPairOfTransactions.hashPairOfTransactions
import helpers.MerkleProof
import helpers.SHA256Hash.sha256Hash

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer
import scala.io.StdIn.readLine

class MerkleTree {

  private val transactionsList = new ArrayBuffer[String]()
  private val transactionsHashes = new ArrayBuffer[String]()
  private[this] var _merkleProof: MerkleProof = new MerkleProof()

  private def merkleProof: MerkleProof = _merkleProof

  private def merkleProof_=(value: MerkleProof): Unit = {
    _merkleProof = value
  }

  def addTransaction(tx: String): Unit = {
    transactionsList.append(tx)
    transactionsHashes.append(sha256Hash(tx))
  }

  def getTransactionIDOfTransaction (tx:String): Int = {
    transactionsList.indexOf(tx)
  }

  def addBulkTransactions(transactions:ArrayBuffer[String]): Unit = {
    transactions.foreach(tx => {
      this.addTransaction(tx)
    })
  }

  def listTransactions(): Unit = {
    println("_____________________________________________________")
    println("\n*** List All Transactions ***\n")
    transactionsList.foreach(println)
  }

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

  final def findMerkleProof(txID: Int,
                            txHashList: ArrayBuffer[String] = this.transactionsHashes): MerkleProof = {

    val proof = new MerkleProof()
    proof.position = txID
    if(txID>=transactionsHashes.length) {
      println("Enter a tx ID less than the number of transactions")
      return new MerkleProof()
    }
    val tx:String = transactionsHashes(txID)
    merkleProof = calculateMerkleProof(tx,proof,txHashList)
    merkleProof
  }

  @tailrec
  private def calculateMerkleProof (tx: String,
                                  proof: MerkleProof,
                                  txHashList: ArrayBuffer[String] = this.transactionsHashes) : MerkleProof= {
    var newTx: String = tx
    val tree = new ArrayBuffer[String]()

    if (txHashList.isEmpty) {
      println("There are no transactions to generate the proofs for")
      return proof
    }
    else if (txHashList.length == 1) {
      if (proof.nodes.isEmpty) {
        println("The transaction does not exist within this list of transactions")
      }
      else {
        println("Merkle Proof found")
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

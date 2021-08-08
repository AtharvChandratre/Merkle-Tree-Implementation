import helpers.HashPairOfTransactions.hashPairOfTransactions
import helpers.SHA256Hash.sha256Hash

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer
import scala.io.StdIn.readLine

object MerkleTreeExperiment {

  def listTransactions (txList: ArrayBuffer[String]): Unit = {
    println("_____________________________________________________")
    println("\n*** List All Transactions ***\n")
    txList.foreach(println)
  }

  @tailrec
  def computeMerkleRoot (txHashList: ArrayBuffer[String]): String = {
    if(txHashList.isEmpty) "No transactions to compute Merkle Tree for"
    else if (txHashList.length == 1)
      txHashList(0)
    else {
      val tree = new ArrayBuffer[String]()
      txHashList.grouped(2).foreach(x => tree.append(hashPairOfTransactions(x)))
      computeMerkleRoot(tree)
    }
  }

  def checkIfTransactionIsPresent (tx:String): Unit = {

  }

  def main(args: Array[String]): Unit = {

    val transactionsList = new ArrayBuffer[String]()
    val transactionsHashes = new ArrayBuffer[String]()
    var merkleTree = new ArrayBuffer[String]()

    do{
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
          transactionsList.append(tx)
          transactionsHashes.append(sha256Hash(tx))
        case "2" =>
          listTransactions(transactionsList)
        case "3" =>
          println("Merkle Root is : "+computeMerkleRoot(transactionsHashes))
        case "4" =>
//
        case "5" => return;
        case _ => println("Please enter a valid input.");
      }
    }while(true)
  }
}

import scala.collection.mutable.ArrayBuffer
import scala.io.StdIn.readLine

object merkleTree {

  var transactionsList = new ArrayBuffer[String]()

  def sha256Hash(text: String) : String = {
    String.format("%064x",
      new java.math.BigInteger(1,
        java.security.MessageDigest.getInstance("SHA-256")
          .digest(text.getBytes("UTF-8"))))
  }


  def addTransaction(): Unit = {
    println("_____________________________________________________")
    println("\n*** Add Transaction ***\n")
    val inputTransaction: String = readLine("Enter the transaction to add to the Merkle Tree: ")
    transactionsList.append(inputTransaction)
    println("Transaction added")
  }

  def listTransactions(): Unit = {
    println("_____________________________________________________")
    println("\n*** List Transactions ***\n")
    for(i <- transactionsList.indices) {
      println((i+1).toString+". "+transactionsList(i))
    }
  }

  def hashTransactions(prevTransactionsList: ArrayBuffer[String]): ArrayBuffer[String] = {
    val tempTransactionsList = new ArrayBuffer[String]()
    var i:Int = 0
    while(i<(prevTransactionsList.size-prevTransactionsList.size%2)) {
      val left: String = prevTransactionsList(i)
      val right: String = prevTransactionsList(i + 1)
      val hashedValue: String = sha256Hash(left + right)
      tempTransactionsList.append(hashedValue)
      i+=2
    }
    if(prevTransactionsList.size%2==1){
      tempTransactionsList.append(prevTransactionsList(prevTransactionsList.size-1))
    }
    return tempTransactionsList
  }

  def findMerkleRoot(): Unit = {
    println("_____________________________________________________")
    println("\n*** Find Merkle Tree Root ***\n")

    if(transactionsList.isEmpty) {
      println("No transactions yet. Add transactions to generate a merkle root")
      return
    }

    var tempTransactionsList = new ArrayBuffer[String]()
    for(transaction <- transactionsList) {
      tempTransactionsList.append(sha256Hash(transaction))
    }

    do {
      tempTransactionsList = hashTransactions(tempTransactionsList)
    } while(tempTransactionsList.size!=1)

    println("Merkle Tree Root: "+tempTransactionsList(0))
  }

  def checkIfTransactionIsPresentInMerkleTree(): Unit = {
    println("_____________________________________________________")
    println("\n*** Check if a transaction is present in a merkle tree ***\n")
  }

  def main(args: Array[String]) {
    do{
      println("_____________________________________________________")
      println("1. Add a transaction to the merkle tree")
      println("2. List transactions")
      println("3. Find merkle root")
      println("4. Check if the transaction is present")
      println("5. Exit")
      val userInput = readLine("Enter your choice: ")
      userInput match {
        case "1" => addTransaction();
        case "2" => listTransactions();
        case "3" => findMerkleRoot();
        case "4" => checkIfTransactionIsPresentInMerkleTree();
        case "5" => return;
        case _ => println("Please enter a valid input.");
      }
    }while(true)
  }
}
package helpers

import scala.collection.mutable.ArrayBuffer

/**
 * Class to represent a Merkle Proof
 * Contains an Int position to represent the position of the transaction in the list of transactions
 * Contains an ArrayBuffer of hashes, used to compute the Merkle Root during verification of the proof
 */
class MerkleProof {

  private[this] var _position: Int = -1

  def position: Int = _position

  def position_=(value: Int): Unit = {
    _position = value
  }

  private[this] var _nodes: ArrayBuffer[String] = new ArrayBuffer[String]()

  def nodes: ArrayBuffer[String] = _nodes

  def nodes_=(value: ArrayBuffer[String]): Unit = {
    _nodes = value
  }

}

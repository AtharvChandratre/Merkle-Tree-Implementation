package helpers

import scala.collection.mutable.ArrayBuffer

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

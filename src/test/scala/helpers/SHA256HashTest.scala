package helpers

import helpers.SHA256Hash.sha256Hash

/**
 *
 */
class SHA256HashTest extends org.scalatest.flatspec.AnyFlatSpec {
    "The Hash Function" should "return the correct hash when given a valid input" in {
      val valueToHash: String = "a"
      val hashedValue:String = sha256Hash(valueToHash)
      assert(hashedValue.equals("ca978112ca1bbdcafac231b39a23dc4da786eff8147c4e72b9807785afee48bb"))
    }
}

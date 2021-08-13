package helpers

/**
 * Performs a SHA256 Hash on the input string
 */
object SHA256Hash {

  /**
   * Performs a SHA256 hash on the input and returns the hashed string value
   * Returns sha256(text)
   *
   * @param text String to be hashed
   * @return Hashed string result of the input value
   */
  def sha256Hash(text: String): String = {
    String.format("%064x",
      new java.math.BigInteger(1,
        java.security.MessageDigest.getInstance("SHA-256")
          .digest(text.getBytes("UTF-8"))))
  }
}

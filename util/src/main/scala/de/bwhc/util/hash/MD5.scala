package de.bwhc.util.hash



object MD5 extends (String => String)
{

  // taken from: https://alvinalexander.com/source-code/scala-method-create-md5-hash-of-string/
  def apply(s: String): String = {
    import java.security.MessageDigest
    import java.math.BigInteger
    val md = MessageDigest.getInstance("MD5")
    val digest = md.digest(s.getBytes)
    val bigInt = new BigInteger(1,digest)
    val hash = bigInt.toString(16)
    hash
  }

}

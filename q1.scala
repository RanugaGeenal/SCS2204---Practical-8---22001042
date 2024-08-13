object q1{

  def encrypt(text: String, shift: Int): String = {
    text.map { char =>
      if (char.isLetter) {
        val base = if (char.isUpper) 'A' else 'a'
        ((char - base + shift) % 26 + base).toChar
      } else {
        char
      }
    }
  }

  def decrypt(text: String, shift: Int): String = {
    encrypt(text, 26 - (shift % 26))
  }

  def cipher(text: String, shift: Int, cipherFunc: (String, Int) => String): String = {
    cipherFunc(text, shift)
  }

  def main(args: Array[String]): Unit = {
    val text = "My name is Ranuga"
    val shift = 3

    val encryptedText = cipher(text, shift, encrypt)
    println(s"Encrypted: $encryptedText")

    val decryptedText = cipher(encryptedText, shift, decrypt)
    println(s"Decrypted: $decryptedText")
  }
}

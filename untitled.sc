import java.security.MessageDigest

def sha(string: String): String = {
  val sha = MessageDigest.getInstance("SHA-256")
  sha.update(string.getBytes("UTF-8"))
  String.format("%064x", new java.math.BigInteger(1, sha.digest()))
}

def replacementForSingle(string: String) = {
  println(s"${sha(string.replaceAll("\\bR\\b", "~"))} -> ${sha(string)}")
}

def replacementForBoth(name: String, definition: String) = {
  replacementForSingle(s"premise \u2227 relation R A $definition\n$name R A")
  replacementForSingle(s"premise $name R A\n\u2227 relation R A $definition")
}

replacementForBoth("reflexiveRelation", "\u2200 \u2192 \u2208 $0 A \u2208 orderedPair $0 $0 R")
replacementForBoth("irreflexiveRelation", "\u2200 \u2192 \u2208 $0 A \u00ac \u2208 orderedPair $0 $0 R")
replacementForBoth("symmetricRelation", "\u2200 \u2192 \u2208 $0 A \u2200 \u2192 \u2208 $0 A \u2192 \u2208 orderedPair $$0 $0 R \u2208 orderedPair $0 $$0 R")
replacementForBoth("asymmetricRelation", "\u2200 \u2192 \u2208 $0 A \u2200 \u2192 \u2208 $0 A \u2192 \u2208 orderedPair $$0 $0 R \u00ac \u2208 orderedPair $0 $$0 R")
replacementForBoth("antisymmetricRelation", "\u2200 \u2192 \u2208 $0 A \u2200 \u2192 \u2208 $0 A \u2192 \u2227 \u2208 orderedPair $$0 $0 R \u2208 orderedPair $0 $$0 R = $$0 $0")
replacementForBoth("transitiveRelation", "\u2200 \u2192 \u2208 $0 A \u2200 \u2192 \u2208 $0 A \u2200 \u2192 \u2208 $0 A \u2192 \u2227 \u2208 orderedPair $$$0 $$0 R \u2208 orderedPair $$0 $0 R \u2208 orderedPair $$$0 $0 R")
replacementForBoth("completeRelation", "\u2200 \u2192 \u2208 $0 A \u2200 \u2192 \u2208 $0 A \u2228 \u2208 orderedPair $$0 $0 R \u2208 orderedPair $0 $$0 R")
replacementForBoth("semicompleteRelation", "\u2200 \u2192 \u2208 $0 A \u2200 \u2192 \u2208 $0 A \u2228 \u2208 orderedPair $$0 $0 R \u2228 = $$0 $0 \u2208 orderedPair $0 $$0 R")



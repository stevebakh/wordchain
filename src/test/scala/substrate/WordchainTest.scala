package substrate

import org.scalatest.{FunSpec, Matchers}

class WordchainTest extends FunSpec with Matchers {

  private val dict = new Dictionary {
    override val words: Set[String] = Set("cold", "cord", "card", "ward", "warm")
  }

  describe("word chain game") {
    it("only accepts words to chain of the same length") {
      an [IllegalArgumentException] should be thrownBy new Wordchain(dict).chain("cat", "castle")
    }

    describe("only accepts valid words contained in the dictionary") {
      val validWords = new Dictionary {
        override val words: Set[String] = Set("cat", "dog")
      }

      it("throws an exception when the from word is not in the dictionary") {
        an [InvalidWordException] should be thrownBy new Wordchain(validWords).chain("cit", "dog")
      }

      it("throws an exception when the to word is not in the dictionary") {
        an [InvalidWordException] should be thrownBy new Wordchain(validWords).chain("cat", "dag")
      }
    }

    it("when the from and to words are the same, return just the one word") {
      val result = new Wordchain(dict).chain("cold", "cold")
      result.get should contain only "cold"
    }

    it("when given two words with only 1 letter difference, returns the two words") {
      val from = "cord"
      val to = "card"
      val result = new Wordchain(dict).chain(from, to)
      result.get should contain inOrderOnly (from, to)
    }

    it("returns nothing if no chain can be found between words") {
      val dictionary = new Dictionary {
        override val words: Set[String] = Set("cold", "warm")
      }
      val wordchain = new Wordchain(dictionary)
      val result = wordchain.chain("cold", "warm")
      result shouldBe None
    }

    describe("finds the shortest possible chain between two words") {
      it("the chain should have a length of 3 for 'cold' to 'card'") {
        val dictionary = new Dictionary {
          override val words: Set[String] = Set("cold", "cord", "word", "ward", "card")
        }
        val wordchain = new Wordchain(dictionary)
        val result = wordchain.chain("cold", "card")
        result.get should have size 3
        result.get should contain inOrderOnly("cold", "cord", "card")
      }

      it("the chain should have a length of 5 for 'cold' to 'warm'") {
        val dictionary = new Dictionary {
          override val words: Set[String] = Set(
            "cold", "sold", "bold", "fold", "cord", "word", "corn", "card",
            "lard", "cart", "ward", "wart", "worm", "warn", "warm")
        }
        val wordchain = new Wordchain(dictionary)
        val result = wordchain.chain("cold", "warm")
        result.get should have size 5
        result.get should contain inOrderOnly("cold", "cord", "card", "ward", "warm")
      }

      it("the chain should have a length of 6 for 'ape' to 'man'") {
        val dictionary = new Dictionary {
          override val words: Set[String] = Set(
            "ape", "paw", "nap", "pya", "zap", "pad", "bad", "tab",
            "opt", "tau", "tap", "pan", "man", "oat", "mat", "apt")
        }
        val wordchain = new Wordchain(dictionary)
        val result = wordchain.chain("ape", "man")
        result.get should have size 6
        result.get should contain inOrderOnly("ape", "apt", "opt", "oat", "mat", "man")
      }
    }
  }
}

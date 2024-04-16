import zio.*
import zio.test.*
import zio.test.Assertion.*

import java.io.File

object BookerSpec extends ZIOSpecDefault:
  def nope(s: String): TestResult =
    assertTrue(parseChapter(File(s)).isEmpty)

  def yuup(s: String, i: Int): TestResult =
    val f = File(s)
    assertTrue(parseChapter(f).get == i -> f)

  def spec = suite("BookerSpec") (
    suite("parseChapter") (
      test("must start with a num") {
        nope("fooo.md") &&
        nope("_foo.md") &&
        nope("0foo.md") &&
        yuup("0_foooo.md", 0) &&
        yuup("01_fooo.md", 1) &&
        yuup("12_foo.md", 12)
      },
      test("must end with .md") {
        nope("1_foooomd") &&
        nope("1_foo.txt") &&
        yuup("1_fooo.md", 1)
      },
    ),
    suite("duplicates") (
      test("must work") {
        val in = Set(1 -> File("1_foo.md"), 1 -> File("1_bar.md"), 2 -> File("2_baz.md"))
        val out = Set(1 -> File("1_foo.md"), 1 -> File("1_bar.md"))
        assertTrue(duplicates(in) == out)
      }
    ),
    suite("resolveDups") (
      test("must work") {
        val dups = Set(1 -> File("1_foo.md"), 1 -> File("1_bar.md"))
        for
          _ <- TestConsole.feedLines("1")
          _ <- resolveDups(dups)
          output <- TestConsole.output
        yield
          assertTrue(output(0) == "Conflict detected:\n")
      }
    )
  )

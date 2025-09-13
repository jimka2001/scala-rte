package adjuvant

// This code was created by ChatGPT https://chatgpt.com/share/68c524f9-890c-800d-888b-ef4c17532e0c
// as an attempt to mimick the behavior of the following Clojure code function.
// https://github.com/jimka2001/clojure-rte/blob/741ac7f3e354c4ee92291f9fa60d760b98a91382/src/util.clj#L757
//
object FileLock {
  import java.nio.file._
  import java.util.UUID
  import scala.util.control.NonFatal

  def callInBlock[A](lockFileName: String)(f: => A): A = {
    val lockFile = Paths.get(lockFileName)
    val lockKey  = UUID.randomUUID().toString
    val ms       = 500L

    // Safety check: symbolic link targets must not contain '/'
    require(!lockKey.contains("/"), s"lockKey must not contain '/': $lockKey")

    def tryCreateSymlink(): Unit = {
      try {
        Files.createSymbolicLink(lockFile, Paths.get(lockKey))
      } catch {
        case _: FileAlreadyExistsException =>
          () // lock already held by someone else
        case NonFatal(e) =>
          throw e
      }
    }

    def readSymlink(): Option[String] = {
      try {
        Some(Files.readSymbolicLink(lockFile).toString.trim)
      } catch {
        case _: NoSuchFileException => None
        case NonFatal(_)            => None
      }
    }

    def loop(): A = {
      readSymlink() match {
        case Some(content) if content == lockKey =>
          try {
            f
          } finally {
            try Files.deleteIfExists(lockFile)
            catch { case NonFatal(_) => () }
          }

        case other =>
          println(s"retry $lockFileName, got [${other.getOrElse("")}], expecting [$lockKey]")
          Thread.sleep(ms)
          tryCreateSymlink()
          loop()
      }
    }

    tryCreateSymlink()
    loop()
  }
}

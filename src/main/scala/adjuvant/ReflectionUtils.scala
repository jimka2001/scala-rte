package adjuvant
import io.github.classgraph.{ClassGraph, ScanResult}
import scala.collection.mutable
import scala.jdk.CollectionConverters._

// https://chatgpt.com/share/68cc0926-5900-800d-a2ae-c5d831889a5f
object ReflectionUtils {

  // Global cached ClassGraph scan result.
  // Scanning the classpath is expensive, so we only do this once.
  // Restrict to your packages to reduce memory and improve performance.
  private lazy val globalScanResult: ScanResult =
    new ClassGraph()
      .enableClassInfo()
      //.acceptPackages("genus", "your.domain") // <-- strongly recommended!
      .scan()

  // Cache for subclass lookups
  private val subclassCache = mutable.Map[Class[_], Seq[Class[_]]]()

  /**
   * Compute all subclasses (or implementing classes, if `sup` is an interface)
   * of a given superclass/interface by scanning the classpath.
   *
   * Implementation details:
   * - Uses ClassGraph to perform a one-time scan of the classpath (`globalScanResult`).
   *   This avoids rescanning on each call, which is expensive and can exhaust memory.
   * - Results are further cached per-superclass in `subclassCache`.
   * - If `sup` is a trait/interface, returns all implementors and sub-interfaces.
   * - If `sup` is a concrete class, returns all subclasses.
   *
   * Limitations:
   * - This finds all subclasses available **on the classpath**, not just the ones
   *   already loaded into the JVM.
   * - If you don’t restrict the scan with `.acceptPackages(...)`, ClassGraph will
   *   scan the *entire* classpath (JDK + all dependencies), which can be slow and
   *   memory-intensive. For most projects, you should limit it to your own packages.
   *
   * @param sup The superclass or interface to search for subclasses.
   * @return A sequence of discovered subclass Class[_] objects.
   */
  def computeSubclassesOf(sup: Class[_]): Seq[Class[_]] =
    subclassCache.getOrElseUpdate(sup, {
      val className = sup.getName
      val classInfos =
        if (sup.isInterface) {
          val implementors = globalScanResult.getClassesImplementing(className).asScala
          val extendingTraits = globalScanResult.getAllInterfaces.asScala.filter { iface =>
            iface.getInterfaces.asScala.exists(_.getName == className)
          }
          (implementors ++ extendingTraits).distinct
        } else {
          globalScanResult.getSubclasses(className).asScala
        }

      classInfos.toSeq.map(_.loadClass())
    })
}

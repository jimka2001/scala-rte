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
  private val subclassCache = mutable.Map[Class[?], Seq[Class[?]]]()

  /**
   * Compute all subclasses (or implementing classes, if `sup` is an interface)
   * of a given superclass/interface by scanning the classpath.
   *
   * Implementation details:
   * - Uses ClassGraph to perform a one-time scan of the classpath (`globalScanResult`).
   *   This avoids rescanning on each call, which is expensive and can exhaust memory.
   * - Results are cached per-superclass in `subclassCache`.
   * - If `sup` is a trait/interface, returns all implementors and sub-interfaces.
   * - If `sup` is a concrete class, returns all subclasses.
   * - All discovered classes are reloaded using the same ClassLoader as `sup`.
   *   This is critical: on the JVM, class identity depends on both name and
   *   classloader. Without this normalization, equality checks (e.g. `contains`)
   *   may fail even for classes with the same name.
   *
   * Limitations:
   * - Finds subclasses available on the classpath, not just those already loaded.
   * - If `.acceptPackages(...)` is not used, the entire classpath (including JDK
   *   and all dependencies) is scanned, which can be slow and memory-intensive.
   * - Some discovered classes may not be loadable by `sup`'s classloader; these
   *   are skipped.
   *
   * @param sup The superclass or interface to search for subclasses.
   * @return A sequence of discovered subclass Class[_] objects.
   */
  def computeSubclassesOf(sup: Class[?]): Seq[Class[?]] =
    subclassCache.getOrElseUpdate(sup, {
      val className = sup.getName

      val classInfos =
        if (sup.isInterface) {
          val implementors =
            globalScanResult.getClassesImplementing(className).asScala

          val extendingTraits =
            globalScanResult.getAllInterfaces.asScala.filter { iface =>
              iface.getInterfaces.asScala.exists(_.getName == className)
            }

          implementors ++ extendingTraits
        } else {
          globalScanResult.getSubclasses(className).asScala
        }

      // 🔑 CRITICAL: use the same classloader that loaded `sup`
      val loader =
        Option(sup.getClassLoader)
          .getOrElse(ClassLoader.getSystemClassLoader)

      // Normalize all classes into that loader
      classInfos.toSeq.flatMap { ci =>
        val name = ci.getName
        try {
          Some(Class.forName(name, false, loader))
        } catch {
          case _: ClassNotFoundException => None
        }
      }
    })
}

package au.edu.imb.ppii.network.annotation

/**
 * Base class to filter GO annotations according to their evidence codes
 */
abstract class EvidenceFilter {
  // Note that evidence codes come with with/from info, e.g. info IEA:UniProtKB-SubCell
  def apply(evidence:String):Boolean
  /** extracts the three letter evidence code */
  protected def code(evidence:String) = evidence.split(':')(0)
}

/**
 * Base class for evidence filters that provide a set of evidence codes to accept.
 */
abstract class EvidenceFilterSet extends EvidenceFilter {
  protected val accept:Set[String] // set of three letter uppercase! evidence codes to accept
  def apply(evidence:String) = accept.contains(code(evidence))
}

/** Accepts all evidence codes */
object EvidenceFilterAll extends EvidenceFilter {
  def apply(evidence:String) = true
}

class EvidenceFilterExclude(ec:String) extends EvidenceFilter {
  def apply(evidence:String)  = code(evidence) != ec
}

/** Accepts only experimental evidence */
object EvidenceFilterExperimental extends EvidenceFilterSet {
  val accept = Set("EXP","IDA","IPI","IMP","IGI","IEP")
}

/** Accepts only computational evidence */
object EvidenceFilterComputational extends EvidenceFilterSet {
  val accept = Set("ISS","ISO","ISA","ISM","IGC","RCA")
}

/** Accepts only evidence that has not been inferred by curator */
object EvidenceFilterNotInferred extends EvidenceFilter {
  def apply(evidence:String) = code(evidence) != "IC"
}

/** Accepts only curated evidence */
object EvidenceFilterCurated extends EvidenceFilter {
  def apply(evidence:String) = code(evidence) != "IEA"
}

/** Accepts only uncurated, electronically inferred evidence */
object EvidenceFilterIEA extends EvidenceFilter {
  def apply(evidence:String) = code(evidence) == "IEA"
}

/** Excludes all annotation based on physical/genetic interactions */
object EvidenceFilterNotIFI extends EvidenceFilter {
  def apply(evidence:String) = code(evidence) != "IGI" && code(evidence) != "IPI"
}
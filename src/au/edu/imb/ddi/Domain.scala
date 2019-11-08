package au.edu.imb.ddi

/**
  * Describes a domain.
  * @param pdbId PDB structure/id the domain was predicted for
  * @param id Id of domain
  * @param name Name of domain
  * @param src Source of prediction, e.g. HMMSmart
  * @param start Zero based start position
  * @param end Zero based end position
  */
case class Domain(pdbId:String, id:String, name:String, src:String, start:Int, end:Int) {
  override def toString = "%s %s %d %d" format (pdbId,id,start,end)
}

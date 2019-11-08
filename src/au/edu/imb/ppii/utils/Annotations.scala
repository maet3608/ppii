package au.edu.imb.ppii.utils

/**
 * Wrapper around a map that stores tag-values pairs to annotate objects,
 * specifically proteins.
 * Version: 1.00
 * Author : Stefan Maetschke
 */

import scala.collection.mutable.Map


/**
 * Encapsulates a set of annotation.
 * It is essentially a set of tag-values pairs that maps a tag to a list of values.
 * http://www.geneontology.org/GO.format.obo-1_2.shtml
 */
class Annotations(tagvalues:Iterator[(String,String)]) extends Iterable[(String,List[String])] {
  /** maps a tag to a list of values */
  private val annotations = Map[String,List[String]]()
  tagvalues.foreach(+=)   // fill map with annotations
  
  /** auxiliary constructor that takes list of tag-value pairs */
  def this(tagvalues:List[(String,String)]) = this(tagvalues.iterator)
  
  /** adds a tag-value pair to the annotations */
  def += (tagvalue:(String,String)) = annotations(tagvalue._1) =
    annotations.getOrElse(tagvalue._1, List[String]()):::List(tagvalue._2)

  /** sets the values of the given tag */
  def set(tag:String, values:List[String]) = annotations(tag) = values

  /** removes the tag and all its values from the annotation */
  def remove(tag:String) = annotations.remove(tag)

  /** tests if there is an entry for the given tag */
  def contains(tag:String) = annotations.contains(tag)
  
  /** getter for the annotation value for the given tag. 
   * Only the first value is returned. Fails if tag does not exist */
  def apply(tag:String) = annotations(tag).head
      
  /** returns all values for the given tag. Fails if tag does not exist. */
  def values(tag:String):Seq[String] = annotations(tag)

  /** returns all values for the given tag or an empty list if tag does not exist. */
  def valuesOrElse(tag:String) = annotations.getOrElse(tag, List[String]())
  
  /** implements the iterable trait and returns an iterator over tag-values pairs  */
  def iterator = annotations.iterator
  
  /** returns string with all tags and values*/
  override def toString = 
    annotations.map(t=>"  %s: %s".format(t._1,t._2.mkString(","))).
    mkString("Annotation {\n","\n","\n}")
}  
  
  
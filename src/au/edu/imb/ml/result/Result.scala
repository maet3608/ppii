package au.edu.imb.ml.result

import java.io.Writer

/**
 * Abstract trait of an inducer result, e.g. the result of a classifier (CResult or
 * a regressor (RResult)
 * Author : Stefan Maetschke
 * Version: 1.00
 * Date   : 29/06/2010
 */

trait Result {
  /** Stores results using a writer, e.g. writing to a file */
  def write(writer:Writer):Unit
}
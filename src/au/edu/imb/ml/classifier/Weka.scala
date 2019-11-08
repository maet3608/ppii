package au.edu.imb.ml.classifier

import au.edu.imb.ml.{CertaintyFactors, Classifier}
import au.edu.imb.ml.sampleset._
import au.edu.imb.ml.xperiment.XValidation
import au.edu.imb.ml.result.CResult
import weka.classifiers.{Classifier => WClassifier}
import weka.core.{Instance, Instances, SparseInstance, FastVector, Attribute}


/**
 * A wrapper around classifiers of the WEKA library.
 * See www.cs.waikato.ac.nz/~ml/weka/ for details.
 * It also converts samples and sample sets to instances and data sets as
 * they are used in Weka.
 * Author : Stefan Maetschke
 * Version: 1.00
 * Date   : 02/07/2010
 */

class Weka(val name:String, val options:Array[String], val labels:Seq[String]) extends
  Classifier with CertaintyFactors with Serializable {
  protected val classifier = WClassifier.forName(name, options)
  private var instances:Instances = null

  def cfs[S <: ASample](sample:S) =
    classifier.distributionForInstance(convert(sample,instances))
  
  def train[S <: ASample](sampleSet:ASampleSet[S]) = {
    instances = convert(sampleSet)
    classifier.buildClassifier(instances)
    instances.delete()  // remove all instances after training but keep attribute info
  }

  /**
   * Converts a dense sample into a Weka {@link Instance}.
   * @param sample sample to convert.
   * @param instances Weka dataset the instance/sample should belong to
   * @return Returns a Weka Instance.
   */
  private def convert(sample:ASample, instances:Instances):Instance = {
    val instance = new Instance(sample.length+1)
    instance.setDataset(instances)
    instance.setWeight(sample.weight)
    instance.setClassValue(labels(sample.classID))
    (0 until sample.length).foreach(i => instance.setValue(i, sample(i)))
    instance
  }

  /**
   * Converts a spares sample into a Weka {@link SparseInstance}.
   * The reference to the dataset and therefore the attribute information
   * (e.g. nominal value) is set but I don't know if Weka classifiers on
   * sparse instances are using that information, since all constructors
   * in Weka for sparse instances set m_Dataset to null!
   * @param sample sample to convert.
   * @param instances Weka dataset the instance/sample should belong to
   * @return Returns a Weka Instance.
   */
  private def convert(sample:ASparseSample, instances:Instances):Instance = {
    val components = sample.components:+((sample.dimension,sample.classID.toDouble))
    val instance = new SparseInstance(
      sample.weight,
      components.map{case(i,v) => v}.toArray,  // values
      components.map{case(i,v) => i}.toArray,  // indices
      components.length)
    instance.setDataset(instances)
    instance
  }
  
  /**
   * Converts a sample set into an Weka instances dataset.
   * Sparse samples are converted to {@link SparseInstance}s and
   * dense smples are converted to dense {@link Instance}s.
   * @param sampleSet Sample set.
   * @return Returns an instances dataset.
   */
  private def convert[S <: ASample](sampleSet:ASampleSet[S]):Instances = {
    val dimension    = sampleSet.dimension
    val sampleNumber = sampleSet.length

    // set attributes of input columns
    val attrInfo = new FastVector(dimension+1)
    sampleSet.features.foreach(a => attrInfo.addElement(convert(a)))
    // set label and possible values for output column = nominal value
    attrInfo.addElement(new Attribute("class", convert(labels)))

    // create empty set of instances
    val instances = new Instances("dataset", attrInfo, sampleNumber)
    instances.setClassIndex(dimension)
    instances.setClass(attrInfo.elementAt(dimension).asInstanceOf[Attribute])

    // fill set with samples converted to instances
    sampleSet.foreach(sample => instances.add(convert(sample,instances)))

    instances
  }

  private def convert(values:Seq[String]) = {
    val vector = new FastVector(values.length)
    values.foreach(vector.addElement)
    vector
  }

  /**
   * Converts sample set features into WEKA attributes.
   * @param feature Feature to convert
   * @return Returns a WEKA attribute.
   */
  private def convert(feature:Feature):Attribute = {
    feature match {
      case Nominal(name, values) => new Attribute(name, convert(values)) // return nominal
      case Numeric(name) =>  new Attribute(name) // return numeric
      case _ =>  sys.error("Unknown feature: "+feature)
    }
  }
}

/** Factory */
object Weka  {
  // Shortcuts for classifier names
  val SMO = "weka.classifiers.functions.SMO"
  val Logistic = "weka.classifiers.functions.Logistic"
  val Pegasos = "weka.classifiers.functions.SPegasos"
  val LibSVM = "weka.classifiers.functions.LibSVM"
  val IBk = "weka.classifiers.lazy.IBk"
  val IB1 = "weka.classifiers.lazy.IB1"
  val LBR = "weka.classifiers.lazy.LBR"
  val NaiveBayes = "weka.classifiers.bayes.NaiveBayes"
  val NaiveBayesSimple = "weka.classifiers.bayes.NaiveBayesSimple"
  val RandomForest = "weka.classifiers.trees.RandomForest"
  val FastRandomForest = "hr.irb.fastRandomForest.FastRandomForest"
  val J48 = "weka.classifiers.trees.J48"
  val NBTree = "weka.classifiers.trees.NBTree"
  val FT = "weka.classifiers.trees.FT"
  val LMT = "weka.classifiers.trees.LMT"

  /**
   * Constructor.
   * @param name Classifier name, e.g. SMO or "weka.classifiers.lazy.IBk"
   * @param options Classifier options, e.g.
   * Array("-C","1.0","-K","weka.classifiers.functions.supportVector.RBFKernel -G 0.01"
   * (Note that the options for the kernel are in one string!)
   * @parms labels List of class labels the classifier predicts.
   */
  def apply(name:String, options:Array[String], labels:Seq[String]) =
    new Weka(name,options,labels)

  /**
   * Constructor.
   * @param name Classifier name, e.g. SMO or "weka.classifiers.lazy.IBk"
   * @param options Classifier options as one string. Does not allow to
   * specify parameters for components such as kernels or meta classifiers!
   * @parms labels List of class labels the classifier predicts.
   */
  def apply(name:String, parameters:String, labels:Seq[String]) =
    new Weka(name, if(parameters==null) null else parameters.split("\\s+"), labels)
}


/** Usage example */
object WekaExample extends App {
  val sampleSet = SampleSet(
     SparseSample(4, "0:1 2:1", 0),
     SparseSample(4, "0:2 2:2", 0),
     SparseSample(4, "0:3 2:3", 1),
     SparseSample(4, "0:4 2:4", 1),
     SparseSample(4, "0:5 2:5", 1),
     SparseSample(4, "0:6 2:6", 1)
    )

  val smo = Weka(Weka.SMO,
        Array("-C","1.0","-K","weka.classifiers.functions.supportVector.RBFKernel -G 0.01"),
        List("y","n"))

  smo.train(sampleSet)
  for(sample <- sampleSet)
    printf("%s   %s   %d\n", smo.cfs(sample).mkString(" "), sample, smo.predict(sample))

  val knn = Weka("weka.classifiers.lazy.IBk", "-K 1", List("y","n"))
  val result = CResult(XValidation(sampleSet,3,1).map(knn.evaluate))
  printf("knn ACC = %f\n", result.acc)
}
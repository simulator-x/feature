/*
 * Copyright 2015 The SIRIS Project
 *
 *    Licensed under the Apache License, Version 2.0 (the "License");
 *    you may not use this file except in compliance with the License.
 *    You may obtain a copy of the License at
 *
 *        http://www.apache.org/licenses/LICENSE-2.0
 *
 *    Unless required by applicable law or agreed to in writing, software
 *    distributed under the License is distributed on an "AS IS" BASIS,
 *    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *    See the License for the specific language governing permissions and
 *    limitations under the License.
 *
 * The SIRIS Project is a cooperation between Beuth University, Berlin and the
 * HCI Group at the University of Würzburg. The project is funded by the German
 * Federal Ministry of Education and Research (grant no. 17N4409).
 */

package simx.components.ai.mipro.supervisedlearning.slprovider.neuralnetwork

import java.io.File
import java.util.UUID
import simx.components.ai.mipro.supervisedlearning.slprovider.{ConfigSerializationHelper, XmlHelper, SlConfiguration}
import simx.components.ai.mipro.supervisedlearning.util.ConfigurationSavingMode._
import simx.components.ai.mipro.supervisedlearning.util.NeuralNetworkState._
import simx.components.ai.mipro.supervisedlearning.util.{ConfigurationSavingMode, NeuralNetworkState, SlConstants}

import scala.collection.mutable
import scala.xml.Elem

/**
 * Specific configuration object for neural networks. Input and hidden layer sizes are optional parameters.
 *
 * @param aQualifiedName Name of the config, so it can be recognized in the file system if re-used later.
 * @param aInputLayerSize Number of input features.
 * @param aHiddenLayers Number of nodes in hidden layer.
 * @param aNumLabels Number of exit points (one for binary classification, more for multi class).
 * @param aLambda Parameter for regularization (over fitting the data = high variance).
 * @param aForceRecreation Indicates whether the recreation of the config should forceed
 *   if false, the config may be restored from the file system
 *
 * @author Thomas Krause
 */
case class NeuralNetworkConfiguration(aQualifiedName: String,
                                      aTargetFolder: File = new File(SlConstants.CONFIG_SUB_DIR),
                                      aSavingMode: ConfigurationSavingMode = ConfigurationSavingMode.XML,
                                      aInputLayerSize: Int = SlConstants.DEFAULT_INPUT_LAYER_SIZE,
                                      aHiddenLayers: Array[Int] = SlConstants.DEFAULT_HIDDEN_LAYERS,
                                      aNumLabels: Int = SlConstants.DEFAULT_NUM_LABELS,
                                      aLambda: Float = SlConstants.DEFAULT_LAMBDA,
                                      aForceRecreation: Boolean = false,
                                      aId: UUID = UUID.randomUUID(),
                                      aEncogConfig: EncogConfiguration = EncogConfiguration())
  extends SlConfiguration(aQualifiedName, aSavingMode, aTargetFolder, aId)
{
  private var iInputLayerSize = aInputLayerSize
  private var iHiddenLayers = aHiddenLayers
  private var iNumLabels = aNumLabels
  private val iLambda = aLambda

  /* The current state of the configuration object, only use enum @link{NeuralNetworkState}, no plain integers. */
  private var iCurrentState: NeuralNetworkState = NeuralNetworkState.Prepare

  /* The training data set by the neural network after data preparation. */
  private var iTrainingData: TrainData = new TrainData()
  /* The test data set by the neural network after data preparation. */
  private var iTestData: TestData = new TestData()
  /* The matrices set by the neural network after training. */
  private var iWeights: Option[Weights] = None

  private var iTrainingDataFileName = iQualifiedName + "_training"
  private var iTestDataFileName = iQualifiedName + "_testing"
  private var iEncogFileName = iQualifiedName + "_encog"

  private var iEncogConfig = aEncogConfig

  /*
   * Check if a configuration object with the desired parameters already exists in the file system and use its
   * matrices.
   */
  private val iWasRestored = if(!aForceRecreation) tryRestore() else false

  def appendTrainingData(aX: Array[Double], aY: Array[Double]): Unit = {iTrainingData.addDataSet(aX, aY)}
  def appendTestData(aX: Array[Double], aY: Array[Double]): Unit = {iTestData.addDataSet(aX, aY)}

  /** SETTERS */
  def setCurrentState(aState: NeuralNetworkState): Unit = {iCurrentState = aState}
  def setTrainingData(data: TrainData) = iTrainingData = data
  def setTestData(data: TestData) = iTestData = data
  def setWeights(weights: Weights) = iWeights = Some(weights)
  def setTestDataFileName(aFileName: String) = iTestDataFileName = aFileName
  def setTrainingDataFileName(aFileName: String) = iTrainingDataFileName = aFileName
  def setEncogFileName(aFileName: String) = iEncogFileName = aFileName
  def setInputLayerSize(size: Int) = iInputLayerSize = size
  def setEncogConfig(aEncogConfig: EncogConfiguration) = iEncogConfig = aEncogConfig

  /** GETTERS */
  def getCurrentState: NeuralNetworkState = iCurrentState
  def getInputLayerSize: Int = iInputLayerSize
  def getHiddenLayers: Array[Int] = iHiddenLayers
  def getNumLabels: Int = iNumLabels
  def getLambda: Float = iLambda
  def getTrainingData: TrainData = iTrainingData
  def getTestData: TestData = iTestData
  def getEncogConfig: EncogConfiguration = iEncogConfig

  def isRestored: Boolean = iWasRestored
  def getWeights: Option[Weights] = iWeights


  /**
   * Try to restore already existing parameters of the configuration object from the file system.
   *
   * @return
   */
  override def tryRestore(): Boolean =
  {
    val tRestored: Option[SlConfiguration] = restoreFromFileSystem()

    if (tRestored.isDefined)
    {
      val tConfigObj: SlConfiguration = tRestored.get

      tConfigObj match
      {
        case tNNConfigObj: NeuralNetworkConfiguration =>
          if (tNNConfigObj.equals(this))
          {
            /* Use State, X, Y, Weights from the file system */
            iId = tNNConfigObj.getId()
            iInputLayerSize = tNNConfigObj.getInputLayerSize
            iNumLabels = tNNConfigObj.getNumLabels
            iHiddenLayers = tNNConfigObj.getHiddenLayers
            iCurrentState = tNNConfigObj.getCurrentState
            iTrainingData = tNNConfigObj.getTrainingData
            iTestData = tNNConfigObj.getTestData
            iWeights = tNNConfigObj.getWeights
            tNNConfigObj.loadEncogConfig()
            iEncogConfig = tNNConfigObj.getEncogConfig
            true
          }
          else
          {
            false
          }

        case _ => false
      }
    }
    else
    {
      false
    }
  }

  /**
   * Override, NeuralNetwork-specific update method.
   */
  override def updateToFileSystem(): Unit =
  {
    /* Parent call updates the main config file. */
    super.updateToFileSystem()

    /* NN-specific saving of additional config files happens here. */
    if (iSavingMode == ConfigurationSavingMode.XML)
    {
      if (iTrainingData.getSize() > 0)
      {
//        ConfigSerializationHelper.writeXml(iTrainingDataFileName, iTrainingData.toXML, iTargetFolder)
        iTrainingData.toBinary(iTrainingDataFileName, iTargetFolder)
      }

      if (iTestData.getSize() > 0)
      {
//        ConfigSerializationHelper.writeXml(iTestDataFileName, iTestData.toXML, iTargetFolder)
        iTestData.toBinary(iTestDataFileName, iTargetFolder)
      }
      ConfigSerializationHelper.writeXml(iEncogFileName, iEncogConfig.toXml(), iTargetFolder)
    }
  }

  def loadTrainingData(func: mutable.MutableList[() => Unit] = mutable.MutableList()): Unit =
  {
    val tXmlOpt = ConfigSerializationHelper.readXml(iTrainingDataFileName, iTargetFolder)

    if (tXmlOpt.isDefined)
    {
      val tXml = tXmlOpt.get
      val tTraining = tXml \\ "TRAINING"

      val tTrainingData = new TrainData()
      tTrainingData.fromXML(tTraining)
      iTrainingData = tTrainingData
      println(Console.GREEN +"[Neural Network Configuration] Finished Loading Training Files from XML " + Console.RESET)
      func.foreach{f => f()}
    } else {
      val nnBinaryData = ConfigSerializationHelper.deserializeNNData(iTrainingDataFileName, iTargetFolder)
      nnBinaryData.foreach{data =>
        val trainData = new TrainData()
        trainData.fromBinary(data)
        println(Console.GREEN +"[Neural Network Configuration] Finished Loading Training Files from Binary " + Console.RESET)
        iTrainingData = trainData
      }
    }
  }

  def loadTestData(func: mutable.MutableList[() => Unit] = mutable.MutableList()): Unit =
  {
    val tXmlOpt = ConfigSerializationHelper.readXml(iTestDataFileName, iTargetFolder)

    if (tXmlOpt.isDefined)
    {
      val tXml = tXmlOpt.get
      val tTesting = tXml \\ "TESTING"

      val tTestData = new TestData()
      tTestData.fromXML(tTesting)
      iTestData = tTestData
      println(Console.GREEN +"[Neural Network Configuration] Finished Loading Test Files" + Console.RESET)
      func.foreach{f => f()}
    }  else {
      val nnBinaryData = ConfigSerializationHelper.deserializeNNData(iTrainingDataFileName, iTargetFolder)
      nnBinaryData.foreach{data =>
        val testData = new TestData()
        testData.fromBinary(data)
        println(Console.GREEN +"[Neural Network Configuration] Finished Loading Test Files from Binary " + Console.RESET)
        iTestData = testData
    }}
  }

  def loadEncogConfig(): Unit =
  {
    val tXmlOpt = ConfigSerializationHelper.readXml(iEncogFileName, iTargetFolder)

    if (tXmlOpt.isDefined)
    {
      val tXml = tXmlOpt.get
      iEncogConfig.fromXml(tXml)
    }
  }

  /**
   * Overriden equals method.
   * @param aConfigObj The configuration object to check against.
   * @return
   */
  override def equals(aConfigObj: Any): Boolean =
  {
    aConfigObj match
    {
      case tNNConfigObj: NeuralNetworkConfiguration =>
        if (tNNConfigObj.getQualifiedName.equals(iQualifiedName) // &&
//          tNNConfigObj.getInputLayerSize == iInputLayerSize &&
//          (tNNConfigObj.getHiddenLayers sameElements iHiddenLayers) &&
//          tNNConfigObj.getNumLabels == iNumLabels &&
//          tNNConfigObj.getLambda == iLambda
    )
        {
          true
        }
        else
        {
          false
        }
      case _ => false
    }
  }

  /**
   * Overriden hash code method.
   * @return
   */
  override def hashCode(): Int =
  {
    val tPrime = 31
    var tHash = 1

    tHash = tPrime * tHash + iQualifiedName.hashCode()
    tHash = tPrime * tHash + iInputLayerSize
    //TODO hashCode for Array[Int]
  //  tHash = tPrime * tHash + iHiddenLayers
    tHash = tPrime * tHash + iNumLabels
    tHash = tPrime * tHash + iLambda.hashCode()
    tHash
  }

  /**
   * Creates a XML representation of a NeuronalNetWorkConfiguration.
   *
   * @return An XML element that can be passed into a parser.
   */
  override def toXml(): Elem =
  {
    val tXml =
      <SL_CONFIG>
        <CONFIG_PROPERTIES>
          <UUID>{iId.toString}</UUID>
          <NAME>{iQualifiedName}</NAME>
        </CONFIG_PROPERTIES>
        <NN_PROPERTIES>
          <INPUT_LAYER_SIZE>{String.valueOf(iInputLayerSize)}</INPUT_LAYER_SIZE>
          <HIDDEN_LAYER_SIZE>{XmlHelper.toXml(iHiddenLayers)}</HIDDEN_LAYER_SIZE>
          <NUM_LABLES>{String.valueOf(iNumLabels)}</NUM_LABLES>
          <LAMBDA>{String.valueOf(iLambda)}</LAMBDA>
          <STATE>{iCurrentState.toString}</STATE>
        </NN_PROPERTIES>
        <NN_DATA>
          <TRAINING>{iTrainingDataFileName}</TRAINING>
          <TESTING>{iTestDataFileName}</TESTING>
          <ENCOG>{iEncogFileName}</ENCOG>
          {if (getWeights.isDefined) getWeights.get.toXML}
        </NN_DATA>
      </SL_CONFIG>
    tXml
  }

  /**
   * Creates an Instance of an NeuronalNetworkConfiguration from a XML representation
   * @param aXml
   * @return
   */
  override def fromXml(aXml: Elem): NeuralNetworkConfiguration =
  {
    val tUUIDElem = aXml \\ "UUID"
    val tNameElem = aXml \\ "NAME"
    val tInputLayerSizeElem = aXml \\ "INPUT_LAYER_SIZE"
    val tHiddenLayerSizeElem = aXml \\ "HIDDEN_LAYER_SIZE"
    val tNumLablesElem = aXml \\ "NUM_LABLES"
    val tLambdaElem = aXml \\ "LAMBDA"
    val tStateElem = aXml \\ "STATE"

    val tTrainingElem = aXml \\ "TRAINING"
    val tTestingElem = aXml \\ "TESTING"
    val tEncogElem = aXml \\ "ENCOG"
    val tWeightsElem = aXml \\ "WEIGHTS"

    val tInputLayerSize: Int = tInputLayerSizeElem.text.toInt
    val tHiddenLayerSize: Array[Int] = XmlHelper.toArray(tHiddenLayerSizeElem).map(_.toInt)
    val tNumLabels: Int = tNumLablesElem.text.toInt
    val tLambda: Float = tLambdaElem.text.toFloat
    val tState: NeuralNetworkState = NeuralNetworkState.withName(tStateElem.text)

    val tNNConfig = new NeuralNetworkConfiguration(tNameElem.text, iTargetFolder, ConfigurationSavingMode.XML,
      tInputLayerSize, tHiddenLayerSize, tNumLabels, tLambda, true, UUID.fromString(tUUIDElem.text))

    tNNConfig.setCurrentState(tState)

    val weights = new Weights()
    weights.fromXML(tWeightsElem)
    tNNConfig.setWeights(weights)

    tNNConfig.setTrainingDataFileName(tTrainingElem.text)
    tNNConfig.setTestDataFileName(tTestingElem.text)
    tNNConfig.setEncogFileName(tEncogElem.text)

    tNNConfig
  }
}

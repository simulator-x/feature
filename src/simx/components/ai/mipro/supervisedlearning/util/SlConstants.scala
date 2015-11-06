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

package simx.components.ai.mipro.supervisedlearning.util

/**
 * Constants for the supervised learning mechanisms.
 *
 * @author Thomas Krause
 */
object SlConstants
{
  /* General */
  val CONFIG_SUB_DIR = "sl-configurations"
  /* Neural Network specific */
  val DEFAULT_INPUT_LAYER_SIZE = 8000
  val DEFAULT_HIDDEN_LAYERS = Array[Int](25)
  val DEFAULT_NUM_LABELS = 1
  val DEFAULT_LAMBDA = 1.0f
}

/**
 * State enum for the neural network.
 *
 * @author Thomas Krause
 */
object NeuralNetworkState extends Enumeration
{
  type NeuralNetworkState = Value
  val Prepare, Train, Predict = Value
}

/**
 * Enum different types of saving a SlConfiguration object.
 *
 * @author Thomas Krause
 */
object ConfigurationSavingMode extends Enumeration
{
  type ConfigurationSavingMode = Value
  val BINARY, XML = Value
}


/*
 * Copyright 2013 The SIRIS Project
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

package simx.components.ai.feature.local

import simx.core.ontology.types.OntologySymbol

/**
 * Created by IntelliJ IDEA.
 * User: martin
 * Date: 9/2/13
 * Time: 1:51 PM
 */
object Symbols{
  
  object accepted extends OntologySymbol(Symbol("acceptedFeature"))
  object rejected extends OntologySymbol(Symbol("rejectedFeature"))
  object feature extends OntologySymbol(Symbol("Feature"))
  object fakeSource extends OntologySymbol(Symbol("FakeSource"))
  object inputFeature extends OntologySymbol(Symbol("InputFeature"))
  object recordOutputFile extends OntologySymbol(Symbol("recordOutputFile"))
  object vrpnToWorldOffsetInWorldCS extends OntologySymbol(Symbol("vrpnToWorldOffsetInWorldCS"))
  object vrpnToWorldRotationInWorldCS extends OntologySymbol(Symbol("vrpnToWorldRotationInWorldCS"))
  object vrpnToWorldScale extends OntologySymbol(Symbol("vrpnToWorldScale"))
}
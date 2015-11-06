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

import simx.core.ontology.{types, SValDescription}
import simx.components.ai.feature.local
import simx.core.ontology.types.NullType
import simx.core.ontology

/**
 * Created by IntelliJ IDEA.
 * User: martin
 * Date: 9/3/13
 * Time: 12:00 PM
 */

//TODO: Integrate into core ontology
object Debug extends SValDescription(ontology.types.Enabled as ontology.Symbols.debug)

object Feature extends SValDescription(NullType as local.Symbols.feature withType classOf[simx.components.ai.feature.FeatureBase])

object InputFeature extends SValDescription(NullType as local.Symbols.inputFeature withType classOf[simx.components.ai.feature.InputFeatureBase])

object Record extends SValDescription(ontology.types.Enabled as ontology.Symbols.record)
object RecordOutputFile extends SValDescription(NullType as Symbols.recordOutputFile withType classOf[java.io.File])

//Todo: Check if converts can be used instead
object VrpnToWorldOffsetInWorldCS extends SValDescription(types.Position as Symbols.vrpnToWorldOffsetInWorldCS)
object VrpnToWorldRotationInWorldCS extends SValDescription(types.Orientation as Symbols.vrpnToWorldRotationInWorldCS)
object VrpnToWorldScale extends SValDescription(types.Vector3 as Symbols.vrpnToWorldScale)
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

package simx.components.ai.feature

import simx.core.component.ConfigureComponentMessage
import simplex3d.math.float.{ConstVec3, ConstQuat4, ConstMat4}
import simplex3d.math.float.functions.rotationMat
import simx.core.entity.description.SValSet
import simx.core.ontology.{Symbols, types}
import simx.core.svaractor.SVarActor
import scala.annotation.meta.param

/**
 * Created by IntelliJ IDEA.
 * User: martin
 * Date: 9/16/13
 * Time: 2:43 PM
 */
case class FeatureComponentCfg(vrpnToWorldOffsetInWorldCS: ConstVec3, vrpnToWorldScale: ConstVec3, vrpnToWorldRotationInWorldCS: ConstQuat4)
  (implicit @(transient @param) actorContext : SVarActor.Ref)
  extends ConfigureComponentMessage(SValSet(
    types.Transformation.withAnnotations(Symbols.vrpn, Symbols.orientation)(ConstMat4(rotationMat(vrpnToWorldRotationInWorldCS))),
    types.Vector3.withAnnotations(Symbols.vrpn, Symbols.scale)(vrpnToWorldScale),
    types.Origin.withAnnotations(Symbols.vrpn, Symbols.offset)(vrpnToWorldOffsetInWorldCS)
  ))




/*
Copyright 2015 Mfg labs.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
*/

package com.mfglabs
package precepte

import org.scalatest._
import Matchers._
import Inspectors._

import org.scalatest.concurrent.ScalaFutures
import org.scalatest.concurrent.PatienceConfiguration.Timeout
import org.scalatest.time.{Millis, Seconds, Span => TSpan}

import scala.language.higherKinds

class Composition1Spec extends FlatSpec {

  "Composition1" should "compose" in {
    val toS: Int => String  = _.toString
    val yolo: String => String = _ + " YOLO"
    val comp = Compose1(yolo).compose(toS)
    comp(1) should ===("1 YOLO")
  }

  it should "andThen" in {
    val toS: Int => String  = _.toString
    val yolo: String => String = _ + " YOLO"
    val comp = Compose1(toS).andThen(yolo)
    comp(1) should ===("1 YOLO")
  }

  it should "not stack overflow on compose" in {
    val plusOne: Int => Int = _ + 1
    val comp =
      (1 to 1000000).foldLeft(Compose1(identity[Int])){ (f, _) =>
        f.compose(plusOne)
      }
    comp(0) should ===(1000000)
  }

  it should "not stack overflow on andThen" in {
    val plusOne: Int => Int = _ + 1
    val comp =
      (1 to 1000000).foldLeft(Compose1(identity[Int])){ (f, _) =>
        f.andThen(plusOne)
      }
    comp(0) should ===(1000000)
  }


}
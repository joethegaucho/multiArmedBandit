package com.multiarmedbandit.model

import spray.json.DefaultJsonProtocol._
import spray.json.RootJsonFormat
import agents._
import simulation.Simulation._

case class Result(agent: String, results: List[Double])

object Result {

  implicit val resultJsonFormat: RootJsonFormat[Result] = jsonFormat2(
    Result.apply
  )

}

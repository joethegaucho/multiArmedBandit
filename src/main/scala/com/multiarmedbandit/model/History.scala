package com.multiarmedbandit.model

import spray.json.DefaultJsonProtocol._
import spray.json.RootJsonFormat

case class History(name: String, history: List[Double])

object History {

    implicit val historyJsonFormat: RootJsonFormat[History] = jsonFormat2(History.apply)

}
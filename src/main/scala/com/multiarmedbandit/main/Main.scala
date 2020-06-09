package com.multiarmedbandit.main

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport._
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.{Route}
import akka.http.scaladsl.server.directives.RouteDirectives.complete
import ch.megard.akka.http.cors.scaladsl.CorsDirectives._
import akka.stream.ActorMaterializer
import breeze.stats.distributions._
import agents._
import simulation.Simulation._
import utils.Utils._
import com.multiarmedbandit.model.{Result}

object Main extends App {

  val epsilonGreedyAgent = EpsilonGreedyAgent(0.1)

  val epsilonGreedyReward = runSimulation(epsilonGreedyAgent, 1000)

  println(s"Epsilon Greedy score ${epsilonGreedyReward.results.sum}")

  implicit val system: ActorSystem = ActorSystem("Main")
  implicit val materializer: ActorMaterializer = ActorMaterializer()

  val routes: Route = cors() {
    pathPrefix("data") {
      path("epsilongreedy"){
        get {
          complete(epsilonGreedyReward)
        } 
      } 
    }
  }

  Http().bindAndHandle(routes, "localhost", 8080)
  println(s"Server online at http://localhost:8080/")

}
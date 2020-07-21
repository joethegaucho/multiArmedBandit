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
  val epsilonFirstAgent = EpsilonFirstAgent(0.1)
  val epsilonDecreasingAgent = EpsilonDecreasingAgent(1.0)
  val vdbeBoltzmannAgent = VDBEBoltzmannAgent(1.0, 0.33)

  val epsilonGreedyReward = runSimulation(epsilonGreedyAgent, 1000)
  val epsilonFirstReward = runSimulation(epsilonFirstAgent, 1000)
  val epsilonDecreasingReward = runSimulation(epsilonDecreasingAgent, 1000)
  val vdbeBoltzmannReward = runSimulation(vdbeBoltzmannAgent, 1000)

  println(s"Epsilon Greedy score     : ${epsilonGreedyReward.results.sum}")
  println(s"Epsilon First score      : ${epsilonFirstReward.results.sum}")
  println(s"Epsilon Decreasing score : ${epsilonDecreasingReward.results.sum}")
  println(s"VDBE Boltzmann score     : ${vdbeBoltzmannReward.results.sum}")

  implicit val system: ActorSystem = ActorSystem("Main")
  implicit val materializer: ActorMaterializer = ActorMaterializer()

  val routes: Route = cors() {
    pathPrefix("data") {
      path("epsilongreedy") {
        get {
          complete(epsilonGreedyReward)
        }
      } ~
        path("epsilonfirst") {
          get {
            complete(epsilonFirstReward)
          }
        } ~
        path("epsilondecreasing") {
          get {
            complete(epsilonDecreasingReward)
          }
        } ~
        path("vdbeboltzmann") {
          get {
            complete(vdbeBoltzmannReward)
          }
        }
    }
  }

  Http().bindAndHandle(routes, "localhost", 8080)
  println(s"Server online at http://localhost:8080/")

}

package com.multiarmedbandit.main

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport._
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.{Route}
import akka.http.scaladsl.server.directives.RouteDirectives.complete
import ch.megard.akka.http.cors.scaladsl.CorsDirectives._
import akka.stream.ActorMaterializer
import collection.mutable.ListBuffer
import breeze.stats.distributions._
import math.exp
import util.Random
import scala.collection.immutable.ListMap
import scala.util.control.Breaks._
import com.multiarmedbandit.model.{History}

object Main extends App {

    val randomNum = scala.util.Random

    def mean(nums: List[Double]): Double = nums.sum/nums.length

    def getMovingAverage(nums: List[Double]): List[Double] = {
        var sum:Double = nums(0)
        var movingAverageList = ListBuffer[Double](sum)

        for(i <- 1 to nums.length - 1){
            sum += nums(i)
            movingAverageList += (sum / (i + 1))
        }
        movingAverageList.toList
    }

    class Lever(name: String, variance: Double = 0.1, stationary: Boolean = false) {

        var meanPayout = Gaussian(0, 1.0).draw
        val payoutDistribution = Gaussian(meanPayout, 1.0)
        val leverName = name
        val distributionChangeProbability = 0.1
        var distributionChange = 1.0/Random.nextDouble

        def getPayout(stationary:Boolean = stationary): Double = {
            if(stationary == false){
                payoutDistribution.draw
            } 
            else {
                if(Random.nextDouble < distributionChangeProbability){
                    distributionChange = 1.0/Random.nextDouble
                    meanPayout *= (distributionChange * 0.1)
                    payoutDistribution.draw
                }
                else {
                    meanPayout *= (distributionChange * 0.1)
                    payoutDistribution.draw
                }
            }
        }
    }

    class EpsilonGreedyAgent {

        val epsilon = .1
        var history = Map[String, ListBuffer[Double]]()
        var trial = 0
        var total_score = 0

        def pullLever(lever: Lever): Unit = {

            val leverName = lever.leverName
            val leverPayout = lever.getPayout()

            if(history.contains(leverName)) {
                history(leverName) += leverPayout
            }
            else {
                history += (leverName -> ListBuffer(leverPayout))
            }
        }

        def decideLever(totalTrials: Int, trialNum: Int, levers: List[Lever]): Unit = {
            
            trial += 1
            if(trial == 1){
                pullLever(levers(randomNum.nextInt(levers.length - 2)+ 1))
            }
            else {

                var leverResults = Map[String, Double]()
                for ((k,v) <- history) leverResults = leverResults ++ Map(k -> mean(v.toList))
                val bestLever = levers(leverResults.maxBy { case (key, value) => value }._1.toInt)

                if(randomNum.nextDouble < (1 - epsilon)){
                    pullLever(bestLever)
                }
                else {
                    pullLever(levers(randomNum.nextInt(levers.length - 2)+ 1))
                }
            }
        }

    }

    class EpsilonFirstAgent {

        val epsilon = .1
        var history = Map[String, ListBuffer[Double]]()
        var trial = 0
        var total_score = 0

        def pullLever(lever: Lever): Unit = {

            val leverName = lever.leverName
            val leverPayout = lever.getPayout()

            if(history.contains(leverName)) {
                history(leverName) += leverPayout
            }
            else {
                history += (leverName -> ListBuffer(leverPayout))
            }
        }

        def decideLever(totalTrials: Int, trialNum: Int, levers: List[Lever]): Unit = {
            
            trial += 1
            if(trialNum < (totalTrials * epsilon).ceil) {
                pullLever(levers(randomNum.nextInt(levers.length - 2)+ 1))
            }
            else {
                var leverResults = Map[String, Double]()
                for ((k,v) <- history) leverResults = leverResults ++ Map(k -> mean(v.toList))
                val leverToPull = levers(leverResults.maxBy { case (key, value) => value }._1.toInt)
                pullLever(leverToPull)
            }
        }

    }

    class EpsilonDecreasingAgent {

        val startEpsilon = 1.0
        var history = Map[String, ListBuffer[Double]]()
        var trial = 0
        var total_score = 0

        def pullLever(lever: Lever): Unit = {

            val leverName = lever.leverName
            val leverPayout = lever.getPayout()

            if(history.contains(leverName)) {
                history(leverName) += leverPayout
            }
            else {
                history += (leverName -> ListBuffer(leverPayout))
            }
        }

        def decideLever(totalTrials: Int, trialNum: Int, levers: List[Lever]): Unit = {
            
            trial += 1
            if(trial == 1){
                pullLever(levers(randomNum.nextInt(levers.length - 2)+ 1))
            }
            else {

                var leverResults = Map[String, Double]()
                for ((k,v) <- history) leverResults = leverResults ++ Map(k -> mean(v.toList))
                val bestLever = levers(leverResults.maxBy { case (key, value) => value }._1.toInt)

                val currentEpsilon = startEpsilon/trial
                if(randomNum.nextDouble < (1 - currentEpsilon)){
                    pullLever(bestLever)
                }
                else {
                    pullLever(levers(randomNum.nextInt(levers.length - 2)+ 1))
                }
            }
        }

    }

    
    class VDBEBoltzmannAgent {

        var inverseSensitivity = 0.33
        var epsilon = 1.0
        var history = Map[String, ListBuffer[Double]]()
        var trial = 0
        var total_score = 0

        def pullLever(lever: Lever): Unit = {

            val leverName = lever.leverName
            val leverPayout = lever.getPayout()

            if(history.contains(leverName)) {
                history(leverName) += leverPayout
            }
            else {
                history += (leverName -> ListBuffer(leverPayout))
            }
        }
        
        def decideLever(totalTrials: Int, trialNum: Int, levers: List[Lever]): Unit = {

            val delta = 1.0/levers.length
            trial += 1

            if(randomNum.nextDouble < epsilon){
                val leverToPull = levers(randomNum.nextInt(levers.length - 2)+ 1)
                pullLever(leverToPull)
                val leverRewardHistory = history(leverToPull.leverName).toList

                if(leverRewardHistory == 1){
                    val tdError = leverRewardHistory(0)
                    val boltzmannEval = ((1 - exp(-1 * tdError))/inverseSensitivity)/((1 + exp(-1 * tdError))/inverseSensitivity)
                    epsilon = delta * boltzmannEval + (1 - delta) * epsilon
                }
                else {
                    val tdError = (1/leverRewardHistory.length) * (leverRewardHistory(leverRewardHistory.length - 1) - mean(leverRewardHistory))
                    val boltzmannEval = ((1 - exp(-1 * tdError))/inverseSensitivity)/((1 + exp(-1 * tdError))/inverseSensitivity)
                    epsilon = delta * boltzmannEval + (1 - delta) * epsilon
                }
            }
            else {
                var leverResults = Map[String, Double]()
                for ((k,v) <- history) leverResults = leverResults ++ Map(k -> mean(v.toList))
                val leverToPull = levers(leverResults.maxBy { case (key, value) => value }._1.toInt)
                pullLever(leverToPull)
            }
        }
    }

    class SoftmaxAgent {

        var inverseSensitivity = 0.2
        var history = Map[String, ListBuffer[Double]]()
        var trial = 0
        var total_score = 0

        def pullLever(lever: Lever): Unit = {

            val leverName = lever.leverName
            val leverPayout = lever.getPayout()

            if(history.contains(leverName)) {
                history(leverName) += leverPayout
            }
            else {
                history += (leverName -> ListBuffer(leverPayout))
            }
        }

        def decideLever(totalTrials: Int, trialNum: Int, levers: List[Lever]): Unit = {

            var leverResults = Map[String, Double]()
            var leverProbabilities = scala.collection.mutable.Map[String, Double]()
            for ((k,v) <- history) leverResults = leverResults ++ Map(k -> mean(v.toList))
            for (i <- levers) leverProbabilities += (i.leverName -> 1.0/levers.length)

            trial += 1

            for ((k,v) <- leverResults) {
                leverProbabilities(k) = (exp(v)/inverseSensitivity)/(leverResults.values.toList.map(x => exp(x)/inverseSensitivity).sum)
            }

            val leverProbabilitiesSorted = ListMap(leverProbabilities.toSeq.sortBy(_._2):_*)

            val roll = randomNum.nextDouble
            var cumulativeProbability = 0.0
            var iterator = 0

            while(cumulativeProbability < roll){
                cumulativeProbability += leverProbabilitiesSorted.values.toList(iterator)
                iterator += 1
            }

            val leverToPull = (leverProbabilitiesSorted.keys.toList)(iterator - 1).toInt

            pullLever(levers(leverToPull))
                
        }
    }


    def runSimulation(trials: Int, numLevers: Int): List[History] = {

        var levers = new ListBuffer[Lever]()
        val epsilonGreedyAgent = new EpsilonGreedyAgent()
        val epsilonFirstAgent = new EpsilonFirstAgent()
        val epsilonDecreasingAgent = new EpsilonDecreasingAgent()
        val vDBEBoltzmannAgent = new VDBEBoltzmannAgent()
        val softmaxAgent = new SoftmaxAgent()


        for (i <- 0 to numLevers - 1) levers += new Lever(i.toString)

        for (i <- 1 to trials) epsilonGreedyAgent.decideLever(trials, epsilonGreedyAgent.trial, levers.toList)
        for (i <- 1 to trials) epsilonFirstAgent.decideLever(trials, epsilonFirstAgent.trial, levers.toList)
        for (i <- 1 to trials) epsilonDecreasingAgent.decideLever(trials, epsilonFirstAgent.trial, levers.toList)
        for (i <- 1 to trials) vDBEBoltzmannAgent.decideLever(trials, vDBEBoltzmannAgent.trial, levers.toList)
        for (i <- 1 to trials) softmaxAgent.decideLever(trials, softmaxAgent.trial, levers.toList)


        val epsilonGreedyHistory = epsilonGreedyAgent.history.values.toList.flatten
        val epsilonFirstHistory = epsilonFirstAgent.history.values.toList.flatten
        val epsilonDecreasingHistory = epsilonDecreasingAgent.history.values.toList.flatten
        val vDBEBoltzmannHistory = vDBEBoltzmannAgent.history.values.toList.flatten
        val softmaxHistory = softmaxAgent.history.values.toList.flatten


        println(s"Epsilon Greedy Strategy score      : ${epsilonGreedyHistory.sum}")
        println(s"Epsilon First Strategy score       : ${epsilonFirstHistory.sum}")
        println(s"Epsilon Decreasing Strategy score  : ${epsilonDecreasingHistory.sum}")
        println(s"VDBE Boltzmann Strategy score      : ${vDBEBoltzmannHistory.sum}")
        println(s"Softmax Strategy score             : ${softmaxHistory.sum}")

        
        
        List(History("Epsilon Greedy", getMovingAverage(epsilonGreedyHistory)),
        History("Epsilon First", getMovingAverage(epsilonFirstHistory)),
        History("Epsilon Decreasing", getMovingAverage(epsilonDecreasingHistory)),
        History("VDBE Boltzmann", getMovingAverage(vDBEBoltzmannHistory)),
        History("Softmax", getMovingAverage(softmaxHistory)))

    }

    val histories = runSimulation(1000, 10)

    implicit val system: ActorSystem = ActorSystem("Main")
    implicit val materializer: ActorMaterializer = ActorMaterializer()


    val routes: Route = cors() {
      pathPrefix("data") {
        path("epsilongreedy"){
          get {
            complete(histories(0))
          }
        } ~
        path("epsilonfirst"){
          get {
            complete(histories(1))
          }
        } ~
        path("epsilondecreasing"){
          get {
            complete(histories(2))
          }
        } ~
        path("vdbeboltzmann"){
          get {
            complete(histories(3))
          }
        } ~
        path("softmax"){
          get {
            complete(histories(4))
          }
        } 
      }
    }

    Http().bindAndHandle(routes, "localhost", 8080)
    println(s"Server online at http://localhost:8080/")

}




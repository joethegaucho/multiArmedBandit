package agents

import simulation.Simulation._
import utils.Utils._
import math._
import util.control.Breaks._

sealed trait Agent {

  def policy(state: State): Action

  def evaluateActions(
      actionValueMethod: State => ActionValues,
      state: State
  ): ActionValues = actionValueMethod(state)

  def updateAgent(state: State): Agent

  def epsilon: Double

}

final case class EpsilonGreedyAgent(val epsilon: Double) extends Agent {

  def policy(state: State): Action = {
    if (state.history.isEmpty || scala.util.Random.nextDouble < epsilon) {
      state.actionSpace(scala.util.Random.nextInt(state.actionSpace.length))
    } else {

      val actionValues = evaluateActions(mean, state)
      actionValues.maxBy { case (key, value) => value }._1

    }
  }

  def updateAgent(state: State) = EpsilonGreedyAgent(this.epsilon)

}

final case class EpsilonFirstAgent(val epsilon: Double) extends Agent {

  def policy(state: State): Action = {
    if (state.time < state.numTrials * epsilon) {
      state.actionSpace(scala.util.Random.nextInt(state.actionSpace.length))
    } else {

      val actionValues = evaluateActions(mean, state)
      actionValues.maxBy { case (key, value) => value }._1

    }
  }

  def updateAgent(state: State) = EpsilonFirstAgent(this.epsilon)

}

final case class EpsilonDecreasingAgent(val epsilon: Double) extends Agent {

  def policy(state: State): Action = {
    if (state.history.isEmpty || scala.util.Random.nextDouble < epsilon) {
      state.actionSpace(scala.util.Random.nextInt(state.actionSpace.length))
    } else {

      val actionValues = evaluateActions(mean, state)
      actionValues.maxBy { case (key, value) => value }._1

    }
  }

  def updateAgent(state: State) = {
    val newEpsilon = this.epsilon / 2
    EpsilonDecreasingAgent(newEpsilon)
  }

}

final case class VDBEBoltzmannAgent(
    val epsilon: Double,
    val inverseSensitivity: Double
) extends Agent {

  def policy(state: State): Action = {
    if (state.history.isEmpty || scala.util.Random.nextDouble < epsilon) {
      state.actionSpace(scala.util.Random.nextInt(state.actionSpace.length))
    } else {

      val actionValues = evaluateActions(mean, state)
      actionValues.maxBy { case (key, value) => value }._1

    }
  }

  def getTemporalDifferenceWeight(state: State): Double = {

    if (state.history.isEmpty) {
      0
    } else {
      val previousAction = state.history.last._1
      val previousReward = state.history.last._2

      val stepSizeParameter = 1 / (1 + state.history
        .groupBy(action => action._1)(previousAction)
        .length)

      val temporalDifferenceError =
        previousReward - evaluateActions(mean, state)(previousAction)

      val eTerm = exp(
        -1.0 * abs(
          stepSizeParameter * temporalDifferenceError
        ) / this.inverseSensitivity
      )
      val numerator = 1.0 - eTerm
      val denominator = 1.0 + eTerm

      numerator / denominator
    }
  }

  def updateAgent(state: State) = {

    if (state.history.isEmpty) {
      VDBEBoltzmannAgent(epsilon, inverseSensitivity)
    }

    val delta = 1 / state.actionSpace.length
    val newEpsilon =
      delta * getTemporalDifferenceWeight(state) + (1 - delta) * this.epsilon

    VDBEBoltzmannAgent(newEpsilon, inverseSensitivity)

  }

}

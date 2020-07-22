package agents

import simulation.Simulation._
import utils.Utils._
import math._
import util.control.Breaks._
import scala.collection.immutable.ListMap

sealed trait Agent {

  def policy(state: State): Action
  def updateAgent(state: State): Agent

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
    val newEpsilon = 1.0 / state.time
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

      val stepSizeParameter = 1.0 / (1 + state.history
        .groupBy(action => action._1)(previousAction)
        .length)

      val temporalDifferenceError =
        previousReward - evaluateActions(mean, state)(previousAction)

      val eTerm = exp(
        -1.0 * (abs(
          stepSizeParameter * temporalDifferenceError
        ) / inverseSensitivity)
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

    val delta = 1.0 / state.actionSpace.length
    val newEpsilon =
      (delta * getTemporalDifferenceWeight(state)) + ((1 - delta) * epsilon)

    VDBEBoltzmannAgent(newEpsilon, inverseSensitivity)

  }

}

final case class SoftMaxAgent(temperature: Double) extends Agent {

  def policy(state: State): Action = {
    if (state.history.length <= 1) {
      state.actionSpace(scala.util.Random.nextInt(state.actionSpace.length))
    } else {

      val actionProbabilities = convertActionValuesToActionProbabilities(
        getActionValueEstimates(mean, state)
      )

      chooseProbabilisticAction(actionProbabilities)

    }
  }

  def convertActionValuesToActionProbabilities(
      actionValueEstimates: ActionValueEstimates
  ): ActionProbabilities = {

    actionValueEstimates.view
      .mapValues(x =>
        (exp(x / temperature)) / (actionValueEstimates.view
          .mapValues(y => exp(y / temperature))
          .toMap
          .values
          .sum)
      )
      .toMap

  }

  def chooseProbabilisticAction(
      actionProbabilities: ActionProbabilities
  ): Action = {

    val sortedActionProbabilities = ListMap(
      actionProbabilities.toSeq.sortBy(_._2): _*
    )
    val cumulativeActionProbabilities = getCumulativeSum(
      sortedActionProbabilities.toList.map(x => x._2)
    )
    val roll = util.Random.nextDouble()

    sortedActionProbabilities
      .take(
        cumulativeActionProbabilities
          .indexOf(
            cumulativeActionProbabilities.filter(x => x > roll).head
          ) + 1
      )
      .last
      ._1
  }

  def updateAgent(state: State) = SoftMaxAgent(temperature)

}

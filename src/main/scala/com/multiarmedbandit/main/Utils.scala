package utils

import simulation.Simulation._
import collection.mutable.ListBuffer

object Utils {

  def mean(state: State) =
    state.history.groupBy(action => action._1).map {
      case (key, value) =>
        key -> (value.map(_._2)).sum / (value.map(_._2).length)
    }

  def getMovingAverage(nums: List[Double]): List[Double] = {
    var sum: Double = nums(0)
    var movingAverageList = ListBuffer[Double](sum)

    for (i <- 1 to nums.length - 1) {
      sum += nums(i)
      movingAverageList += (sum / (i + 1))
    }
    movingAverageList.toList
  }

  def getCumulativeSum(nums: List[Double]): List[Double] = {
    var sum = 0.0
    var cumulativeSumList = ListBuffer[Double]()
    for (i <- 0 to nums.length - 1) {
      sum = sum + nums(i)
      cumulativeSumList += sum
    }
    cumulativeSumList.toList
  }

  def getDefaultActionProbabilities(state: State): ActionProbabilities = {
    (state.actionSpace zip List
      .tabulate(state.actionSpace.length)(x =>
        1 / state.actionSpace.length.toDouble
      )).toMap
  }

  def evaluateActions(
      actionValueMethod: State => ActionValues,
      state: State
  ): ActionValues = actionValueMethod(state)

  def getActionValueEstimates(
      actionValueMethod: State => ActionValueEstimates,
      state: State
  ): ActionValueEstimates = {

    val defaultEstimates =
      (state.actionSpace zip List.tabulate(state.actionSpace.length)(x =>
        0.0
      )).toMap

    val availableEstimates = actionValueMethod(state)

    (defaultEstimates -- availableEstimates.keySet) ++ availableEstimates

  }

}

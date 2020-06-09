package utils

import simulation.Simulation._
import collection.mutable.ListBuffer


object Utils {

    def mean(state: State) = state.history.transform((key, value) => value.sum/value.length)

    def getMovingAverage(nums: List[Reward]): List[Reward] = {
        var sum:Double = nums(0)
        var movingAverageList = ListBuffer[Reward](sum)

        for(i <- 1 to nums.length - 1){
            sum += nums(i)
            movingAverageList += (sum / (i + 1))
        }
        movingAverageList.toList
  }

}

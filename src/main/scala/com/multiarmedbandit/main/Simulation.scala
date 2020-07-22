package simulation

import scala.annotation.tailrec
import breeze.stats.distributions._
import agents._
import utils.Utils._
import java.time.LocalDateTime
import com.multiarmedbandit.model.{Result}

object Simulation {

  type Reward = Double

  case class Lever(id: String) {

    val meanPayout = Gaussian(0, 1.0).draw
    val payoutDistribution = Gaussian(meanPayout, 1.0)

    def pull(): Reward = payoutDistribution.draw

  }

  case class Action(lever: Lever)

  case class State(
      levers: List[Lever],
      actionSpace: List[Action],
      history: List[(Action, Reward)],
      time: Int = 1,
      numTrials: Int = 0
  )

  type ActionValues = Map[Action, Reward]
  type ActionValueEstimates = Map[Action, Reward]
  type ActionProbabilities = Map[Action, Double]

  def initializeLevers(numLevers: Int): List[Lever] =
    List.tabulate(numLevers)(id => Lever(id.toString))

  def initializeActionSpace(levers: List[Lever]): List[Action] =
    levers.map(lever => Action(lever))

  def initializeState(
      levers: List[Lever],
      actionSpace: List[Action],
      numTrials: Int
  ): State = {
    State(levers, actionSpace, List[(Action, Reward)](), numTrials = numTrials)
  }

  def runSimulation(agent: Agent, numRuns: Int, numTrials: Int): Result = {

    @tailrec
    def runTrial(
        agent: Agent,
        state: State
    ): List[Reward] = {

      if (state.time == numTrials) {

        state.history.map(reward => reward._2)

      } else {

        val action = agent.policy(state)
        val reward = action.lever.pull()

        val successorState = State(
          state.levers,
          state.actionSpace,
          state.history :+ (action, reward),
          state.time + 1,
          numTrials
        )

        val updatedAgent = agent.updateAgent(state)

        runTrial(updatedAgent, successorState)

      }
    }

    val runs = for (run <- 1 to numRuns) yield {

      val levers = initializeLevers(10)
      val actionSpace = initializeActionSpace(levers)
      runTrial(agent, initializeState(levers, actionSpace, numTrials))

    }

    Result(
      agent.getClass.getName,
      getMovingAverage(
        runs.transpose.map(result => result.sum / result.length).toList
      ).toList
    )

  }

}

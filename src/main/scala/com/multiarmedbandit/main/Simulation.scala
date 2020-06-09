package simulation

import scala.annotation.tailrec 
import breeze.stats.distributions._
import agents._
import utils.Utils._
import com.multiarmedbandit.model.{Result}

object Simulation {

    type Reward = Double

    case class Lever(id: String) {

        val meanPayout = Gaussian(0, 1.0).draw
        val payoutDistribution = Gaussian(meanPayout, 1.0)

        def pull(): Reward = payoutDistribution.draw

    }

    case class Action(lever: Lever)

    case class State(levers: List[Lever], actionSpace: List[Action], history: Map[Action, List[Reward]])

    type ActionValues = Map[Action, Double]

    def initializeLevers(numLevers: Int): List[Lever] = List.tabulate(numLevers)(id => Lever(id.toString))

    def initializeActionSpace(levers: List[Lever]): List[Action] = levers.map(lever => Action(lever))

    def initializeState(levers: List[Lever], actionSpace: List[Action]): State = {
        val emptyHistory = (actionSpace zip List.fill(actionSpace.length)(List[Reward]())).toMap 
        State(levers, actionSpace, emptyHistory)
    }

    def runSimulation(agent: Agent, numTrials: Int): Result = {

        val levers = initializeLevers(10)
        val actionSpace = initializeActionSpace(levers)
        
        @tailrec
        def runTrial(agent: Agent, state: State = initializeState(levers, actionSpace), currentTrial: Int = 1): Result = {
        
            if(currentTrial == numTrials){
        
                Result(agent.getClass.getName, getMovingAverage(state.history.values.toList.flatten))

            }
            else {

                val action = agent.policy(state)
                val reward = action.lever.pull() 
                
                val newState = State(levers,
                actionSpace,
                if(state.history.contains(action)) state.history + (action -> (state.history(action) ::: List(reward))) else state.history + (action -> List(reward))
                )

                runTrial(agent, newState, currentTrial = currentTrial + 1)

            }
        }

        runTrial(agent)

}


}
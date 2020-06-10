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

    case class State(levers: List[Lever], actionSpace: List[Action], history: Map[Action, List[Reward]], time: Int = 1, numTrials: Int = 0)

    type ActionValues = Map[Action, Double]

    def initializeLevers(numLevers: Int): List[Lever] = List.tabulate(numLevers)(id => Lever(id.toString))

    def initializeActionSpace(levers: List[Lever]): List[Action] = levers.map(lever => Action(lever))

    def initializeState(levers: List[Lever], actionSpace: List[Action], numTrials: Int): State = {
        val emptyHistory = (actionSpace zip List.fill(actionSpace.length)(List[Reward]())).toMap 
        State(levers, actionSpace, emptyHistory, numTrials = numTrials)
    }

    def updateAgent(agent: Agent): Agent = {
        agent match {
            case EpsilonGreedyAgent(_) => agent
            case EpsilonFirstAgent(_) => agent
            case EpsilonDecreasingAgent(_) => EpsilonDecreasingAgent(agent.epsilon/2)
        }
    }

    def runSimulation(agent: Agent, numTrials: Int): Result = {

        val levers = initializeLevers(10)
        val actionSpace = initializeActionSpace(levers)
        
        @tailrec
        def runTrial(agent: Agent, state: State = initializeState(levers, actionSpace, numTrials = numTrials)): Result = {
        
            if(state.time == numTrials){
        
                Result(agent.getClass.getName, getMovingAverage(state.history.values.toList.flatten))

            }
            else {

                val action = agent.policy(state)
                val reward = action.lever.pull() 
                
                val newState = State(
                    levers,
                    actionSpace,
                    if(state.history.contains(action)) state.history + (action -> (state.history(action) ::: List(reward))) else state.history + (action -> List(reward)),
                    state.time + 1,
                    numTrials
                )

                runTrial(updateAgent(agent), newState)

            }
        }

        runTrial(agent)

    }   

}

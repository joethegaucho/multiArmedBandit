import $ivy.`org.scalanlp::breeze:1.0`

import breeze.stats.distributions._
import scala.annotation.tailrec 
import scala.collection.mutable.ListBuffer

type Reward = Double

case class Result(agent: String, results: List[Reward])

case class Lever(id: String) {

    val meanPayout = Gaussian(0, 1.0).draw
    val payoutDistribution = Gaussian(meanPayout, 1.0)

    def pull(): Reward = payoutDistribution.draw

}


case class Action(lever: Lever)

case class State(levers: List[Lever], actionSpace: List[Action], history: Map[Action, List[Reward]])

type ActionValues = Map[Action, Double]

def initializeLevers(numLevers: Int): List[Lever] = List.tabulate(numLevers)(id => new Lever(id.toString))

def initializeActionSpace(levers: List[Lever]): List[Action] = levers.map(lever => Action(lever))

def mean(state: State) = state.history.transform((key, value) => value.sum/value.length)

def getMovingAverage(nums: List[Reward]): List[Reward] = {
    var sum:Reward = nums(0)
    var movingAverageList = ListBuffer[Reward](sum)

    for(i <- 1 to nums.length - 1){
        sum += nums(i)
        movingAverageList += (sum / (i + 1))
    }
    movingAverageList.toList
}


def initializeState(levers: List[Lever], actionSpace: List[Action]): State = {
    val emptyHistory = (actionSpace zip List.fill(actionSpace.length)(List[Reward]())).toMap 
    State(levers, actionSpace, emptyHistory)
}

trait Agent {

    def policy(state: State): Action
    def evaluateActions(actionValueMethod: State => ActionValues, state: State): ActionValues = actionValueMethod(state)
    
}

case class EpsilonGreedyAgent(val epsilon: Double) extends Agent {
    
    def policy(state: State): Action = {
        if(state.history.isEmpty || scala.util.Random.nextDouble < epsilon) {
            state.actionSpace(scala.util.Random.nextInt(state.actionSpace.length))
        } 
        else {

            val actionValues = evaluateActions(mean, state)
            actionValues.maxBy{case (key, value) => value}._1

        }
    }

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

val epsilonGreedyAgent = EpsilonGreedyAgent(0.1)


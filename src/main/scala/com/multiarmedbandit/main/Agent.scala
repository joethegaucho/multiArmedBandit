package agents

import simulation.Simulation._
import utils.Utils._

sealed trait Agent {

    def policy(state: State): Action
    def evaluateActions(actionValueMethod: State => ActionValues, state: State): ActionValues = actionValueMethod(state)
    
}

final case class EpsilonGreedyAgent(val epsilon: Double) extends Agent {
    
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
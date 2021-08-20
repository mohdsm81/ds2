package edu.utah.cs.gauss.ds2.core.schedulers.algorithms.generic

import edu.utah.cs.gauss.ds2.core.ir.datastructures.{Agent, DistributedSystem}

import scala.util.Random
/**
 * @author
 *        	Mohammed S. Al-Mahfoudh <p>
 * 		   	mahfoudh@cs.utah.edu <p>
 * 		   	Gauss Group - SoC <p>
 * 		   	The University of Utah <p>
 *
 */
class DelayBoundedScheduler( harnessssFilePath: String,
                             distribSys: DistributedSystem,
                             benchmark: Boolean = false,
                             log: Boolean = false,
                             delays: Int = 0,
                             iterationsLimit: Int = Int.MaxValue,
                             historySizeLimit: Int = 4048,
                             numOfSchedulesLimit: Int = Int.MaxValue
                           ) extends ExhaustiveDFSScheduler(
  harnessssFilePath,
  distribSys,
  benchmark,
  log,
  iterationsLimit,
  historySizeLimit,
  numOfSchedulesLimit
) {
  /*
  Zvon replied: each schedule delays are selected completely randomly while generating the schedule in the Exhaustive one.
   */

  currentState.dc = delays
  var wasDelayed = false
  var delayedAgent: Agent = targetAgents.head
  var delayCounter: Int = 0
  var random = new Random()

  def flipACoin: Boolean = Math.random < 0.20
  def randomNumOfDelays: Int = random.nextInt(currentState.dc + 1)

  override def getNext: Agent = {
    /*
    STEPS:
    - [X] flip a coin: for whether a delay is inserted or not
    - [X] flip another coin: for how many delays to insert (from remaining ones)
    - [x] store the 'delayedAgent' to execute
    - [x] perform the delay (i.e. skipping all those agents in the RoundRobin order)
    - [x] once the new 'current' is returned, does its thing,,, the next agent to
      execute is the previous-current followed by the rest of the round robin
      order (after the delay)
    - NOTE: the 'current' agent is stored globally, the 'delayedAgent' also should
      be, and there should be a 'wasDelayed' boolean that is global.
    - NOTE: Skipping a NOT enabled agent is NOT counted as a delay, so the 'dc' is
      not 'decremented'
     */

    if(!wasDelayed){
      if(flipACoin){ // try to delay
        delayedAgent = current
        delayCounter = randomNumOfDelays
        var counter = delayCounter // delayCounter to be passed to next state, while counter is used locally
        if(counter > 0) wasDelayed = true
        while(counter > 0 && items.exists(_.hasWork)){
          // count only if enabled (i.e. has a receive)
          if(agentHasReceive(current)) counter -= 1
          super.getNext
        }
      } else super.getNext // the common (normal) case
    } else{
      // schedule the delayed
      current = delayedAgent
      wasDelayed = false
    }

    current
  }

  override def push(state: DFSState): Unit = {
    super.push(state)
    // passing the new remaining delays quota to the next state
    if(wasDelayed) currentState.dc -= delayCounter
  }

} // end of DelayBoundedScheduler

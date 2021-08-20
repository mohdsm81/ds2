package edu.utah.cs.gauss.ds2.core.time
/**
 * @author
 *        	Mohammed S. Al-Mahfoudh <p>
 * 		   	mahfoudh@cs.utah.edu <p>
 * 		   	Gauss Group - SoC <p>
 * 		   	The University of Utah <p>
 *
 */
case class StopWatch(){

  private var startTime: Long = 0
  private var endTime: Long = 0
  private var started = false
  var accumulator: Long = 0

  def start: Unit = if(!started) {
    startTime = System.currentTimeMillis
    started = true
  }

  def stop: Unit =  if(started) {
    endTime = System.currentTimeMillis
    accumulator += endTime - startTime
    started = false
  }

  def reset: Unit = {
    startTime = 0
    endTime = 0
    started = false
    accumulator = 0
  }

  def isStarted: Boolean = started

  /**
   Calculates the difference between the last start and end times of the
   stopwatch and returns it.
   @return time in millis
   */
  def getTime: Long = if(!started) endTime - startTime else throw new Error("Can't get time from a running stopwatch, it has to stop first.")

  /**
   If start and stop were called multiple times, each time the timer is stopped its accumulator field is updated with the new timing.
   @return the accumulated time accross multiple stops of the stop watch
   */
  def getAccumulatedTime: Long = accumulator

  def copy: StopWatch = {
    val newOne = StopWatch()
    newOne.startTime = startTime
    newOne.endTime = endTime
    newOne.started = started
    newOne.accumulator = accumulator
    newOne
  }
}

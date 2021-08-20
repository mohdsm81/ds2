package edu.utah.cs.gauss.ds2.core.tracing

import edu.utah.cs.gauss.ds2.core.ir.datastructures.{Agent, Message}

/**
 *
 * @author <br>
 *         Mohammed S. Al-Mahfoudh <br/>
 *         mahfoudh@cs.utah.edu <br/>
 *         SoC - Gauss Group <br/>
 *
 */
object Logging {
  object AgentMessaging {
    // Logging
    def received(src: Agent, mm: Message, dst: Agent): Unit = println(s"RECEIVED:\t${dst.name}\t\t<--\t\t${src.name}\t$mm")

    def sent(src: Agent, mm: Message, dst: Agent): Unit = println(s"SENT:\t\t${src.name}\t\t-->\t\t${dst.name}\t\t$mm")
  }
}

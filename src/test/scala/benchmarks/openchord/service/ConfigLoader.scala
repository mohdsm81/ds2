package benchmarks.openchord.service

import com.typesafe.config.ConfigFactory
import com.typesafe.config.Config

object ConfigLoader {
  def load(host:String,port:Int):Config ={
    ConfigFactory.parseString("""
    akka {
    loglevel = "INFO"
    log-config-on-start = "off"
    debug {
    receive = on
    }
    actor {
    provider = "akka.remote.RemoteActorRefProvider"
    }
    remote {
    enabled-transports = ["akka.remote.netty.tcp"]
    netty.tcp {
    hostname = """+host +
    """
      port = """+port+
    """
    }
    log-sent-messages = on
    log-received-messages = on
   }
  }
  """)   
  }

}
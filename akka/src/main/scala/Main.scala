import akka.actor.Actor.Receive
import akka.actor.{Actor, Props, ActorSystem}
import com.typesafe.config.ConfigFactory

/**
 * Created by hokada on 3/9/15.
 */

class ServerActor extends Actor {
  override def receive: Receive = {
    case message =>
      println(message)
      sender ! s"hello, $message"
  }
}

class ClientActor extends Actor {
  override def receive: Receive = {
    case Message(msg) =>
      val remoteActorRef = context.actorSelection("akka.tcp://RemoteServerApp@127.0.0.1:2552/user/Receive")
      remoteActorRef ! msg
    case message => println(s"reply: $message")
  }
}

case class Message(msg: String)

object Server {
  def run() = {
    val config = ConfigFactory.load("conf/server.conf")
    val system = ActorSystem.apply("RemoteServerApp", config)
    val actor = system.actorOf(Props[ServerActor], "Receive")

    while(true) {
      actor ! io.StdIn.readLine()
    }
  }
}

object Client {
  def run() = {
    val config = ConfigFactory.load("conf/client.conf")
    val system = ActorSystem.apply("RemoteClientApp", config)
    val remoteActorRef = system.actorSelection("akka.tcp://RemoteServerApp@127.0.0.1:2552/user/Receive")
    val actor = system.actorOf(Props[ClientActor], "Receive")

    while(true) {
      actor ! Message(io.StdIn.readLine())
    }
  }
}

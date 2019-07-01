import akka.actor.{Actor, ActorRef, ActorSystem, Props}

class Server(bound : Int) extends Actor {

  val N = scala.util.Random.nextInt(bound)

  println(s"Guess my number from the interval [0..$bound] $N")

  def receive = {
    case Server.M(guess) => {
      if (guess == N) {sender ! Client.R("equal");}
      if (guess > N) { Thread.sleep(1000); sender ! Client.R("bigger")}
      if (guess < N) { Thread.sleep(1000); sender ! Client.R("lower")}
    }
    case x => throw new Exception(s"Server exception: $x")
  }
}

object Server {
  def props = Props[Server]
  case class M(res: Int)
}

class Client(name: String, server: ActorRef, bound: Int) extends Actor {

  println(s"$name starting")

  var a = 0
  var b = bound
  var guess = ((a + b) / 2)+a
  var Firstguess =  scala.util.Random.nextInt(bound)

  def receive = {
    case Client.R(msg) => msg match {
      case "equal" => println(s"$name. I guessed it! $guess");  context.system.terminate()
      case "bigger" =>
        b = guess
        guess =(a + b) /2
        println(s"$name. Response: too big. Trying: $guess")
        //Thread.sleep(1000)
        sender ! Server.M(guess)

      case "lower" =>
        a = guess
        guess = (a + b) /2
        println(s"$name. Response: too small. Trying: $guess")
        //Thread.sleep(1000)
        sender ! Server.M(guess)

    }
    case Client.Start => println(s"$name. Trying: $Firstguess"); server ! Server.M(Firstguess)
    case x => throw new Exception(s"Client exception: $x")
  }
}

object Client {
  def props = Props(classOf[Client], Main.server)
  case class R(response: String)
  case object Start
}

object Main extends App {
  val Bound = 100
  val Clients = 6
  val ourSystem = ActorSystem("system")
  val server: ActorRef = ourSystem.actorOf(Props(classOf[Server], Bound))
  val clients_list = for( i <- 1 until Clients) yield{
      ourSystem.actorOf(Props(classOf[Client], s"Client$i", server, Bound))}

  println("[info] Running Guess")
  for(client <- clients_list)
  { client ! Client.Start
  }
}

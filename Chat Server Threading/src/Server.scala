import java.io.{DataInputStream, DataOutputStream, IOException}
import java.net.{ServerSocket, UnknownHostException}
import scala.util.Random

class Server (port : Int){
  private var ip_users = 0
  private var m= Map[String,DataOutputStream]()

  try{
    val server= new ServerSocket(port)
    println("Server Started")
    println("Waiting for a client...")

    while(!server.isClosed && ip_users<1000){
      var temp_user =""
      val thread = new Thread( () =>{
        val socket = server.accept()
        val userPort = socket.getPort
        ip_users +=1
        val input= new DataInputStream(socket.getInputStream)
        val output = new DataOutputStream(socket.getOutputStream)

        val user = input.readUTF()
        println(s"$user joined with Port no : $userPort")

        val token= Password()
        val timeLimit= 60000 //new Random().between(2000*token.length,3000*token.length)
        output.writeUTF(s"Print the passcode : $token, time limit = ${timeLimit/1000} seconds")
        val time0= System.currentTimeMillis()
        val tokenPass = input.readUTF()

        val timeTaken= System.currentTimeMillis() - time0
        var line =""

        if(tokenPass==token && timeLimit>=timeTaken ) {
          output.writeUTF("SUCCESS")
          temp_user = user
          Thread.sleep(100)
          try {
            val available_users = m.keys.toList.reduce(_ + " " + _)
            output.writeUTF("Available users : " + available_users)
          }
          catch {
            case _: Throwable => output.writeUTF("No user is available now")
          }
          m += (temp_user -> output)
          for (i <- m.keys) m(i).writeUTF(s"Server:::all:::$user joined in the chat")
        }
        else {
          output.writeUTF("Failure")
          line="close"
        }
        while(isClose(line)) {
          try {
            line = input.readUTF()
            val decoded = decoding(line)
            if(decoded(1).toLowerCase()!="all") {
              try{
                m(decoded(1)).writeUTF(line)
                println(s"${decoded.head} sent a message to ${decoded(1)}")
              }
              catch{
                case _:Throwable => output.writeUTF(s"Server:::_:::${decoded(1)} is off! Try Later!")
              }
            }
            else{
              for(i <- m.keys) m(i).writeUTF(line)
              println(s"${decoded.head} broadcasts a message!!")
            }
            line = decoded.last

          }
          catch {
            case _: Throwable => println(s"$user got disconnected !!"); line = "close"
          }
        }
        println(s"$user has left")
        m -= user
        for(i <- m.keys) m(i).writeUTF(s"Server:::all:::$user has left the chat")
        println("Total users now : " + m.keys.toList.length + ", IP Users Created: "+ ip_users)
        socket.close()
        input.close()
      })
      thread.start()
      Thread.sleep(5000)
    }

  }
  catch {
    case t: UnknownHostException => println(t)
    case i: IOException =>println(i)
  }
  private def decoding(str:String):List[String]={
    str.split(":::").toList
  }

  private def isClose(x: String): Boolean={
    x.toLowerCase() match{
      case "close" | "exit" => false
      case _ => true
    }
  }
  private def Password():String={
    val charSet=('a' to 'z') ++ ('A' to 'Z') ++ "@#$!-"
    val random= new Random()
    val size=random.between(8,11)
    var str=""
    while(str.length<size){
      val index= random.nextInt(charSet.length)
      if(!str.contains(charSet(index))) str += charSet(index)
    }
    str
  }

}

object Server{

  def main(args: Array[String]): Unit = {
    new Server(8080)
  }
}

import java.io.{DataInputStream, DataOutputStream, IOException}
import java.net.{Socket, UnknownHostException}
import scala.io.StdIn.readLine
import scala.util.Random


class Client (user:String,address : String, port : Int) {
  private var ss: Socket = _
  private var input: DataInputStream = _
  private var output: DataOutputStream = _
  try {
    ss = new Socket(address, port)
    println("Connected")
    input = new DataInputStream(ss.getInputStream)
    output = new DataOutputStream(ss.getOutputStream)
  }
  catch {
    case t: UnknownHostException => println(t)
    case i: IOException => println(i)
  }

  output.writeUTF(user)
  println(input.readUTF()) // Passcode
  output.writeUTF(readLine("Enter the passcode : "))

  println("="*100)
  println(" "*47 + "Chat Started")
  println("="*100)
  private val code = input.readUTF()

  if(code=="SUCCESS") {
    println(input.readUTF())
    //    val readObject = new Object
    var line = ""
    val messages = scala.collection.mutable.Queue[String]()
    val outputThread = new Thread(() => {
      while (isClose(line)) {
        line = readLine("")
        output.writeUTF(encoding(line))
      }
      /*    if(!isClose(line)){
//      println("notice...")
      readObject.synchronized{readObject.notify()}
//      println("notified")
    }*/
    })
    outputThread.start()
    val inputThread = new Thread(() => {
      while (isClose(line)) {
        messages.enqueue(input.readUTF())
        while (messages.nonEmpty) {
          decoding(messages.front)
          messages.dequeue()
        }
      }
    })

    inputThread.start()
    inputThread.join()
    outputThread.join()
  }
  else println("Are you a bot?")
  /*  readObject.synchronized{
  //    println("I am waiting")
      readObject.wait()
  //    println("I am here")
    }*/

  try{
    input.close()
    //    println("input closed")
    output.close()
    //    println("output closed")
    ss.close()
    //    println("socket closed")
  }
  catch{
    case _: Throwable=> println("Closing Error")
  }
  println("Chat Again!!")


  private def isClose(x: String): Boolean={
    val y=x.toLowerCase()
    !(y== "close" || y=="exit" ||  y=="over")
  }

  def encoding(str: String):String ={
    val regex= "@\\w+".r
    try {
      user+":::"+regex.findFirstIn(str).get.filter(_!='@')+":::"+regex.split(str).filter(_!="").reduce(_ +":::" + _)
    }
    catch{
      case _ : Throwable => user+ ":::"+ "all"+":::"+ str
    }
  }

  def decoding(str:String):Unit={
    val Line = str.split(":::").toList
    if (Line(1) == user) println(s"[${Line.head}] : ${Line.last}")
    else if (Line(1).toLowerCase == "all" && Line.head!=user) println(s"[${Line.head}] : ${Line.last}")

  }

}

object Client {

  def main(args: Array[String]): Unit = {
    val username= readLine("Enter your username: ")



    // code here
    val IP= "localhost"
    new Client(username,IP, 8080)



    println("="*100)
    println(" "*47 + "Chat Ended")
    println("="*100)
  }


  //  private def Password():String={
  //    val charSet=('a' to 'z') ++ ('A' to 'Z') ++ "@#$!-"
  //    val random= new Random()
  //    val size=random.between(8,11)
  //    var str=""
  //    while(str.length<size){
  //      val index= random.nextInt(charSet.length)
  //      if(!str.contains(charSet(index))) str += charSet(index)
  //    }
  //    str
  //  }
}

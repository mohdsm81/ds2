package edu.utah.cs.gauss.serialization

import java.io._

import edu.utah.cs.gauss.ds2.core.ir.datastructures.{Agent, Message}
/**
 * @author  	Mohammed S. Al-Mahfoudh
 * 		   	mahfoudh@cs.utah.edu
 * 		   	Gauss Group - SoC
 * 		   	The University of Utah
 */
object IO {

  def saveObjToFile(obj: Any, file: File): Unit = {
    val fos = new FileOutputStream(file.getAbsolutePath)
    val oos = new ObjectOutputStream(fos)
    oos.writeObject(obj)
    oos.flush()
    oos.close()
    fos.close()
  }

  def writeLinesToFile(lines: Seq[String], file: File): Unit = {
    val bw = new BufferedWriter(new FileWriter(file))
    lines.map{x => bw.write(x + "\n")}
    bw.flush()
    bw.close()
  }

  def readLinesFromFile(file: File): Seq[String] = scala.io.Source.fromFile(file).getLines.toList

  def readObjFromFile(file: File): Option[Any] = {
    
    val fis = new FileInputStream(file.getAbsolutePath)
    val ois = new ObjectInputStream(fis)
    val ob = ois.readObject()
    ois.close()
    fis.close()

    if (ob != -1)
      Some(ob)
    else
      None
  }

  def readObjFromFileAs[T](file: File):T = {
    readObjFromFile(file) match{
      case Some(x) => x.asInstanceOf[T]
      case None => throw new Error(s"Can't parse object from file: ${file.getName}")
    }
  }

  def toBytes(obj: Any): Seq[Byte] = {
    val baos = new ByteArrayOutputStream() 
    val oos = new ObjectOutputStream(baos)
    oos.writeObject(obj)
    oos.flush()
    oos.close()
    val byteArr = baos.toByteArray()
    baos.flush()
    baos.close()
    byteArr
  }
  
  def fromBytes[T](bytes: Seq[Byte]): T = {
    fromBytes[T](bytes.toArray)
  }
  
  def fromBytes[T](bytes: Array[Byte]): T = {
    val bais = new ByteArrayInputStream(bytes)
    val ois = new ObjectInputStream(bais)
    val obj = ois.readObject()
    ois.close()
    bais.close()
    obj.asInstanceOf[T]
  }

  def deleteFile(filePath: String) = {
    val file = new File(filePath)
    if(file.exists)
      file.delete()
  }

  // def appendToFile(filePath: String, string: String ) = {
  //   val fw = new FileWriter(filePath,true)
  //   try{
  //     fw.write(string)
  //   } finally fw.close
  // }

  def appendToFile(filePath: String, lines: String*) = {
    // I re implemented it so that it doesn't open and close it too often
    val bw = new BufferedWriter(new FileWriter(filePath, true))
    try{
      lines.map{ x => bw.write( x + "\n")}
    } finally bw.close
  }

  def appendSeqToFile(filePath: String, lines: Seq[String]): Unit = lines.map{l => appendToFile(filePath,l)}
}

object IOTest extends App{

    import IO._
        
    println("awesome Scala")
    val code = (m:Message, a:Agent) => {println("GOOD!")}
    
    println(toBytes(code))
    
    fromBytes[Function2[Message,Agent,Unit]](toBytes(code))(new Message("hey"),new Agent("cool"))
//    
//    saveObjToFile(new Whatever, new File("output.dat"))
//    
//    println(readObjFromFile(new File("output.dat")).get)
}

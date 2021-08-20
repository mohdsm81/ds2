# Open Chord in Scala and Akka Actor
## Overview
<prev>Open_Chord_Scala is an implemenation of a peer-to-peer <a href="http://open-chord.sourceforge.net" target="_blank">Chord</a> distributed hash table using <a href="http://www.scala-lang.org" target="_blank">Scala</a> and <a href="http://akka.io" target="_blank">Akka Actor</a>. The detailed idea of distributed hash table is described in a MIT <a href="http://pdos.csail.mit.edu/papers/chord:sigcomm01/chord_sigcomm.pdf" target="_blank"> paper </a>.This project was built initially as one of the distributed system benchmarks to help with the DS2 lang research project in the <a href="http://formalverification.cs.utah.edu" target="_blank">Gauss Group</a> at the <a href="http://www.cs.utah.edu" target="_blank">SOC University of Utah.</a> The code is less and simpler thanks to an extremely advanced language Scala and the Akka Actor package, users can definely run this project to help understand the main idea of peer-to-peer distributed hash table.</prev>
##Run the project using <a href="http://www.scala-sbt.org" target="_blank">SBT</a> (Simple Build Tool)
 * Install SBT on your computer.
  * For mac, you can install sbt using homebrew, from terminal, run command line: 
   ```
     ->brew install sbt
   ```
  * For more instructions about installing SBT on any type of OS, visit <a href="http://www.scala-sbt.org/release/tutorial/Setup.html" target="_blank">this link</a>
 * Clone the project from repo: https://github.com/allenfromu/Open_Chord_Scala.git
 * Go to the Open_Chord_Scala directory from terminal and then run command: 
 ``` 
 -> sbt run
 ```
 * After that, you will be prompted to provide an Actor system name, an Actor name and a port number.
  <br> **For instance:**
```
    ➜  Open_Chord_Scala git:(master) sbt run
    [info] Loading global plugins from /Users/zepengzhao/.sbt/0.13/plugins
    [info] Set current project to Open_Chord_Scala (in build file:/Users/zepengzhao/hello/Open_Chord_Scala/)
    [info] Running service.Main 
    Actor System Name:America 
    Actor Name:Utah
    Port Number:2015
 ```
  <br>By providing the names and port number, the process you just created represents a unique node with a unique URL, like the example above, 
 <br>I had created a chord with a unique URL: **akka.tcp://America@192.168.137.3:2015/user/Utah**

 * After you create more than one chord, a chord can join in other chords to form a DHT. 
 <br>To join in a system from an existing node, from the console, type 'join'. And provide the detailed information of the existing node.
 <br>**Below is an example**
```
   >join
   Actor System Name:America
   Host Name:192.168.137.3
   Port Number:2015
   Actor Name:Utah
  >
```

 * More commands from the console for users to control the DHT.
  * To upload key-value pair from a chord to a system, type 'upload' from console.
  * To lookup the value of a given key, type 'lookup' from console.
  * To display the data in a current node, a serials of commands is provided as below.
    1. To look up entries(key-value pair) in a current node, type 'entries' from console
    2. To display the successor list, type 'SL' from console.
    3. To display the finger table, type 'FT' from console.
    4. To display the successor, type 'SUC' from console.
    5. To display the predecessor, type 'PRE' from console.
  
##Import the project to eclipse
 * Download <a href="http://www.eclipse.org" target="_blank">Eclipse</a> into your computer if you don't have it installed.
 * Add sbteclipse to your plugin definition file (or create one if doesn't exist). You can use either:
  * The global file (for version 0.13 and up) at ~/.sbt/0.13/plugins/plugins.sbt
  * The project-specific file at PROJECT_DIR/project/plugins.sbt
<br>For the latest version:
```
addSbtPlugin("com.typesafe.sbteclipse" % "sbteclipse-plugin" % "4.0.0")

```
 * In the Open_Chord_Scala directory, run command:

```
->sbt eclipse
```
 * From the eclipse IDE, import the project to workspace, File->Import->Existing Projects into Workspace.
 * Note you might need to add Akka package into eclipse, to do so:
  * Download the latest version of <a href="http://akka.io/downloads/" target="_blank"> Akka Actor</a>
  * In eclipse, right click the Open_Chord_Scala project,go Build Paths -> Add External Archives ...
 * Note that if you want to run it from eclipse, main function is in **src/main/scala/service/Main.scala**
 
##Fault Tolerance and Testing
 * For Testing I am running the following chords in a same computer.
  ```
   Below is the URL of the chords I run for fault tolerance testing
   
   Chord One:   akka.tcp://America@155.99.174.68:2015/user/Utah
   Chord Two:   akka.tcp://China@155.99.174.68:2016/user/Beijing
   Chord Three: akka.tcp://Australia@155.99.174.68:2017/user/Sydney
  ```
 * Here is the successor lists for each node
  * **Successor List for Chord One**
  ```
  >SL
  akka.tcp://China@155.99.174.68:2016/user/Beijing
  akka.tcp://Australia@155.99.174.68:2017/user/Sydney
  >
  ```
  * **Successor List for Chord Two**
  ```
  >SL
  akka.tcp://Australia@155.99.174.68:2017/user/Sydney
  akka.tcp://America@155.99.174.68:2015/user/Utah
  >
  ```
  * **Successor List for Chord Three**
  ```
  >SL
  akka.tcp://America@155.99.174.68:2015/user/Utah
  akka.tcp://China@155.99.174.68:2016/user/Beijing
  >
  ```
  **By Observation, you can see that those nodes form a ring:** Chord 1(America)->Chord 2(China)->Chord 3(Australia)->Chord 1(America)

* Now I do the following operation from the console of Chord 1.

 ```
  >upload
  Key:mp3
  value:Hey Jude
  >lookup
  Key:mp3
  Hey Jude
  >entries
  >
 ```
 Apparently, 'mp3'->'Hey Jude' key-value pair is not uploaded to the chord 1.
 
* Now lookup the entries table of Chord 3

 ```
 >entries
 key:mp3, value:Hey Jude
 >
 ```
 As far as what we can see, the key-value pair is uploaded to chord 3(Australia)
* Now I am going to kill chord 3 from the system, and its successor should backup its entries for it.
 * First, Chord 3 should be removed from the system, let's lookup the successor list of Chord 2(China)
   ```
   >SL
   akka.tcp://America@155.99.174.68:2015/user/Utah
   >
   ```
  Obviously, Australia is removed from the system.
 
  * How about the key-value entry? Let's look it up from console of Chord 2(China)
  ```
 >lookup
 Key:mp3
 Hey Jude
 >
 ```
 Great, it is still in the system.
  * Theoretically, this key-value pair should go to the successor of Australia, which is America, and yes, when I lookup the entries of America, I got this:
  ```
  >entries
  key:mp3, value:Hey Jude
  >
  ```
 
 
##Supports and further improvement.
This is project can't be done without the support of my supervisors and advisors, <a href="http://www.cs.utah.edu/~ganesh/" target="_blank">Ganesh Gopalakrishnan</a> and <a href="https://sites.google.com/site/mohammedmahfoudh/home" target="_blank"> Mohammed S. Al-Mahfoudh</a>. Also, thanks to the contribution from my partner Heath J. French.
<br>I am still working on it to make this project bettter, and I like to hear any advice to help make it better. Your advice is invaluable for me, please email me at allenzhaofromu@gmail.com if you think any aspect of the project can be made better.

name := "lightocr"
organization := "com.ruimo"
scalaVersion := "2.12.6"

publishTo := Some(
  Resolver.file(
    "lightocr",
    new File(Option(System.getenv("RELEASE_DIR")).getOrElse("/tmp"))
  )
)

resolvers += "ruimo.com" at "http://static.ruimo.com/release"

scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature")

libraryDependencies += "org.specs2" %% "specs2-core" % "4.3.3" % Test
libraryDependencies += "com.ruimo" %% "scoins" % "1.22"
libraryDependencies += "com.ruimo" %% "graphics" % "1.14"
libraryDependencies += "org.deeplearning4j" % "deeplearning4j-core" % "1.0.0-beta3"
libraryDependencies += "org.nd4j" % "nd4j-native-platform" % "1.0.0-beta3"
libraryDependencies += "com.twelvemonkeys.imageio" % "imageio-core" % "3.4.1"

enablePlugins(JavaAppPackaging)

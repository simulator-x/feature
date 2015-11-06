libraryDependencies ++= Seq(
	compilerPlugin("org.scala-lang.plugins" % ("scala-continuations-plugin_" + scalaVersion.value) % "1.0.2"),
	"com.thoughtworks.xstream" % "xstream" % "1.4.8",
	"xuggle" % "xuggle-xuggler" % "5.4", 
	"org.scalanlp" % "breeze_2.11" % "0.11.2", 
	"org.scalanlp" % "breeze-viz_2.11" % "0.11.2",
	"org.encog" % "encog-core" % "3.3.0"
)

scalaSource in Compile <<= baseDirectory(_ / "src")

javaSource in Compile <<= baseDirectory(_ / "src")

compileOrder := CompileOrder.JavaThenScala

unmanagedJars in Compile <<= baseDirectory map { base => ((base ** "lib") ** "*.jar").classpath }

resolvers += "xuggler" at "http://xuggle.googlecode.com/svn/trunk/repo/share/java/"

autoCompilerPlugins := true

scalacOptions += "-P:continuations:enable"

classDirectory in Compile <<= target(_ / "scala/classes")

classDirectory in Test <<= target(_ / "scala/test-classes")

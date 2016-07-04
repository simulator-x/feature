libraryDependencies ++= Seq(
	compilerPlugin("org.scala-lang.plugins" % ("scala-continuations-plugin_" + scalaVersion.value) % "1.0.2"),
	"com.thoughtworks.xstream" % "xstream" % "1.4.8" excludeAll ExclusionRule( organization = "ch.qos.logback"),
	"org.bytedeco" % "javacv" % "1.2",
	"org.bytedeco.javacpp-presets" % "ffmpeg" % "3.0.2-1.2", 
	"org.bytedeco.javacpp-presets" % "ffmpeg" % "3.0.2-1.2" classifier (sys.props("os.name").toLowerCase.replaceAll(" ", "") + "-x86_64"),
	"org.scalanlp" % "breeze_2.11" % "0.11.2" excludeAll ExclusionRule( organization = "ch.qos.logback") excludeAll ExclusionRule( organization = "org.slf4j"),
	"org.scalanlp" % "breeze-viz_2.11" % "0.11.2" excludeAll ExclusionRule( organization = "ch.qos.logback") excludeAll ExclusionRule( organization = "org.slf4j"),
	"org.encog" % "encog-core" % "3.3.0" excludeAll ExclusionRule( organization = "ch.qos.logback")
)

classpathTypes += "maven-plugin"

scalaSource in Compile <<= baseDirectory(_ / "src")

javaSource in Compile <<= baseDirectory(_ / "src")

compileOrder := CompileOrder.JavaThenScala

unmanagedJars in Compile <<= baseDirectory map { base => ((base ** "lib") ** "*.jar").classpath }

autoCompilerPlugins := true

scalacOptions += "-P:continuations:enable"

classDirectory in Compile <<= target(_ / "scala/classes")

classDirectory in Test <<= target(_ / "scala/test-classes")

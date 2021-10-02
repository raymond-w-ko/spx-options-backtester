JVM_ARGS := -J-Djdk.attach.allowAttachSelf
CLJ_EXTRA_SAFETY_ARGS := -J-Dclojure.core.async.go-checking=true

javac:
	clj -M:javac
repl:
	clojure $(JVM_ARGS) $(CLJ_EXTRA_SAFETY_ARGS) -M:repl
run:
	clojure $(JVM_ARGS) -J-Xmx16G -M:none -m app.core
upgrade-deps:
	clojure -M:outdated --upgrade

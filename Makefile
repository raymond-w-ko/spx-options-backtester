repl: rebel-repl
rebel-repl:
	exec clojure -M:repl/rebel
run:
	clojure -M:run
upgrade-deps:
	clojure -M:outdated --upgrade
javac:
	exec clojure -T:build javac

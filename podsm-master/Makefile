SOURCE_DIR = /home/beakerx/podsm/
JUPYTER_DIR ?= notebooks
FULL_JUPYTER_DIR = /home/beakerx/podsm/$(JUPYTER_DIR)

clean:
	rm -rf target

doc:
	lein codox
jupyter:
	docker build -t podsm-jupyter .
	docker run -v `pwd`/:/home/beakerx/podsm \
		-p 8888:8888 -w $(FULL_JUPYTER_DIR) podsm-jupyter

run-jvm:
	lein run
build-jvm:
	lein uberjar
execute-jvm:
	java -jar target/jvm/uberjar/podsm-0.1.0-SNAPSHOT-standalone.jar

clr-deps:
	./fix-clojure-clr-linking.sh
run-clr: clr-deps
	lein clr run -m podsm.core
build-clr: clr-deps
	lein clr compile :all
	cp target/clr/clj/Clojure.1.9.0-alpha15/all/net40/*.dll target/clr/bin
execute-clr: clr-deps
	mono target/clr/bin/podsm.core.exe

build: clean build-jvm build-clr

test:
	docker run -v `pwd`/:/home/beakerx/podsm podsm-jupyter lein test

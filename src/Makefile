

CHICKEN_WOLFRAM_PORT=31415

graph:
	cd test && csi -s hansei-graph.scm


hansei:
	cd test && csi -s test.scm
	cd test && csi -s hansei-base.scm
	cd test && csi -s hansei-variable-elimination.scm

install:
	chicken-install -sudo

format:
	scheme-indent -T 2 < wolfram.scm > wolfram.scm.tmp && mv wolfram.scm.tmp wolfram.scm
	scheme-indent -T 2 < test/test.scm > test/test.scm.tmp && mv test/test.scm.tmp test/test.scm
	scheme-indent -T 2 < test/hansei-base.scm > test/hansei-base.scm.tmp && mv test/hansei-base.scm.tmp test/hansei-base.scm
	scheme-indent -T 2 < test/hansei-variable-elimination.scm > test/hansei-variable-elimination.scm.tmp && mv test/hansei-variable-elimination.scm.tmp test/hansei-variable-elimination.scm

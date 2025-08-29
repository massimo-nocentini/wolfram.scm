

test:
	cd test && CHICKEN_WOLFRAM_HOST=localhost CHICKEN_WOLFRAM_PORT=8081 csi -s test.scm
	cd test && CHICKEN_WOLFRAM_HOST=localhost CHICKEN_WOLFRAM_PORT=8081 csi -s hansei-base.scm
	cd test && CHICKEN_WOLFRAM_HOST=localhost CHICKEN_WOLFRAM_PORT=8081 csi -s hansei-variable-elimination.scm

install:
	chicken-install -sudo

format:
	scheme-indent -T 2 < wolfram.scm > wolfram.scm.tmp && mv wolfram.scm.tmp wolfram.scm
	scheme-indent -T 2 < test/test.scm > test/test.scm.tmp && mv test/test.scm.tmp test/test.scm
	scheme-indent -T 2 < test/hansei-base.scm > test/hansei-base.scm.tmp && mv test/hansei-base.scm.tmp test/hansei-base.scm
	scheme-indent -T 2 < test/hansei-variable-elimination.scm > test/hansei-variable-elimination.scm.tmp && mv test/hansei-variable-elimination.scm.tmp test/hansei-variable-elimination.scm
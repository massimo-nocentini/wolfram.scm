

test:
	cd test && CHICKEN_WOLFRAM_PORT=8081 csi -s test.scm

install:
	chicken-install -sudo

format:
	scheme-indent -T 2 < wolfram.scm > wolfram.scm.tmp && mv wolfram.scm.tmp wolfram.scm
	scheme-indent -T 2 < test/test.scm > test/test.scm.tmp && mv test/test.scm.tmp test/test.scm
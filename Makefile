test:
	ml-build example.cm Example.main
	sml @SMLload=example.x86-darwin

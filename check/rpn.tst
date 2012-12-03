.data	256
.$($int = 4)
C:
.c "\nC:"
F:
.c "\nF:"
format:
.c "%3d "
newline:
.c "\n"

.code
	jmpi main

c2f:
	prolog
	arg $in

	allocai $(32 * $int) $index

	getarg %r2 $in

	// 32x9*5/+
	movi %r0 32

	// x9*5/+
	stxi_i $index %fp %r0
.	$($index = $index + $int)
	movr %r0 %r2

	// 9*5/+
	stxi_i $index %fp %r0
.	$($index = $index + $int)
	movi %r0 9

	// *5/+
.	$($index = $index - $int)
	ldxi_i %r1 %fp $index
	mulr %r0 %r1 %r0

	// 5/+
	stxi_i $index %fp %r0
.	$($index = $index + $int)
	movi %r0 5

	// /+
.	$($index = $index - $int)
	ldxi_i %r1 %fp $index
	divr %r0 %r1 %r0

	// +
.	$($index = $index - $int)
	ldxi_i %r1 %fp $index
	addr %r0 %r1 %r0

	retr %r0
	epilog

f2c:
	prolog
	arg $in

	allocai $(32 * $int) $index

	getarg %r2 $in

	// x32-5*9/
	stxi_i $index %fp %r0
.	$($index = $index + $int)
	movr %r0 %r2

	// 32-5*9/
	stxi_i $index %fp %r0
.	$($index = $index + $int)
	movi %r0 32

	// -5*9/
.	$($index = $index - $int)
	ldxi_i %r1 %fp $index
	subr %r0 %r1 %r0

	// 5*9/
	stxi_i $index %fp %r0
.	$($index = $index + $int)
	movi %r0 5

	// *9/
.	$($index = $index - $int)
	ldxi_i %r1 %fp $index
	mulr %r0 %r1 %r0

	// 9/
	stxi_i $index %fp %r0
.	$($index = $index + $int)
	movi %r0 9

	// /
.	$($index = $index - $int)
	ldxi_i %r1 %fp $index
	divr %r0 %r1 %r0

	retr %r0
	epilog

//-----------------------------------------------------------------------
main:
	prolog

	prepare 1
		pushargi C
	finishi @printf
	movi %v0 0
loopC:
	prepare 1
		pushargi format
		pushargr %v0
	finishi @printf
	addi %v0 %v0 10
	blei loopC %v0 100
	prepare 1
		pushargi F
	finishi @printf
	movi %v0 0
loopC2F:
	prepare 0
		pushargr %v0
	finishi c2f
	retval %r0
	prepare 2
		pushargi format
		pushargr %r0
	finishi @printf
	addi %v0 %v0 10
	blei loopC2F %v0 100
	prepare 1
		pushargi newline
	finishi @printf

	prepare 1
		pushargi F
	finishi @printf
	movi %v0 32
loopF:
	prepare 2
		pushargi format
		pushargr %v0
	finishi @printf
	addi %v0 %v0 18
	blei loopF %v0 212
	prepare 1
		pushargi C
	finishi @printf
	movi %v0 32
loopF2C:
	prepare 0
		pushargr %v0
	finishi f2c
	retval %r0
	prepare 2
		pushargi format
		pushargr %r0
	finishi @printf
	addi %v0 %v0 18
	blei loopF2C %v0 212
	prepare 1
		pushargi newline
	finishi @printf

	ret
	epilog

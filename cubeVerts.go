package main

var cubeVerts []float32 = []float32{
	+0.5, +0.5, +0.5, 1.0, 1.0, 1.0, +0.0, +1.0, +0.0,
	-0.5, +0.5, -0.5, 1.0, 1.0, 1.0, +0.0, +1.0, +0.0,
	+0.5, +0.5, -0.5, 1.0, 1.0, 1.0, +0.0, +1.0, +0.0,
	+0.5, +0.5, +0.5, 1.0, 1.0, 1.0, +0.0, +1.0, +0.0,
	-0.5, +0.5, +0.5, 1.0, 1.0, 1.0, +0.0, +1.0, +0.0,
	-0.5, +0.5, -0.5, 1.0, 1.0, 1.0, +0.0, +1.0, +0.0,

	//
	+0.5, -0.5, +0.5, 1.0, 1.0, 1.0, +0.0, -1.0, +0.0,
	-0.5, -0.5, -0.5, 1.0, 1.0, 1.0, +0.0, -1.0, +0.0,
	+0.5, -0.5, -0.5, 1.0, 1.0, 1.0, +0.0, -1.0, +0.0,
	+0.5, -0.5, +0.5, 1.0, 1.0, 1.0, +0.0, -1.0, +0.0,
	-0.5, -0.5, +0.5, 1.0, 1.0, 1.0, +0.0, -1.0, +0.0,
	-0.5, -0.5, -0.5, 1.0, 1.0, 1.0, +0.0, -1.0, +0.0,

	+0.5, -0.5, +0.5, 1.0, 1.0, 1.0, +0.0, +0.0, +1.0,
	-0.5, -0.5, +0.5, 1.0, 1.0, 1.0, +0.0, +0.0, +1.0,
	+0.5, +0.5, +0.5, 1.0, 1.0, 1.0, +0.0, +0.0, +1.0,
	+0.5, +0.5, +0.5, 1.0, 1.0, 1.0, +0.0, +0.0, +1.0,
	-0.5, +0.5, +0.5, 1.0, 1.0, 1.0, +0.0, +0.0, +1.0,
	-0.5, -0.5, +0.5, 1.0, 1.0, 1.0, +0.0, +0.0, +1.0,
	//
	+0.5, -0.5, -0.5, 1.0, 1.0, 1.0, +0.0, +0.0, -1.0,
	-0.5, -0.5, -0.5, 1.0, 1.0, 1.0, +0.0, +0.0, -1.0,
	+0.5, +0.5, -0.5, 1.0, 1.0, 1.0, +0.0, +0.0, -1.0,
	+0.5, +0.5, -0.5, 1.0, 1.0, 1.0, +0.0, +0.0, -1.0,
	-0.5, +0.5, -0.5, 1.0, 1.0, 1.0, +0.0, +0.0, -1.0,
	-0.5, -0.5, -0.5, 1.0, 1.0, 1.0, +0.0, +0.0, -1.0,

	+0.5, +0.5, +0.5, 1.0, 1.0, 1.0, +1.0, +0.0, +0.0,
	+0.5, -0.5, -0.5, 1.0, 1.0, 1.0, +1.0, +0.0, +0.0,
	+0.5, -0.5, +0.5, 1.0, 1.0, 1.0, +1.0, +0.0, +0.0,
	+0.5, +0.5, +0.5, 1.0, 1.0, 1.0, +1.0, +0.0, +0.0,
	+0.5, +0.5, -0.5, 1.0, 1.0, 1.0, +1.0, +0.0, +0.0,
	+0.5, -0.5, -0.5, 1.0, 1.0, 1.0, +1.0, +0.0, +0.0,
	//
	-0.5, +0.5, +0.5, 1.0, 1.0, 1.0, -1.0, +0.0, +0.0,
	-0.5, -0.5, -0.5, 1.0, 1.0, 1.0, -1.0, +0.0, +0.0,
	-0.5, -0.5, +0.5, 1.0, 1.0, 1.0, -1.0, +0.0, +0.0,
	-0.5, +0.5, +0.5, 1.0, 1.0, 1.0, -1.0, +0.0, +0.0,
	-0.5, +0.5, -0.5, 1.0, 1.0, 1.0, -1.0, +0.0, +0.0,
	-0.5, -0.5, -0.5, 1.0, 1.0, 1.0, -1.0, +0.0, +0.0,
}

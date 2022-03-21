package main

import "math"

func sin(x float32) float32 {
	return float32(math.Sin(float64(x)))
}

func cos(x float32) float32 {
	return float32(math.Cos(float64(x)))
}

func makeSphereVerts(n int) ([]float32, []uint16) {
	var verts []float32
	for x := 0; x <= n; x++ {
		for y := 0; y <= n; y++ {
			// Parameterized version of a sphere.
			// https://en.wikipedia.org/wiki/Sphere
			//
			// 0 <= theta <= pi && 0 <= phi < 2pi
			//
			// x = sin(theta) cos(phi)
			// y = sin(theta) sin(phi)
			// z = cos(theta)
			theta := (float32(x) / float32(n)) * math.Pi
			phi := (float32(y) / float32(n)) * 2 * math.Pi

			xPos := sin(theta) * cos(phi)
			yPos := sin(theta) * sin(phi)
			zPos := cos(theta)

			verts = append(verts,
				xPos, yPos, zPos, // Positions
				+1.0, +1.0, +1.0, // Colors
				xPos, yPos, zPos, // Normals
			)
		}
	}

	var indices []uint16
	for y := 0; y < n; y++ {
		if y%2 != 0 {
			for x := 0; x <= n; x++ {
				indices = append(indices,
					uint16(y*(n+1)+x), uint16((y+1)*(n+1)+x),
				)
			}
		} else {
			for x := n; n >= 0; n-- {
				indices = append(indices,
					uint16(y*(n+1)+x), uint16(y*(n+1)+x),
				)
			}
		}
	}

	return verts, indices
}

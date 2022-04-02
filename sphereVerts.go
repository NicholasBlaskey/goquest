package main

import (
	"math"
	"math/rand"

	mgl "github.com/go-gl/mathgl/mgl32"
)

func sin(x float32) float32 {
	return float32(math.Sin(float64(x)))
}

func cos(x float32) float32 {
	return float32(math.Cos(float64(x)))
}

func randPointOnSphere(radius float32) mgl.Vec3 {
	// 0 <= theta <= pi
	theta := rand.Float32() * math.Pi
	// 0 <= phi < 2pi
	phi := rand.Float32() * math.Pi * 2

	return thetaPhiToPos(theta, phi).Mul(radius)
}

func thetaPhiToPos(theta, phi float32) mgl.Vec3 {
	xPos := sin(theta) * cos(phi)
	yPos := sin(theta) * sin(phi)
	zPos := cos(theta)
	return mgl.Vec3{xPos, yPos, zPos}
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

			pos := thetaPhiToPos(theta, phi)

			verts = append(verts, pos[:]...)        // Pos
			verts = append(verts, +1.0, +1.0, +1.0) // Colors
			verts = append(verts, pos[:]...)        // Normals
			/*
				verts = append(verts,
					xPos, yPos, zPos, // Positions
					+1.0, +1.0, +1.0, // Colors
					xPos, yPos, zPos, // Normals
				)
			*/
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
			for x := n; x >= 0; x-- {
				indices = append(indices,
					uint16((y+1)*(n+1)+x), uint16(y*(n+1)+x),
				)
			}
		}
	}

	return verts, indices
}

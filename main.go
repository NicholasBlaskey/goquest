//go:build darwin || linux || windows
// +build android, gldebug
package main

// https://github.com/makepad/hello_quest/blob/master/build.sh

/*
#cgo CPPFLAGS: -I./Include -I/usr/local/include
#cgo LDFLAGS: -v -march=armv8-a -shared -L./lib/arm64-v8a/ -lvrapi -landroid

#include <android/native_activity.h>
#include <android/native_window_jni.h>

#include <EGL/egl.h>
#include <EGL/eglext.h>
*/
import "C"

import (
	"encoding/binary"
	"fmt"
	"log"
	"reflect"
	"runtime"
	"unsafe"

	//"github.com/go-gl/gl/v2.1/gl"
	"golang.org/x/mobile/app"

	//	"runtime"
	//"github.com/monzo/gomobile/app"

	//gl "github.com/go-gl/gl/v3.1/gles2"

	"golang.org/x/mobile/event/key"
	"golang.org/x/mobile/event/lifecycle"

	"golang.org/x/mobile/exp/f32"
	"golang.org/x/mobile/exp/gl/glutil"
	"golang.org/x/mobile/gl"
	//	internalApp "golang.org/x/mobile/internal/app"
	//internalApp "golang.org/x/mobile/internal/app"

	mgl "github.com/go-gl/mathgl/mgl32"

	"github.com/nicholasblaskey/vrapi"
	"github.com/nicholasblaskey/vrapi/ovrMatrix4f"
)

var glctx gl.Context
var vrctx vrapi.Context

func runApp(vrApp *App) error {
	// Default params
	params := vrapi.DefaultInitParms(vrApp.Java)
	fmt.Printf("params %+v\n", params)

	log.Println("After entering vr mode")
	if err := vrctx.Initialize(&params); err != nil {
		panic(err)
	}
	appEnterVRMode(vrApp)

	log.Println("Creating renderer")
	r, err := rendererCreate(vrApp)
	if err != nil {
		panic(err)
	}

	log.Println("Starting render loop")

	// Render loop
	for {
		vrApp.FrameIndex++

		displayTime := vrapi.GetPredictedDisplayTime(vrApp.OVR, vrApp.FrameIndex)
		tracking := vrapi.GetPredictedTracking2(vrApp.OVR, displayTime)

		layer := r.Render(tracking, displayTime)
		frame := &vrapi.OVRSubmitFrameDescription2{
			SwapInterval: 1,
			FrameIndex:   uint64(vrApp.FrameIndex),
			DisplayTime:  displayTime,
			LayerCount:   1,
			Layers:       []*vrapi.OVRLayerHeader2{&layer.Header},
		}

		vrctx.SubmitFrame2(vrApp.OVR, frame)
		if err := handleInput(vrApp, displayTime); err != nil {
			panic(err) // TODO
		}
	}

	return nil
}

func handleInput(vrApp *App, displayTime float64) error {
	vrApp.HandPosLeft, vrApp.OrientationLeft = nil, nil
	vrApp.HandPosRight, vrApp.OrientationRight = nil, nil

	i := uint32(0)
	var capability vrapi.OVRInputCapabilityHeader
	for vrapi.EnumerateInputDevices(vrApp.OVR, i, &capability) >= 0 {
		i++

		switch capability.Type {
		case vrapi.OVRControllerType_TrackedRemote:
			var inputState vrapi.OVRInputStateTrackedRemote
			inputState.Header.ControllerType = vrapi.OVRControllerType_TrackedRemote
			inputState.Header.TimeInSeconds = displayTime

			err := vrapi.GetCurrentInputState(vrApp.OVR,
				capability.DeviceID, &inputState.Header)
			if err != nil {
				panic(err)
			}
			//log.Printf("%+v", inputState)
		case vrapi.OVRControllerType_StandardPointer:
			// Get input state information.
			var inputState vrapi.OVRInputStateStandardPointer
			inputState.Header.ControllerType = vrapi.OVRControllerType_StandardPointer
			inputState.Header.TimeInSeconds = displayTime

			err := vrapi.GetCurrentInputState(vrApp.OVR,
				capability.DeviceID, &inputState.Header)
			if err != nil {
				panic(err)
			}

			// Get if left or right hand.
			var caps vrapi.OVRInputStandardPointerCapabilities
			caps.Header = capability
			err = vrapi.GetInputDeviceCapabilities(vrApp.OVR, &caps.Header)

			if err != nil {
				return nil
			}
			isLeft := caps.ControllerCapabilities&vrapi.OVRControllerCaps_LeftHand > 0

			// Update game state.
			if isLeft {
				vrApp.HandPosLeft = &inputState.GripPose.Position
				vrApp.OrientationLeft = &inputState.GripPose.Orientation
			} else {
				vrApp.HandPosRight = &inputState.GripPose.Position
				vrApp.OrientationRight = &inputState.GripPose.Orientation
			}
		default:
			//log.Printf("Unrecognized input device %+v", capability)
		}
	}

	return nil
}

type EGL struct {
	Display C.EGLDisplay
	Context C.EGLContext
	Surface C.EGLSurface
}

// https://www.khronos.org/registry/EGL/sdk/docs/man/html/eglGetError.xhtml
func eglError(code int) error {
	log.Printf("Checking error code %d\n", code)
	switch code {
	case C.EGL_SUCCESS:
		log.Println("Sucess?")
		return nil
	case C.EGL_NOT_INITIALIZED:
		return fmt.Errorf("EGL is not initialized, or could not be initialized, for the specified EGL display connection.")
	case C.EGL_BAD_ACCESS:
		return fmt.Errorf("EGL cannot access a requested resource (for example a context is bound in another thread).")
	case C.EGL_BAD_ALLOC:
		return fmt.Errorf("EGL failed to allocate resources for the requested operation.")
	case C.EGL_BAD_ATTRIBUTE:
		return fmt.Errorf("An unrecognized attribute or attribute value was passed in the attribute list.")
	case C.EGL_BAD_CONTEXT:
		return fmt.Errorf("An EGLContext argument does not name a valid EGL rendering context.")
	case C.EGL_BAD_CONFIG:
		return fmt.Errorf("An EGLConfig argument does not name a valid EGL frame buffer configuration.")

	case C.EGL_BAD_CURRENT_SURFACE:
		return fmt.Errorf("The current surface of the calling thread is a window, pixel buffer or pixmap that is no longer valid.")
	case C.EGL_BAD_DISPLAY:
		return fmt.Errorf("An EGLDisplay argument does not name a valid EGL display connection.")
	case C.EGL_BAD_SURFACE:
		return fmt.Errorf("An EGLSurface argument does not name a valid surface (window, pixel buffer or pixmap) configured for GL rendering.")
	case C.EGL_BAD_MATCH:
		return fmt.Errorf("Arguments are inconsistent (for example, a valid context requires buffers not supplied by a valid surface).")

	case C.EGL_BAD_PARAMETER:
		return fmt.Errorf("One or more argument values are invalid.")
	case C.EGL_BAD_NATIVE_PIXMAP:
		return fmt.Errorf("A NativePixmapType argument does not refer to a valid native pixmap.")
	case C.EGL_BAD_NATIVE_WINDOW:
		return fmt.Errorf("A NativeWindowType argument does not refer to a valid native window.")
	case C.EGL_CONTEXT_LOST:
		return fmt.Errorf("A power management event has occurred. The application must destroy all contexts and reinitialise OpenGL ES state and objects to continue rendering.")
	default:
		return fmt.Errorf("Did not recognize error")
	}
}

// TODO make this return errors and clean up logging.
func createEGL() (*EGL, error) {
	egl := &EGL{}
	log.Println("Creating egl context")
	log.Println("Getting egl display?")
	egl.Display = C.eglGetDisplay(C.EGL_DEFAULT_DISPLAY)
	log.Printf("eglDisplay %+v", C.EGL_DEFAULT_DISPLAY)
	if egl.Display == C.EGL_NO_DISPLAY {
		log.Printf("egl error %+v", eglError(int(C.eglGetError())))
		panic("could not create display")
	}

	log.Println("Initializing egl display")
	if C.eglInitialize(egl.Display, nil, nil) == C.EGL_FALSE {
		log.Printf("egl error %+v", eglError(int(C.eglGetError())))
		panic("Could not initialize egl display")
	}

	log.Println("Get number of EGL configs")
	numConfigs := C.EGLint(0)
	if C.eglGetConfigs(egl.Display, nil, 0, &numConfigs) == C.EGL_FALSE {
		log.Printf("egl error %+v", eglError(int(C.eglGetError())))
		panic("Could not get number of egl configs")
	}
	log.Printf("Found %d configss", numConfigs)

	log.Println("Allocating configs")
	configs := make([]C.EGLConfig, numConfigs)

	log.Println("getting EGL configs")
	if C.eglGetConfigs(egl.Display, &configs[0], numConfigs, &numConfigs) == C.EGL_FALSE {
		log.Printf("egl error %+v", eglError(int(C.eglGetError())))
		panic("Could not get EGL configs")
	}
	log.Println(configs)

	log.Println("Choose EGL config")
	var foundConfig C.EGLConfig
	// DepthSize of 8???
	configAttribs := []C.EGLint{
		C.EGL_RED_SIZE, 8, C.EGL_GREEN_SIZE, 8, C.EGL_BLUE_SIZE, 8,
		C.EGL_ALPHA_SIZE, 8, C.EGL_DEPTH_SIZE, 0, C.EGL_STENCIL_SIZE, 0,
		C.EGL_SAMPLES, 0,
	}
	for _, config := range configs {
		// Renderable type
		log.Println("Get EGL Config renderable type")
		renderableType := C.EGLint(0)
		if C.eglGetConfigAttrib(egl.Display, config, C.EGL_RENDERABLE_TYPE,
			&renderableType) == C.EGL_FALSE {
			log.Printf("egl error %+v", eglError(int(C.eglGetError())))
			panic("Cant get EGL config renderable type")
		}
		if renderableType&C.EGL_OPENGL_ES3_BIT_KHR == 0 {
			log.Println("Does not support EGL_OPENGL?")
			continue
		}

		// Surface type
		log.Println("get EGL config surface type")
		surfaceType := C.EGLint(0)
		if C.eglGetConfigAttrib(egl.Display, config, C.EGL_SURFACE_TYPE,
			&surfaceType) == C.EGL_FALSE {
			log.Printf("egl error %+v", eglError(int(C.eglGetError())))
			panic("Cant get EGL config surface  type")
		}
		if surfaceType&C.EGL_PBUFFER_BIT == 0 {
			log.Println("Does not support EGL_PBUFFER_BIT?")
			continue
		}
		if surfaceType&C.EGL_WINDOW_BIT == 0 {
			log.Println("Does not support EGL_WINDOW_BIT?")
			continue
		}

		log.Println("Past that continue")

		// Config attribs
		matches := true
		for i := 0; i < len(configAttribs); i += 2 {
			log.Println("Getting EGL config attrib")
			val := C.EGLint(0)
			if C.eglGetConfigAttrib(egl.Display, config,
				configAttribs[i], &val) == C.EGL_FALSE {
				log.Printf("egl error %+v", eglError(int(C.eglGetError())))
				panic("Cant get EGL config attrib")
			}
			if val != configAttribs[i+1] {
				log.Printf("Config attribs did not match i=%d expected %d got %d\n",
					i, configAttribs[i+1], val)
				matches = false
				break
			}
		}

		if matches {
			foundConfig = config
			log.Println("Found right config")
			break
		}

	}
	log.Printf("Found config %+v", foundConfig)

	// Do we need to free this??? configs???

	// Context creation
	log.Println("Creating egl context")
	contextAttribs := []C.EGLint{
		C.EGL_CONTEXT_CLIENT_VERSION, 3, C.EGL_NONE,
	}
	egl.Context = C.eglCreateContext(egl.Display, foundConfig,
		C.EGL_NO_CONTEXT, &contextAttribs[0])
	if egl.Context == C.EGL_NO_CONTEXT {
		log.Printf("egl error %+v", eglError(int(C.eglGetError())))
		panic("Cant create EGL context")
	}

	// Surface creation
	log.Println("Creating egl surface")
	surfaceAttribs := []C.EGLint{
		C.EGL_WIDTH, 16, C.EGL_HEIGHT, 16, C.EGL_NONE,
	}
	egl.Surface = C.eglCreatePbufferSurface(egl.Display, foundConfig, &surfaceAttribs[0])
	if egl.Surface == C.EGL_NO_SURFACE {
		log.Printf("egl error %+v", eglError(int(C.eglGetError())))
		panic("Cant create EGL pixel buffer surface")
	}

	// Make our context current
	log.Println("Making egl context current")
	if C.eglMakeCurrent(egl.Display, egl.Surface, egl.Surface, egl.Context) == C.EGL_FALSE {
		log.Printf("egl error %+v", eglError(int(C.eglGetError())))
		panic("Cant make EGL context current")
	}

	return egl, nil
}

type Renderer struct {
	VRApp        *App
	Width        int
	Height       int
	Framebuffers []*Framebuffer
	Program      *Program
	Heart        *Geometry
	Cube         *Geometry
	Sphere       *Geometry
	Spheres      []*Sphere
}

type Geometry struct {
	VertexArray  gl.VertexArray
	VertexBuffer gl.Buffer
	IndexBuffer  gl.Buffer
	N            int
}

type Sphere struct {
	Model                   mgl.Mat4
	IntersectsWithLeftHand  bool
	IntersectsWithRightHand bool
}

func (s *Sphere) HandIntersect(handPos mgl.Vec3, handOrient mgl.Quat, leftOrRight bool) {

}

func (g *Geometry) Draw() {
	glctx.BindVertexArray(g.VertexArray)
	if g.IndexBuffer.Value > 0 {
		//log.Println("Drawing sphere", g.N)
		glctx.DrawElements(gl.TRIANGLE_STRIP, g.N, gl.UNSIGNED_SHORT, 0)
		return
	}

	glctx.DrawArrays(gl.TRIANGLES, 0, g.N)
}

type Program struct {
	GLProgram        gl.Program
	UniformLocations map[string]gl.Uniform
}

func toByteSlice(s []uint16) []byte {
	h := (*reflect.SliceHeader)(unsafe.Pointer(&s))
	h.Len *= 2
	h.Cap *= 2
	return *(*[]byte)(unsafe.Pointer(h))
}

func makeGeometry(verts []float32, indices []uint16) *Geometry {
	g := &Geometry{N: len(verts) / 9} // TODO don't hard code attribs
	g.VertexArray = glctx.CreateVertexArray()
	glctx.BindVertexArray(g.VertexArray)

	g.VertexBuffer = glctx.CreateBuffer()
	glctx.BindBuffer(gl.ARRAY_BUFFER, g.VertexBuffer)
	glctx.BufferData(gl.ARRAY_BUFFER, f32.Bytes(binary.LittleEndian, verts...),
		gl.STATIC_DRAW)

	pos := gl.Attrib{Value: 0}
	glctx.EnableVertexAttribArray(pos)
	glctx.VertexAttribPointer(pos, 3, gl.FLOAT, false, 4*9, 0)
	col := gl.Attrib{Value: 1}
	glctx.EnableVertexAttribArray(col)
	glctx.VertexAttribPointer(col, 3, gl.FLOAT, false, 4*9, 4*3)
	norm := gl.Attrib{Value: 2}
	glctx.EnableVertexAttribArray(norm)
	glctx.VertexAttribPointer(norm, 3, gl.FLOAT, false, 4*9, 4*6)

	if indices != nil {
		g.IndexBuffer = glctx.CreateBuffer()
		g.N = len(indices)
		glctx.BindBuffer(gl.ELEMENT_ARRAY_BUFFER, g.IndexBuffer)
		glctx.BufferData(gl.ELEMENT_ARRAY_BUFFER, toByteSlice(indices),
			gl.STATIC_DRAW)
	}

	return g
}

func (r *Renderer) createGeometry() {
	r.Heart = makeGeometry(heartVerts, nil)
	r.Cube = makeGeometry(cubeVerts, nil)
	r.Sphere = makeGeometry(makeSphereVerts(32))
}

const vertexShader = `
#version 300 es
in vec3 aPosition;
in vec3 aColor;
in vec3 aNormal;
uniform mat4 uModelMatrix;
uniform mat4 uViewMatrix;
uniform mat4 uProjectionMatrix;
uniform mat4 uNormalMatrix;
out vec3 vColor;
out vec3 vNormal;
out vec3 vFragPos;
void main() {
	gl_Position = uProjectionMatrix * (uViewMatrix * (uModelMatrix * vec4(aPosition, 1.0)));
	vColor = aColor;
	vNormal = vec3(uNormalMatrix * vec4(aNormal, 1.0));
	vFragPos = vec3(uModelMatrix * vec4(aPosition, 1.0));
}
`

const fragmentShader = `
#version 300 es
in lowp vec3 vColor;
in lowp vec3 vNormal;
in lowp vec3 vFragPos;
out lowp vec4 outColor;

uniform vec3 uSolidColor;
uniform vec3 uViewPos;
uniform int uUseCheckerBoard;
void main() {
	vec3 col = vColor;
	if (uUseCheckerBoard == 1) {
		/*
		col.r = 0.0;
		col.b = 1.0;
		*/
		if (mod(floor(vFragPos[0]) + floor(vFragPos[2]), 2.0) == 0.0) {
			col = vec3(1.0, 1.0, 1.0);
		} else {
			col = vec3(0.3, 0.3, 0.3);
		}
	} else if (uUseCheckerBoard == 2) {
		col = uSolidColor;
	}

	vec3 ambient = 0.25 * col;
	//vec3 lightPos = vec3(-1.0, 1.0, -1.0);
	vec3 lightPos = uViewPos;
    vec3 lightColor = vec3(1.0, 1.0, 1.0);
	
	vec3 normalized = normalize(vNormal);
	vec3 lightDir = normalize(lightPos - vFragPos);
	float nDotL = max(dot(lightDir, normalized), 0.0);
	vec3 diffuse = lightColor * col * nDotL;

	// Specular
	vec3 viewDir = normalize(uViewPos - vFragPos);
	vec3 halfwayDir = normalize(lightDir + viewDir);
	float spec = pow(max(dot(normalized, halfwayDir), 0.0), 32.0);
	vec3 specular = vec3(0.3) * spec;

	outColor = vec4(diffuse + ambient + specular, 1.0);
	//outColor = vec4(1.0, 1.0, 1.0, 1.0);
	//outColor = vec4((viewDir + 1.0) / 2.0, 1.0); // Visualize viewDir
}
`

func (r *Renderer) createProgram() error {
	p, err := glutil.CreateProgram(glctx, vertexShader, fragmentShader)
	if err != nil {
		return err
	}

	r.Program = &Program{GLProgram: p}

	// Attribs (do something better)
	glctx.BindAttribLocation(p, gl.Attrib{Value: 0}, "aPosition")
	glctx.BindAttribLocation(p, gl.Attrib{Value: 1}, "aColor")
	glctx.BindAttribLocation(p, gl.Attrib{Value: 2}, "aNormal")

	// Uniforms (do something better)
	r.Program.UniformLocations = make(map[string]gl.Uniform)
	uniforms := []string{"uModelMatrix", "uViewMatrix", "uProjectionMatrix",
		"uNormalMatrix", "uViewPos", "uUseCheckerBoard", "uSolidColor"}
	for _, name := range uniforms {
		r.Program.UniformLocations[name] = glctx.GetUniformLocation(p, name)
	}

	return nil
}

func (r *Renderer) Render(tracking vrapi.OVRTracking2, dt float64) vrapi.OVRLayerProjection2 {
	// Create layer and attach tracking to it.
	layer := vrapi.DefaultLayerProjection2()
	layer.Header.Flags |= vrapi.FRAME_LAYER_FLAG_CHROMATIC_ABERRATION_CORRECTION
	layer.HeadPose = tracking.HeadPose

	// For each framebuffer
	for i, f := range r.Framebuffers {
		view := tracking.Eye[i].ViewMatrix.Transpose()
		projection := tracking.Eye[i].ProjectionMatrix.Transpose()

		// Attach framebuffer to texture
		layer.Textures[i].ColorSwapChain = f.ColorTextureSwapChain
		layer.Textures[i].SwapChainIndex = f.SwapChainIndex
		layer.Textures[i].TexCoordsFromTanAngles = ovrMatrix4f.TanAngleMatrixFromProjection(
			&tracking.Eye[i].ProjectionMatrix)

		// CULLFACE?
		glctx.Enable(gl.SCISSOR_TEST)
		glctx.Enable(gl.DEPTH_TEST)

		// Bind framebuffer and program we are going to use
		glctx.UseProgram(r.Program.GLProgram)
		glctx.BindFramebuffer(gl.DRAW_FRAMEBUFFER, f.Framebuffers[f.SwapChainIndex])

		// Clear
		glctx.Viewport(0, 0, f.Width, f.Height)
		glctx.Scissor(0, 0, int32(f.Width), int32(f.Height))
		glctx.ClearColor(0.15, 0.15, 0.15, 1.0)
		glctx.Clear(gl.COLOR_BUFFER_BIT | gl.DEPTH_BUFFER_BIT)
		{ // Per frame uniforms
			posX, posY, posZ := view[12], view[13], view[14] // Get camera position
			glctx.UniformMatrix4fv(r.Program.UniformLocations["uViewMatrix"], view[:])
			glctx.UniformMatrix4fv(r.Program.UniformLocations["uProjectionMatrix"], projection[:])
			glctx.Uniform3f(r.Program.UniformLocations["uViewPos"], posX, posY, posZ)
		}

		{ // Heart
			model := mgl.Translate3D(+0.3, 0.0, -0.2)
			scaleAmount := float32(0.01)
			model = model.Mul4(mgl.Scale3D(scaleAmount, scaleAmount, scaleAmount))
			normal := model.Inv().Transpose()

			glctx.Uniform1i(r.Program.UniformLocations["uUseCheckerBoard"], 0) // off
			glctx.UniformMatrix4fv(r.Program.UniformLocations["uModelMatrix"], model[:])
			glctx.UniformMatrix4fv(r.Program.UniformLocations["uNormalMatrix"], normal[:])

			r.Heart.Draw()
		}

		{ // Floor
			model := mgl.Translate3D(0.0, r.VRApp.FloorHeight, 0.0)
			//model.Mul4(mgl.HomogRotate3D
			model = model.Mul4(mgl.Scale3D(10.0, 0.1, 10.0))
			normal := model.Inv().Transpose()

			glctx.Uniform1i(r.Program.UniformLocations["uUseCheckerBoard"], 1) // on
			glctx.UniformMatrix4fv(r.Program.UniformLocations["uModelMatrix"], model[:])
			glctx.UniformMatrix4fv(r.Program.UniformLocations["uNormalMatrix"], normal[:])

			r.Cube.Draw()
		}

		for i := 0; i < 2; i++ { // Hands(s)
			var pos, col mgl.Vec3
			var orient mgl.Quat

			// Right
			if i == 0 && r.VRApp.HandPosRight == nil {
				continue
			} else if i == 0 {
				pos, orient = *r.VRApp.HandPosRight, *r.VRApp.OrientationRight
				col = mgl.Vec3{0.3, 0.3, 0.5}
			}

			// Left
			if i == 1 && r.VRApp.HandPosLeft == nil {
				continue
			} else if i == 1 {
				pos, orient = *r.VRApp.HandPosLeft, *r.VRApp.OrientationLeft
				col = mgl.Vec3{0.5, 0.5, 0.3}
			}

			rot := orient.Mat4()

			glctx.Uniform3f(r.Program.UniformLocations["uSolidColor"], col[0], col[1], col[2])
			glctx.Uniform1i(r.Program.UniformLocations["uUseCheckerBoard"], 2)

			{ // Cube hand holder
				model := mgl.Translate3D(pos[0], pos[1], pos[2])
				model = model.Mul4(rot)
				model = model.Mul4(mgl.Scale3D(0.1, 0.1, 0.1))
				normal := model.Inv().Transpose()

				glctx.UniformMatrix4fv(r.Program.UniformLocations["uNormalMatrix"], normal[:])
				glctx.UniformMatrix4fv(r.Program.UniformLocations["uModelMatrix"], model[:])

				r.Cube.Draw()
			}

			{ // Blade part of sword
				bladeLength := float32(0.35)
				v := rot.Mul4x1(mgl.Vec4{0.0, 0.0, -bladeLength, 0.0}).Vec3().Add(pos)
				model := mgl.Translate3D(v[0], v[1], v[2])
				model = model.Mul4(rot)
				model = model.Mul4(mgl.Scale3D(0.01, 0.01, bladeLength*2))
				normal := model.Inv().Transpose()

				glctx.UniformMatrix4fv(r.Program.UniformLocations["uNormalMatrix"], normal[:])
				glctx.UniformMatrix4fv(r.Program.UniformLocations["uModelMatrix"], model[:])

				r.Cube.Draw()
			}

			for _, s := range r.Spheres {
				s.HandIntersect(pos, orient, i == 0)
				s.HandIntersect(pos, orient, i == 1)
			}
		}

		{ // Spheres
			for _, s := range r.Spheres {
				model := s.Model
				normal := model.Inv().Transpose()

				if s.IntersectsWithLeftHand && s.IntersectsWithRightHand {

				} else if s.IntersectsWithLeftHand {

				} else if s.IntersectsWithRightHand {

				} else {
					glctx.Uniform1i(r.Program.UniformLocations["uUseCheckerBoard"], 0) // off

				}

				//glctx.Uniform1i(r.Program.UniformLocations["uUseCheckerBoard"], 0) // off

				glctx.UniformMatrix4fv(r.Program.UniformLocations["uModelMatrix"], model[:])
				glctx.UniformMatrix4fv(r.Program.UniformLocations["uNormalMatrix"], normal[:])

				r.Sphere.Draw()
			}
		}

		// Cleanup
		glctx.BindVertexArray(gl.VertexArray{0})
		glctx.Flush()
		glctx.BindFramebuffer(gl.DRAW_FRAMEBUFFER, gl.Framebuffer{0})
		glctx.Clear(gl.COLOR_BUFFER_BIT | gl.DEPTH_BUFFER_BIT)

		f.SwapChainIndex = (f.SwapChainIndex + 1) % int32(f.SwapChainLength)
	}

	return layer
}

type Framebuffer struct {
	SwapChainIndex        int32
	SwapChainLength       int
	Width                 int
	Height                int
	ColorTextureSwapChain *vrapi.OVRTextureSwapChain //C.ovrTextureSwapChain
	ColorTexture          []gl.Texture
	Renderbuffers         []gl.Renderbuffer
	Framebuffers          []gl.Framebuffer
}

// This needs to run on the mainthread, for
// vrapi_GetTextureSwapChain stuff
func rendererCreate(vrApp *App) (*Renderer, error) {
	r := &Renderer{VRApp: vrApp}
	r.Width = vrapi.GetSystemPropertyInt(
		vrApp.Java, vrapi.SYS_PROP_SUGGESTED_EYE_TEXTURE_WIDTH)
	r.Height = vrapi.GetSystemPropertyInt(
		vrApp.Java, vrapi.SYS_PROP_SUGGESTED_EYE_TEXTURE_HEIGHT)

	// Framebuffers
	if err := r.framebufferCreate(); err != nil {
		return nil, err
	}

	// Shaders
	log.Println("Started creating shaders")
	if err := r.createProgram(); err != nil {
		return nil, err
	}
	log.Println("Finished creating shaders")

	// Geometry
	log.Println("Started creating geometry")
	r.createGeometry()
	log.Println("Finished creating geometry")

	// Create geometry objects (TODO maybe better as vrApp but we will see)
	{
		model := mgl.Translate3D(0.0, 1.0, 0.0)
		scaleAmount := float32(0.5)
		model = model.Mul4(mgl.Scale3D(scaleAmount, scaleAmount, scaleAmount))
		r.Spheres = append(r.Spheres, &Sphere{Model: model})
	}

	return r, nil
}

func (r *Renderer) framebufferCreate() error {
	log.Println("rendered w=%d h=%d\n", r.Width, r.Height)

	// Create swapchain for each framebuffer
	for i := 0; i < vrapi.FRAME_LAYER_EYE_MAX; i++ {
		f := &Framebuffer{Width: r.Width, Height: r.Height}

		log.Println("Creating color texture swap chain")
		f.ColorTextureSwapChain = vrctx.CreateTextureSwapChain3(
			vrapi.TEXTURE_TYPE_2D, gl.RGBA8, f.Width, f.Height, 1, 3)
		if f.ColorTextureSwapChain == nil {
			log.Printf("egl error %+v", eglError(int(C.eglGetError())))
			return fmt.Errorf("Cant get color texture swap chain")
		}

		f.SwapChainLength = vrctx.GetTextureSwapChainLength(f.ColorTextureSwapChain)
		log.Printf("Length of SwapChain %d\n", f.SwapChainLength)

		for i := 0; i < f.SwapChainLength; i++ {
			handle := vrctx.GetTextureSwapChainHandle(f.ColorTextureSwapChain, i)
			f.ColorTexture = append(f.ColorTexture, gl.Texture{handle})
		}

		r.initializeFramebuffer(f)
		r.Framebuffers = append(r.Framebuffers, f)
	}

	return nil
}

func (r *Renderer) initializeFramebuffer(f *Framebuffer) *Framebuffer {
	// Create depth renderbuffers and framebuffers.
	for i := 0; i < f.SwapChainLength; i++ {
		log.Printf("renderbuffs%+v ctx%+v", glctx, glctx)
		f.Renderbuffers = append(f.Renderbuffers, glctx.CreateRenderbuffer())
		f.Framebuffers = append(f.Framebuffers, glctx.CreateFramebuffer())
	}

	log.Println(f.SwapChainLength)
	for i := 0; i < f.SwapChainLength; i++ {
		colorTexture := f.ColorTexture[i]
		glctx.BindTexture(gl.TEXTURE_2D, colorTexture)
		glctx.TexParameteri(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, gl.LINEAR)
		glctx.TexParameteri(gl.TEXTURE_2D, gl.TEXTURE_MAG_FILTER, gl.LINEAR)
		glctx.TexParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_S, gl.CLAMP_TO_EDGE)
		glctx.TexParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_T, gl.CLAMP_TO_EDGE)
		glctx.BindTexture(gl.TEXTURE_2D, gl.Texture{})

		glctx.BindRenderbuffer(gl.RENDERBUFFER, f.Renderbuffers[i])
		glctx.RenderbufferStorage(gl.RENDERBUFFER, gl.DEPTH_COMPONENT24, f.Width, f.Height)
		glctx.BindRenderbuffer(gl.RENDERBUFFER, gl.Renderbuffer{})

		log.Printf("Creating framebuffer %d", i)
		glctx.BindFramebuffer(gl.DRAW_FRAMEBUFFER, f.Framebuffers[i])
		glctx.FramebufferTexture2D(gl.DRAW_FRAMEBUFFER, gl.COLOR_ATTACHMENT0,
			gl.TEXTURE_2D, colorTexture, 0)
		glctx.FramebufferRenderbuffer(gl.DRAW_FRAMEBUFFER, gl.DEPTH_ATTACHMENT,
			gl.RENDERBUFFER, f.Renderbuffers[i])
		status := glctx.CheckFramebufferStatus(gl.DRAW_FRAMEBUFFER)
		if status != gl.FRAMEBUFFER_COMPLETE {
			panic("Can't create framebuffer")
		}
	}

	log.Println("Finished creating framebuffer")
	log.Println("Returning ", f)
	return f
}

type App struct {
	Java        *vrapi.OVRJava
	OVR         *vrapi.OVRMobile
	EGL         *EGL
	AndroidApp  app.App
	FrameIndex  int64
	GL          gl.Context
	Worker      gl.Worker
	FloorHeight float32
	//
	OrientationLeft  *mgl.Quat
	HandPosLeft      *mgl.Vec3
	OrientationRight *mgl.Quat
	HandPosRight     *mgl.Vec3
}

func appEnterVRMode(vrApp *App) {
	// Only enter for now, deal with leaving soon
	if vrApp.OVR == nil {
		log.Println("Entering vr mode")
		modeParams := vrapi.DefaultModeParms(vrApp.Java)

		// Idealy remove some of these ugly casts
		modeParams.Flags |= vrapi.MODE_FLAG_NATIVE_WINDOW
		modeParams.Flags &= ^vrapi.MODE_FLAG_RESET_WINDOW_FULLSCREEN
		modeParams.Display = uint64(vrApp.EGL.Display)
		modeParams.WindowSurface = uint64(uintptr(unsafe.Pointer(app.Window)))
		modeParams.ShareContext = uint64(uintptr(vrApp.EGL.Context))

		vrApp.OVR = vrctx.EnterVrMode(&modeParams)
		//vrApp.OVR = vrapi.EnterVrMode(&modeParams)
		log.Println("Is OVR nil???")
		log.Println(vrApp)
	}

}

// https://github.com/makepad/hello_quest/blob/master/src/main/cpp/hello_quest.c
func main() {
	log.Println("Calling func")
	//C.onNativeWindowCreated(nil, nil)
	log.Println("Calling wrap")
	//C.__wrap_onNativeWindowCreated(nil, nil)
	//onNativeWindowCreated(nil, nil)

	log.Println("Starting main")
	app.Main(func(a app.App) {
		vrApp := &App{AndroidApp: a, FloorHeight: -1.0}
		//vrApp.GL.ClearColor(0.0, 0.0, 0.0, 1.0)

		// Host vr and gl workers on mainthread.
		// TODO get rid of this EGL initialize and use the gomobile
		// initialize egl. (only issue is of course it has to be called from
		// the same thread?)
		waitForEGL := make(chan struct{})
		go app.RunOnJVM(func(vm, jniEnv, ctx uintptr) error {
			java := vrapi.CreateJavaObject(vm, jniEnv, ctx)
			vrApp.Java = &java

			// Init egl
			var err error
			log.Println("Initializing egl")
			vrApp.EGL, err = createEGL()
			if err != nil {
				return err
			}

			vrcontext, vrWorker := vrapi.NewContext()
			glcontext, glWorker := gl.NewContext()
			glctx, vrctx = glcontext, vrcontext

			//go runApp(vrApp)

			log.Println("Started listening for work")
			glWork := glWorker.WorkAvailable()
			vrWork := vrWorker.WorkAvailable()

			waitForEGL <- struct{}{}
			runtime.LockOSThread()
			for {
				select {
				case <-glWork:
					glWorker.DoWork()
				case <-vrWork:
					vrWorker.DoWork()
				}
			}
		})

		<-waitForEGL
		go runApp(vrApp) //

		log.Println("Starting")
		for e := range a.Events() {
			log.Printf("Event of type %T", e)
			switch e := a.Filter(e).(type) {

			case lifecycle.Event:
				log.Printf("Starting %+v", e)
				//vrApp.GL = initGL()
			case key.Event:
				if e.Direction == key.DirPress {
					dy := float32(0.1)
					if e.Code == key.CodeVolumeUp {
						vrApp.FloorHeight += dy
					} else {
						vrApp.FloorHeight -= dy
					}
				}
			}
		}
	})
}

//go:build darwin || linux || windows
// +build android, gldebug

// TODO above build tag?? Does not seem to be working?!!?
// An app that draws a green triangle on a red background.
//
// In order to build this program as an Android APK, using the gomobile tool.
//
// See http://godoc.org/golang.org/x/mobile/cmd/gomobile to install gomobile.
//
// Get the basic example and use gomobile to build or install it on your device.
//
//   $ go get -d golang.org/x/mobile/example/basic
//   $ gomobile build golang.org/x/mobile/example/basic # will build an APK
//
//   # plug your Android device to your computer or start an Android emulator.
//   # if you have adb installed on your machine, use gomobile install to
//   # build and deploy the APK to an Android target.
//   $ gomobile install golang.org/x/mobile/example/basic
//
// Switch to your device or emulator to start the Basic application from
// the launcher.
// You can also run the application on your desktop by running the command
// below. (Note: It currently doesn't work on Windows.)
//   $ go install golang.org/x/mobile/example/basic && basic
package main

// https://github.com/makepad/hello_quest/blob/master/build.sh

/*
#cgo CPPFLAGS: -I./Include -I/usr/local/include
#cgo LDFLAGS: -v -march=armv8-a -shared -L./lib/arm64-v8a/ -lvrapi -landroid

//-Wl --wrap=onNativeWindowCreated

#include <android/native_activity.h>

#include <android/native_window_jni.h>

#include <EGL/egl.h>
#include <EGL/eglext.h>
#include <VrApi.h>
#include <VrApi_Helpers.h>
#include <VrApi_Input.h>
#include <VrApi_SystemUtils.h>

#include <GLES3/gl3.h>

#include <stdio.h>
#include <stdlib.h>

// TODO why when we return
// ovrLayerHead2* does the vrAPI think the layer type is -175551239215???
// This is fine for now but further investigate because something
// really intresting seems to be going on here. Its really weird how its a negative
// number, like in the case of it somehow being intrepeted as the address of it
// it would be positive.
// Twos complmenet maybe? Somehow intrepeting as a signed value. Seems the number might be
// off?
void submitFrame(ovrMobile* ovr, ovrSubmitFrameDescription2* frame, ovrLayerProjection2 layer) {
//void addLayers(ovrMobile* ovr, ovrSubmitFrameDescription2* frame, ovrLayerCube2 layer) {
	const ovrLayerHeader2* layers[] = { &layer.Header };
	(*frame).Layers = layers;

	//printf("%p", *layers);
    vrapi_SubmitFrame2(ovr, frame);
}
*/
import "C"

import (
	"encoding/binary"
	"fmt"
	"log"
	"reflect"
	"runtime"
	"syscall"
	"time"
	"unsafe"

	//"github.com/go-gl/gl/v2.1/gl"
	"golang.org/x/mobile/app"

	//	"runtime"
	//"github.com/monzo/gomobile/app"

	//gl "github.com/go-gl/gl/v3.1/gles2"

	"golang.org/x/mobile/event/key"
	"golang.org/x/mobile/event/lifecycle"

	"golang.org/x/mobile/exp/app/debug"
	"golang.org/x/mobile/exp/f32"
	"golang.org/x/mobile/exp/gl/glutil"
	"golang.org/x/mobile/gl"
	//	internalApp "golang.org/x/mobile/internal/app"
	//internalApp "golang.org/x/mobile/internal/app"
)

/*
// DO we need to lock this???
func init() {
	runtime.LockOSThread()
}
*/

var (
	images *glutil.Images
	fps    *debug.FPS
	//program  gl.Program
	//position gl.Attrib
	//offset gl.Uniform
	//color  gl.Uniform
	//buf    gl.Buffer

	green  float32
	touchX float32
	touchY float32
)

var glctx gl.Context

/*
// So what we want is to actually lock this GL thread to here?
func init() {
	runtime.LockOSThread()
}
*/

var handPos []float32

func initGL() (gl.Context, gl.Worker) {
	//runtime.LockOSThread()
	//runtime.LockOSThread()

	//runtime.LockOSThread()

	log.Println("Initializing gl")
	glc, worker := gl.NewContext()

	/*
		workAvailable := worker.WorkAvailable()
		go func() {
			for {
				log.Println("Starting listen")
				select {
				case <-workAvailable:
					log.Println("Work ready")
					worker.DoWork()
					log.Println("Finishing work")
				}
			}
		}()
	*/

	return glc, worker
}

// export myWindow
func myWindow(activity *C.ANativeActivity, window *C.ANativeWindow) {
	for i := 0; i < 100; i++ {
		log.Println("my window", i)
	}
}

func initVRAPI(java *C.ovrJava, vrApp *App) func(vm, jniEnv, ctx uintptr) error {
	return func(vm, jniEnv, ctx uintptr) error {
		runtime.LockOSThread()

		// Create java object
		java.Vm = (*C.JavaVM)(unsafe.Pointer(vm))
		java.Env = (*C.JNIEnv)(unsafe.Pointer(jniEnv))
		java.ActivityObject = (C.jobject)(unsafe.Pointer(ctx))
		fmt.Printf("java %+v\n", java)

		// Default params
		params := C.vrapi_DefaultInitParms(java)
		fmt.Printf("params %+v\n", params)

		// Initialize api
		status := C.vrapi_Initialize(&params)
		fmt.Printf("status %+v\n", status)
		if status != C.VRAPI_INITIALIZE_SUCCESS {
			return fmt.Errorf("Could not initialize vr API with status %+v", status)
		}

		mainThreadID := syscall.Gettid()

		// Init egl
		var err error
		log.Println("Initializing egl")
		vrApp.EGL, err = createEGL()
		if err != nil {
			return err
		}

		// Enter VRMode
		appEnterVRMode(vrApp)

		// Init gl
		vrApp.GL, vrApp.Worker = initGL()
		glctx = vrApp.GL

		submitChan := make(chan int)
		var frame *C.ovrSubmitFrameDescription2
		var layer C.ovrLayerProjection2
		log.Println("After entering vr mode")

		var displayTime C.double
		var tracking C.ovrTracking2
		r := rendererCreate(vrApp)
		go func() {
			// Init renderer
			log.Println("Creating renderer")
			if err := r.rendererGLInit(); err != nil {
				panic(err)
				//return err
			}

			log.Println("Calling render loop")
			time.Sleep(1 * time.Second)
			log.Println("Starting render loop")
			for {
				//time.Sleep(10 * time.Millisecond)
				// Draw frame
				vrApp.FrameIndex++
				displayTime = C.vrapi_GetPredictedDisplayTime(vrApp.OVR,
					C.longlong(vrApp.FrameIndex))
				tracking = C.vrapi_GetPredictedTracking2(vrApp.OVR, displayTime)
				//log.Printf("tracking %+v\n", tracking)

				layer = r.Render(tracking, float32(displayTime))
				frame = &C.ovrSubmitFrameDescription2{}
				frame.Flags = 0
				frame.SwapInterval = 1
				frame.FrameIndex = C.ulong(vrApp.FrameIndex)
				frame.DisplayTime = displayTime
				frame.LayerCount = 1

				submitChan <- 0
			}
		}()

		log.Println(mainThreadID)
		runtime.LockOSThread()
		workAvailable := vrApp.Worker.WorkAvailable()
		for {
			select {
			case <-workAvailable:
				vrApp.Worker.DoWork()
			case <-submitChan:
				C.submitFrame(vrApp.OVR, frame, layer)
				frame = nil

				var capability C.ovrInputCapabilityHeader
				i := 0
				for C.vrapi_EnumerateInputDevices(vrApp.OVR, C.uint(i), &capability) >= 0 {
					/*
						log.Printf("%+v,----remote:%+v hand:%+v Standardpointer:%+v", capability,
							C.ovrControllerType_TrackedRemote, C.ovrControllerType_Hand,
							C.ovrControllerType_StandardPointer)
					*/

					if capability.Type == C.ovrControllerType_TrackedRemote && false {
						var inputState C.ovrInputStateTrackedRemote
						inputState.Header.ControllerType = C.ovrControllerType_TrackedRemote
						status := C.vrapi_GetCurrentInputState(vrApp.OVR,
							capability.DeviceID, &inputState.Header)
						//log.Printf("status %+v---%+v", status, C.ovrSuccess)
						if status == C.ovrSuccess {
							//log.Printf("%+v\n", inputState)
						}
					}

					if capability.Type == C.ovrControllerType_StandardPointer {
						// TODO are orientation of position capabilities set?
						var inputState C.ovrInputStateStandardPointer
						inputState.Header.ControllerType = C.ovrControllerType_StandardPointer
						r := C.vrapi_GetCurrentInputState(vrApp.OVR, capability.DeviceID,
							&inputState.Header)

						if r == C.ovrSuccess {
							//log.Printf("%+v\n", inputState.GripPose)
							handPose := inputState.GripPose

							//test := inputState.GripPose.
							handPos = nil
							for i := 0; i < 3; i++ {
								offset := unsafe.Sizeof(handPose) - 3*4 // vec3 so 4 fields of float32
								res := *(*float32)(unsafe.Add(
									unsafe.Pointer(&handPose), offset+uintptr(4*i)))
								handPos = append(handPos, res)
							}
							//log.Println(handPos)
						} else {
							log.Println("error", r)
						}
					}

					/*
						// Hand poise requires that to be enabled in settings???
						// I think we want something else though?
						// Since other games can have you pick things up?
						// Why are we not getting a C.ovrControllerType_Hand???
						// Think hand tracking is without the controller?
						// I think we want it with th econtroller?
						// Possible in trackedRemote???
						// Like we get 64 which isn't a number provided
						if capability.Type == C.ovrControllerType_Hand {
							log.Println("DEVICE?")

							// Setup handPose?
							var handPose C.ovrHandPose
							handPose.Header.Version = C.ovrHandVersion_1

							// Call? DeviceID here is the only main difference
							r := C.vrapi_GetHandPose(vrApp.OVR, capability.DeviceID,
								0, &(handPose.Header))
							if r != C.ovrSuccess {
								log.Println("ERROR getting hand pose", r)
							} else {
								log.Printf("hands %+v", handPose)
								panic("SuCESSFULLY")
							}
						}
					*/

					i++
				}

			}
		}

		return nil
	}
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
	Geometry     *Geometry
	GeometryCube *Geometry
}

type Geometry struct {
	VertexArray  gl.VertexArray
	VertexBuffer gl.Buffer
	IndexBuffer  gl.Buffer
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

func (r *Renderer) createGeometry() {
	/*
		vertices := []float32{
			-1.0, +1.0, -1.0, 1.0, 0.0, 1.0,
			+1.0, +1.0, -1.0, 0.0, 1.0, 0.0,
			+1.0, +1.0, +1.0, 0.0, 0.0, 1.0,
			-1.0, +1.0, +1.0, 1.0, 0.0, 0.0,
			-1.0, -1.0, -1.0, 0.0, 0.0, 1.0,
			-1.0, -1.0, +1.0, 0.0, 1.0, 0.0,
			+1.0, -1.0, +1.0, 1.0, 0.0, 1.0,
			+1.0, -1.0, -1.0, 1.0, 0.0, 0.0,
		}
		indices := []uint16{
			0, 2, 1, 2, 0, 3,
			4, 6, 5, 6, 4, 7,
			2, 6, 7, 7, 1, 2,
			0, 4, 5, 5, 3, 0,
			3, 5, 6, 6, 2, 3,
			0, 1, 7, 7, 4, 0,
		}
	*/

	// Heart
	{
		vertices := heartVerts
		r.Geometry = &Geometry{}
		log.Println("Bind here!?!?!?")
		r.Geometry.VertexArray = glctx.CreateVertexArray()
		log.Println("Post create?")
		glctx.BindVertexArray(r.Geometry.VertexArray)
		log.Println("Post bind?")

		log.Println("VertexBuffer")
		r.Geometry.VertexBuffer = glctx.CreateBuffer()
		glctx.BindBuffer(gl.ARRAY_BUFFER, r.Geometry.VertexBuffer)
		glctx.BufferData(gl.ARRAY_BUFFER, f32.Bytes(binary.LittleEndian, vertices...),
			gl.STATIC_DRAW)

		log.Println("attribs")
		pos := gl.Attrib{Value: 0}
		glctx.EnableVertexAttribArray(pos)
		glctx.VertexAttribPointer(pos, 3, gl.FLOAT, false, 4*9, 0)
		col := gl.Attrib{Value: 1}
		glctx.EnableVertexAttribArray(col)
		glctx.VertexAttribPointer(col, 3, gl.FLOAT, false, 4*9, 4*3)
		norm := gl.Attrib{Value: 2}
		glctx.EnableVertexAttribArray(norm)
		glctx.VertexAttribPointer(norm, 3, gl.FLOAT, false, 4*9, 4*6)
	}

	// Cube
	{
		vertices := cubeVerts
		r.GeometryCube = &Geometry{}
		log.Println("Bind here!?!?!?")
		r.GeometryCube.VertexArray = glctx.CreateVertexArray()
		log.Println("Post create?")
		glctx.BindVertexArray(r.GeometryCube.VertexArray)
		log.Println("Post bind?")

		log.Println("VertexBuffer")
		r.GeometryCube.VertexBuffer = glctx.CreateBuffer()
		glctx.BindBuffer(gl.ARRAY_BUFFER, r.GeometryCube.VertexBuffer)
		glctx.BufferData(gl.ARRAY_BUFFER, f32.Bytes(binary.LittleEndian, vertices...),
			gl.STATIC_DRAW)

		log.Println("attribs")
		pos := gl.Attrib{Value: 0}
		glctx.EnableVertexAttribArray(pos)
		glctx.VertexAttribPointer(pos, 3, gl.FLOAT, false, 4*9, 0)
		col := gl.Attrib{Value: 1}
		glctx.EnableVertexAttribArray(col)
		glctx.VertexAttribPointer(col, 3, gl.FLOAT, false, 4*9, 4*3)
		norm := gl.Attrib{Value: 2}
		glctx.EnableVertexAttribArray(norm)
		glctx.VertexAttribPointer(norm, 3, gl.FLOAT, false, 4*9, 4*6)
	}

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

/*
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
	vColor = aColor;
	vNormal = normalize(vec3(uNormalMatrix * vec4(aNormal, 1.0)));
	vFragPos = vec3(uModelMatrix * vec4(aPosition, 1.0));

	gl_Position = uProjectionMatrix * (uViewMatrix * (uModelMatrix * vec4(aPosition * 0.1, 1.0)));
}
`

const fragmentShader = `
#version 300 es

in lowp vec3 vColor;
in lowp vec3 vNormal;
in lowp vec3 vFragPos;

out lowp vec4 outColor;
void main() {
	outColor = vec4(1.0, 1.0, 1.0, 1.0);

	//vec3 ambient = vec3(0.5, 0.5, 0.5);
	vec3 ambient = vec3(0.2, 0.2, 0.2);
	vec3 lightPos = vec3(0.0, 1.0, 0.0);
    vec3 lightColor = vec3(1.0, 1.0, 1.0);

	vec3 normalized = normalize(vNormal);

	vec3 lightDir = normalize(lightPos - vFragPos);
	float nDotL = max(dot(lightDir, normalized), 0.0);
	vec3 diffuse = lightColor * vColor * nDotL;

	//outColor = vec4(1.0, 1.0, 1.0, 1.0);
	//outColor = vec4((vNormal + 1.0) / 2.0, 1.0);

	outColor = vec4(ambient + diffuse, 1.0);
}
`
*/

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
		"uNormalMatrix", "uViewPos", "uUseCheckerBoard"}
	for _, name := range uniforms {
		r.Program.UniformLocations[name] = glctx.GetUniformLocation(p, name)
	}

	return nil
}

func convertToFloat32(m C.ovrMatrix4f) []float32 {
	res := make([]float32, 16)
	for i := 0; i < 16; i++ {
		p := unsafe.Pointer(&m)
		res[i] = *(*float32)(unsafe.Add(p, uintptr(i)*4))
	}
	return res
}

func (r *Renderer) Render(tracking C.ovrTracking2, dt float32) C.ovrLayerProjection2 {
	// Create layer and tracking
	layer := C.vrapi_DefaultLayerProjection2()
	layer.Header.Flags |= C.VRAPI_FRAME_LAYER_FLAG_CHROMATIC_ABERRATION_CORRECTION
	layer.HeadPose = tracking.HeadPose

	// For each framebuffer
	for i, f := range r.Framebuffers {
		view := convertToFloat32(C.ovrMatrix4f_Transpose(&tracking.Eye[i].ViewMatrix))
		projection := convertToFloat32(C.ovrMatrix4f_Transpose(&tracking.Eye[i].ProjectionMatrix))

		// Attach framebuffer to texture???
		layer.Textures[i].ColorSwapChain = f.ColorTextureSwapChain
		layer.Textures[i].SwapChainIndex = C.int(f.SwapChainIndex)
		layer.Textures[i].TexCoordsFromTanAngles = C.ovrMatrix4f_TanAngleMatrixFromProjection(
			&tracking.Eye[i].ProjectionMatrix)

		// Bind framebuffer
		glctx.BindFramebuffer(gl.DRAW_FRAMEBUFFER, f.Framebuffers[f.SwapChainIndex])

		// Enable gl stuff
		// TODO specify the geometry such that vertex winding is correct
		//glctx.Enable(gl.CULL_FACE)
		glctx.Enable(gl.SCISSOR_TEST)
		glctx.Enable(gl.DEPTH_TEST)

		// viewport, scissor, set color
		glctx.Viewport(0, 0, f.Width, f.Height)
		// Why this int32 while viewport int???
		glctx.Scissor(0, 0, int32(f.Width), int32(f.Height))

		glctx.ClearColor(0.15, 0.15, 0.15, 1.0)
		glctx.Clear(gl.COLOR_BUFFER_BIT | gl.DEPTH_BUFFER_BIT)

		glctx.UseProgram(r.Program.GLProgram)

		// Model
		modelC := C.ovrMatrix4f_CreateTranslation(+0.3, 0.0, -0.2)
		rot := C.ovrMatrix4f_CreateRotation(0.0, 0.0, 0.0)
		scaleAmount := C.float(0.01)
		scale := C.ovrMatrix4f_CreateScale(scaleAmount, scaleAmount, scaleAmount)
		modelC = C.ovrMatrix4f_Multiply(&modelC, &rot)
		modelC = C.ovrMatrix4f_Multiply(&modelC, &scale)
		model := convertToFloat32(C.ovrMatrix4f_Transpose(&modelC))

		// Normal
		normalC := C.ovrMatrix4f_Transpose(&modelC)
		normalC = C.ovrMatrix4f_Inverse(&normalC)
		normal := convertToFloat32(C.ovrMatrix4f_Transpose(&normalC))
		//normal := convertToFloat32(C.ovrMatrix4f_CreateIdentity())

		posX, posY, posZ := view[12], view[13], view[14] // Get camera position
		glctx.UniformMatrix4fv(r.Program.UniformLocations["uViewMatrix"], view)
		glctx.UniformMatrix4fv(r.Program.UniformLocations["uProjectionMatrix"], projection)
		glctx.UniformMatrix4fv(r.Program.UniformLocations["uNormalMatrix"], normal)
		glctx.Uniform3f(r.Program.UniformLocations["uViewPos"], posX, posY, posZ)
		glctx.Uniform1i(r.Program.UniformLocations["uUseCheckerBoard"], 0) // off

		// Heart
		glctx.UniformMatrix4fv(r.Program.UniformLocations["uModelMatrix"], model)
		glctx.BindVertexArray(r.Geometry.VertexArray)
		glctx.DrawArrays(gl.TRIANGLES, 0, len(heartVerts)/9)
		//glctx.DrawElements(gl.TRIANGLES, len(heartIndices), gl.UNSIGNED_SHORT, 0)

		// Floor
		{
			// scale * translate???
			// when it should be translate * scale?
			glctx.Uniform1i(r.Program.UniformLocations["uUseCheckerBoard"], 1) // on

			//log.Println("Floor height", C.float(r.VRApp.FloorHeight))
			//rot := C.ovrMatrix4f_CreateRotation(math.Pi/4.0, math.Pi/4.0, math.Pi/4.0)
			modelC := C.ovrMatrix4f_CreateTranslation(0.0, C.float(r.VRApp.FloorHeight), 0.0)
			//modelC := C.ovrMatrix4f_CreateTranslation(10.0, C.float(r.VRApp.FloorHeight), 0.0)
			scale := C.ovrMatrix4f_CreateScale(10.0, 0.1, 10.0)
			//modelC = C.ovrMatrix4f_Multiply(&modelC, &rot)
			modelC = C.ovrMatrix4f_Multiply(&scale, &modelC)
			model := convertToFloat32(C.ovrMatrix4f_Transpose(&modelC))
			glctx.UniformMatrix4fv(r.Program.UniformLocations["uModelMatrix"], model)
			glctx.BindVertexArray(r.GeometryCube.VertexArray)
			glctx.DrawArrays(gl.TRIANGLES, 0, len(cubeVerts)/9)
		}

		// Hand(s)
		{
			if handPos == nil {
				handPos = []float32{0.0, 0.0, 0.0}
			}

			glctx.Uniform1i(r.Program.UniformLocations["uUseCheckerBoard"], 0) // off
			modelC := C.ovrMatrix4f_CreateTranslation(
				C.float(handPos[0]), C.float(handPos[1]), C.float(handPos[2]))
			scale := C.ovrMatrix4f_CreateScale(0.1, 0.1, 0.1)
			modelC = C.ovrMatrix4f_Multiply(&modelC, &scale)
			//modelC = C.ovrMatrix4f_Multiply(&scale, &modelC)
			model := convertToFloat32(C.ovrMatrix4f_Transpose(&modelC))

			glctx.UniformMatrix4fv(r.Program.UniformLocations["uModelMatrix"], model)
			glctx.BindVertexArray(r.GeometryCube.VertexArray)
			glctx.DrawArrays(gl.TRIANGLES, 0, len(cubeVerts)/9)
		}

		// Cleanup
		glctx.BindVertexArray(gl.VertexArray{0})
		//glctx.UseProgram(gl.Program{Value: 0})
		glctx.Flush()
		glctx.BindFramebuffer(gl.DRAW_FRAMEBUFFER, gl.Framebuffer{0})
		glctx.Clear(gl.COLOR_BUFFER_BIT | gl.DEPTH_BUFFER_BIT)

		f.SwapChainIndex = (f.SwapChainIndex + 1) % f.SwapChainLength
	}

	return layer
}

type Framebuffer struct {
	SwapChainIndex        int
	SwapChainLength       int
	Width                 int
	Height                int
	ColorTextureSwapChain *C.ovrTextureSwapChain
	ColorTexture          []uint32
	Renderbuffers         []gl.Renderbuffer
	Framebuffers          []gl.Framebuffer
}

func (r *Renderer) rendererGLInit() error {
	// Framebuffers
	for i := 0; i < int(C.VRAPI_FRAME_LAYER_EYE_MAX); i++ {
		r.FramebufferCreate(r.Framebuffers[i])
		log.Printf("Buffer create %d\n", i)
		//r.Framebuffers = append(r.Framebuffers, r.FramebufferCreate())
	}

	// Shaders
	log.Println("Started creating shaders")
	if err := r.createProgram(); err != nil {
		return err
	}
	log.Println("Finished creating shaders")

	// Geometry
	log.Println("Started creating geometry")
	r.createGeometry()
	log.Println("Finished creating geometry")

	return nil
}

// This needs to run on the mainthread, for
// vrapi_GetTextureSwapChain stuff
func rendererCreate(vrApp *App) *Renderer {
	r := &Renderer{VRApp: vrApp}
	r.Width = int(C.vrapi_GetSystemPropertyInt(
		vrApp.Java, C.VRAPI_SYS_PROP_SUGGESTED_EYE_TEXTURE_WIDTH))
	r.Height = int(C.vrapi_GetSystemPropertyInt(
		vrApp.Java, C.VRAPI_SYS_PROP_SUGGESTED_EYE_TEXTURE_HEIGHT))
	log.Println("rendered w=%d h=%d\n", r.Width, r.Height)

	// Create swapchain for each framebuffer
	for i := 0; i < C.VRAPI_FRAME_LAYER_EYE_MAX; i++ {
		f := &Framebuffer{Width: r.Width, Height: r.Height}

		log.Println("Creating color texture swap chain")
		f.ColorTextureSwapChain = C.vrapi_CreateTextureSwapChain3(
			C.VRAPI_TEXTURE_TYPE_2D, gl.RGBA8, C.int(f.Width), C.int(f.Height), 1, 3)

		if f.ColorTextureSwapChain == nil {
			log.Printf("egl error %+v", eglError(int(C.eglGetError())))
			panic("Cant get color texture swap chain")
		}

		f.SwapChainLength = int(C.vrapi_GetTextureSwapChainLength(f.ColorTextureSwapChain))
		log.Printf("Length of SwapChain %d\n", f.SwapChainLength)

		log.Println(f.SwapChainLength)
		for i := 0; i < f.SwapChainLength; i++ {
			colorTextureC := C.vrapi_GetTextureSwapChainHandle(
				f.ColorTextureSwapChain, C.int(i))
			log.Println("COLOR TEXTURE C", colorTextureC, f.ColorTextureSwapChain)
			f.ColorTexture = append(f.ColorTexture, uint32(colorTextureC))
		}

		r.Framebuffers = append(r.Framebuffers, f)
	}
	return r
}

func (r *Renderer) FramebufferCreate(f *Framebuffer) *Framebuffer {
	// Create depth renderbuffers and framebuffers
	for i := 0; i < f.SwapChainLength; i++ {
		log.Printf("renderbuffs%+v ctx%+v", glctx, glctx)
		f.Renderbuffers = append(f.Renderbuffers, glctx.CreateRenderbuffer())
		f.Framebuffers = append(f.Framebuffers, glctx.CreateFramebuffer())
	}

	log.Println(f.SwapChainLength)
	for i := 0; i < f.SwapChainLength; i++ {
		colorTexture := gl.Texture{uint32(f.ColorTexture[i])}
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
	Java        *C.ovrJava
	OVR         *C.ovrMobile
	EGL         *EGL
	AndroidApp  app.App
	FrameIndex  int
	GL          gl.Context
	Worker      gl.Worker
	FloorHeight float32
}

func appEnterVRMode(vrApp *App) {
	// Only enter for now, deal with leaving soon
	if vrApp.OVR == nil {
		/*
			modeParams := C.vrapi_DefaultModeParms(vrApp.Java)
			res := C.tryThis(modeParams, vrApp.EGL.Display, vrApp.EGL.Context,
				(*C.ANativeWindow)(unsafe.Pointer(app.Window)))
			log.Println(res)
		*/

		log.Println("Entering vr mode")
		modeParams := C.vrapi_DefaultModeParms(vrApp.Java)
		//modeParams.Flags |= C.VRAPI_MODE_FLAG_FRONT_BUFFER_SRGB
		modeParams.Flags |= C.VRAPI_MODE_FLAG_NATIVE_WINDOW

		// TODO figure this line out.
		modeParams.Flags &= ^C.uint(C.VRAPI_MODE_FLAG_RESET_WINDOW_FULLSCREEN)

		log.Println(app.Window)
		modeParams.Display = C.ulonglong(vrApp.EGL.Display)
		modeParams.WindowSurface = C.ulonglong(uintptr(unsafe.Pointer(app.Window)))
		modeParams.ShareContext = C.ulonglong(uintptr(vrApp.EGL.Context))

		log.Printf("modeParams %+v\n", modeParams)
		log.Printf("Display %+v %+v\n", modeParams.Display, C.eglGetDisplay(C.EGL_DEFAULT_DISPLAY))
		log.Printf("shareContext %+v", vrApp.EGL.Context)
		log.Printf("aNativeWindow %p\n", app.Window)
		log.Printf("eglGetCurrentSurface(EGL_DRAW) = %p\n", C.eglGetCurrentSurface(C.EGL_DRAW))

		vrApp.OVR = C.vrapi_EnterVrMode(&modeParams)
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
		vrApp := &App{Java: &C.ovrJava{}, AndroidApp: a, FloorHeight: -11.6}
		//vrApp.GL.ClearColor(0.0, 0.0, 0.0, 1.0)

		time.Sleep(1 * time.Second)
		log.Println("After init gl")

		// Init vr api
		log.Println("Initializing vrapi")

		go func() {
			err := app.RunOnJVM(initVRAPI(vrApp.Java, vrApp))
			if err != nil {
				panic(err)
			}
		}()

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

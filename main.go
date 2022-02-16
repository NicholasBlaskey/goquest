//go:build darwin || linux || windows
// +build gldebug
// +build android

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
	"math"
	"reflect"
	"runtime"
	"time"
	"unsafe"

	//"github.com/go-gl/gl/v2.1/gl"
	"golang.org/x/mobile/app"

	//	"runtime"
	//"github.com/monzo/gomobile/app"

	//gl "github.com/go-gl/gl/v3.1/gles2"
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

		//End

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

		log.Println("After entering vr mode")
		go func() {
			// Init renderer
			log.Println("Creating renderer")
			r, err := rendererCreate(vrApp, vrApp.Java)
			if err != nil {
				panic("ERR")
				//return err
			}

			log.Println("Calling render loop")
			time.Sleep(1 * time.Second)
			log.Println("Starting render loop")
			for {
				log.Println("Starting frame?\n\n")
				time.Sleep(10 * time.Millisecond)
				// Draw frame
				vrApp.FrameIndex++
				displayTime := C.vrapi_GetPredictedDisplayTime(vrApp.OVR,
					C.longlong(vrApp.FrameIndex))
				tracking := C.vrapi_GetPredictedTracking2(vrApp.OVR, displayTime)
				log.Printf("tracking %+v\n", tracking)

				layer := r.Render(tracking, float32(displayTime))
				frame := &C.ovrSubmitFrameDescription2{}
				frame.Flags = 0
				frame.SwapInterval = 1
				frame.FrameIndex = C.ulong(vrApp.FrameIndex)
				frame.DisplayTime = displayTime
				frame.LayerCount = 1
				C.submitFrame(vrApp.OVR, frame, layer)

				/*
					log.Printf("display time %+v\n", displayTime)
					log.Printf("layer%+v\n", layer)
					log.Printf("frame %+v\n", frame)
					log.Printf("layer %+v\n", layer)
					log.Printf("layer header %+v\n", layer.Header)
				*/

				//f := C.vrapi_SubmitFrame2(vrApp.OVR, frame)
				//log.Printf("Submitted frame! %+v", f)

				// 0 means sucess!?!?
			}
		}()

		workAvailable := vrApp.Worker.WorkAvailable()
		for {
			select {
			case <-workAvailable:
				log.Println("Got work")
				log.Printf("%+v", workAvailable)
				vrApp.Worker.DoWork()
				log.Println("Did work")
				// TODO add a case for submitting frame
			}
		}

		//layer :=
		//		}
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

func rendererCreate(vrApp *App, java *C.ovrJava) (*Renderer, error) {
	r := &Renderer{VRApp: vrApp}
	r.Width = int(C.vrapi_GetSystemPropertyInt(
		java, C.VRAPI_SYS_PROP_SUGGESTED_EYE_TEXTURE_WIDTH))
	r.Height = int(C.vrapi_GetSystemPropertyInt(
		java, C.VRAPI_SYS_PROP_SUGGESTED_EYE_TEXTURE_HEIGHT))
	log.Println("rendered w=%d h=%d\n", r.Width, r.Height)

	// Framebuffers
	for i := 0; i < int(C.VRAPI_FRAME_LAYER_EYE_MAX); i++ {
		log.Printf("Buffer create %d\n", i)
		r.Framebuffers = append(r.Framebuffers, r.FramebufferCreate())
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

	return r, nil
}

func toByteSlice(s []uint16) []byte {
	h := (*reflect.SliceHeader)(unsafe.Pointer(&s))
	h.Len *= 2
	h.Cap *= 2
	return *(*[]byte)(unsafe.Pointer(h))
}

func (r *Renderer) createGeometry() {
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

	r.Geometry = &Geometry{}
	log.Println("Bind")
	r.Geometry.VertexArray = glctx.CreateVertexArray() // Problem statement
	log.Println("Post create?")
	glctx.BindVertexArray(r.Geometry.VertexArray)
	log.Println("Post bind?")
	/*
		C.glGenVertexArrays(1, &r.Geometry.VertexArray)
		C.glBindVertexArray(r.Geometry.VertexArray)
	*/

	log.Println("VertexBuffer")
	r.Geometry.VertexBuffer = glctx.CreateBuffer()
	glctx.BindBuffer(gl.ARRAY_BUFFER, r.Geometry.VertexBuffer)
	glctx.BufferData(gl.ARRAY_BUFFER, f32.Bytes(binary.LittleEndian, vertices...),
		gl.STATIC_DRAW)
	/*
		C.glGenBuffers(1, &r.Geometry.VertexBuffer)
		C.glBindBuffer(C.GL_ARRAY_BUFFER, r.Geometry.VertexBuffer)
		C.glBufferData(C.GL_ARRAY_BUFFER, C.long(len(vertices)*4),
			unsafe.Pointer(&vertices[0]), C.GL_STATIC_DRAW)
	*/

	log.Println("attribs")
	pos := gl.Attrib{0}
	glctx.EnableVertexAttribArray(pos)
	glctx.VertexAttribPointer(pos, 3, gl.FLOAT, false, 4*6, 0)
	col := gl.Attrib{1}
	glctx.EnableVertexAttribArray(col)
	glctx.VertexAttribPointer(col, 3, gl.FLOAT, false, 4*6, 4*3)
	/*
		C.glEnableVertexAttribArray(0)
		C.glVertexAttribPointer(0, 3, C.GL_FLOAT, 0, 4*6, unsafe.Pointer(uintptr(0)))
		C.glEnableVertexAttribArray(1)
		C.glVertexAttribPointer(1, 3, C.GL_FLOAT, 0, 4*6, unsafe.Pointer(uintptr(4*3)))
	*/

	log.Println("indexBuffer")
	r.Geometry.IndexBuffer = glctx.CreateBuffer()
	glctx.BindBuffer(gl.ELEMENT_ARRAY_BUFFER, r.Geometry.IndexBuffer)
	glctx.BufferData(gl.ELEMENT_ARRAY_BUFFER, toByteSlice(indices), gl.STATIC_DRAW)
	/*
		C.glGenBuffers(1, &r.Geometry.IndexBuffer)
		C.glBindBuffer(C.GL_ELEMENT_ARRAY_BUFFER, r.Geometry.IndexBuffer)
		C.glBufferData(C.GL_ELEMENT_ARRAY_BUFFER, C.long(len(indices)*2),
			unsafe.Pointer(&indices[0]), C.GL_STATIC_DRAW)
		C.glBindVertexArray(0)
	*/
}

/*
func compileShader(shaderType C.GLenum, source string) (C.uint, error) {
	shader := C.glCreateShader(shaderType)
	sourceCString := C.CString(source)
	defer C.free(unsafe.Pointer(sourceCString))
	C.glShaderSource(shader, 1, &sourceCString, nil)
	C.glCompileShader(shader)

	var status C.GLint
	C.glGetShaderiv(shader, C.GL_COMPILE_STATUS, &status)
	if status == C.GL_FALSE { // TODO find why this wont error?
		var length C.GLint
		C.glGetShaderiv(shader, C.GL_INFO_LOG_LENGTH, &length)

		errorMsg := (*C.char)(C.CBytes(make([]byte, length)))
		defer C.free(unsafe.Pointer(errorMsg))
		C.glGetShaderInfoLog(shader, length, nil, errorMsg)
		s := C.GoStringN(errorMsg, length)

		return shader, fmt.Errorf("cant compile shader with error of length %d %s\n%s",
			length, s, source)
	}
	return shader, nil
}
*/

const vertexShader = `
#version 300 es

in vec3 aPosition;
in vec3 aColor;
uniform mat4 uModelMatrix;
uniform mat4 uViewMatrix;
uniform mat4 uProjectionMatrix;

out vec3 vColor;
void main() {
	gl_Position = uProjectionMatrix * (uViewMatrix * (uModelMatrix * vec4(aPosition * 0.1, 1.0)));
	vColor = aColor;
}
`

const fragmentShader = `
#version 300 es

in lowp vec3 vColor;
out lowp vec4 outColor;
void main() {
	outColor = vec4(vColor, 1.0);
	//outColor = vec4(1.0, 0.0, 0.0, 1.0);
}
`

func (r *Renderer) createProgram() error {
	p, err := glutil.CreateProgram(glctx, vertexShader, fragmentShader)
	if err != nil {
		return err
	}

	r.Program = &Program{GLProgram: p}

	// Attribs (do something better)
	glctx.BindAttribLocation(p, gl.Attrib{0}, "aPosition")
	glctx.BindAttribLocation(p, gl.Attrib{1}, "aColor")
	/*
		// Attribs (do something better than constants)
		pos := C.CString("aPosition")
		defer C.free(unsafe.Pointer(pos))
		col := C.CString("aColor")
		defer C.free(unsafe.Pointer(col))
		C.glBindAttribLocation(r.Program.GLProgram, 0, pos)
		C.glBindAttribLocation(r.Program.GLProgram, 1, col)
	*/

	// Uniforms (do something better)
	r.Program.UniformLocations = make(map[string]gl.Uniform)
	for _, name := range []string{"uModelMatrix", "uViewMatrix", "uProjectionMatrix"} {
		r.Program.UniformLocations[name] = glctx.GetUniformLocation(p, name)
	}

	/*
		r.Program = &Program{GLProgram: C.glCreateProgram()}
		vs, err := compileShader(C.GL_VERTEX_SHADER, vertexShader)
		if err != nil {
			return err
		}
		fs, err := compileShader(C.GL_FRAGMENT_SHADER, fragmentShader)
		if err != nil {
			return err
		}

		C.glAttachShader(r.Program.GLProgram, vs)
		C.glAttachShader(r.Program.GLProgram, fs)


		C.glLinkProgram(r.Program.GLProgram)
		var status C.GLint
		C.glGetProgramiv(r.Program.GLProgram, C.GL_LINK_STATUS, &status)
		if status == C.GL_FALSE {
			var length C.GLint
			C.glGetProgramiv(r.Program.GLProgram, C.GL_INFO_LOG_LENGTH, &length)

			errorMsg := (*C.char)(C.CBytes(make([]byte, length)))
			defer C.free(unsafe.Pointer(errorMsg))
			C.glGetProgramInfoLog(r.Program.GLProgram, length, nil, errorMsg)
			s := C.GoStringN(errorMsg, length)

			return fmt.Errorf("cant link program with error of length %d\n%s", length, s)
		}
	*/

	return nil
}

func (r *Renderer) Render(tracking C.ovrTracking2, dt float32) C.ovrLayerProjection2 {
	//func (r *Renderer) Render() C.ovrLayerCube2 {
	// Calculate model?

	// Calculate layer headFlags???
	// Calculate headpose???
	layer := C.vrapi_DefaultLayerProjection2()
	layer.Header.Flags |= C.VRAPI_FRAME_LAYER_FLAG_CHROMATIC_ABERRATION_CORRECTION
	layer.HeadPose = tracking.HeadPose
	//layer := C.vrapi_DefaultLayerCube2()
	//layer := C.vrapi_DefaultLayerSolidColorProjection2(&C.ovrVector4f{0.3, 0.5, 0.3, 1.0})

	model := C.ovrMatrix4f_CreateTranslation(+0.3, 0.0, -0.2)
	rot := C.ovrMatrix4f_CreateRotation(C.float(dt), C.float(dt), 0.0)
	scaleAmount := C.float(float32(math.Sin(float64(dt))))
	scale := C.ovrMatrix4f_CreateScale(scaleAmount, scaleAmount, scaleAmount)
	model = C.ovrMatrix4f_Multiply(&model, &rot)
	model = C.ovrMatrix4f_Multiply(&model, &scale)
	model = C.ovrMatrix4f_Transpose(&model)

	// For each framebuffer
	for i, f := range r.Framebuffers {
		view := C.ovrMatrix4f_Transpose(&tracking.Eye[i].ViewMatrix)
		projection := C.ovrMatrix4f_Transpose(&tracking.Eye[i].ProjectionMatrix)
		log.Printf("view, proj %+v %+v\n", view, projection)

		// Attach framebuffer to texture???
		layer.Textures[i].ColorSwapChain = f.ColorTextureSwapChain
		layer.Textures[i].SwapChainIndex = C.int(f.SwapChainIndex)
		layer.Textures[i].TexCoordsFromTanAngles = C.ovrMatrix4f_TanAngleMatrixFromProjection(
			&tracking.Eye[i].ProjectionMatrix)

		// Bind framebuffer
		glctx.BindFramebuffer(gl.DRAW_FRAMEBUFFER, f.Framebuffers[f.SwapChainIndex])

		// Enable gl stuff
		glctx.Enable(gl.CULL_FACE)
		glctx.Enable(gl.SCISSOR_TEST)
		glctx.Enable(gl.DEPTH_TEST)

		// viewport, scissor, set color
		glctx.Viewport(0, 0, f.Width, f.Height)
		// Why this int32 while viewport int???
		glctx.Scissor(0, 0, int32(f.Width), int32(f.Height))
		glctx.ClearColor(0.15, 0.15, 0.15, 1.0)
		glctx.Clear(gl.COLOR_BUFFER_BIT | gl.DEPTH_BUFFER_BIT)

		// TODO are we binding this right?
		ident := []float32{ // TODO convert the mvp to floats
			1.0, 0.0, 0.0, 0.0,
			0.0, 1.0, 0.0, 0.0,
			0.0, 0.0, 1.0, 0.0,
			0.0, 0.0, 0.0, 1.0,
		}
		glctx.UseProgram(r.Program.GLProgram)
		glctx.UniformMatrix4fv(r.Program.UniformLocations["uModelMatrix"], ident)
		glctx.UniformMatrix4fv(r.Program.UniformLocations["uViewMatrix"], ident)
		glctx.UniformMatrix4fv(r.Program.UniformLocations["uProjectionMatrix"], ident)
		/*
			C.glUniformMatrix4fv(r.Program.UniformLocations["uModelMatrix"], 1, C.GL_FALSE,
				(*C.GLfloat)(unsafe.Pointer(&model)))
			log.Printf("model glGetError %+v", C.glGetError())

			C.glUniformMatrix4fv(r.Program.UniformLocations["uViewMatrix"], 1, C.GL_FALSE,
				(*C.GLfloat)(unsafe.Pointer(&view)))
			log.Printf("view glGetError %+v", C.glGetError())

			C.glUniformMatrix4fv(r.Program.UniformLocations["uProjectionMatrix"], 1, C.GL_FALSE,
				(*C.GLfloat)(unsafe.Pointer(&projection)))
			log.Printf("projection glGetError %+v", C.glGetError())
		*/

		log.Printf("layer!!! %+v\n", layer)
		//log.Println(r.Program.UniformLocations)

		glctx.BindVertexArray(r.Geometry.VertexArray)
		glctx.DrawElements(gl.TRIANGLES, 36, gl.UNSIGNED_SHORT, 0)

		// Cleanup
		glctx.BindVertexArray(gl.VertexArray{0})
		glctx.UseProgram(gl.Program{Value: 0})
		glctx.Flush()
		glctx.BindFramebuffer(gl.DRAW_FRAMEBUFFER, gl.Framebuffer{0})
		glctx.Clear(gl.COLOR_BUFFER_BIT | gl.DEPTH_BUFFER_BIT)

		f.SwapChainIndex = (f.SwapChainIndex + 1) % f.SwapChainLength
	}

	// Unfortanetly its an all or nothing type thing.
	// We need all of the statements to happen in order.
	r.VRApp.Worker.DoWork() // TODO move this somewhere else?
	r.VRApp.Worker.DoWork() // TODO move this somewhere else?
	time.Sleep(time.Millisecond * 5)
	//r.VRApp.Worker.DoWork() // TODO move this somewhere else?
	//r.VRApp.Worker.DoWork() // TODO move this somewhere else?
	//r.VRApp.Worker.DoWork() // TODO move this somewhere else?
	//r.VRApp.Worker.DoWork() // TODO move this somewhere else?

	return layer
}

type Framebuffer struct {
	SwapChainIndex        int
	SwapChainLength       int
	Width                 int
	Height                int
	ColorTextureSwapChain *C.ovrTextureSwapChain
	Renderbuffers         []gl.Renderbuffer
	Framebuffers          []gl.Framebuffer
}

func (r *Renderer) FramebufferCreate() *Framebuffer {
	f := &Framebuffer{Width: r.Width, Height: r.Height}

	log.Println("Creating color texture swap chain")
	f.ColorTextureSwapChain = C.vrapi_CreateTextureSwapChain3(
		C.VRAPI_TEXTURE_TYPE_2D, C.GL_RGBA8, C.int(f.Width), C.int(f.Height), 1, 3)
	if f.ColorTextureSwapChain == nil {
		log.Printf("egl error %+v", eglError(int(C.eglGetError())))
		panic("Cant get color texture swap chain")
	}

	f.SwapChainLength = int(C.vrapi_GetTextureSwapChainLength(f.ColorTextureSwapChain))
	log.Printf("Length of SwapChain %d\n", f.SwapChainLength)

	// Create depth renderbuffers and framebuffers
	for i := 0; i < f.SwapChainLength; i++ {
		log.Printf("renderbuffs%+v ctx%+v", glctx, glctx)
		f.Renderbuffers = append(f.Renderbuffers, glctx.CreateRenderbuffer())
		f.Framebuffers = append(f.Framebuffers, glctx.CreateFramebuffer())
	}
	/*
		// Allocate depth render buffers + framebuffers
		f.RenderBuffers = make([]C.GLuint, f.SwapChainLength)
		f.Framebuffers = make([]C.GLuint, f.SwapChainLength)

		// Generate frame + render buffers
		C.glGenRenderbuffers(C.int(f.SwapChainLength), &f.RenderBuffers[0])
		C.glGenFramebuffers(C.int(f.SwapChainLength), &f.Framebuffers[0])
		log.Printf("Generated framebuffers %+v generated render buffers %+v\n",
			f.Framebuffers, f.RenderBuffers)
	*/

	log.Println(f.SwapChainLength)
	for i := 0; i < f.SwapChainLength; i++ {
		colorTextureC := C.vrapi_GetTextureSwapChainHandle(
			f.ColorTextureSwapChain, C.int(i))
		colorTexture := gl.Texture{uint32(colorTextureC)}

		glctx.BindTexture(gl.TEXTURE_2D, colorTexture)
		glctx.TexParameteri(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, gl.LINEAR)
		glctx.TexParameteri(gl.TEXTURE_2D, gl.TEXTURE_MAG_FILTER, gl.LINEAR)
		glctx.TexParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_S, gl.CLAMP_TO_EDGE)
		glctx.TexParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_T, gl.CLAMP_TO_EDGE)
		/*
			log.Printf("Creating color texture i=%d colorTexture=%d", i, colorTexture)
			C.glBindTexture(C.GL_TEXTURE_2D, colorTexture)
			C.glTexParameteri(C.GL_TEXTURE_2D, C.GL_TEXTURE_MIN_FILTER, C.GL_LINEAR)
			C.glTexParameteri(C.GL_TEXTURE_2D, C.GL_TEXTURE_MAG_FILTER, C.GL_LINEAR)
			C.glTexParameteri(C.GL_TEXTURE_2D, C.GL_TEXTURE_WRAP_S, C.GL_CLAMP_TO_EDGE)
			C.glTexParameteri(C.GL_TEXTURE_2D, C.GL_TEXTURE_WRAP_T, C.GL_CLAMP_TO_EDGE)
			C.glBindTexture(C.GL_TEXTURE_2D, 0)
		*/

		glctx.BindRenderbuffer(gl.RENDERBUFFER, f.Renderbuffers[i])
		glctx.RenderbufferStorage(gl.RENDERBUFFER, gl.DEPTH_COMPONENT24, f.Width, f.Height)
		glctx.BindRenderbuffer(gl.RENDERBUFFER, gl.Renderbuffer{})
		/*
			log.Printf("Creating depth renderbuffer %d", i)
			C.glBindRenderbuffer(C.GL_RENDERBUFFER, f.RenderBuffers[i])
			C.glRenderbufferStorage(C.GL_RENDERBUFFER, C.GL_DEPTH_COMPONENT24,
				C.int(f.Width), C.int(f.Height))
			C.glBindRenderbuffer(C.GL_RENDERBUFFER, 0)
		*/

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
		/*
			C.glBindFramebuffer(C.GL_DRAW_FRAMEBUFFER, f.Framebuffers[i])
			C.glFramebufferTexture2D(C.GL_DRAW_FRAMEBUFFER, C.GL_COLOR_ATTACHMENT0,
				C.GL_TEXTURE_2D, colorTexture, 0)
			C.glFramebufferRenderbuffer(C.GL_DRAW_FRAMEBUFFER, C.GL_DEPTH_ATTACHMENT,
				C.GL_RENDERBUFFER, f.RenderBuffers[i])
			status := C.glCheckFramebufferStatus(C.GL_DRAW_FRAMEBUFFER)
			if status != C.GL_FRAMEBUFFER_COMPLETE {
				panic("Can't create framebuffer")
			}
		*/
	}

	log.Println("Finished creating framebuffer")
	log.Println("Returning ", f)
	return f
}

type App struct {
	Java       *C.ovrJava
	OVR        *C.ovrMobile
	EGL        *EGL
	AndroidApp app.App
	FrameIndex int
	GL         gl.Context
	Worker     gl.Worker
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
		vrApp := &App{Java: &C.ovrJava{}, AndroidApp: a}
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
			}

			/*
				//panic("LIFE CYCLE!!!")
				log.Println("Lifecycle event", e.String())
				vrApp.GL = e.DrawContext.(gl.Context)
				log.Printf("%+v", vrApp.GL)
			*/
			//panic("EH")
			//C.onWindowFocusChanged
			/*
						switch e.Crosses(lifecycle.StageAlive) {
						//switch e.Crosses(lifecycle.StageVisible) {
						case lifecycle.CrossOn:
							log.Println("Starting")

							var ok bool
							//glctx, ok = e.DrawContext.(gl.Context)
								if !ok {
									log.Println("Could not get draw context")
								}

							onStart(glctx)
							a.Send(paint.Event{})
						case lifecycle.CrossOff:
							log.Println("Ending")
							onStop(glctx)
							glctx = nil
						default:
							log.Println("Not on or off")
						}
					case size.Event:
						log.Println("Size event")
						sz = e
						touchX = float32(sz.WidthPx / 2)
						touchY = float32(sz.HeightPx / 2)
					case paint.Event:
						//log.Println("Paint event")
						if glctx == nil || e.External {
							// As we are actively painting as fast as
							// we can (usually 60 FPS), skip any paint
							// events sent by the system.
							continue
						}

						onPaint(glctx, sz)
						a.Publish()
						// Drive the animation by preparing to paint the next frame
						// after this one is shown.
						a.Send(paint.Event{})
					case touch.Event:
						log.Println("Touch event")
						touchX = e.X
						touchY = e.Y
					default:
						log.Printf("Unrecognized event %+v", e)
				}
			*/
			//}
		}
	})
}

/*
func onStart(glctx gl.Context) {
	log.Println("FINDTHIS")
	var err error
	program, err = glutil.CreateProgram(glctx, vertexShader, fragmentShader)
	if err != nil {
		log.Printf("error creating GL program: %v", err)
		return
	}

	buf = glctx.CreateBuffer()
	glctx.BindBuffer(gl.ARRAY_BUFFER, buf)
	glctx.BufferData(gl.ARRAY_BUFFER, triangleData, gl.STATIC_DRAW)

	position = glctx.GetAttribLocation(program, "position")
	color = glctx.GetUniformLocation(program, "color")
	offset = glctx.GetUniformLocation(program, "offset")

	images = glutil.NewImages(glctx)
	fps = debug.NewFPS(images)
}

func onStop(glctx gl.Context) {
	//log.Println("FINDTHIS")
	glctx.DeleteProgram(program)
	glctx.DeleteBuffer(buf)
	fps.Release()
	images.Release()
}

func onPaint(glctx gl.Context, sz size.Event) {
	//log.Println("This process")
	glctx.ClearColor(1, 0, 0, 1)
	glctx.Clear(gl.COLOR_BUFFER_BIT)

	glctx.UseProgram(program)

	green += 0.01
	if green > 1 {
		green = 0
	}
	glctx.Uniform4f(color, 0, green, 0, 1)

	glctx.Uniform2f(offset, touchX/float32(sz.WidthPx), touchY/float32(sz.HeightPx))

	glctx.BindBuffer(gl.ARRAY_BUFFER, buf)
	glctx.EnableVertexAttribArray(position)
	glctx.VertexAttribPointer(position, coordsPerVertex, gl.FLOAT, false, 0, 0)
	glctx.DrawArrays(gl.TRIANGLES, 0, vertexCount)
	glctx.DisableVertexAttribArray(position)

	fps.Draw(sz)
}

var triangleData = f32.Bytes(binary.LittleEndian,
	0.0, 0.4, 0.0, // top left
	0.0, 0.0, 0.0, // bottom left
	0.4, 0.0, 0.0, // bottom right
)

const (
	coordsPerVertex = 3
	vertexCount     = 3
)

const vertexShader = `#version 100
uniform vec2 offset;

attribute vec4 position;
void main() {
	// offset comes in with x/y values between 0 and 1.
	// position bounds are -1 to 1.
	vec4 offset4 = vec4(2.0*offset.x-1.0, 1.0-2.0*offset.y, 0, 0);
	gl_Position = position + offset4;
}`

const fragmentShader = `#version 100
precision mediump float;
uniform vec4 color;
void main() {
	gl_FragColor = color;
}`
*/

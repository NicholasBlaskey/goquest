# Build and replace apk onto device.
rm gorift.apk

export CGO_LDFLAGS_ALLOW=".*" # Not a good idea for the future security wise! 
gomobile build -target=android/arm64 #-tags gldebug
aapt add gorift.apk lib/arm64-v8a/libvrapi.so

# Sign again after adding apk
apksigner\
	sign\
	-ks ~/.android/debug.keystore\
	--ks-key-alias androiddebugkey\
	--ks-pass pass:android\
	gorift.apk

adb install -r gorift.apk

# Get package name.
pkg=$(aapt dump badging gorift.apk|awk -F" " '/package/ {print $2}'|awk -F"'" '/name=/ {print $2}')
act=$(aapt dump badging gorift.apk|awk -F" " '/launchable-activity/ {print $2}'|awk -F"'" '/name=/ {print $2}')

# Uninstall previous version
#adb uninstall $pkg

# Start and stop program
adb shell am start -n $pkg/$act

# Exit upon this script exiting
function cleanup {
    adb shell am force-stop $pkg
    adb uninstall $pkg
}
trap cleanup EXIT

# Logs
sleep 0.5
#adb logcat | grep go
#adb logcat | grep go
echo "getting logs"
adb logcat --pid=`adb shell pidof -s $pkg`

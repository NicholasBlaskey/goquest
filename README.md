# gorift

A minimal example of running on the vrapi with Go.

Features demostrated with this example
- GLES2
- Head tracking (and updating the scenes camera according to that)
- Controller position and orientation
- log package printed to stdout (and a script to output to shell of run command)

### How to

Build, install, and run application on the Oculus Quest. The Quest must be put in developer mode and plugged into a computer through the usb-C cable.
```
./run.sh
```

##### Preqs

Install needed packages for interacting with android and APK files
```
apt-get install adb apksigner aapt
```

TODO install android SDK + NDK and so on and set environmnet varibaels

TODO install gomobile / whatever we end up with for accessing android native window
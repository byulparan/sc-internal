# SC-INTERNAL

A extention library of <a href="https://github.com/byulparan/cl-collider">cl-collider</a>.  
It's implementation to internal server of scsynth.

## Dependencies:

- [cl-collider](https://github.com/byulparan/cl-collider)
- [SuperCollider sources](https://github.com/supercollider/supercollider)

## Required:

#### libscsynth / libscsynth_add   
  You need **`libscsynth`** as shared library. defaultly, SuperCollider application not provide it.  
  so you should be build manually it from SuperCollider sources.  
  
  **`libscsynth_add`** are helper library for communicate between libscsynth and CL.  
  why need it? SBCL and CCL support foreign thread callback. but foreign thread are managed by Lisp's GC.  
  When GC work, it occur stop the audio. so sc-internal used UNIX pipe for avoid this problem.  
  
  **I will provide prebuilt both, as soon as possible.**

## Installation:

- build **`libscsynth`**  
  see **INSTALL** in SuperCollider Sources.
  
- build **`libscsynth_add`**  
```
	$ cd /path/to/sc-internal
	$ cp /path/to/libscsynth .
	$ export SC3_SRC=/path/to/directory_of_supercollider/
	$ make
```

## Usage:


# Running Lasp Language on GRiSP boards
This is a repository intended to gather information and provide useful support for design and configuration of applications written in [Lasp](https://lasp-lang.readme.io/) and running on [GRiSP](https://www.grisp.org/) boards.

## Proposed design 
Based on the principles of edge computing specified by [LightKone](https://www.lightkone.eu/), the distributed nodes should be able to replicate a fraction of the state and operate locally. As an attempt to implement that pattern, the following design is proposed for an edge node :

<p align="center">
  <img src="Untitled Diagram.png" alt="Design"/>
</p>

## Cloning/Copying existing project :
If the rebar3 grisp build command returns an error code that contains anything similar to :
```
configure: error: C compiler cannot create executables
```
Eventhough the toolchain path is correctly set in the rebar.config of the project, emptying the rebar3 cache and rebuilding in a new project should provide a workaround. It can be done with the following commands (shell) :

```
rm -rdf {~/.cache/rebar3/*,/path/to/defect/project}
rebar3 new grispapp=newproject dest=/path/to/sd
cp -a /path/to/defect/project/{grisp,rebar.config,src} /path/to/newproject && cd /path/to/newproject && rebar3 grisp build
```

Since the environment and path can be altered if builds/deployments have been made if sudo was previously called, the files in otp/20.2/build/make can contain incorrect references when the configuration is run after a build. Erasing the cache and building from a new app folder can solve this issue. 

### Dependencies source files inclusion :
If the sources of the application's dependencies are not correctly included in the build for the SD card, cleaning the previous build and unlocking the rebar lock file can be helpful :

```
cd /path/to/project && rm -rdf {_build,_grisp}
rebar3 unlock && rebar3 grisp build && rebar3 compile
```

NOTE : when calling commands cp and rm on multiple targets such as {file1,file2,...}, the files must always be separated by commas that are never followed by spaces e.g. never {file1, file2, ...} otherwise the shell is unable to locate them.

## Auto-deployment script usage :

The script detects when the SD Card is inserted and compiles the project, deploys and unmounts the card automatically. The script requires sudo privilegies for unmounting. 

Note : the rebar.config must not call unmount/umount in the post_script argument for the grisp plugin.

Usage :

```
$ sudo ./grisp_deploy.sh -s /Path/to/GRISP -a /Path/to/application/ [-n <name>]
```
Where the parameters are the following :

- `-s` : SD Card path e.g. : `-s /Volumes/GRISP`

- `-a` : Path to the target application root folder e.g. : `-a /GRISP/robot/`

- `n` : (optional) Name of the target application e.g. : robot. If not supplied, the first found module will be deployed.

## Installation :

Documentation is available in the [Grisp wiki](https://github.com/grisp/grisp/wiki/). This section describes specific encountered installation cases and further remarks/notes.

### Ubuntu 16.04.3 LTS

Once Rebar3 has been installed with the help of

```
$ mkdir ~/bin
$ curl -o ~/bin/rebar3 https://s3.amazonaws.com/rebar3/rebar3
$ chmod +x ~/bin/rebar3
```

Running the `rebar3` command resulted in an error of type :

```
No command 'rebar3' found
```

The following commands must be executed :

```
cd ~/bin/
./rebar3 local install
```

And after appending the resulting line to the `~/.bashrc` file :

```
export PATH=$HOME/.cache/rebar3/bin:$PATH
```

The `rebar3 plugins list` command can be runned in order to check whether the binary can now be executed.

After creating a new grispapp and adding the toolchain in the `rebar.config` of that app, it can be built using :

```
cd <grisp app name>
rebar3 grisp build
```

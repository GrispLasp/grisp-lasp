
# Running Lasp Language on GRiSP boards
This is a repository intended to gather information and provide useful support for design and configuration of applications written in [Lasp](https://lasp-lang.readme.io/) and running on [GRiSP](https://www.grisp.org/) boards.

## Auto-deployment script usage :

The script detects when the SD Card is inserted and compiles the project, deploys and unmounts the card automatically. The script requires sudo privilegies for unmounting.

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

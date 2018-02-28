
# Running Lasp Language on GRiSP boards
This is a repository intended to gather information and provide useful support for design and configuration of applications written in [Lasp](https://lasp-lang.readme.io/) and running on [GRiSP](https://www.grisp.org/) boards.

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

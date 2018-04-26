
# Setting up Atom for :b:est workflow

## Overleaf Submodule
Initialize the `DissertationDraft` submodule that has been added to the repo and fetch the current data :

```
cd GrispLaps/DissertationDraft
git submodule init
git submodule update
```

This can be easily done using a git client simply by opening DissertationDraft in the submodules section.

## Installing Git+
Git+ is an Atom package that adds support for Git commands within Atom as if from the terminal. Simply install `git-plus` from the Atom package marketplace and it is ready to go.

### Commands
Once installed, use the following shortcut to open the Git command list :

```
CTRL + SHIFT + H (Windows)
CMD + SHIFT + H (Mac)
```

And once you have edited files, you can use :

```
CTRL + SHIFT + A (Windows)
CMD + SHIFT + A (Mac)

P (Mac and Windows)
```

To Add + Commit + Push your changes to the submodule

<p align="center">
  <img src="http://g.recordit.co/9rqF0BShQP.gif" alt="Pushing to Overleaf submodule"/>
</p>

## GitKraken Glo
Install the `gitkraken-glo` package from the Atom marketplace to start using [Glo](https://atom.io/packages/gitkraken-glo/) :octocat: as an embedded tab in Atom. Accept the invitations that you have received by mail for the GrispLasp board and you are now able to manage cards and workflow that is synced with the GrispLasp repository and DissertationDraft submodule. Features such as issues, assignment, etc. are directly available inside the Atom tab and published to GitHub according to cards :muscle: :muscle:.  

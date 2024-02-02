# Lamtask - a (somewhat) functional Todo and Event manager
Lamtask is a simple terminal todo application written in Haskell that aims to provide a simple CLI for managing todos and events. It is, though, mostly a learning project for me to practice writing Haskell.

## Usage
To use the program, once you have followed the installation instructions below, simply type 
```
> lamtask help
```
to get a list of available commands. You may want to consider adding a call to "lamtask" without any arguments (assuming you have set up the alias as described; else put "Lamtask f=/path/to/storage/tasks.txt) to the very end of your shell configuration to list your open tasks when you open a shell. 
## Installation
There are currently two options to install this program:
- Installation from source using cabal and GHC
- Installation using a binary from the releases section
### From binary (Linux x86_64 only!)
First, grab the latest binary from the releases of this GitHub repo. There should be a file simply named "Lamtask", download it and place it in an appropriate location (for example, /home/yourusername/.bin or, if you are feeling spicy, /usr/bin or equivalent).

If the location you chose is *not* in your $PATH already, make sure to add an appropriate line to your shell program configuration file (.bash_profile, .zshrc, ...). This should look something like:
```
export PATH= "$PATH:/path/to/containing/folder"
```
Finally, for maximum convenience, it is currently recommended that you create an alias in this same location to save yourself the "f=filename" parameter on every run. This would go in the same file (somewhere after the previously mentioned line) and look like:
```
alias lamtask = Lamtask f=/path/to/storage/tasks.txt
```
Then, either reopen your terminal emulator, or reload the edited configuration file using "source .bashrc" or equivalent.

Voilà!

### Installation from source (Advanced, more platforms)

To install from source, you will need the following toolchain:

- GHC 9.8.4
- Cabal 3.10.2.1

These can be installed using the free tool GHCup.

With these tools, clone or download the repository, enter it on the command line, and enter
```
> cabal build && cabal install /path/to/desired/location
```
(obviously adjust the && and /'s to your operating system)

This should build and install the program to /path/to/desired/location.

If the location you chose is *not* in your $PATH already, make sure to add an appropriate line to your shell program configuration file (.bash_profile, .zshrc, ...). This should look something like:
```
export PATH= "$PATH:/path/to/desired/location"
```
Obvious Note: *If you are on Windows, or another non-UNIX, add it to your path in the way needed for that OS...*

Finally, for maximum convenience, it is currently recommended that you create an alias in this same location to save yourself the "f=filename" parameter on every run. This would go in the same file (somewhere after the previously mentioned line) and look like:
```
alias lamtask = Lamtask f=/path/to/storage/tasks.txt
```
Then, either reopen your terminal emulator, or reload the edited configuration file using "source .bashrc" or equivalent.

Obvious Note: *If you are on Windows, or another non-UNIX, do these in the way needed for that OS as far as is possible... You might need to touch some unholy batch scripting.*

Voilà!
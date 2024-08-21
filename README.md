# ss14-watchdogplus

**ss14-watchdogplus** is a multi-codebase, multi-server server manager for [Space Station 14](https://spacestation14.com/) servers and forks.
It is designed primarily for server operators who operate multiple SS14 servers from different codebases on one big box.

Compared to [SS14.Watchdog](https://github.com/space-wizards/SS14.Watchdog), **ss14-watchdogplus** can:

- Pre-build server binaries from Git to reduce startup time, like [Robust.Cdn](https://github.com/space-wizards/Robust.Cdn) but without the extra setup
- Reboot instances independently, even if they are on the same build
- Gracefully notify servers to reboot or shut down at the end of a round

## Building

1. Clone this repository to a location that ASDF can find, e.g. *~/common-lisp/*
2. Load the system, e.g.:

    ````
    (ql:quickload "ss14-watchdogplus")
    ````

3. Build the binary:

    ````
    (asdf:make "ss14-watchdogplus")
    ````

## Configuration
Copy the [sample configuration file](config.lisp) into the directory in which you want to run the watchdog, then modify it as needed.

There are two terms you need to understand for the configuration to make sense:

- *build* - a specific build of a particular codebase from Git, identified by a repository URL and Git commit hash
- *instance* - a Space Station 14 server running on a particular *build*

## Usage

### Startup
Inside the watchdog directory:

````
$ ./ss14-watchdogplus
````

The watchdog should tell you the builds and instances that are configured, then start building the builds.
After the builds are complete, it will launch all the configured instances.

### Interactive Command-Line
While the watchdog is running, press CTRL-C to enter the interactive watchdog prompt.
At this prompt, you can execute:

- `(notify-update "BUILD")` - Notify the watchdog that the build identified by *BUILD* was updated.
   The watchdog will automatically re-build this build, then notify any running instances to gracefully reboot at round end.
- `(shutdown)` - Stop all running instances and exit the watchdog

> [!IMPORTANT]
> You must exit the watchdog interactive command-line for the watchdog to resume its normal watchdog duties (restarting servers).
> To exit the interactive command-line, type `0` at the prompt.

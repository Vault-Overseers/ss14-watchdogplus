# ss14-watchdogplus

**ss14-watchdogplus** is a multi-codebase, multi-server server manager for [Space Station 14](https://spacestation14.com/) servers and forks.
It is designed primarily for server operators who operate multiple SS14 servers from different codebases on one big box.

Compared to [SS14.Watchdog](https://github.com/space-wizards/SS14.Watchdog), **ss14-watchdogplus** can:

- Pre-build server binaries from Git to reduce startup time, like [Robust.Cdn](https://github.com/space-wizards/Robust.Cdn) but without the extra setup
- Reboot instances independently, even if they are on the same build
- Gracefully notify servers to reboot or shut down at the end of a round

## Installation

## Configuration

- *build* - a specific build of a particular codebase from Git, identified by a repository URL and Git commit hash
- *instance* - a Space Station 14 server running on a particular *build*

A sample configuration file showing two *builds* from different codebases with two *instances* each (one for production, one for mapping) is shown below:

    ; defines a build called "n14" that is built from Git using the given repository and revision specifier
    (defbuild "n14" :git "https://github.com/Vault-Overseers/nuclear-14.git" "master")

    ; ditto for a build called "ds14"
    (defbuild "ds14" :git "https://github.com/SS14-Classic/deep-station-14.git" "master")

    ; define a production and mapping server for using the "n14" build
    (definstance "n14-prod" :build "n14")
    (definstance "n14-map" :build "n14")

    (definstance "ds14-prod" :build "ds14")
    (definstance "ds14-map" :build "ds14")

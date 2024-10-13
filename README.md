# ss14-watchdogplus

**ss14-watchdogplus** is a multi-codebase, multi-server server manager for [Space Station 14](https://spacestation14.com/) servers and forks.
It is designed primarily for server operators who operate multiple SS14 servers from different codebases on one big box.

**ss14-watchdogplus** can:

- Pre-build server binaries from Git to reduce startup time, like [Robust.Cdn](https://github.com/space-wizards/Robust.Cdn) but without the extra setup
- Reboot instances independently, even if they are on the same build
- Gracefully notify servers to reboot or shut down at the end of a round

## Building

1. Install dependencies:
    - The [dialog](https://invisible-island.net/dialog/) command-line tool is needed for the TUI menu
    - We recommend using [Quicklisp](https://www.quicklisp.org/beta/) for Lisp dependencies

2. Clone this repository to a [location that ASDF can find](https://asdf.common-lisp.dev/asdf.html#Controlling-where-ASDF-searches-for-systems), e.g. *~/common-lisp/*

3. Load the system, e.g.:

    ````
    (ql:quickload "ss14-watchdogplus")
    ````

4. Build the binary:

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
1. Inside the watchdog directory:

    ````
    $ ./ss14-watchdogplus
    ````

    The interactive menu will appear.

2. Before your first run, you should `update` all of the builds.

3. Then, `start` the instances that you want the watchdog to monitor.

### Interactive Menu
While the watchdog is running, press CTRL-C to enter the interactive menu.

> [!IMPORTANT]
> You must exit the interactive menu for the watchdog to resume its normal watchdog duties (restarting servers).

## License
Copyright 2024 Kevin Zheng

This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.

You should have received a copy of the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.

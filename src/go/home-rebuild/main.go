package main

import (
	"fmt"
	"os"
	"os/exec"
	"strconv"
	"strings"
)

func main() {
	if supportsFlakes() {
		withFlakes()
	} else {
		withoutFlakes()
	}
}

func supportsFlakes() bool {
	output, err := exec.Command(
		"nix-instantiate",
		"--eval",
		"--expr", "builtins ? getFlake",
		"--json",
	).Output()

	if err != nil {
		if exitError, ok := err.(*exec.ExitError); ok {
			exitStatus := exitError.ExitCode()
			fmt.Printf("nix-instantiate failed with exit code %d\n", exitStatus)
		} else {
			fmt.Println("Failed to execute nix-instantiate:", err)
			os.Exit(1)
		}
	}

	supportsFlakes, err := strconv.ParseBool(strings.TrimSpace(string(output)))

	if err != nil {
		fmt.Println("Failed to parse output from nix-instantiate into a bool:", err)
		os.Exit(1)
	}

	return supportsFlakes
}

func withFlakes() {
	args := strings.Join(os.Args[1:], " ")

	command := exec.Command(
		"sh", "-c",
		"home-manager --flake .#$(hostname -s) "+args,
	)

	command.Stdout = os.Stdout
	command.Stderr = os.Stderr

	err := command.Run()

	if err != nil {
		if exitError, ok := err.(*exec.ExitError); ok {
			exitStatus := exitError.ExitCode()
			fmt.Printf("home-manager failed with exit code %d\n", exitStatus)
		} else {
			fmt.Println("Failed to execute home-manager:", err)
			os.Exit(1)
		}
	}
}

func withoutFlakes() {
	usage := "usage: home-rebuild (build | switch)"

	if len(os.Args[1:]) != 1 {
		fmt.Println(usage)
		os.Exit(1)
	}

	subcommand := os.Args[1]

	command := exec.Command(
		"sh", "-c",
		"nix-build --attr homeConfigurations.$(hostname -s).activationPackage",
	)

	command.Stderr = os.Stderr

	output, err := command.Output()

	if err != nil {
		if exitError, ok := err.(*exec.ExitError); ok {
			exitStatus := exitError.ExitCode()
			fmt.Printf("nix-build failed with exit code %d\n", exitStatus)
		} else {
			fmt.Println("Failed to execute nix-build:", err)
			os.Exit(1)
		}
	}

	storePath := strings.TrimSpace(string(output))

	switch subcommand {
	case "build":
		fmt.Println(storePath)
	case "switch":
		command := exec.Command(storePath + "/activate")

		command.Stdout = os.Stdout
		command.Stderr = os.Stderr

		err := command.Run()

		if err != nil {
			if exitError, ok := err.(*exec.ExitError); ok {
				exitStatus := exitError.ExitCode()
				fmt.Printf("activate failed with exit code %d\n", exitStatus)
			} else {
				fmt.Println("Failed to execute activate:", err)
				os.Exit(1)
			}
		}
	default:
		fmt.Println(usage)
		os.Exit(1)

	}
}

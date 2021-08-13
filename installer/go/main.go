package main

import (
	"fmt"
	ansi "github.com/mgutz/ansi"
	flag "github.com/spf13/pflag"
	"os"
	"os/exec"
	"strings"
)

type options struct {
	dryRun   bool
	packages []string
}

func parseOptions() options {
	dryRun := flag.Bool("dry-run", false, "Run in dry run mode")
	flag.Parse()
	rawPackages := flag.Args()
	lenRawPackages := len(rawPackages)
	packages := make([]string, lenRawPackages)

	for i := 0; i < lenRawPackages; i++ {
		packages[i] = strings.TrimRight(rawPackages[i], "/")
	}

	return options{dryRun: *dryRun, packages: packages}
}

func discardNonexistent(packages *[]string) []string {
	lenPackages := len(*packages)

	var existent []string

	e := 0
	for p := 0; p < lenPackages; p++ {
		pkg := (*packages)[p]
		if pathExists(pkg) {
			existent = append(existent, pkg)
			e++
		} else {
			warn(fmt.Sprintf("[%s] Configuration doesn't exist", pkg))
		}
	}

	return existent
}

func pathExists(path string) bool {
	_, err := os.Stat(path)
	return (err == nil)
}

func install(pkg string) {
	runHook("before", pkg)
	stow(pkg)
	runHook("after", pkg)
}

func runHook(hook string, pkg string) {
	script := fmt.Sprintf("%s/%s-hook", pkg, hook)
	if pathExists(script) {
		log(fmt.Sprintf("[%s] Running %s hook", pkg, hook))
		sh(fmt.Sprintf("./%s", script))
	}
}

func stow(pkg string) {
	log(fmt.Sprintf("[%s] Stowing configuration", pkg))

	sh(
		"stow",
		"--stow",
		"--target",
		os.Getenv("HOME"),
		"--no-folding",
		pkg,
		"--ignore",
		"-hook",
	)
}

func sh(executable string, args ...string) {
	command := fmt.Sprintf("%s %s", executable, strings.Join(args, " "))
	if dryRun {
		log(fmt.Sprintf("dry-run> %s", command))
	} else {
		log(fmt.Sprintf("+ %s", command))
		cmd := exec.Command(executable, args...)
		cmd.Stdout = os.Stdout
		cmd.Stderr = os.Stderr
		if err := cmd.Run(); err != nil {
			fail(fmt.Sprintf("Command '%s' failed with error: %s", command, err))
		}
	}
}

func log(message string) {
	fmt.Println(ansi.Color(message, "magenta"))
}

func warn(message string) {
	fmt.Println(ansi.Color(message, "yellow"))
}

func fail(message string) {
	fmt.Println(ansi.Color(message, "red"))
	os.Exit(1)
}

var dryRun bool

func main() {
	options := parseOptions()

	dryRun = options.dryRun

	options.packages = discardNonexistent(&options.packages)

	lenPackages := len(options.packages)

	if lenPackages <= 0 {
		fail("No packages specified")
	} else {
		for i := 0; i < lenPackages; i++ {
			install(options.packages[i])
		}
	}
}

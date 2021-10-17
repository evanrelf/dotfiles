#!/usr/bin/env janet

(var dry-run true)

(defn sh [& command]
  (def command-string (string/join command " "))
  (if dry-run
    (print "dry-run> " command-string)
    (do
      (print "+ " command-string)
      (os/execute
        @("sh" "-c" command-string)
        # Search PATH
        :p
        # Raise error on non-zero exit code
        :x))))

(defn run-hook [hook-name package]
  (def script (string package "/" hook-name "-hook"))
  (when (= (os/stat package :mode) :file)
    (sh "./" script)))

(defn stow [package]
  (print "[" package "] Stowing configuration")
  (sh
    "stow"
    "--stow"
    `--target "${HOME}"`
    "--no-folding" package
    `--ignore "-hook"`))

(defn install [package]
  (run-hook "before" package)
  (stow package)
  (run-hook "after" package))

(defn discard-nonexistent [packages]
  (var existent @[])
  (defn f [package]
    (if (= (os/stat package :mode) :directory)
      (array/push existent package)
      (print "[" package "] Configuration doesn't exist")))
  (map f packages)
  existent)

(defn main [executable & args]
  # TODO: --dry-run
  (def all-packages (map (fn [x] (string/replace-all "/" "" x)) args))
  (def existent-packages (discard-nonexistent all-packages))
  (if (empty? existent-packages)
    (do
      (print "No packages specified")
      (os/exit 1))
    (map install existent-packages)))

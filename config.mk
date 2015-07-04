MAKEFLAGS += --warn-undefined-variables
SHELL := bash

# exit if an undefined variable is used or if any task in a pipeline fails
.SHELLFLAGS := -eu -o pipefail

.DEFAULT_GOAL := all

.DELETE_ON_ERROR:
.SUFFIXES:

DB_NAME = "sunrooms"

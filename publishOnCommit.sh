#!/usr/bin/env bash

# AUTHOR: michael.gough@csc.gov.au
# Useful for lower environments, this script can be scheduled to have 
# the application automatically publish a new version when changes have 
# been committed to the git branch. The version of the published 
# application will be the short git hash.

# check if a branch has pending changes
function parse_git_changes() {
  git log HEAD..origin/$(parse_git_branch) --oneline
}

# get the current git branch name
function parse_git_branch() {
  git rev-parse --abbrev-ref HEAD
}

# get the last commit hash prepended with @ (i.e. @8a323d0)
function parse_git_hash() {
  git rev-parse --short HEAD 2> /dev/null | sed "s/\(.*\)/\1/"
}

# Get details of any git commits
function parse_git_origin() {
  git fetch origin
}

function get_git_changes() {
  git merge origin/$(parse_git_changes)
}

# Get details of any git commits
parse_git_origin

# If a change has been found, publish the change
if [ -z "$(parse_git_changes)" ] ; then
  echo "No changes to git branch detected"
else
  echo "Change to git branch detected, publishing the changes"
  
  # complete the pull
  get_git_changes

  #publish the changes
  chmod +x publish.sh
  ./publish.sh $(parse_git_hash)
fi
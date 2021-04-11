# bugzilla-redhat version history

## next
- BugillaServer can now be fully qualified
- Add isNotEmpty search expression operator
- Add searchBugsAllWithLimit, searchBugsAll and getBugAll to get all the bug fields
- Add getBugIncludeFields to select the included fields
- Change Bug to include ExternalBugs information

## 0.3.1 (2021-02-07)
- export sendBzRequest

## 0.3.0 (2020-08-05)
- a fork of Seth Fowler's [bugzilla library](https://hackage.haskell.org/package/bugzilla)
- updated to build with Stackage LTS > 5 up to LTS 16 and Nightly
- minor API tweaks needed for redhat.bugzilla.com v5 (e3b424e2)
- expose BugzillaToken, newBzRequest, intAsText
- drop closeBugzillaContext and withBugzillaContext
- add evalSearchExpr
- hlint fixes

## bugzilla 0.2 and older
See Seth Fowler's original changelog.orig file.

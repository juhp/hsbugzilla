# bugzilla-redhat version history

## 0.3.2 (2021-06-19)
- BugillaServer can now be fully qualified (@TristanCacqueray)
- Add apikeySession to support api_key auth (@TristanCacqueray)
- Add isNotEmpty search expression operator (@TristanCacqueray)
- Add changedSince and changedUntil search expression (@TristanCacqueray)
- Add searchBugsAllWithLimit, searchBugsAll and getBugAll
  to get all the bug fields (@TristanCacqueray)
- Change Bug to include ExternalBugs information (@TristanCacqueray)
- Export Request
- support ghc-9.0 (@juhp)

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

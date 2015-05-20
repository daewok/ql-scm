# ql-scm #

ql-scm is an extension to Quicklisp that allows releases to be pulled directly
from their respective source control mechanisms instead of relying on
tarballs. Right now, ql-scm supports only git, but other SCM tools should be
fairly trivial to support as well.

# Requirements beyond Quicklisp #

The primary requirement to using ql-scm beyond having quicklisp is that the SCM
tools used by the installed distributions (git, svn, hg, etc.) must be installed
and accessible on the command line. Additionally, if a release requires
authentication to be accessed, the SCM tool must be set up to obtain that
authentication without any interaction from ql-scm directly.

# Live Releases #

While releases can point to a specific commit in the appropriate SCM, a lot of
the power of this extension comes from the ability to have "live" releases. Live
releases are simply releases that point to symbolic commits, such as branches or
tags. This allows the releases to be updated without needing to release a new
version of the distribution.

With great power comes great responsibility, however. When using live releases
you lose the ability to meaningfully move back in time to an earlier version of
the distribution. Additionally, adding a new dependency to a system without
releasing a new version of the distribution with an updated systems.txt database
can lead to inconsistent metadata and behavior (particularly with Quicklisp's
bundling mechanism).

# Why should I use this? #

Good question! For the most part, you shouldn't use this to release systems for
public consumption. Instead, submit your software to the
[canonical Quicklisp distribution](https://github.com/quicklisp/quicklisp-projects/).

This extension was designed primarily for the internal use of Common Lisp
development teams. The inclusion of "live" releases allows everyone on the team
to easily retrieve the latest development version of every installed library
using the standard, familiar Quicklisp commands. Support for releases being
pegged to a specific commit easily allow internal testing of
releases. Additionally, most version control systems also handle authentication,
meaning there is no additional overhead in setting up an authentication
architecture to ensure only the developers on the team have access to the latest
version of the code. This is a particular boon for teams that work on an open
network (such as in universities).

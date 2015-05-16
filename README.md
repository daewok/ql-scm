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




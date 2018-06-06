---
title: Technical Links
order: 50
---

Collection of links to various git related documentation,

[https://gist.github.com/schacon/6092633](https://gist.github.com/schacon/6092633)

[https://github.com/git/git/blob/master/Documentation/technical/pack-protocol.txt](https://github.com/git/git/blob/master/Documentation/technical/pack-protocol.txt)

[https://github.com/git/git/blob/master/Documentation/technical/http-protocol.txt](https://github.com/git/git/blob/master/Documentation/technical/http-protocol.txt)

[https://github.com/git/git/blob/master/Documentation/technical/protocol-capabilities.txt](https://github.com/git/git/blob/master/Documentation/technical/protocol-capabilities.txt)

[https://github.com/git/git/blob/master/Documentation/technical/pack-format.txt](https://github.com/git/git/blob/master/Documentation/technical/pack-format.txt)

[https://github.com/git/git/tree/master/Documentation/technical](https://github.com/git/git/tree/master/Documentation/technical)

[http://alblue.bandlem.com/2011/08/git-tip-of-week-objects.html](http://alblue.bandlem.com/2011/08/git-tip-of-week-objects.html)

[https://git-scm.herokuapp.com/book/en/Git-Internals-Transfer-Protocols](https://git-scm.herokuapp.com/book/en/Git-Internals-Transfer-Protocols)

[https://github.com/git/git/blob/master/builtin/receive-pack.c](https://github.com/git/git/blob/master/builtin/receive-pack.c)

[http://stefan.saasen.me/articles/git-clone-in-haskell-from-the-bottom-up/](http://stefan.saasen.me/articles/git-clone-in-haskell-from-the-bottom-up/)

```bash
set GIT_CURL_VERBOSE=1
$env:GIT_CURL_VERBOSE=1
$env:GIT_TRACE=1
git config --global http.proxy %HTTP_PROXY%
git config --global --unset http.proxy
git config --global http.sslVerify false
GIT_TRACE=2 GIT_CURL_VERBOSE=2 GIT_TRACE_PERFORMANCE=2 GIT_TRACE_PACK_ACCESS=2 GIT_TRACE_PACKET=2 GIT_TRACE_PACKFILE=2 GIT_TRACE_SETUP=2 GIT_TRACE_SHALLOW=2
```

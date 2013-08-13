i3-emacs
========

*Note: Emacs 24.3 required.*

i3 emacs integration.

If you, like me, use i3 as your window manager and Emacs as your editor, you might want to make them play nicer together. Emacs was built to be working in a normal window manager or in absolute absence of one, however, in the presence of proper tiling manager, some of its habits can be annoying. It doesn't really know what frames are visible and how to make them visible. It insist on splitting the frames or create new frames on too many occasions.
This little library tries to solve some of its problems. It also enables you to solve many more yourself by allowing to speak to i3 through its IPC channel right from elisp code.

Deployment is simple, drop those two .el files somewhere in your load-path and do `(require 'i3)` from your code.
You can also do `(require 'i3-integration)` from your .emacs file to enable some of the pre-packaged integrations. At the moment, only two are supported: (visible-frame-list) will contain *only* frames that are actually visible, and one-window-per-frame-mode, which is disabled by default.

one-window-per-frame-mode
-------------------------

Since you are using i3, you want full control over you windows layout. Emacs does not play nice with this desire, however. It thinks it knows better than WM it is running under and trying to emulate some of it functions by splitting frames into set of windows. However, i3 is more than capable of doing that itself, and you can mix Emacs and terminals windows in the way Emacs can't help you with.

So, you probably want to make Emacs use just one window per frame and reuse those frames for all the pop-up needs it have. Just do `(i3-one-window-per-frame-mode-on)` in your .emacs file and Emacs will behave exactly like this. It will reuse existing frames instead of splitting existing ones or creating new ones, it will only use truly visible frames for this as well.

Bugs, support, etc.
-------------------

There is no support. I can answer you questions but it is as far as it goes.
Any bug you want to be fixed have to have a proper description *and a patch* that is fixing it. This code works for me and I just want to share it so others can benefit as well but I simply have no time improving it in any but trivial way.

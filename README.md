# Loophole

Loophole provides temporary key bindings management feature for Emacs.
Keys can be set by interactive interface in disposable keymaps
which are automatically generated for temporary use.

## Installation

Download and save loophole.el to your `load-path`,
and add the following lines to you init file like `.emacs`.

``` emacs-lisp
(require 'loophole)
(loophole-mode)
```

## Usage

### Basics

Call `loophole-set-key` to set the temporary key bindings.

The key binding is set in the disposable keymap generated automatically.
Once keymap is generated, `loophole-set-key` edit it for a while,
i.e. bindings will be set in it.
`loophole-stop-editing` stops editing, and then next `loophole-set-key`
generates another keymap.
You can see editing status on the mode-line as
`loophole-mode-lighter-editing-sign` (is "+" by default).

If you done your operation with temporary key bindings,
call `loophole-disable-latest-map` to abandon that one.
Note that even after disabling the keymap,
it is stored in `loophole-n-map` for a while. (`n` is something integer.)
You can reactivate it by `loophole-enable-map`,
and can use multiple temporary keymaps at the same time.

### Example

When you encountered the case you just read the buffer temporarily,
the following key inputs provide simple navigating interface
which save your left little finger.

* `C-c [` `n` `next-line`
* `C-c [` `p` `previous-line`
* `C-c [` `f` `scroll-up-command`
* `C-c [` `b` `scroll-down-command`

When you are finished reading the buffer,
type `C-c \` (`loophole-disable-latest-map`) to abandon the key bindings above.
Then, your keymap environment stays clean.

### Details

#### Keymap cascade

Loophole uses three layers of keymaps.

First one is keymaps for temporary key bindings.
I call them as "loophole-map".
They are mainly generated automatically,
and listed in `loophole--map-alist`.
They take effect by adding `loophole--map-alist` to `emulation-mode-map-alists`.

Second one is `loophole-base-map` which will be inherited to most loophole-maps.
This keymap offers the place for binding which can be used commonly when
any of loophole-map is activated.
For example, binding `C-q` to `loophole-disable-latest-map` reduces the number
of typing, and `C-q` recovers the original binding `quoted-insert` immediately
after all loophole-map are disabled.

Third one is the keymap for manipulating loophole itself,
named `loophole-mode-map` which holds some loophole commands.
This is, as the name suggests, activated when `loophole-mode` is enabled.

#### Buffer local behavior

All loophole-maps are shared globally but their activating states are buffer
local.
Order of loophole-maps and editing state are also buffer local.
You can control these conditions for each buffer.

#### Naming loophole-map

If you like some loophole-map, naming them may help.
Loophole-maps are initially named as `loophole-n-map`.
They can be renamed by `loophole-name`.
Once loophole-map gets name other than `loophole-n-map`,
it goes out of range of automatic keymap generation and is never overwritten.

#### Loophole mode

`loophole-mode` is the minor mode for managing temporary key bindings.

When `loophole-mode` is enabled,
`loophole--map-alist` is added to the head of `emulation-mode-map-alists`,
and thus temporary key bindings are activated.
Furthermore, you can use `loophole-mode-map` for binding loophole commands.

Even while `loophole-mode` is activated, whole temporary key bindings can be
disabled temporarily by calling `loophole-suspend`,
which keeps state of each loophole-map.
`loophole-resume` restores suspended temporary key bindings.
These functions realize suspension by removing and adding `loophole--map-alist`
in `emulation-mode-map-alists`.

Note that disabling `loophole-mode` also calls `loophole-suspend` and keeps
state of each loophole-map.
If you want to completely disable loophole, use `loophole-quit`.
It disables all loophole-maps and `loophole-mode`.

#### Key binding entry

`loophole-set-key` ask you the command symbol to be bound
by using `read-command` like `global-set-key`.
However, you can bind not only command
but also keyboard macro, keymap and symbol as well.
Furthermore, you can choose specifying method for these entries.
For example, command can alternatively be specified by key sequence
which is found in the currently active keymaps.
Details of major specifying method is described below.

You can use prefix arguments to specify
which key binding entry and its specifying method is employed.
By default, `loophole-set-key` uses the following table, which is customizable.
Details of customization is described in the customization section below.

| Prefix arguments    | Entry   | Specifying method |
| ------------------- | ------- | ----------------- |
| No prefix arguments | command | read command      |
| `C-u`, `C-1`        | kmacro  | recursive edit    |
| `C-u` `C-u`, `C-2`  | command | key sequence      |
| `C-u`\*3, `C-3`     | kmacro  | read key          |
| `C-u`\*4, `C-4`     | command | lambda form       |
| `C-u`\*5, `C-5`     | kmacro  | recall record     |
| `C-u`\*6, `C-6`     | object  | eval minibuffer   |

If prefix argument is negative, `loophole-set-key` ask user obtaining method by
using `completing-read`.

##### command by read command

Default method.
`read-command` and use read symbol as command.

##### command by key sequence

`read-key-sequence` and use the command which is bound with read key sequence
in currently active keymaps.

##### command by lambda form

Setup temporary buffer with template for writing lambda form and start
recursive edit.
When you finish writing lambda form,
type `C-c C-c` (`loophole-complete-writing-lisp`).
Then, buffer contents will be read and evaluated,
and returned object will be bound.
When you want to abort, type `C-c C-k` (`loophole-abort-writing-lisp`).

Actually, buffer contents can be any lisp forms other than sole lambda form.
If returned value is a valid command, it will be bound anyway.

Note that while you are in recursive edit, it looks like top-level of Emacs
and you may feel that the control is returned to you,
but actually you are still in the command.
For that reason, it is recommended to call either
`loophole-complete-writing-lisp` or `loophole-abort-writing-lisp` which can
properly end recursive edit.

##### kmacro by recursive edit

Similar to built-in keyboard macro defining environment except that
we are in recursive edit.
In recursive edit, you can define your keyboard macro while you are getting
the feedback on your eyes.
When you are finished, type `C-c [` (`loophole-end-kmacro`),
or you want to abort, type `C-c \` (`loophole-abort-kmacro`).

While you are defining keyboard macro, not only recursive edit but also
kbd-macro environment is enabled.
In order to handle these conditions, it is highly recommended to call
either `loophole-end-kmacro` or `loophole-abort-kmacro`.
Even if you want to abort binding, `C-g` (`keyboard-quit`) is not enough.
It stops defining keyboard macro, but it does not abort recursive edit.
`loophole-abort-kmacro` takes care of it as it does both of them.

##### kmacro by read key

`read-key` recursively, and keys read will be the keyboard macro.
When you are finished, type finish key which may be `C-g`.
Finish key is specified by customizable variable
`loophole-kmacro-by-read-key-finish-key`,
whose default value is key sequence for `keyboard-quit`.
By default, you can only finish and cannot abort definition.
Once you set another key sequence to `loophole-kmacro-by-read-key-finish-key`,
you can finish by your finish key,
and can abort by the key sequence bound to `keyboard-quit`.

##### kmacro by recall record

`competing-read` the `kmacro-ring`.
Therefore, the keyboard macro which defined in outside of loophole
also can be bound.

##### object

Fallback method.
`eval-minibuffer` and use returned object.

#### Editing keymap

Old keymap can be edited afterwards by calling `loophole-start-editing`.
Keymap which is been edited is displayed in mode line with
`loophole-mode-lighter-editing-sign`.
Details of mode line format is described in customization section.

#### Describe loophole-map

`loophole-describe` describes loophole-map.
As well as `M-x` `loophole-describe`,
`help-char` in minibuffer for some commands which reads loophole-map
ike `loophole-enable`, `loophole-name`, ... also invokes `loophole-describe`.
Multiple input of `help-char` in minibuffer cycles loophole-map description
among completion candidates.

#### Alternative binding commands

Loophole provides some binding commands other than `loophole-set-key`.
For example, `loophole-bind-entry`, `loophole-bind-command`,
`loophole-bind-kmacro` `loophole-bind-array`, `loophole-bind-keymap`
and `loophole-bind-symbol`.
They are a little more primitive or specific than `loophole-set-key`.
In most case, `loophole-set-key` may be sufficient,
but when you focused on specific binding entry,
the commands prefixed by `loophole-bind-` may be convenient.

## Customization

### Automation and timer

Loophole offers some customization functions for automation and timer.
Functions for automation setup some sequential call of loophole function like
`loophole-stop-editing` after some events.
Functions for timer setup some facilities to manage `timer`s of Emacs to
disable loophole-map or stop editing keymap.

These functions are prefixed by `loophole-turn-` on or off.
Adding them in your init file setup automation and timers.

They are also setter for custom variables like `loophole-use-auto-stop-editing`.
Hence, they can be called via `custom-set-variables`.
Default value of these custom variables are `t` for automation, and `nil` for
timer.

Note that `setq` for these variables does not work.


### Prefix arguments table of key binding commands

As mentioned above, `loophole-set-key` refers prefix arguments table
to determine how to specify the entry of key bindings.
The table is stored in `loophole-set-key-order`.
Each element of `loophole-set-key-order` is a function which takes one argument 
the key to be bound and returns any Lisp object suitable for key binding entry.

Each element optionally can be a list whose car is a
function described above, and cdr is a plist which has
a property :key.  It looks like
(OBTAIN-ENTRY :key READ-KEY).
READ-KEY is a function which takes no arguments and returns
key sequence to be bound.
However, if `loophole-determine-obtaining-method-after-read-key` is
t, or while it is negative-argument and
binding commands are called with `negative-argument`,
:key property will be omitted and default
`loophole-read-key` will be used.

Default value of `loophole-set-key-order` is

```emacs-lisp
(loophole-obtain-command-by-read-command
 loophole-obtain-kmacro-by-recursive-edit
 loophole-obtain-command-by-key-sequence
 (loophole-obtain-kmacro-by-read-key
  :key loophole-read-key-for-array-by-read-key)
 loophole-obtain-command-by-lambda-form
 loophole-obtain-kmacro-by-recall-record
 loophole-obtain-object)
```

If you prefer binding command by key sequence and keyboard macro by read key,
(They tends to need fewer input to bind something.)
and you do not need some other obtaining method, use the following lines.

``` emacs-lisp
(setq loophole-set-key-order
      '(loophole-obtain-command-by-key-sequence
        (loophole-obtain-kmacro-by-read-key
         :key loophole-read-key-for-array-by-read-key)
        loophole-obtain-command-by-read-command
        loophole-obtain-kmacro-by-recall-record))
```

Furthermore, if you prefer builtin `read-key-sequence` to read key for
`loophole-obtain-command-by-key-sequence`,
use the following lines.

``` emacs-lisp
(setq loophole-set-key-order
      '((loophole-obtain-command-by-key-sequence :key read-key-sequence)
        (loophole-obtain-kmacro-by-read-key
         :key loophole-read-key-for-array-by-read-key)
        loophole-obtain-command-by-read-command
        loophole-obtain-kmacro-by-recall-record))
```

To force using the setting above even while binding commands are called with
`negative-argument`, add the following line to your init file.
However, be carefull that this setting change order of prompt when
binding commands are called with `negative-argument`.

``` emacs-lisp
(setq loophole-determine-obtaining-method-after-read-key nil)
```

Some other binding commands (`loophole-bind-entry`, `loophole-bind-command`,
`loophole-bind-kmacro`, `loophole-bind-array`, `loophole-bind-keymap`,
`loophole-bind-symbol`) also use the prefix arguments table
(`loophole-bind-entry-order`, `loophole-bind-command-order`,
`loophole-bind-kmacro-order`, `loophole-bind-array-order`,
`loophole-bind-keymap-order`,`loophole-bind-symbol-order`).
It also can be customized by the same way.

Elements of these variable can contain :keymap property in addition to :key
property.
It looks like (OBTAIN-ENTRY :key READ-KEY :keymap OBTAIN-KEYMAP).
OBTAIN-KEYMAP is a function which takes two arguments the
key and entry to be bound, and returns keymap object on
which key and entry are bound; this overrides editing loophole-map.

You can also define your specifying method for entry.
The requirements for specifying method is that
it takes one argument the key to be bound and returns
any Lisp object suitable for key binding entry.
Entire customization codes may look like below.

``` emacs-lisp
(defun your-specifying-method (key)
  (let (entry)
    ; Some codes for specifying entry
    ; KEY may help to build prompt
    entry))
(setq loophole-set-key-order
      '(your-specifying-method
        loophole-obtain-key-and-command-by-read-command
        loophole-obtain-key-and-kmacro-by-recursive-edit
        loophole-obtain-key-and-command-by-key-sequence
        (loophole-obtain-kmacro-by-read-key
         :key loophole-read-key-for-array-by-read-key)
        loophole-obtain-key-and-command-by-lambda-form
        loophole-obtain-key-and-kmacro-by-recall-record
        loophole-obtain-key-and-object))
```

### Loophole mode map

By default, `loophole-mode-map` holds many key bindings
for manipulating loophole.
This may violate the key bindings of other features or your settings.

If the default `loophole-mode-map` does not suit with your environment,
overwrite it by using settings like the following lines.

``` emacs-lisp
(setcdr loophole-mode-map nil)
(define-key loophole-mode-map (kbd "C-c [") #'loophole-set-key)
(define-key loophole-mode-map (kbd "C-c ,") #'loophole-disable-latest-map)
```

### Mode line format

Loophole shows its status on mode-line dynamically.
By default, it shows `loophole-mode-lighter-base`,
`loophole-mode-lighter-suspending-sign` when suspending loophole,
`loophole-mode-lighter-editing-sign` when editing loophole-map,
and concatenated tags of enabled loophole-maps.
Here, tag is short string which represents loophole-map.

You can change this style by user option `loophole-mode-lighter` and
`loohpole-mode-lighter-preset-alist`.
If the dynamic lighter is annoying, use the static style as follows.
Then, lighter shows `loophole-mode-lighter-base` only.
This might improve performance.

``` emacs-lisp
(setq loophole-mode-lighter
      (cdr (assq 'static loophole-mode-lighter-preset-alist)))
```

If you want to use your own format,
it can be set directly to `loophole-mode-lighter`.
Any mode line construct is valid.

### Finish key for defining keyboard macro

When defining keyboard macro by recursive `read-key`,
the finish key is the key sequence for `keyboard-quit`.
The reason why `keyboard-quit` is employed is that
built-in keyboard macro system treat the key sequence for `keyboard-quit`
as just `keyboard-quit`, not the part of keyboard macro.
It can be changed by the following line.

``` emacs-lisp
(setq loophole-kmacro-by-read-key-finish-key (kbd "C-c C-c"))
```

### Other considerable user options

#### loophole-temporary-map-max

Maximum number of temporary keymap.
Keymaps will be stored temporarily up to `loophole-temporary-map-max`,
even if they are disabled.
When the number of generated keymaps exceeds `loophole-temporary-map-max`
and thereafter,
newly generated keymap overwrites the oldest one and completely abandons it.

Default value of this user option is `8`.

#### loophole-allow-keyboard-quit

The flag if loophole allows `keyboard-quit` during reading key for binding.
When `keyboard-quit` is allowed,
loophole cannot set key bindings for the key sequence of `keyboard-quit`,
which may be `C-g`.

Default value of this user option is `t`.

### Defining Loophole map

You can use existing or your own keymap and state variable on loophole.
To register them, use `loophole-register`.

Although loophole main focus is interactive interface,
you can setup your loophole-map during initialization by putting the following
forms in your init file.

```emacs-lisp
(defvar loophole-navigation-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "n") #'next-line)
    (define-key map (kbd "p") #'previous-line)
    (define-key map (kbd "f") #'scroll-up-command)
    (define-key map (kbd "b") #'scroll-down-command)
    map)
  "Keymap for simple navigation.")

(defvar loophole-navigation-map-state nil "State of `loophole-navigation-map'")
(make-variable-buffer-local 'loophole-navigation-map-state)

(loophole-register 'loophole-navigation-map 'loophole-navigation-map-state "n")
```

Loophole also offers the macro `loophole-define-map` to do this by one form.

```emacs-lisp
(loophole-define-map loophole-navigation-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "n") #'next-line)
    (define-key map (kbd "p") #'previous-line)
    (define-key map (kbd "f") #'scroll-up-command)
    (define-key map (kbd "b") #'scroll-down-command)
    map)
  "Keymap for simple navigation."
  loophole-navigation-map-state nil "State of `loophole-navigation-map'"
  "n")
```

In such cases, defining minor-mode is a canonical way.

```emacs-lisp
(define-minor-mode simple-navigation-mode
  "Minor mode for simple navigation."
  nil
  " Nav"
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "n") #'next-line)
    (define-key map (kbd "p") #'previous-line)
    (define-key map (kbd "f") #'scroll-up-command)
    (define-key map (kbd "b") #'scroll-down-command)
    map))
```

However, loophole offers some utilities for managing keymaps.
If your keymap is small, developing or with which loophole utilities work well,
please try loophole.

# Loophole

[![MELPA](https://melpa.org/packages/loophole-badge.svg)](https://melpa.org/#/loophole)

Loophole provides temporary key bindings management feature for Emacs.
Keys can be set by interactive interface in disposable keymaps
which are automatically generated for temporary use.

## Installation

### MELPA

Loophole is [available on MELPA](https://melpa.org/#/loophole).
Use `package.el` to install,
and add the following line to your init file like `init.el` or `.emacs`.

``` emacs-lisp
(loophole-mode)
```

### Manual

Download and save loophole.el to your `load-path`,
and add the following lines to your init file.

``` emacs-lisp
(require 'loophole)
(loophole-mode)
```

## Usage

### Basics

Call `loophole-set-key` to set a temporary key binding.

The key binding is set in the disposable keymap which is generated
automatically.
Once keymap is generated, `loophole-set-key` edit it for a while,
i.e. bindings will be set in it.
`loophole-stop-editing` stops editing, and then next `loophole-set-key`
generates another keymap.
You can see editing status on the mode-line as
`loophole-mode-lighter-editing-sign` (is "+" by default).

When you are done your operation with temporary key bindings,
call `loophole-disable-latest` to abandon that one.
Note that even after disabling the keymap,
it is stored in `loophole-n-map` for a while. (`n` is something integer.)
You can reactivate it by `loophole-enable`,
and can use multiple temporary keymaps at the same time.

### Example

When you encounter the case you just read the buffer temporarily,
the following key inputs provide simple navigating interface
which save your left little finger.

* `C-c [` `n` `next-line`
* `C-c [` `p` `previous-line`
* `C-c [` `f` `scroll-up-command`
* `C-c [` `b` `scroll-down-command`

When you are finished reading the buffer,
type `C-c \` (`loophole-disable-latest`) to abandon the key bindings above.
Then, your keymap environment stays clean.

### Details

#### Keymap cascade

Loophole uses three layers of keymaps.

First one is keymaps for temporary key bindings.
I call them as "Loophole map".
They are mainly generated automatically,
and added to `emulation-mode-map-alists`.

Second one is `loophole-base-map` which will be inherited to most Loophole maps.
This keymap offers the place for binding which can be used commonly when
any of Loophole map is activated.
For example, binding `C-q` to `loophole-disable-latest` on
`loophole-base-map` reduces keystroke against `C-c \`.
After all Loophole maps are disabled,
original binding of `C-q` to `quoted-insert` is recoverd.

Third one is the keymap for manipulating Loophole itself,
named `loophole-mode-map` which holds some Loophole commands.
This is, as the name suggests, activated when `loophole-mode` is enabled.

#### Buffer local behavior

All Loophole maps are shared globally but their activating states are buffer
local by default.
If you want to use Loophole map globally, use `loophole-globalize`.
`loophole-localize` can make globalized Loophole map back to buffer local.

Order of Loophole maps and editing map are also buffer local.
You can control these conditions for each buffer.

#### Naming Loophole map

If you like some Loophole map, naming them may help.
Loophole maps are initially named as `loophole-n-map`.
They can be renamed by `loophole-name`.
Once Loophole map gets a name other than `loophole-n-map`,
it goes out of range of automatic keymap generation and is never overwritten.

#### Loophole mode

`loophole-mode` is the minor mode for managing temporary key bindings.

When `loophole-mode` is enabled,
Loophole maps are added to the head of `emulation-mode-map-alists`,
and thus temporary key bindings are activated.
Furthermore, you can use `loophole-mode-map` for binding Loophole commands.

Even while `loophole-mode` is enabled, whole temporary key bindings can be
deactivated temporarily by calling `loophole-suspend`,
which keeps state of each Loophole map.
`loophole-resume` restores suspended temporary key bindings.
These functions realize suspension by removing and adding Loophole maps
in `emulation-mode-map-alists`.

Note that disabling `loophole-mode` also calls `loophole-suspend` and keeps
state of each Loophole map.
If you want to completely disable Loophole, use `loophole-quit`.
It disables all Loophole maps and `loophole-mode`.

Actually, Loophole can be used without enabling `loophole-mode`,
which just calls `loophole-resume` and set up auxiliary facilities,
that is some key bindings, mode-line lighter, and variable-watcher
which works only for speeding up internal process.
Major part of Loophole works independently of `loophole-mode`.
If you don't need the auxiliary facilities, use `loophole-resume`
instead of `loophole-mode` to activate temporary key bindings.

#### State variable

Each Loophole map has corresponding state variable.
If a value of state variable is non-nil, corresponding Loophole map is
activated.
Relation between Loophole map variable and state variable is analogous to
minor-mode-map variable and minor-mode variable.
State variable of Loophole map is usualy named as `loophole-map-name-state`.
For example, state variable of `loophole-1-map` is usualy
`loophole-1-map-state`.

You can set a state variable directly to turn on or off Loophole map,
but in most cases, the functions `loophole-enable` or `loophole-disable` are
useful.
These functions set state variable and run hooks `loophole-enable-functions`
or `loophole-disable-functions`, that may call some assisting functions.

#### Key binding entry

`loophole-set-key` ask you the command symbol to be bound
by using `read-command` like `global-set-key`.
However, you can bind not only command
but also keyboard macro, array, keymap and symbol as well.
Furthermore, you can choose obtaining method for these entries.
For example, command can alternatively be obtained by raw key sequence
which is found in the currently active keymaps.
Details of major obtaining method is described below.

You can use prefix arguments to specify
which key binding entry and its obtaining method is employed.
By default, `loophole-set-key` uses the following table, which is customizable.
Details of customization is described in the customization section below.

| Prefix arguments           | Entry   | Obtaining method |
|----------------------------|---------|------------------|
| No prefix arguments, `C-0` | command | read command     |
| `C-u`, `C-1`               | kmacro  | recursive edit   |
| `C-u` `C-u`, `C-2`         | command | key sequence     |
| `C-u`\*3, `C-3`            | kmacro  | read key         |
| `C-u`\*4, `C-4`            | command | lambda form      |
| `C-u`\*5, `C-5`            | kmacro  | recall record    |
| `C-u`\*6, `C-6`            | object  | eval minibuffer  |

If prefix argument is negative, `loophole-set-key` ask you the obtaining method
by using `completing-read`.

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
type `C-c C-c` (`loophole-read-buffer-finish-key`).
Then, buffer contents will be read and bound.
When you want to abort, type `C-c C-k` (`loophole-read-buffer-abort-key`).

When multiple forms are written in the buffer,
the forms other than the first will be omitted.

Note that while you are in recursive edit, it looks like top-level of Emacs
and you may feel that the control is returned to you,
but actually you are still in the command.
For that reason, it is recommended to call commands bound to either
`loophole-read-buffer-finish-key` or `loophole-read-buffer-abort-key`,
which can properly end recursive edit.

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

`read-key` recursively, and read keys will be the keyboard macro.
When you are finished, type finish key which may be `C-g`.
Reading key is operated by `loophole-read-key-until-termination-key` and finish
key is specified by customizable variable
`loophole-read-key-termination-key`,
whose default value is key sequence for `keyboard-quit`.
By default, you can only finish and cannot abort definition.
Once you set another key sequence to `loophole-read-key-termination-key`,
and `loophole-allow-keyboard-quit` ia non-nil,
you can abort by the key sequence bound to `keyboard-quit`.

##### kmacro by recall record

`competing-read` the `kmacro-ring`.
Therefore, the keyboard macro which defined in outside of Loophole
also can be bound.

##### object

Fallback method.
`eval-minibuffer` and use returned object.

#### Editing keymap

Old keymap can be edited afterwards by calling `loophole-start-editing`.
Keymap which is been edited is displayed in mode line with
`loophole-mode-lighter-editing-sign`.
Details of mode line format is described in customization section below.

#### Timers

Timers for disabling Loophole map and stopping editing session can be started
by `loophole-start-timer` and `loophole-start-editing-timer`.
Started timer can be controlled by `loophole-start-timer`,
`loophole-start-editing-timer` again, and `loophole-stop-timer`,
`loophole-stop-editing-timer`, `loophole-extend-timer` and
`loophole-extend-editing-timer`.

#### Save Loophole maps and use them in other session of Emacs

`loophole-save` saves Loophole maps into file storage and `loophole-load`
restores them.
The following lines in your init file call them automatically.

``` emacs-lisp
(with-eval-after-load 'loophole
  (loophole-load)
  (add-hook 'kill-emacs-hook #'loophole-save))
```

#### Describe Loophole map

`loophole-describe` describes Loophole map.
As well as `M-x` `loophole-describe`,
`help-char` in minibuffer for some commands which reads Loophole map
like `loophole-enable`, `loophole-name`, ... also invokes `loophole-describe`.
Multiple input of `help-char` in minibuffer cycles Loophole map description
among completion candidates.

#### Alternative binding commands

Loophole provides some binding commands other than `loophole-set-key`.
For example, `loophole-bind-entry`, `loophole-bind-command`,
`loophole-bind-kmacro` `loophole-bind-array`, `loophole-bind-keymap`
and `loophole-bind-symbol`.
They are a little more primitive or specific than `loophole-set-key`.
In most cases, `loophole-set-key` may be sufficient,
but when you focus on specific binding entry,
the commands prefixed by `loophole-bind-` may be convenient.

## Customization

Default settings of Loophole is conservative to follow convention and keep
behavior analogous to bare Emacs.
The following example violates
[key binding convention](https://www.gnu.org/software/emacs/manual/html_node/elisp/Key-Binding-Conventions.html)
and makes behavior Loophole original, but reduces key inputs and makes the
feature more convenient.
Subsequent section describes details of customization.
See also documentation strings of user options and commands.

### Example

``` emacs-lisp
(custom-set-variables
 '(loophole-read-buffer-inhibit-recursive-edit t)
 '(loophole-make-load-overwrite-map t)
 '(loophole-use-auto-timer t)
 '(loophole-use-auto-editing-timer t)
 '(loophole-use-auto-start-editing-for-existing-binding t)
 '(loophole-use-idle-prioritize t)
 '(loophole-use-idle-save t)
 '(loophole-read-key-termination-key (kbd "C-]"))
 '(loophole-defining-kmacro-map-tag
   "<End: \\[loophole-end-kmacro], Abort: \\[loophole-abort-kmacro]>")
 '(loophole-set-key-order
   '(loophole-obtain-command-by-key-sequence
     loophole-obtain-kmacro-on-top-level
     (loophole-obtain-command-by-read-command
      :key loophole-read-key-with-time-limit)
     loophole-obtain-kmacro-by-read-key
     loophole-obtain-command-by-lambda-form
     loophole-obtain-kmacro-by-recall-record
     loophole-obtain-symbol-by-read-keymap-function
     loophole-obtain-keymap-by-read-keymap-variable
     loophole-obtain-array-by-read-key
     (loophole-obtain-object :key loophole-read-key-with-time-limit)

     loophole-obtain-array-by-read-string
     loophole-obtain-symbol-by-read-array-function

     loophole-obtain-kmacro-by-recursive-edit
     loophole-obtain-symbol-by-read-command
     loophole-obtain-keymap-by-read-keymap-function)))

(loophole-mode)

(loophole-load)
(add-hook 'kill-emacs-hook #'loophole-save)

(define-key loophole-mode-map (kbd "C-]") #'loophole-set-key)
(define-key loophole-mode-map (kbd "M-]") #'loophole-unset-key)
(define-key loophole-mode-map (kbd "C-}") #'loophole-disable-latest)
(define-key loophole-mode-map (kbd "C-{") #'loophole-stop-editing)
(define-key loophole-mode-map (kbd "C-c [") nil)
(define-key loophole-mode-map (kbd "C-c \\") nil)
(define-key loophole-mode-map (kbd "C-c ] ]") #'loophole-enable)

(define-key loophole-defining-kmacro-map (kbd "C-c [") nil)
(define-key loophole-defining-kmacro-map (kbd "C-c \\") nil)
(define-key loophole-defining-kmacro-map (kbd "C-]") #'loophole-end-kmacro)
(define-key loophole-defining-kmacro-map (kbd "C-}") #'loophole-abort-kmacro)
(put 'loophole-end-kmacro :advertised-binding (kbd "C-]"))
(put 'loophole-abort-kmacro :advertised-binding (kbd "C-}"))
```

### Details

#### Automation

Loophole offers some customization functions for automation.
These functions setup some hooks and timers.
These functions are prefixed by `loophole-turn-` on or off.
Adding them in your init file setup automation.

For example, `loophole-turn-on-auto-stop-editing` setup hooks to call
`loophole-stop-editing` after some commands like `loophole-globalize`,
`loophole-enable`, `loophole-prioritize`, ...

They are also setter for custom variables like `loophole-use-auto-stop-editing`.
Hence, they can be called via `custom-set-variables`.
Note that because some of initial values of these custom variables are `t`,
some of `loophole-turn-on-*` are called when loading loophole.el.
If you want to prevent these calls, modify your init file as follows.

``` emacs-lisp

(custom-set-variables
 '(loophole-use-auto-prioritize nil)
 '(loophole-use-auto-stop-editing nil)
 '(loophole-use-auto-resume nil))

;; Before loading loophole.el

(loophole-mode)
```


#### Prefix arguments table of key binding commands

##### Basics

As mentioned above, `loophole-set-key` refers prefix arguments table
to decide how to obtain the entry of key bindings.
The table is stored in `loophole-set-key-order`.
Each element of `loophole-set-key-order` should be a function which takes one
argument the key to be bound and returns any Lisp object suitable for key
binding entry.

Each element optionally can be a list whose car is a
function described above, and cdr is a plist which has
a property `:key`.  It looks like `(OBTAIN-ENTRY :key READ-KEY)`.
`READ-KEY` is a function which takes one argument a standard prompt string
for reading key, and returns key sequence to be bound.
However, if `loophole-decide-obtaining-method-after-read-key` is
`t`, or while it is the symbol `negative-argument` and
binding commands are called with `negative-argument`,
`:key` property will be omitted and default
`loophole-read-key` will be used.

Default value of `loophole-set-key-order` is

```emacs-lisp
(loophole-obtain-command-by-read-command
 loophole-obtain-kmacro-by-recursive-edit
 loophole-obtain-command-by-key-sequence
 loophole-obtain-kmacro-by-read-key
 loophole-obtain-command-by-lambda-form
 loophole-obtain-kmacro-by-recall-record
 loophole-obtain-object)
```

If you prefer binding command by key sequence and keyboard macro by read key,
(they tends to need fewer input to bind something,)
and you do not need some other obtaining method, use the following lines.

``` emacs-lisp
(setq loophole-set-key-order
      '(loophole-obtain-command-by-key-sequence
        loophole-obtain-kmacro-by-read-key
        loophole-obtain-command-by-read-command
        loophole-obtain-kmacro-by-recall-record))
```

Alternatively, if you prefer your own obtaining method with default
`loophole-set-key-order`, use the following lines.

``` emacs-lisp
(defun your-obtaining-method (key)
  (let (entry)
    ; Some codes for obtaining entry
    ; KEY may help to build prompt
    entry))
(setq loophole-set-key-order
      '(your-obtaining-method
        loophole-obtain-key-and-command-by-read-command
        loophole-obtain-key-and-kmacro-by-recursive-edit
        loophole-obtain-key-and-command-by-key-sequence
        loophole-obtain-kmacro-by-read-key
        loophole-obtain-key-and-command-by-lambda-form
        loophole-obtain-key-and-kmacro-by-recall-record
        loophole-obtain-key-and-object))
```

Furthermore, if you prefer `loophole-read-key-with-time-limit` to read key for
`loophole-obtain-command-by-read-command`, use the following lines.

``` emacs-lisp
(setq loophole-set-key-order
      '((loophole-obtain-command-by-read-command
         :key loophole-read-key-with-time-limit)
        loophole-obtain-key-and-kmacro-by-recursive-edit
        loophole-obtain-key-and-command-by-key-sequence
        loophole-obtain-kmacro-by-read-key
        loophole-obtain-key-and-command-by-lambda-form
        loophole-obtain-key-and-kmacro-by-recall-record
        loophole-obtain-key-and-object))
```

`loophole-read-key-with-time-limit` asks user keys recursively until some idle
time (= 1.0 sec by default) is spent, and can read arbitrary key sequence.
Loophole offers one more reading key function variant,
`loophole-read-key-until-termination-key` which also can read arbitrary key
sequence.

To force using the setting for `:key` property even while binding commands are
called with `negative-argument`, add the following line to your init file.
However, be carefull that this setting change order of prompt when
binding commands are called with `negative-argument`.

``` emacs-lisp
(setq loophole-decide-obtaining-method-after-read-key nil)
```

##### Table for alternative binding commands

Some other binding commands (`loophole-bind-entry`, `loophole-bind-command`,
`loophole-bind-kmacro`, `loophole-bind-array`, `loophole-bind-keymap`,
`loophole-bind-symbol`) also use their own prefix arguments table
(`loophole-bind-entry-order`, `loophole-bind-command-order`,
`loophole-bind-kmacro-order`, `loophole-bind-array-order`,
`loophole-bind-keymap-order`,`loophole-bind-symbol-order`).
It also can be customized by the same way.

Elements of these `loophole-bind-*-order` may contain `:keymap` property in
addition to `:key` property.
It looks like `(OBTAIN-ENTRY :key READ-KEY :keymap OBTAIN-KEYMAP)`.
`OBTAIN-KEYMAP` is a function which takes one argument the key to be bound,
and returns keymap object on which key and entry are bound;
this overrides editing loophole map.
When using `:keymap` property, OBTAIN-ENTRY takes two arguments the key and
the keymap.
If you use your own obtaining method for `loophole-set-key` and
`loophole-bind-*`, it should takes one normal argument the key and one optional
argument the keymap.

#### Key bindings for Loophole commands

By default, Loophole uses some key sequences for Loophole commands.
They are defined in `loophole-mode-map`, and `loophole-defining-kmacro-map`.
They are all customizable.
If the default key sequences does not suit with your environment,
Modify these user options.

Especially, `loophole-mode-map` holds many key bindings.
To clear them out, use the following line.

``` emacs-lisp
(setcdr loophole-mode-map nil)
```

#### Mode line format

Loophole shows its status on mode-line dynamically.
By default, it shows `loophole-mode-lighter-base`,
`loophole-mode-lighter-suspending-sign` when suspending Loophole,
`loophole-mode-lighter-editing-sign` with tag of the map being edited when
editing Loophole map,
and concatenated tags of enabled Loophole maps.
Here, tag is a short string which represents Loophole map.

You can change this style by user option `loophole-mode-lighter` and
constant `loohpole-mode-lighter-preset-alist`.
If the dynamic lighter is annoying, use the static style as follows.
Then, lighter shows `loophole-mode-lighter-base` only.
This might improve performance.

``` emacs-lisp
(setq loophole-mode-lighter
      (cdr (assq 'static loophole-mode-lighter-preset-alist)))
```

If you want to use your own format,
it can be set directly to `loophole-mode-lighter`.
Any mode-line construct is valid.

#### Defining Loophole map

You can use existing or your own keymap as Loophole map, with a variable which
represents activation state of the map.
To register them, use `loophole-register`.

Although Loophole main focus is interactive interface,
you can setup your Loophole map during initialization by putting forms like
the following in your init file.

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

However, Loophole offers some utilities for managing keymaps.
If your keymap is small, developing or with which Loophole utilities work well,
please try Loophole.

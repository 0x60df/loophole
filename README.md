# Loophole

Loophole provides temporary key bindings management feature for Emacs.
Keys can be set by interactive interface in disposable keymaps
which are automatically generated for temporary use.

## Installation

Download and save loophole.el to your `load-path`,
and add the following line to you init file like `.emacs`.

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
Some actions or explicit call for `loophole-stop-edit` stop editing,
then next `loophole-set-key` generates another keymap.
You can see editing status on the mode-line.
If `loophole-mode-lighter-editing-sign` (is "+" by default)
is shown in mode-line, the latest keymap is been edited.

If you done your operation with temporary key bindings,
call `loophole-disable-last-map` to abandon that one.
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
* `C-c [` `v` `scroll-up-command`
* `C-c [` `V` `scroll-down-command`

When you are finished reading the buffer,
type `C-c \` (`loophole-disable-last-map`) to abandon the key bindings above.
Then, your keymap environment stays clean.

### Details

#### Keymap cascade

Loophole uses three layers of keymaps.

First one is keymaps for temporary key bindings.
I call them as "loophole-map".
They are mainly generated automatically,
and listed in `loophole-map-alist`.
They take effect by adding `loophole-map-alist` to `emulation-mode-map-alists`.

Second one is `loophole-base-map` which will be inherited to most of
loophole-map.
This keymap offers the place for binding which can be used commonly when
any of loophole-map is activated.
For example, binding `C-q` to `loophole-disable-last-map` reduces the number
of typing, and `C-q` recovers the original binding `quoted-insert` immediately
after all loophole-map are disabled.

Third one is the keymap for manipulating loophole itself,
named `loophole-mode-map` which holds some loophole commands.
This is, as the name suggests, activated when `loophole-mode` is enabled.

#### Naming loophole-map

If you like some loophole-map, naming them may help.
Loophole-maps are initially named as `loophole-n-map`.
They can be renamed by `loophole-name`.
Once loophole-map gets name other than `loophole-n-map`,
it goes out of range of automatic keymap generation and never overwritten.

#### Loophole mode

`loophole-mode` is the minor mode for managing temporary key bindings.

When `loophole-mode` is enabled,
`loophole-map-alist` is added to the head of `emulation-mode-map-alists`,
and thus temporary key bindings are activated.
Furthermore, you can use `loophole-mode-map` for binding loophole commands.

Even while `loophole-mode` is activated, whole temporary key bindings can be
disabled temporarily by calling `loophole-suspend`,
which keeps state of each loophole-map.
`loophole-resume` restores suspended temporary key bindings.
These functions realize suspension by removing and adding `loophole-map-alist`
in `emulation-mode-map-alists`.

Note that disabling `loophole-mode` also calls `loophole-suspend` and keeps
state of each loophole-map.
If you want to completely disable loophole, use `loophole-quit`.
It disables all loophole-maps and `loophole-mode`.

#### Key binding entry

`loophole-set-key` ask you the command symbol to be bound
by using `read-command` like `global-set-key`.
However, you can bind not only command
but also keyboard macro as key binding entry.
Furthermore, you can choose specifying method for these entries.
For example, command can alternatively be specified by key sequence
which is found in the currently active keymaps.
Details of all specifying method is described below.

You can use prefix arguments to specify
which key binding entry and its specifying method is employed.
By default, `loophole-set-key` uses the following table, which is customizable.
Details of customization is described in the customization section below.

| Prefix arguments    | Entry   | Specifying method |
| ------------------- | ------- | ----------------- |
| No prefix arguments | command | symbol            |
| `C-u`, `C-1`        | kmacro  | recursive edit    |
| `C-u` `C-u`, `C-2`  | command | key sequence      |
| `C-u`\*3, `C-3`     | kmacro  | read key          |
| `C-u`\*4, `C-4`     | kmacro  | recall record     |

##### command by symbol

Default method.
`read-command` and use read symbol as command.

##### command by key sequence

`read-key-sequence` and use the command which is bound with read key sequence
in currently active keymaps.

##### kmacro by recursive edit

Similar to built-in keyboard macro defining environment except that
we are in recursive edit.
In recursive edit, you can define your keyboard macro while you are getting
the feedback on your keys.
When you are finished, type `C-c )` (`loophole-end-kmacro`),
or you want to abort, type `C-c !` (`loophole-abort-kmacro`).

Note that while you are in recursive edit, it looks like top-level of Emacs
and you may feel the control is returned to you,
but actually you are still in the command.
Therefore, it is highly recommended to call `loophole-abort-kmacro`
even if you abort binding.
`C-g` (`keyboard-quit`) in recursive edit stops defining keyboard macro,
but do not abort recursive edit.
`loophole-abort-kmacro` takes care of it, and does both of them.

##### kmacro by read key

`read-key` recursively, and keys read will be the keyboard macro.
When you are finished, type completing key which may be `C-g`.
Completing key is specified by customizable variable
`loophole-kmacro-completing-key`,
whose default value is key sequence for `keyboard-quit`.
By default, you can only finish and cannot abort definition.
Once you set another key sequence to `loophole-kmacro-completing-key`,
you can finish by your completing key,
and can abort by the key sequence bound to `keyboard-quit`.

##### kmacro by recall record

`competing-read` the `kmacro-ring`.
Therefore, the keyboard macro which defined in outside of loophole
also can be bound.

#### Alternative binding commands

Loophole provides some binding commands other than `loophole-set-key`.
For example, `loophole-bind-entry`, `loophole-bind-command`,
and `loophole-bind-kmacro`.
They are a little more primitive or specific than `loophole-set-key`.
In most case, `loophole-set-key` may be sufficient,
but when you focused on specific binding entry,
the commands prefixed by `loophole-bind-` may be convenient.

## Customization

### Prefix arguments table of key binding commands

As mentioned above, `loophole-set-key` refers prefix arguments table
to determine how to specify the entry (and key) of key bindings.
The table is stored in `loophole-set-key-order`.
Each element of `loophole-set-key-order` is the function which returns
the list looks like `(KEY ENTRY)`

Default value of `loophole-set-key-order` is

```emacs-lisp
(loophole-obtain-key-and-command-by-symbol
 loophole-obtain-key-and-kmacro-by-recursive-edit
 loophole-obtain-key-and-command-by-key-sequence
 loophole-obtain-key-and-kmacro-by-read-key
 loophole-obtain-key-and-kmacro-by-recall-record)
```

If you prefer binding command by key sequence and keyboard macro by read key,
(They tends to need fewer input to bind something.)
and you do not need recursive edit environment for defining keyboard macro,
use the following lines.

``` emacs-lisp
(setq loophole-set-key-order
      '(loophole-obtain-key-and-command-by-key-sequence
        loophole-obtain-key-and-kmacro-by-read-key
        loophole-obtain-key-and-command-by-symbol
        loophole-obtain-key-and-kmacro-by-recall-record))
```

Some other commands (`loophole-bind-command`, `loophole-bind-kmacro`) also use
the prefix arguments table
(`loophole-bind-command-order`, `loophole-bind-kmacro-order`).
It also can be customized by the same way.

You can also define your specifying method for entry (and key).
The requirements for specifying method is that
it returns a list looks like `(KEY ENTRY)`.
Entire customization codes may look like below.

``` emacs-lisp
(defun your-specifying-method ()
  (let (key entry)
    ; Some codes for specifying key and entry
    (list key entry)))
(setq loophole-set-key-order
      '(your-specifying-method
        loophole-obtain-key-and-command-by-symbol
        loophole-obtain-key-and-kmacro-by-recursive-edit
        loophole-obtain-key-and-command-by-key-sequence
        loophole-obtain-key-and-kmacro-by-read-key
        loophole-obtain-key-and-kmacro-by-recall-record))
```

`loophole-bind-command` and `loophole-bind-kmacro` accept
more complex specifying method,
whose return value can contain two more additional element.
They are assigned to the arguments of
`loophole-bind-command` and `loophole-bind-kmacro`.
See documentation string of these functions for more details.

### Loophole mode map

By default, `loophole-mode-map` holds many key bindings
for manipulating loophole.
This may violate the key bindings of other features or your settings.

If the default `loophole-mode-map` does not suit with your environment,
overwrite it like the following lines.

``` emacs-lisp
(setcdr loophole-mode-map nil)
(define-key loophole-mode-map (kbd "C-c [") #'loophole-set-key)
(define-key loophole-mode-map (kbd "C-c ,") #'loophole-disable-last-map)
```

### Mode line format

Loophole shows its status on mode-line dynamically.
By default, it shows `loophole-mode-lighter-base`,
`loophole-mode-lighter-suspending-sign` when suspending loophole,
`loophole-mode-lighter-editing-sign` when editing loophole-map,
and the number of enabled loophole-maps.

You can change this style by the function `loophole-mode-set-lighter-format`.
For example, the following line make lighter show the list of tag string of
enabled loophole-map instead of the number.

``` emacs-lisp
(loophole-mode-set-lighter-format 'tag)
```

If the dynamic lighter is annoying, use the following line instead.
Then, lighter shows `loophole-mode-lighter-base` only.
This may improve performance.

``` emacs-lisp
(loophole-mode-set-lighter-format 'static)
```

If you want to use your own format,
the following line achieve this.

``` emacs-lisp
(loophole-mode-set-lighter-format 'custom your-format)
```

See documentation string of `loophole-mode-set-lighter-format` for more details.

### Completing key for defining keyboard macro

When defining keyboard macro by recursive `read-key`,
the completing key is the key sequence for `keyboard-quit`.
The reason why `keyboard-quit` is employed is that
built-in keyboard macro system treat the key sequence for `keyboard-quit`
as just `keyboard-quit`, not the part of keyboard macro.
It can be changed by the following line.

``` emacs-lisp
(setq loophole-kmacro-completing-key (kbd "C-c C-c"))
```

### Other user option

#### loophole-temporary-map-max

Maximum number of temporary keymap.
Keymaps will be stored temporarily up to `loophole-temporary-map-max`,
even if they are disabled.
When the number of generated keymaps exceeds `loophole-temporary-map-max`
and thereafter,
newly generated keymap overwrites the oldest one and completely abandoned it.

Default value of this user option is `8`.

#### loophole-allow-keyboard-quit

The flag if loophole allows `keyboard-quit` during reading key for binding.
When `keyboard-quit` is allowed,
loophole cannot set key bindings for the key sequence of `keyboard-quit`,
which may be `C-g`.

Default value of this user option is `t`.

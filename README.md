# Emacs Notes

## Calculations on Region

From: https://superuser.com/a/1077158

```
Highlight the numbers in the source buffer
M-x calc-grab-region (C-x * g)
M-x calc-grab-rectangle (C-x * r)
M-x calc-vector-sum, M-x calc-vector-mean, M-x calc-vector-sdev, etc.
```

## Changing Font Size

```
C-x C-+
C-x C--
C-x C-0
```

## Keyboard Macros

* [Keyboard Macros](https://www.gnu.org/software/emacs/manual/html_node/emacs/Keyboard-Macros.html)
* [The Keyboard Macro Counter](https://www.gnu.org/software/emacs/manual/html_node/emacs/Keyboard-Macro-Counter.html)

## Remote Files

* [Remote Files](https://www.gnu.org/software/emacs/manual/html_node/emacs/Remote-Files.html) in Emacs manual
* [Tramp](https://www.gnu.org/software/tramp/) manual

Edit file in a remote host. In general:
```
C-x C-f /<METHOD>:<USER>@<HOST>#<PORT>:<FILENAME>
```

Most useful connection methods:
* `docker`
* `ssh` (in Linux) `sshx` (in Windows)

See [connection types](https://www.gnu.org/software/tramp/#Connection-types) for all available methods.

In practice when the [SSH](https://en.wikipedia.org/wiki/Secure_Shell) has been configured properly:
```
C-x C-f /ssh:<HOST>:<FILENAME>
```

In Windows use [`sshx`](https://www.gnu.org/software/emacs/manual/html_node/tramp/Inline-methods.html#index-method-sshx) method instead.

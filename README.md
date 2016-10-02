# aws.el

Ultimate goal of this package is to replace the manipulation you do on aws console with emacs interface.

It is under construction, but you can try out what this package tries to achieve by:

   `M-x aws-instances`

You'll see a table-list buffer of AWS instances.
Note that it is a tablist buffer so various table view operations are supported.
(refer: https://github.com/politza/tablist )

For aws operation, currently following is implemented.

 * `S` will popup the state change; start, stop or terminate.
 * `I` will popup for inspection.
 * `A` will popup for other actions.
 * `C` will popup ssh configuration popup.
   Instance ssh info can be appended to `~/.ssh/config`.

# Requirements

You'll need to have aws-cli installed on your PC, and have it configured so that aws commands are available.

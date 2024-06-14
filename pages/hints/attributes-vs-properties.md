# Attributes vs Properties

In the `lustre/attribute` module, there are two functions for constructing a
Lustre `Attribute` value: `attribute` and `property`. There is a subtle but
significant difference between the two!

## Attributes

Attributes exist on the HTML element itself. If you open up your browser's dev
tools and inspect the HTML of a page you'll see any attributes you've created.
Likewise if you convert a Lustre `Element` to a string, any attributes will be
serialised into the HTML.

Attributes are *always* strings, even "boolean" attributes like `disabled` (for
these attributes the existence of the attribute is what makes it true, not the
value).

In JavaScript, you would set an attribute on an element like so:

```js
element.setAttribute("value", "wibble")
element.setAttribute("class", "wobble")
```

## Properties

Properties, on the other hand, exist on the DOM object that represents a particular
HTML element. Because they are object properties, they can be of *any* type not
just strings. For example, the `checked` property of an `input` element is a
boolean value, not a string.

Conversely, that means properties are not serialised into the HTML when you
convert a Lustre `Element` to a string, and they are not reflected in the HTML
when you inspect the page in your browser's dev tools.

In JavaScript, you would set a property on an element like so:

```js
element.value = "wibble"
element.className = "wobble"
```

Notice how the property names are not always the same as the attribute names, even
when their values are reflected between the two!

## When to use which

It's important to know about the element you are trying to render and whether it
is expecting an attribute or a property for a particular value. Often DOM properties
*reflect* attributes in a way that means you can use either, but sometimes that
is not the case.

For SVG elements, you often cannot use properties at all (they are typically
read-only) and must *always* use attributes.

Conversely, if you're using Lustre's components and wrote your `on_attribute_change`
decoders in a way that did not anticipate receiving strings you may find that
only properties work as you intended (the [decipher](https://hexdocs.pm/decipher/decipher.html)
package can help you decode number-like strings and booleans from attributes.)

## Other resources

Here are some other resources from around the Web that you might also find useful:

- [Attributes and Properties](https://javascript.info/dom-attributes-and-properties)
  from JavaScript.info.

- [HTML attributes vs DOM properties](https://jakearchibald.com/2024/attributes-vs-properties/)
  by Jake Archibald.

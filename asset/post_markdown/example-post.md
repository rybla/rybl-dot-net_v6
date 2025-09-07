---
title: Example Post
pubDate: 2025-08-26
tags:
  - example
  - markdown
  - post
---

# h1 Heading

Sample content.

## h2 Heading

Sample content.

### h3 Heading

Sample content.

#### h4 Heading

Sample content.

##### h5 Heading

Sample content.

###### h6 Heading

Sample content.

## Horizontal Rules

Sample content.

---

---

---

Sample content.

## Emphasis

**This is bold text**

**This is bold text**

_This is italic text_

_This is italic text_

~~Strikethrough~~

## Blockquotes

> Blockquotes can also be nested...
>
> > ...by using additional greater-than signs right next to each other...
> >
> > > ...or with spaces between arrows.

## Lists

Unordered

- Create a list by starting a line with `+`, `-`, or `*`
- Sub-lists are made by indenting 2 spaces:
  - Marker character change forces new list start:
    - Ac tristique libero volutpat at
    * Facilisis in pretium nisl aliquet
    - Nulla volutpat aliquam velit
- Very easy!

Ordered

1. Lorem ipsum dolor sit amet
2. Consectetur adipiscing elit
3. Integer molestie lorem at massa

4. You can use sequential numbers...
5. ...or keep all the numbers as `1.`

Start numbering with offset:

57. foo
1. bar

## Code

Inline `code`

Indented code

    // Some comments
    line 1 of code
    line 2 of code
    line 3 of code

Block code "fences"

```
Sample text here...
```

Syntax highlighting

```ts
functin f() {
  return 1;
}
```

```js
var foo = function (bar) {
  return bar++;
};

console.log(foo(5));
```

## Links

[link text](https://google.com)

[link with title](https://google.com)

Autoconverted link https://github.com/nodeca/pica (enable linkify to see)

## Images

![Minion](https://octodex.github.com/images/minion.png)
![Stormtroopocat](https://octodex.github.com/images/stormtroopocat.jpg "The Stormtroopocat")

Like links, Images also have a footnote style syntax

![Alt text][id]

With a reference later in the document defining the URL location:

[id]: https://octodex.github.com/images/dojocat.jpg "The Dojocat"

### Subscript / Superscript

- 19^th^
- H~2~O

### <ins>

++Inserted text++

### <mark>

==Marked text==

### Custom containers

::: warning
_here be dragons_
:::

### Custom code blocks

```my-dsl
this
  is
  my
  custom
    dsl
```

### Math

Inline math: $\int_{x=0}^\infty \frac{1}{x - 1}$.

Display math:

$$
\int_{x=0}^\infty \frac{1}{x - 1}
$$

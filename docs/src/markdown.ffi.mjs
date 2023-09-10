import { attribute } from "../lustre/lustre/attribute.mjs";
import { element, text } from "../lustre/lustre/element.mjs";
import { List, Empty, NonEmpty } from "./gleam.mjs";
import { fromMarkdown } from "mdast-util-from-markdown";
import { highlight_element } from "./highlight.ffi.mjs";
import * as Markdown from "./app/ui/markdown.mjs";

const empty = new Empty();
const singleton = (val) => new NonEmpty(val, empty);
const fold_into_list = (arr, f) =>
  arr.reduceRight((acc, val) => new NonEmpty(f(val), acc), empty);

function compute_hash(str) {
  let hash = 0;
  for (let i = 0, len = str.length; i < len; i++) {
    let chr = str.charCodeAt(i);
    hash = (hash << 5) - hash + chr;
    hash |= 0; // Convert to 32bit integer
  }
  return `${~hash}`;
}

function linkify(el) {
  for (const [t, url] of Object.entries(links)) {
    el.innerHTML = el.innerHTML.replace(
      new RegExp(`\\b${t}\\b`, "g"),
      `<a href="${url}" class="hover:underline">${t}</a>`
    );
  }
}

const base = import.meta.env.BASE_URL;
const stdlib = "https://hexdocs.pm/gleam_stdlib/gleam/";
const links = {
  App: `${base}api/lustre#app-type`,
  Attribute: `${base}api/lustre/attribute#attribute-type`,
  Bool: `${stdlib}bool.html`,
  Decoder: `${stdlib}dynamic.html#Decoder`,
  Dynamic: `${stdlib}dynamic.html#Dynamic`,
  Effect: `${base}api/lustre/effect#effect-type`,
  Element: `${base}api/lustre/element#element-type`,
  Error: `${base}api/lustre#error-type`,
  Float: `${stdlib}float.html`,
  Int: `${stdlib}int.html`,
  List: `${stdlib}list.html`,
  Map: `${stdlib}map.html#Map`,
  Option: `${stdlib}option.html#Option`,
  Result: `${stdlib}result.html`,
  String: `${stdlib}string.html`,
  StringBuilder: `${stdlib}string_builder.html#StringBuilder`,
};

const cashe = new Map();

export function parse_markdown(md) {
  window.requestAnimationFrame(() => {
    const selector = `[data-hash]:not(.hljs)`;

    for (const code of document.querySelectorAll(selector)) {
      highlight_element(code, code.dataset.lang).then(() => {
        linkify(code);
      });
    }
  });

  if (cashe.has(md)) return cashe.get(md);

  const ast = fromMarkdown(md);
  const summary = [];
  const content = fold_into_list(
    ast.children,
    function to_lustre_element(node) {
      switch (node.type) {
        case "code":
          return Markdown.code(node.value, compute_hash(node.value), node.lang);

        case "emphasis":
          return Markdown.emphasis(
            fold_into_list(node.children, to_lustre_element)
          );

        case "heading": {
          const [title, rest] = node.children[0].value.split("|");
          const tags = List.fromArray(rest ? rest.trim().split(" ") : []);
          const id =
            /^[A-Z]/.test(title.trim()) &&
            node.depth === 3 &&
            window.location.pathname.includes("/api/")
              ? `${title
                  .toLowerCase()
                  .trim()
                  .replace(/\s/g, "-")
                  .replace(/[^a-zA-Z0-9-_]/g, "")}-type`
              : `${title
                  .toLowerCase()
                  .trim()
                  .replace(/\s/g, "-")
                  .replace(/[^a-zA-Z0-9-_]/g, "")}`;

          if (node.depth > 1) {
            summary.unshift(
              element(
                "a",
                List.fromArray([
                  attribute("href", `#${id}`),
                  attribute("class", "text-sm text-gray-400 no-underline"),
                  attribute("class", "hover:text-gray-700 hover:underline"),
                  attribute(
                    "class",
                    node.depth === 2 ? `mt-4 first:mt-0` : `ml-2`
                  ),
                ]),
                singleton(text(title.trim()))
              )
            );
          }

          return Markdown.heading(node.depth, title.trim(), tags, id);
        }

        case "inlineCode":
          return Markdown.inline_code(node.value);

        case "link":
          return Markdown.link(
            node.url.startsWith("/")
              ? import.meta.env.BASE_URL + node.url.slice(1)
              : node.url,
            fold_into_list(node.children, to_lustre_element)
          );

        case "list":
          return Markdown.list(
            !!node.ordered,
            fold_into_list(node.children, to_lustre_element)
          );

        case "listItem":
          return Markdown.list_item(
            fold_into_list(node.children, to_lustre_element)
          );

        case "paragraph":
          return Markdown.paragraph(
            fold_into_list(node.children, to_lustre_element)
          );

        case "strong":
          return Markdown.strong(
            fold_into_list(node.children, to_lustre_element)
          );

        case "text":
          return Markdown.text(node.value);

        default:
          return Markdown.text("");
      }
    }
  );

  cashe.set(md, [content, List.fromArray(summary)]);
  return [content, List.fromArray(summary)];
}

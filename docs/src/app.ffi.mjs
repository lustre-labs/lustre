import { fromMarkdown } from "mdast-util-from-markdown";
import { attribute } from "../lustre/lustre/attribute.mjs";
import { element, text } from "../lustre/lustre/element.mjs";
import { List } from "./gleam.mjs";
import * as Markdown from "./app/ui/markdown.mjs";

// Parsing markdown and then walking the AST is expensive so we're going to trade
// some memory for speed and cache the results.
const cache = new Map();

// fn parse_markdown(md: String) -> #(List(Element(msg)), List(Element(msg)))
export const parse_markdown = (md) => {
  if (cache.has(md)) return cache.get(md);

  const ast = fromMarkdown(md);
  const summary = [];
  const content = ast.children.map(function to_lustre_element(node) {
    switch (node.type) {
      case "code":
        return Markdown.code(node.value);

      case "emphasis":
        return Markdown.emphasis(
          List.fromArray(node.children.map(to_lustre_element))
        );

      case "heading": {
        const [title, rest] = node.children[0].value.split("|");
        const tags = List.fromArray(rest ? rest.trim().split(" ") : []);
        const id =
          /^[A-Z]/.test(title.trim()) && node.depth === 3
            ? `${title.toLowerCase().trim().replace(/\s/g, "-")}-type`
            : `${title.toLowerCase().trim().replace(/\s/g, "-")}`;

        if (node.depth > 1) {
          summary.push(
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
              List.fromArray([text(title.trim())])
            )
          );
        }

        return Markdown.heading(node.depth, title.trim(), tags, id);
      }

      case "inlineCode":
        return Markdown.inline_code(node.value);

      case "link":
        return Markdown.link(node.url, List.fromArray(node.children));

      case "list":
        return Markdown.list(
          !!node.ordered,
          List.fromArray(node.children.map(to_lustre_element))
        );

      case "listItem":
        return Markdown.list_item(
          List.fromArray(node.children.map(to_lustre_element))
        );

      case "paragraph":
        return Markdown.paragraph(
          List.fromArray(node.children.map(to_lustre_element))
        );

      case "strong":
        return Markdown.strong(
          List.fromArray(node.children.map(to_lustre_element))
        );

      case "text":
        return Markdown.text(node.value);

      default:
        return Markdown.text("");
    }
  });

  const result = [List.fromArray(content), List.fromArray(summary)];
  cache.set(md, result);

  return result;
};

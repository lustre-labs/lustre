import "highlight.js/styles/github.css";

let did_register_html = false;
let did_register_js = false;
let did_register_sh = false;
let did_register_gleam = false;

export async function highlight_element(el, lang) {
  if (el.classList.contains("hljs")) return;

  const { default: highlight } = await import("highlight.js/lib/core");

  switch (lang) {
    case !did_register_html && "html":
      const { default: html } = await import("highlight.js/lib/languages/xml");
      highlight.registerLanguage("html", html);
      did_register_html = true;
      break;

    case !did_register_js && "javascript":
      const { default: js } = await import(
        "highlight.js/lib/languages/javascript"
      );
      highlight.registerLanguage("javascript", js);
      did_register_js = true;
      break;

    case !did_register_sh && "shell":
      const { default: sh } = await import("highlight.js/lib/languages/shell");
      highlight.registerLanguage("sh", sh);
      highlight.registerLanguage("shell", sh);
      did_register_sh = true;
      break;

    case !did_register_gleam && "gleam":
      highlight.registerLanguage("gleam", gleam);
      did_register_gleam = true;
      break;
  }

  highlight.highlightElement(el);
}

function gleam(hljs) {
  const KEYWORDS =
    "as assert case const fn if import let panic use opaque pub todo type";
  const STRING = {
    className: "string",
    variants: [{ begin: /"/, end: /"/ }],
    contains: [hljs.BACKSLASH_ESCAPE],
    relevance: 0,
  };
  const NAME = {
    className: "variable",
    begin: "\\b[a-z][a-z0-9_]*\\b",
    relevance: 0,
  };
  const DISCARD_NAME = {
    className: "comment",
    begin: "\\b_[a-z][a-z0-9_]*\\b",
    relevance: 0,
  };
  const NUMBER = {
    className: "number",
    variants: [
      {
        // binary
        begin: "\\b0[bB](?:_?[01]+)+",
      },
      {
        // octal
        begin: "\\b0[oO](?:_?[0-7]+)+",
      },
      {
        // hex
        begin: "\\b0[xX](?:_?[0-9a-fA-F]+)+",
      },
      {
        // dec, float
        begin: "\\b\\d(?:_?\\d+)*(?:\\.(?:\\d(?:_?\\d+)*)*)?",
      },
    ],
    relevance: 0,
  };

  return {
    name: "Gleam",
    aliases: ["gleam"],
    contains: [
      hljs.C_LINE_COMMENT_MODE,
      STRING,
      {
        // bit string
        begin: "<<",
        end: ">>",
        contains: [
          {
            className: "keyword",
            beginKeywords:
              "binary bytes int float bit_string bits utf8 utf16 utf32 " +
              "utf8_codepoint utf16_codepoint utf32_codepoint signed unsigned " +
              "big little native unit size",
          },
          KEYWORDS,
          STRING,
          NAME,
          DISCARD_NAME,
          NUMBER,
        ],
        relevance: 10,
      },
      {
        className: "function",
        beginKeywords: "fn",
        end: "\\(",
        excludeEnd: true,
        contains: [
          {
            className: "title",
            begin: "[a-z][a-z0-9_]*\\w*",
            relevance: 0,
          },
        ],
      },
      {
        className: "attribute",
        begin: "@",
        end: "\\(",
        excludeEnd: true,
      },
      {
        className: "keyword",
        beginKeywords: KEYWORDS,
      },
      {
        // Type names and constructors
        className: "title",
        begin: "\\b[A-Z][A-Za-z0-9]*\\b",
        relevance: 0,
      },
      {
        className: "operator",
        begin: "[+\\-*/%!=<>&|.]+",
        relevance: 0,
      },
      NAME,
      DISCARD_NAME,
      NUMBER,
    ],
  };
}

export default {
  content: ["./index.html", "./src/**/*.{mjs,gleam}"],
  theme: {
    extend: {
      fontFamily: {
        serif: ["Fraunces", "serif"],
        sans: ["Inter", "sans-serif"],
      },
      typography: (theme) => ({
        lustre: {
          css: {
            "--tw-prose-pre-code": "var(--tw-prose-body)",
            "--tw-prose-pre-bg": theme("colors.gray[50]"),
          },
        },
      }),
    },
  },
  plugins: [require("@tailwindcss/typography")],
};

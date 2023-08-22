export default {
  content: ["./index.html", "./src/**/*.{mjs,gleam}"],
  theme: {
    extend: {
      fontFamily: {
        sans: ["NTDapper"],
      },
    },
  },
  plugins: [require("@tailwindcss/typography")],
};

export function base() {
  return import.meta.env.BASE_URL;
}

export const fetch_post = (path, dispatch) => {
  fetch(`${import.meta.env.BASE_URL}page/${path.slice(1)}.md`)
    .then((res) => res.text())
    .then((content) => dispatch(content))
    .catch(console.error);
};

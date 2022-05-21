export const after = (f, delay) => {
    const id = window.setTimeout(() => {
        f(id)
    }, delay)
}

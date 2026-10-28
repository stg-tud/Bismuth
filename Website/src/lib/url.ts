/** prefix a site-absolute path with the configured base (e.g. /Bismuth) */
export const url = (path: string) => import.meta.env.BASE_URL.replace(/\/$/, "") + path;

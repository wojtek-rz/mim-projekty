/** @type {import('next').NextConfig} */
const nextConfig = {
  reactStrictMode: true,
  async rewrites() {
    return [
      {
        source: '/api/users/:path*',
        destination: 'http://users/' + 'users/:path*',
      },
      {
        source: '/api/auth/:path*',
        destination: 'http://users/' + 'auth/:path*',
      },
      {
        source: '/api/newsletters/:path*',
        destination: 'http://newsletters/' + 'newsletters/:path*',
      },
    ]
  },
  output: 'standalone',
}

module.exports = nextConfig

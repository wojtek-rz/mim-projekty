/** @type {import('next').NextConfig} */
const nextConfig = {
  reactStrictMode: true,
  async rewrites() {
    return [
      {
        source: '/api/:path*',
        destination: 'http://kubernetes.docker.internal/:path*',
      },
    ]
  },
  output: 'standalone',
}

module.exports = nextConfig

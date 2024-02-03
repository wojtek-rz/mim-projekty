import Image from 'next/image'
import { Inter } from 'next/font/google'
import { Headline } from '@/components/text'

const inter = Inter({ subsets: ['latin'] })

export default function Home() {
  return (
    <main className='flex flex-col gap-4'>
      <Headline>
        Newsletter app
      </Headline>

      <div>
        Newsletter app for JNP2 MIMUW course.
      </div>

      <div>
        Check out navigation bar for more info.
      </div>
    </main>
  )
}

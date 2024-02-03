import Nav from '@/components/nav'
import '@/styles/globals.css'

export default function App({ Component, pageProps }) {

   return (
      <div>
         <Nav />

         <div className="max-w-xl mx-auto my-10  p-4">

            <Component {...pageProps} />
         </div>
      </div>
   )
}

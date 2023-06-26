import { getAuthHeaderServer, getNewslettersAddrServer } from "@/components/api"
import { Headline } from "@/components/text"
import Main from "@/components/main"
import Link from "next/link"
import { Button } from "flowbite-react"

export const getServerSideProps = async ({ req, res, params }) => {
    const response = await fetch(getNewslettersAddrServer() + '/' + params.id,
        {
            headers: getAuthHeaderServer(req, res)
        }
    )

    const newsletter = await response.json()
    const join_link = process.env.PUBLIC_ADDR + `/newsletters/${newsletter.data.id}/join`
    return { props: { newsletter, join_link } }
}


function Newsletter({ newsletter: newsletterFull, join_link }) {
    const newsletter = newsletterFull.data
    const recipients = newsletterFull.recipients

    console.log(recipients)

    return (
        <Main>
            <div className="text-lg">
                <Headline>Newsletter</Headline>
                <div>
                    <b>Title: </b> {newsletter.title}
                </div>
                <div>
                    <b>Description: </b> {newsletter.content}
                </div>
                <div>
                    <b>Join link:  </b>
                    <Link href={join_link}>
                        {join_link}
                    </Link>
                </div>

            </div>
            {recipients.length > 0 && (
                 <Link href={`/newsletters/${newsletter.id}/send`}>
                 <Button>
 
                     Send email
                 </Button>
             </Link>)}
           

            

            <div className="text-lg">
                <Headline>Recipients</Headline>
                {recipients.map((recipient) => (
                    <div key={recipient}>
                        {recipient}
                    </div>
                ))}
                {recipients.length == 0 && (
                    <div>
                        No recipients
                    </div>
                    )}
            </div>

        </Main>
    );
}



export default Newsletter;
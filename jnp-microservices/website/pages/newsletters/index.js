import { getAuthHeaderServer, getNewslettersAddrServer } from "@/components/api"
import { Headline } from "@/components/text"
import Link from "next/link"
export const getServerSideProps = async ({ req, res }) => {
    const response = await fetch(getNewslettersAddrServer(),
        {
            headers: getAuthHeaderServer(req, res)
        }
    )

    const newsletters = await response.json()
    return { props: { newsletters } }
}

function Newsletters({ newsletters }) {
    return (
        <div>
            <Headline>Newsletters</Headline>
            <div>
                {newsletters.map((newsletter) => (
                    <Link key={newsletter.id} href="/newsletters/[id]" as={`/newsletters/${newsletter.id}`}>
                        <div className="p-3 border-b-grey-700 border-b-2 mb-1">
                            <div><b>Title: </b>{newsletter.title}</div>
                            <div><b>Description: </b>{newsletter.content}</div>
                        </div>
                    </Link>
                ))}
                {newsletters.length == 0 && (
                    <div>
                        No newsletters
                    </div>
                )}
            </div>
        </div>
    )
}

export default Newsletters;
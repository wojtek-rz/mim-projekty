import { getAuthHeaderServer, getNewslettersAddrServer, getUsersAddrServer } from "@/components/api"
import { Headline } from "@/components/text"
import Link from "next/link"

export const getServerSideProps = async ({ req, res, params }) => {
    const response = await fetch(getUsersAddrServer() + '/' + params.id + '/verify/' + params.code)
    
    const status = response.status
    return { props: { status } }
}

function Newsletters({ status }) {
    return (
        <div>
            {status == 200 && (<Headline>User verified</Headline>)}
            {status != 200 && (<Headline>Verification failed</Headline>)}
            <div>
                Status: {status}
            </div>
        </div>
    )
}

export default Newsletters;
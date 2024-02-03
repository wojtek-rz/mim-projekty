import { getNewslettersAddrServer } from "@/components/api";
import Main from "@/components/main";
import { Headline } from "@/components/text";


export async function getServerSideProps({ req, res, params }) {
    const response = await fetch(getNewslettersAddrServer() + '/' + params.id + '/recipients/' + params.rid,
        {
            method: 'DELETE'
         } 
    )

    const status = response.status
    return { props: { status } }
}

function Remove({status}) {
    return ( 
        <Main>
            <Headline>Removed!</Headline>
            Status {status}
        </Main>
     );
}

export default Remove;
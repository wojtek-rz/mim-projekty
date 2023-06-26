import Main from "@/components/main";
import { Headline } from "@/components/text";
import { Button } from "flowbite-react";
import Link from "next/link";

function Sent() {
    return (
        <Main>

            <Headline>
                Emails are being sent!
            </Headline>

            <Link href="/newsletters">
                <Button>
                    Back to newsletters
                </Button>
            </Link>
        </Main>
    );
}

export default Sent;
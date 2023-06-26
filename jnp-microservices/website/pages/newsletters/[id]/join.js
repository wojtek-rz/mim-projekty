import { getNewslettersAddrServer, getNewslettersAddrClient } from "@/components/api";
import {useRouter} from 'next/router'
import { useState } from "react";
import { Button, Checkbox, Label, TextInput, Toast, HiX } from 'flowbite-react';
import Main from "@/components/main";
import { Headline } from "@/components/text";

export const getServerSideProps = async ({ req, res, params }) => {
    const response = await fetch(getNewslettersAddrServer() + '/' + params.id)
    
    const newsletter = await response.json()
    return { props: { newsletter } }
}

function CreateUser({ newsletter: newsletterFull}) {
    const newsletter = newsletterFull.data
    const [error, setError] = useState("")
    const router = useRouter()

    const handleSubmit = async (e) => {
        e.preventDefault()
        const body = {
            email: e.currentTarget.email1.value,
        }
        const res = await fetch(getNewslettersAddrClient() + "/" + newsletter.id + "/recipients", {
            method: 'POST',
            headers: { 'Content-Type': 'application/json' },
            body: JSON.stringify(body),
        })
        const result = await res.json()
        if (result.error) {
            setError(result.error)
        }
        else {
            router.push('/newsletters/' + newsletter.id + '/joined')
        }
    }
    console.log(newsletter)


    return (
        <Main>
            <Headline>Join newsletter: {newsletter.title} </Headline>
                {newsletter.content}
            {error && (
                <Toast>
                    <div className="text-red-500 ml-3 text-sm font-normal">
                    {error}
                    </div>
                    <Toast.Toggle />
                </Toast>
            )}
            <form onSubmit={handleSubmit} className="flex max-w-md flex-col gap-4">
                <div>
                    <div className="mb-2 block">
                        <Label
                            htmlFor="email1"
                            value="Your email"
                        />
                    </div>
                    <TextInput
                        id="email1"
                        placeholder="name@flowbite.com"
                        required
                        type="email"
                    />
                </div>
                <Button type="submit">
                    Join
                </Button>
            </form>
        </Main>
    );
}

export default CreateUser;
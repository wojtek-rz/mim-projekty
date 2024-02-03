import { getAuthHeaderClient, getNewslettersAddrClient, getUsersAddrClient } from "@/components/api";
import { useRouter } from 'next/router'
import { useState } from "react";
import { Button, Checkbox, Label, TextInput, Toast, Textarea } from 'flowbite-react';
import Main from "@/components/main";
import { Headline } from "@/components/text";
import cookie, { useCookies } from "react-cookie";
import Link from "next/link";

function CreateUser() {
    const [error, setError] = useState("")
    const [cookies, setCookies] = useCookies(['token'])
    const router = useRouter()

    console.log(cookies)

    const handleSubmit = async (e) => {
        e.preventDefault()
        const body = {
            title: e.currentTarget.title.value,
            content: e.currentTarget.content.value,
        }
        const res = await fetch(getNewslettersAddrClient(), {
            method: 'POST',
            headers: {
                'Content-Type': 'application/json',
                ...getAuthHeaderClient(cookies?.token)
            },
            body: JSON.stringify(body),
        })
        const result = await res.json()
        if (result.error) {
            setError(result.error)
        }
        else {
            router.push('/newsletters/' + result.id)
        }
    }


    return (
        <Main>
            <Headline>Create newsletter</Headline>
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
                            htmlFor="title"
                            value="Your title"
                        />
                    </div>
                    <TextInput
                        id="title"
                        required
                        type="text"
                        placeholder="My newsletter"
                    />
                </div>
                <div>
                    <div className="mb-2 block">
                        <Label
                            htmlFor="content"
                            value="Description"
                        />
                    </div>
                    <Textarea
                        id="content"
                        placeholder="Write something..."
                        rows={4}
                    />
                </div>
                <Button type="submit">
                    Create newsletter
                </Button>
            </form>
        </Main>
    );
}

export default CreateUser;
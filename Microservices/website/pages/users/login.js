import { getAuthAddrClient, getUsersAddrClient } from "@/components/api";
import {useRouter} from 'next/router'
import { useState } from "react";
import { Button, Checkbox, Label, TextInput, Toast, HiX } from 'flowbite-react';
import Main from "@/components/main";
import { Headline } from "@/components/text";
import Link from "next/link";
import { useCookies } from "react-cookie";

function CreateUser() {
    const [error, setError] = useState("")
    const [cookie, setCookie] = useCookies(['token'])
    const router = useRouter()

    const handleSubmit = async (e) => {
        e.preventDefault()
        const body = {
            email: e.currentTarget.email1.value,
            password: e.currentTarget.password1.value,
        }
        const res = await fetch(getAuthAddrClient(), {
            method: 'POST',
            headers: { 'Content-Type': 'application/json' },
            body: JSON.stringify(body),
        })
        const result = await res.json()
        if (result.error) {
            setError(result.error)
        }
        else {
            setCookie('token', result.token, { path: '/' })
            router.push('/newsletters')
        }
    }


    return (
        <Main>
            <Headline>Login</Headline>
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
                <div>
                    <div className="mb-2 block">
                        <Label
                            htmlFor="password1"
                            value="Your password"
                        />
                    </div>
                    <TextInput
                        id="password1"
                        required
                        type="password"
                    />
                </div>
                <Button type="submit">
                    Login
                </Button>
            </form>

            <Link href="/users/create" className="text-blue-600">Create account here.</Link>
        </Main>
    );
}

export default CreateUser;
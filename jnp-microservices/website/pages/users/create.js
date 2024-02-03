import { getUsersAddrClient } from "@/components/api";
import {useRouter} from 'next/router'
import { useState } from "react";
import { Button, Checkbox, Label, TextInput, Toast, HiX } from 'flowbite-react';
import Main from "@/components/main";
import { Headline } from "@/components/text";

function CreateUser() {
    const [error, setError] = useState("")
    const router = useRouter()

    const handleSubmit = async (e) => {
        e.preventDefault()
        const body = {
            email: e.currentTarget.email1.value,
            password: e.currentTarget.password1.value,
        }
        const res = await fetch(getUsersAddrClient(), {
            method: 'POST',
            headers: { 'Content-Type': 'application/json' },
            body: JSON.stringify(body),
        })
        const result = await res.json()
        console.log(getUsersAddrClient(), result.error)
        if (result.error) {
            setError(result.error)
        }
        else {
            router.push('/users/created')
        }
    }


    return (
        <Main>
            <Headline>Create user</Headline>
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
                        placeholder="example@example.com"
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
                <div className="text-red-500">
                    Don't use any of your real passwords here!
                </div>
                <Button type="submit">
                    Create
                </Button>
            </form>
        </Main>
    );
}

export default CreateUser;
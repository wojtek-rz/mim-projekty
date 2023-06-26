import { getUsersAddrServer } from "@/components/api";

export const getServerSideProps = async () => {
    const res = await fetch(getUsersAddrServer())
    const users = await res.json()
    return { props: { users } }
}

function UsersPage({users}) {

    return ( // string of users
        <div>
            {JSON.stringify(users)}
        </div>
     );
}

export default UsersPage;
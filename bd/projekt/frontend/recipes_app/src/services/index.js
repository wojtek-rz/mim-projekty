import axios from "axios"

const base_url = "http://127.0.0.1:8000/"

export default function fetchUrl(url) {
    return axios.get(base_url + url).then(res => res.data)
}
const baseAddr = '/api'
import Cookies from 'cookies'

export function getUsersAddrClient() {
    return baseAddr + "/users";
}

export function getNewslettersAddrClient() {
    return baseAddr + "/newsletters";
}

export function getAuthAddrClient() {
    return baseAddr + "/auth";
}

export function getNewslettersAddrServer() {
    return  process.env.NEWSLETTER_SERVICE_ADDR + '/newsletters';
}

export function getUsersAddrServer() {
    return process.env.USER_SERVICE_ADDR + '/users';
}

export function getAuthAddrServer() {
    return process.env.USER_SERVICE_ADDR + '/auth';
}

export function getAuthHeaderServer(req, res) {
    const cookies = new Cookies(req, res)
    const token = cookies.get('token')
    if (!token) {
        return {}
    }
    return {'Authorization': `Bearer ${token}`}
}

export function getAuthHeaderClient(token) {
    if (!token) {
        return {}
    }
    return {'Authorization': `Bearer ${token}`}
}
import React from 'react';
import { Outlet } from "react-router-dom";
import {
    Box,
    Flex,
    HStack,
    Link,
    IconButton,
    useDisclosure,
    useColorModeValue,
} from '@chakra-ui/react';
import { HamburgerIcon, CloseIcon } from '@chakra-ui/icons';


const NavLink = ({ children, type, href }) => {
    console.log(type);
    let font_color = 'white'
    if (type == 'primary') font_color = 'white';
    if (type == 'secondary') font_color = 'whiteAlpha.800';

    return (
        <Link
            px={4}
            py={2}
            rounded={'lg'}
            _hover={{
                textDecoration: 'none',
                bg: useColorModeValue('red.900', 'red.900'),
            }}
            color={font_color}
            href={href}>
            {children}
        </Link>
    );
}

export default function Navbar() {
    const { isOpen, onOpen, onClose } = useDisclosure(); // hamburger menu

    return (
        <>
            <Flex bg={useColorModeValue('red.800', 'red.800')} px={4}
                py={3} alignItems={'center'} justifyContent={'space-between'}
                color={'white'} fontWeight={'bold'} fontSize={'md'}>
                <IconButton
                    size={'md'}
                    icon={isOpen ? <CloseIcon /> : <HamburgerIcon />}
                    aria-label={'Open Menu'}
                    display={{ md: 'none' }}
                    onClick={isOpen ? onClose : onOpen}
                />
                <Flex gap={8} alignItems={'center'}>
                    <Box py={2} px={4} mx={4} fontSize={'xl'}>Logo here</Box>
                    <HStack
                        as={'nav'} gap={2} display={{ base: 'none', md: 'flex' }}>
                        <NavLink type='secondary' href="/">Home</NavLink>
                        <NavLink type='secondary' href="/recipes">Recipes</NavLink>
                        <NavLink type='secondary' href="/ingredients">Ingredients</NavLink>
                        <NavLink type='secondary' href="/top users">Top users</NavLink>
                    </HStack>
                </Flex>
                <Flex alignItems={'center'}>
                    <NavLink type='primary' href="/signup">Sign Up</NavLink>
                </Flex>
            </Flex>
        </>
    );
}
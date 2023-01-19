import {
    Box,
    chakra,
    Container,
    Flex,
    Link,
    Stack,
    Text,
    useColorModeValue,
    VisuallyHidden,
} from '@chakra-ui/react';
import { ReactNode } from 'react';

export default function Footer() {
    return (
        <Box
            width={'100%'}
            mt="20"
            bg={useColorModeValue('gray.50', 'gray.900')}
            color={useColorModeValue('gray.700', 'gray.200')}>
            <Container
                as={Stack} direction={'row'} justifyContent={'space-between'} py={4} maxW={'6xl'} 
                    spacing={4}>
                <Box>
                Recipe app for database course MIMUW

                </Box>

                <Link href={'#'}>Author</Link>
            </Container>
            <Box
                borderTopWidth={1}
                borderStyle={'solid'}
                borderColor={useColorModeValue('gray.200', 'gray.700')}>
                <Container
                    as={Stack}
                    maxW={'6xl'}
                    py={4}
                    direction={{ base: 'column', md: 'row' }}
                    spacing={4}
                    justify={{ base: 'center', md: 'center' }}
                    align={{ base: 'center', md: 'center' }}>
                    <Text>© 2022 Chakra Templates. All rights reserved</Text>
                    <Box>
                </Box>
                </Container>
            </Box>
        </Box>
    );
}
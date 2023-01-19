import React from 'react';
import {
    Box,
    Heading,
    Link,
    Image,
    Text,
    Divider,
    HStack,
    Tag,
    Wrap,
    WrapItem,
    useColorModeValue,
    Container,
    VStack,
} from '@chakra-ui/react';

const BlogTags = (props) => {
    return (
        <HStack spacing={2} marginTop={props.marginTop}>
            {props.tags.map((tag) => {
                return (
                    <Tag size={'md'} variant="solid" colorScheme="orange" key={tag}>
                        {tag}
                    </Tag>
                );
            })}
        </HStack>
    );
};

const RecipeCard = (props) => (
    <WrapItem width={{ base: '100%', sm: '45%', md: '45%', lg: '30%' }}>
        <Box w="100%">
            <BlogTags tags={['Engineering', 'Product']} marginTop="3" />
            <Heading fontSize="xl" marginTop="2">
                <Link textDecoration="none" _hover={{ textDecoration: 'none' }}>
                    Some blog title
                </Link>
            </Heading>
            <Text as="p" fontSize="md" marginTop="2">
                Some blog text
            </Text>
        </Box>
    </WrapItem>
)


const ArticleList = () => {
    return (
        <Container maxW={'7xl'} p="12">
            <Heading as="h1">Discover thousands of delicious recipes with our new website!</Heading>
            <Box
                marginTop={{ base: '1', sm: '5' }}
                display="flex"
                flexDirection={{ base: 'column', md: 'row' }}
                justifyContent="space-between">
                <Box
                    display="flex"
                    flex="1"
                    marginRight="3"
                    position="relative"
                    alignItems="center">
                    <Box
                        width={{ base: '100%', sm: '85%' }}
                        zIndex="2"
                        marginLeft={{ base: '0', sm: '5%' }}
                        marginTop="5%">
                        <Link textDecoration="none" _hover={{ textDecoration: 'none' }}>
                            <Image
                                borderRadius="lg"
                                src={
                                    'https://images.unsplash.com/photo-1499951360447-b19be8fe80f5?ixlib=rb-1.2.1&ixid=MXwxMjA3fDB8MHxwaG90by1wYWdlfHx8fGVufDB8fHw%3D&auto=format&fit=crop&w=800&q=80'
                                }
                                alt="some good alt text"
                                objectFit="contain"
                            />
                        </Link>
                    </Box>
                    <Box zIndex="1" width="100%" position="absolute" height="100%">
                        <Box
                            bgGradient={useColorModeValue(
                                'radial(orange.600 1px, transparent 1px)',
                                'radial(orange.300 1px, transparent 1px)'
                            )}
                            backgroundSize="20px 20px"
                            opacity="0.4"
                            height="100%"
                        />
                    </Box>
                </Box>
                <Box
                    display="flex"
                    flex="1"
                    flexDirection="column"
                    justifyContent="center"
                    marginTop={{ base: '5', md: '0' }}>
                    <Text mt="2" as="p"
                        color={useColorModeValue('gray.700', 'gray.200')}
                        fontSize="lg">

                        Our easy-to-use platform allows you to search and discover new recipes
                        based on ingredients, tags, and dietary preferences. With convenient
                        tools for organizing and planning your meals, you'll never have to settle
                        for a boring dinner again.
                    </Text>

                    <Heading fontSize={"3xl"} fontWeight={"bold"} mt="5" >
                        <Link textDecoration="none" _hover={{ textDecoration: 'none', bgColor: 'red.500' }}
                            bgColor={'red.600'} rounded={'xl'} px={5} py={2} color={'whiteAlpha.900'}
                            href="/recipes" >
                            Search for recipes!
                        </Link>
                    </Heading>
                </Box>
            </Box>
            <Heading as="h2" marginTop="5">
                Latest articles
            </Heading>
            <Divider marginTop="5" />
            <Wrap spacing="30px" marginTop="5">
                <RecipeCard />
            </Wrap>
            <VStack paddingTop="40px" spacing="2" alignItems="flex-start">
                <Heading as="h2">What we write about</Heading>
                <Text as="p" fontSize="lg">
                    Lorem ipsum dolor sit amet, consectetur adipiscing elit. Donec
                    condimentum quam arcu, eu tempus tortor molestie at. Vestibulum
                    pretium condimentum dignissim. Vestibulum ultrices vitae nisi sed
                    imperdiet. Mauris quis erat consequat, commodo massa quis, feugiat
                    sapien. Suspendisse placerat vulputate posuere. Curabitur neque
                    tortor, mattis nec lacus non, placerat congue elit.
                </Text>
                <Text as="p" fontSize="lg">
                    Lorem ipsum dolor sit amet, consectetur adipiscing elit. Donec
                    condimentum quam arcu, eu tempus tortor molestie at. Vestibulum
                    pretium condimentum dignissim. Vestibulum ultrices vitae nisi sed
                    imperdiet. Mauris quis erat consequat, commodo massa quis, feugiat
                    sapien. Suspendisse placerat vulputate posuere. Curabitur neque
                    tortor, mattis nec lacus non, placerat congue elit.
                </Text>
                <Text as="p" fontSize="lg">
                    Lorem ipsum dolor sit amet, consectetur adipiscing elit. Donec
                    condimentum quam arcu, eu tempus tortor molestie at. Vestibulum
                    pretium condimentum dignissim. Vestibulum ultrices vitae nisi sed
                    imperdiet. Mauris quis erat consequat, commodo massa quis, feugiat
                    sapien. Suspendisse placerat vulputate posuere. Curabitur neque
                    tortor, mattis nec lacus non, placerat congue elit.
                </Text>
            </VStack>
        </Container>
    );
};

export default ArticleList;
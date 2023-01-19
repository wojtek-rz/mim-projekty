import { Box, Heading, Text } from "@chakra-ui/react";

export default function RecipeCard({recipe}){
    return (
        <Box rounded="xl" shadow="lg" p="6" width="100%">
            <Heading size="md" mb="4">{recipe.title}</Heading>
            <Text>{recipe.description}</Text>
        </Box>
    )
}
import { VStack, Heading } from '@chakra-ui/react'
import { useState } from 'react'
import { useLoaderData } from 'react-router-dom'
import RecipeCard from '../components/recipeCard'
import fetchUrl from '../services'

export async function recipes_loader() {
    const recipes = await fetchUrl('recipes')
    console.log(recipes)
    return { recipes }
}

export default function Recipes() {
    const { recipes } = useLoaderData();

    return (
        <div className="App">
            <Heading size="2xl" mb="10" >Recipes</Heading>
            <VStack gap="7">
                {recipes.map((recipe) => (
                    <RecipeCard key={recipe.id} recipe={recipe} />
                ))}
            </VStack>
        </div>)
}
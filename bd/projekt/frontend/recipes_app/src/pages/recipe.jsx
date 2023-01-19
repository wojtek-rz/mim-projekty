import { useLoaderData } from "react-router-dom";
import fetchUrl from "../services";

export async function recipe_loader({ params }) {
    const recipe = await fetchUrl(`recipes/${params.recipe_id}`)
    return { recipe }
}

export default function Recipe(props) {
    const { recipe } = useLoaderData(props.id);

    return (
        <div>
            <h1>Recipe</h1>
            <h2>{recipe.title}</h2>
            <p>{recipe.description}</p>
        </div>)
}
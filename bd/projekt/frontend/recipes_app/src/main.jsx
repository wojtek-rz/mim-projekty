import React from 'react'
import ReactDOM from 'react-dom/client'
import {
    createBrowserRouter,
    RouterProvider,
} from "react-router-dom";
import { ChakraProvider, extendTheme } from '@chakra-ui/react'
import Root from './pages/root.jsx'
import Index from './pages/index.jsx';
import Error from './pages/error.jsx'
import Recipes, { recipes_loader } from './pages/recipes.jsx';
import Recipe, { recipe_loader } from './pages/recipe.jsx';


const router = createBrowserRouter([
    {
        path: "/",
        element: <Root />,
        errorElement: <Error />,
        children: [
            {
                path: "/",
                element: <Index />,
            },
            {
                path: "recipes/",
                element: <Recipes />,
                loader: recipes_loader,
            },
            {
                path: "recipes/:recipe_id",
                element: <Recipe />,
                loader: recipe_loader,
            },
        ],
    }
]);


const theme = extendTheme({
    colors: {
        brand: {
            100: "#3b5363",
            900: "red.900",
        },
    },
})


ReactDOM.createRoot(document.getElementById('root')).render(
    <React.StrictMode>
        <ChakraProvider theme={theme}>
            <RouterProvider router={router} />
        </ChakraProvider>
    </React.StrictMode>,
)

import { Box, Flex } from "@chakra-ui/react";
import Navbar from "../components/navbar";
import { Outlet } from "react-router-dom";
import Footer from "../components/footer";

export default function Root(props){
    return (
        <Flex direction="column" height="100vh">
            <Navbar />
            <Box p={4} mx="auto" maxW={900} flex="1 0 auto">
                    <Outlet />
            </Box>
            <Box flexShrink="0">
                <Footer />
            </Box>
        </Flex>
    )
            
}
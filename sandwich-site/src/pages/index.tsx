
import React from "react";
import { Redirect } from "@docusaurus/router";
import useDocusaurusContext from "@docusaurus/useDocusaurusContext";

import "react-responsive-carousel/lib/styles/carousel.min.css";


function Home() {
  const {siteConfig} = useDocusaurusContext();

  return <Redirect to={siteConfig.baseUrl + "docs"} />;
}

export default Home;

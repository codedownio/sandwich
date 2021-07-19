
import React from "react";
import clsx from "clsx";
import Layout from "@theme/Layout";
import Link from "@docusaurus/Link";
import useDocusaurusContext from "@docusaurus/useDocusaurusContext";
import useBaseUrl from "@docusaurus/useBaseUrl";
import Typist from "react-typist";

import styles from "./styles.module.css";

import "react-responsive-carousel/lib/styles/carousel.min.css";
import { Carousel } from "react-responsive-carousel";


const features = [
  {
    title: "Powerful formatters",
    imageUrl: "img/print_formatter.png",
    description: (
      <>
          Use the terminal UI formatter to inspect test results, re-run selected tests, or jump straight to test sources in your editor. Or, choose one of the more traditional formatters.
      </>
    ),
  }, {
    title: "Easy profiling",
    imageUrl: "img/speedscope.png",
    description: (
      <>
          Automatically record timing data for every test suite and visualize it as a flamegraph.
      </>
    ),
  }, {
    title: "Zero-config Selenium tests",
    imageUrl: "img/selenium_logo.png",
    description: (
      <>
          Automatically obtain Selenium driver binaries to match your Chrome or Firefox version, run tests in graphical or headless mode, and even record videos.
      </>
    ),
  },
];

function Feature({imageUrl, title, description}) {
  const imgUrl = useBaseUrl(imageUrl);
  return (
    <div className={clsx("col col--4", styles.feature)}>
        {imgUrl && (
          <div className="text--center">
              <img className={styles.featureImage} src={imgUrl} alt={title} />
          </div>
        )}
        <h3>{title}</h3>
        <p>{description}</p>
    </div>
  );
}

function Home() {
  const context = useDocusaurusContext();
  const {siteConfig = {}} = context;
  return (
    <Layout title={`Hello from ${siteConfig.title}`}
            description="Yet another test framework for Haskell.">
        <header className={styles.heroBanner}>
            <div className="container">
                <h1 className="hero__title">{siteConfig.title}</h1>
                <p className="hero__subtitle">{siteConfig.tagline}</p>

                <div className={styles.carouselContainer}>
                    <Carousel showThumbs={false}>
                        <div>
                            <div className={styles.carouselHeading}>Terminal UI interface</div>
                            <img src={useBaseUrl("/img/basic.gif")} />
                        </div>
                        <div>
                            <div className={styles.carouselHeading}>Slack integration</div>
                            <img src={useBaseUrl("/img/slack.gif")} />
                        </div>
                        <div>
                            <div className={styles.carouselHeading}>Timing and flamegraphs</div>
                            <img src={useBaseUrl("/img/timing_landing.gif")} />
                        </div>
                    </Carousel>
                </div>

                <div className={styles.buttons}>
                    <Link
                      className={clsx(
                        "button button--outline button--secondary button--lg",
                        styles.getStarted,
                      )}
                      to={useBaseUrl("docs/")}>
                        Read the docs
                    </Link>

                    <span className={styles.indexCtasGitHubButtonWrapper}>
                        <iframe className={styles.indexCtasGitHubButton}
                                src="https://ghbtns.com/github-btn.html?user=codedownio&amp;repo=sandwich&amp;type=star&amp;count=true&amp;size=large"
                                width={160}
                                height={30}
                                title="GitHub Stars" />
                    </span>
                </div>
            </div>
        </header>

        <div className={"section " + styles.section}>
            <div className={styles.container}>
                <span className={styles.heading}>
                    Run the demo from the landing page:
                </span>

                <div className={styles.code}>
                    <Typist avgTypingDelay={1}>
                        git clone git@github.com:codedownio/sandwich.git
                        <br />
                        cd sandwich
                        <br />
                        stack run demo-landing -- --tui
                    </Typist>
                </div>

                <span className={styles.heading}>
                    Or, try one of the other <a className={styles.sectionLink}
                                                href="https://github.com/codedownio/sandwich/tree/master/">demos</a> with <code className={styles.codeInline}>stack run demo-[demoname]</code>.
                </span>
            </div>
        </div>

        <main>
            {features && features.length > 0 && (
              <section className={styles.features}>
                  <div className="container">
                      <div className="row">
                          {features.map((props, idx) => (
                            <Feature key={idx} {...props} />
                          ))}
                      </div>
                  </div>
              </section>
            )}
        </main>
    </Layout>
  );
}

export default Home;

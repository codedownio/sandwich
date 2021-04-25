
import React from 'react';
import clsx from 'clsx';
import Layout from '@theme/Layout';
import Link from '@docusaurus/Link';
import useDocusaurusContext from '@docusaurus/useDocusaurusContext';
import useBaseUrl from '@docusaurus/useBaseUrl';
import styles from './styles.module.css';

import "react-responsive-carousel/lib/styles/carousel.min.css";
import { Carousel } from "react-responsive-carousel";


const features = [
  {
    title: 'Powerful formatters',
    imageUrl: 'img/print_formatter.png',
    description: (
      <>
          Use the terminal UI formatter to inspect test results, re-run selected tests, or jump straight to test sources in your editor. Or, choose one of the more traditional formatters.
      </>
    ),
  }, {
    title: 'Easy profiling',
    imageUrl: 'img/speedscope.png',
    description: (
      <>
          Automatically record timing data for every test suite and visualize it as a flamegraph.
      </>
    ),
  }, {
    title: 'Zero-config Selenium tests',
    imageUrl: 'img/selenium_logo.png',
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
    <div className={clsx('col col--4', styles.feature)}>
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
    <Layout
      title={`Hello from ${siteConfig.title}`}
      description="Description will go into a meta tag in <head />">
        <header className={clsx('hero hero--primary', styles.heroBanner)}>
            <div className="container">
                <h1 className="hero__title">{siteConfig.title}</h1>
                <p className="hero__subtitle">{siteConfig.tagline}</p>

                <div style={{

                }}>
                    <Carousel>
                        <div>
                            Terminal UI interface
                            <img src={useBaseUrl("/img/basic.gif")} />
                        </div>
                        <div>
                            Slack integration
                            <img src={useBaseUrl("/img/slack.gif")} />
                        </div>
                        <div>
                            Timing and flamegraphs
                            <img src={useBaseUrl("/img/timing_parallel.gif")} />
                        </div>
                        <div>
                            <p className="legend">Legend 3</p>
                        </div>
                    </Carousel>
                </div>

                <div className={styles.buttons}>
                    <Link
                      className={clsx(
                        'button button--outline button--secondary button--lg',
                        styles.getStarted,
                      )}
                      to={useBaseUrl('docs/')}>
                        Get Started
                    </Link>
                </div>                
            </div>
        </header>
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

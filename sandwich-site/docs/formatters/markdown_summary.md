---
id: markdown_summary
title: Markdown Summary Formatter
---

import useBaseUrl from '@docusaurus/useBaseUrl';

The Markdown Summary formatter outputs a short summary of your tests results in [Markdown](https://daringfireball.net/projects/markdown/) format. It is designed to be used with [Github Actions job summaries](https://docs.github.com/en/actions/using-workflows/workflow-commands-for-github-actions#adding-a-job-summary).

At the moment, it just reports how many tests failed (if any) and how long the tests took to run. Below is an example in GitHub Actions.

<img alt="Job summary example" src={useBaseUrl('img/job_summary.png')} />

## Usage

To use it, just pass the `--markdown-summary` argument with a file path. The tests will write the Markdown summary to this path.

Within GitHub Actions, you can pass the `GITHUB_STEP_SUMMARY` variable.

```shell
stack run your-tests -- \
   ... \
   --markdown-summary $GITHUB_STEP_SUMMARY
```

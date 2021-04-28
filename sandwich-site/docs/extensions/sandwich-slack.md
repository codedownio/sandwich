---
id: sandwich-slack
title: Slack integration
---

import useBaseUrl from '@docusaurus/useBaseUrl';

The [Slack formatter](http://hackage.haskell.org/package/sandwich-slack) allows you to send live test results to a Slack channel. It shows the overall progress through the tests as a progress bar, and also shows failures as they occur.

To enable it, add `sandwich-slack` to your project and add the formatter to the [optionsFormatters](http://hackage.haskell.org/package/sandwich/docs/Test-Sandwich-Options.html#v:optionsFormatters) of your Sandwich options.

```haskell
import Test.Sandwich.Formatters.Slack

main :: IO ()
main = runSandwich options myTests
  where
    options = defaultOptions {
      optionsFormatters = [SomeFormatter $ defaultSlackFormatter {
        slackFormatterSlackConfig = SlackConfig "MY-SLACK-TOKEN"
        , slackFormatterChannel = "my-channel"
        }]
      }
```

If you're using the [runSandwichWithCommandLineArgs](http://hackage.haskell.org/package/sandwich/docs/Test-Sandwich.html#v:runSandwichWithCommandLineArgs) family of functions, you can also control the Slack formatter settings using command line args. For example, you can pass `--slack-token $SLACK_TOKEN`, which makes it easy to inject a token from a secret within your CI system. Pass `--print-slack-flags` to see all the command line options.

> Note that you need to set up a Slack token first. Nowadays this is done by creating a [Slack App](https://api.slack.com/apps). Make sure your app has "bot" permissions to interact in channels, and permission to make Slack API requests. To allow your app to send messages to a private channel, you'll want to add the app to the channel by typing `/invite app-name` in the channel.

When applied to the arithmetic example from the landing page, you get a live-updating message like this:

<img alt="Simple timing example" src={useBaseUrl('img/slack.gif')} />

## Customizing the message

Several options exist to configure the output.

First of all, you'll probably want to set  [slackFormatterTopMessage](http://hackage.haskell.org/package/sandwich-slack/docs/Test-Sandwich-Formatters-Slack.html#v:slackFormatterTopMessage), which allows you to control the message that appears above the progress bar. This is a useful place to put the name of the test suite and/or a link to the CI job where it's running.

## Limiting output

Most of the other options have to do with controlling how much information appears in the Slack message. We don't want to show an overwhelming amount of output in Slack if your test suite has a large number of failures, or if some failures produce large amounts of text. You can cap the total number of failures reported and the total number of bytes used in the message sent to the Slack API.

Each failure reported in the message consists of two parts: the failure reason and the callstack. You can cap the number of lines that will be devoted to each of these.

## Visibility thresholds

One other interesting option is  [slackFormatterVisibilityThreshold](http://hackage.haskell.org/package/sandwich-slack/docs/Test-Sandwich-Formatters-Slack.html#v:slackFormatterVisibilityThreshold). This option allows you to set a visibility threshold for which nodes are reported in failures. See the [visibility threshold](../node_options#visibility-thresholds) docs for more.

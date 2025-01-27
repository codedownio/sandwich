---
id: sandwich-contexts-smtp
title: SMTP
---

The [Test.Sandwich.Contexts.FakeSmtpServer](https://hackage.haskell.org/package/sandwich-contexts/docs/Test-Sandwich-Contexts-FakeSmtpServer.html) module provides a simple fake SMTP server, useful for testing email systems. The server is based on a slightly tweaked version of [ReachFive/fake-smtp-server](https://github.com/ReachFive/fake-smtp-server).

This module is currently included in [sandwich-contexts](./sandwich-contexts).

## Example

Here's an example of introducing and using the server. The `sendSampleEmail` function here uses [Network.HaskellNet.SMTP](https://hackage.haskell.org/package/HaskellNet/docs/Network-HaskellNet-SMTP.html) to send an email. Then, it waits for the email to appear by polling `fakeSmtpServerGetEmails`. We could test various properties of the email here if necessary.

```haskell title="https://github.com/codedownio/sandwich/blob/master/demos/demo-fake-smtp-server/app/Main.hs"
spec :: TopSpec
spec = describe "Introducing a fake SMTP server" $
  introduceNixContext nixpkgsReleaseDefault $ introduceFakeSmtpServerNix defaultFakeSmtpServerOptions $ do
    it "sends an email and verifies it was received" $ do
      FakeSmtpServer {..} <- getContext fakeSmtpServer
      info [i|Got fake SMTP server on port: #{fakeSmtpServerSmtpPort}|]

      sendSampleEmail fakeSmtpServerHostname fakeSmtpServerSmtpPort
      waitUntil 60 $ do
        fakeSmtpServerGetEmails >>= \case
          [x] -> debug [i|Got email: #{x}|]
          xs -> expectationFailure [i|Unexpected emails result: #{xs}|]
```

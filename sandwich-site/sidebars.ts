import type {SidebarsConfig} from '@docusaurus/plugin-content-docs';

const sidebars: SidebarsConfig = {
  docs: [
    { label: "Basics", type: "category", items: [
      "intro",
      "contexts",
      "node_options",
      "timing",
      "command_line",
      "discovery"
    ] },

    { label: "Formatters", type: "category", items: [
      "formatters/tui",
      "formatters/print",
      "formatters/failure_report",
      "formatters/log_saver",
      "formatters/silent",
      "formatters/slack",
      "formatters/markdown_summary",
    ] },

    { label: "Extensions", type: "category", items: [
      "extensions/sandwich-golden",
      "extensions/sandwich-hedgehog",
      "extensions/sandwich-quickcheck",
      "extensions/sandwich-webdriver",
    ] },

    { label: "Context libraries", type: "category", items: [
      "context-libraries/sandwich-contexts",
    ] },
  ],
};

export default sidebars;

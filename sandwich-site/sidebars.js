module.exports = {
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
    { type: "doc", id: "extensions/sandwich-golden" },
    { type: "doc", id: "extensions/sandwich-hedgehog" },
    { type: "doc", id: "extensions/sandwich-quickcheck" },
    { type: "doc", id: "extensions/sandwich-webdriver" },
  ],
};

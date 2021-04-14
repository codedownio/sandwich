module.exports = {
  title: 'Sandwich',
  tagline: 'Yet another testing framework for Haskell',
  url: 'https://github.com/codedownio/sandwich',
  baseUrl: '/sandwich/',
  onBrokenLinks: 'throw',
  onBrokenMarkdownLinks: 'warn',
  favicon: 'img/favicon.ico',
  organizationName: 'codedownio', // Usually your GitHub org/user name.
  projectName: 'sandwich', // Usually your repo name.
  themeConfig: {
    prism: {
      additionalLanguages: ['haskell'],
    },
    navbar: {
      title: 'Sandwich',
      logo: {
        alt: 'Sandwich',
        src: 'img/logo.svg',
      },
      items: [
        {
          to: 'docs/',
          activeBasePath: 'docs',
          label: 'Docs',
          position: 'left',
        },
        {to: 'blog', label: 'Blog', position: 'left'},
        {
          href: 'https://github.com/codedownio/sandwich',
          label: 'GitHub',
          position: 'right',
        },
      ],
    },
    footer: {
      style: 'dark',
      links: [
        {
          title: 'Docs',
          items: [
            {
              label: 'Introduction',
              to: 'docs/',
            },
            {
              label: 'Slack integration',
              to: 'docs/extensions/sandwich-slack',
            },
            {
              label: 'Selenium integration',
              to: 'docs/extensions/sandwich-webdriver',
            },
          ],
        },
        {
          title: 'Community',
          items: [
            {
              label: 'Stack Overflow',
              href: 'https://stackoverflow.com/questions/tagged/docusaurus',
            },
            {
              label: 'Discord',
              href: 'https://discordapp.com/invite/docusaurus',
            },
            {
              label: 'Twitter',
              href: 'https://twitter.com/docusaurus',
            },
          ],
        },
        {
          title: 'More',
          items: [
            {
              label: 'Blog',
              to: 'blog',
            },
            {
              label: 'GitHub',
              href: 'https://github.com/codedownio/sandwich',
            },
          ],
        },
      ],
      copyright: `Copyright © ${new Date().getFullYear()} Tom McLaughlin. Built with Docusaurus.`,
    },
  },
  presets: [
    [
      '@docusaurus/preset-classic',
      {
        docs: {
          sidebarPath: require.resolve('./sidebars.js'),
          // Please change this to your repo.
          editUrl: 'https://github.com/facebook/docusaurus/edit/master/website/',
        },
        blog: {
          showReadingTime: true,
          // Please change this to your repo.
          editUrl: 'https://github.com/facebook/docusaurus/edit/master/website/blog/',
        },
        theme: {
          customCss: require.resolve('./src/css/custom.css'),
        },
      },
    ],
  ],
};

(window.webpackJsonp=window.webpackJsonp||[]).push([[5],{102:function(e,t,n){"use strict";n.d(t,"b",(function(){return s})),n.d(t,"a",(function(){return o}));var r=n(16),a=n(104);function s(){const{siteConfig:{baseUrl:e="/",url:t}={}}=Object(r.default)();return{withBaseUrl:(n,r)=>function(e,t,n,{forcePrependBaseUrl:r=!1,absolute:s=!1}={}){if(!n)return n;if(n.startsWith("#"))return n;if(Object(a.b)(n))return n;if(r)return t+n;const o=n.startsWith(t)?n:t+n.replace(/^\//,"");return s?e+o:o}(t,e,n,r)}}function o(e,t={}){const{withBaseUrl:n}=s();return n(e,t)}},104:function(e,t,n){"use strict";function r(e){return!0===/^(\w*:|\/\/)/.test(e)}function a(e){return void 0!==e&&!r(e)}n.d(t,"b",(function(){return r})),n.d(t,"a",(function(){return a}))},69:function(e,t,n){"use strict";n.r(t),n.d(t,"frontMatter",(function(){return o})),n.d(t,"metadata",(function(){return i})),n.d(t,"toc",(function(){return c})),n.d(t,"default",(function(){return u}));var r=n(3),a=(n(0),n(99)),s=n(102);const o={id:"intro",title:"Welcome to Sandwich",sidebar_label:"Introduction",slug:"/"},i={unversionedId:"intro",id:"intro",isDocsHomePage:!1,title:"Welcome to Sandwich",description:"Sandwich is a test framework for Haskell, heavily inspired by and (almost) a drop-in replacement for Hspec. This section will show some of its features.",source:"@site/docs/intro.md",slug:"/",permalink:"/sandwich/docs/",editUrl:"https://github.com/facebook/docusaurus/edit/master/website/docs/intro.md",version:"current",sidebar_label:"Introduction",sidebar:"docs",next:{title:"Contexts",permalink:"/sandwich/docs/contexts"}},c=[{value:"Basic tests",id:"basic-tests",children:[]},{value:"Expectations",id:"expectations",children:[]},{value:"TUI interface",id:"tui-interface",children:[]},{value:"On-disk results",id:"on-disk-results",children:[]}],l={toc:c};function u({components:e,...t}){return Object(a.b)("wrapper",Object(r.a)({},l,t,{components:e,mdxType:"MDXLayout"}),Object(a.b)("p",null,"Sandwich is a test framework for Haskell, heavily inspired by and (almost) a drop-in replacement for ",Object(a.b)("a",{parentName:"p",href:"http://hspec.github.io/"},"Hspec"),". This section will show some of its features."),Object(a.b)("h2",{id:"basic-tests"},"Basic tests"),Object(a.b)("p",null,"Let's start with a basic test suite and add more features as we go along. As with other test frameworks, tests are structured as a ",Object(a.b)("strong",{parentName:"p"},"tree"),", defined using a simple free monad with nodes like ",Object(a.b)("inlineCode",{parentName:"p"},"describe")," and ",Object(a.b)("inlineCode",{parentName:"p"},"it"),". There are a total of 8 such basic nodes and we'll see others as we go along."),Object(a.b)("p",null,'The meat of the tests occurs in "it" nodes at the leaves of the tree. Every test runs in a special monad called ',Object(a.b)("inlineCode",{parentName:"p"},"ExampleT"),", which is essentially a ",Object(a.b)("inlineCode",{parentName:"p"},"ReaderT context LoggingT"),". The ",Object(a.b)("inlineCode",{parentName:"p"},"LoggingT")," part gives test the ability to log information, and the ",Object(a.b)("inlineCode",{parentName:"p"},"ReaderT")," gives tests access to ",Object(a.b)("em",{parentName:"p"},"context"),". More on this later. The monad also implements some other useful classes like ",Object(a.b)("inlineCode",{parentName:"p"},"MonadIO"),", so you can run arbitrary IO actions."),Object(a.b)("pre",null,Object(a.b)("code",{parentName:"pre",className:"language-haskell",metastring:'title="https://github.com/thomasjm/sandwich/blob/master/sandwich-demos/demos/basic/Main.hs"',title:'"https://github.com/thomasjm/sandwich/blob/master/sandwich-demos/demos/basic/Main.hs"'},'module Main where\n\nimport Test.Sandwich\n\nbasic :: TopSpec\nbasic = do\n  describe "Arithmetic" $ do\n    it "adds" $ do\n      (2 + 2) `shouldBe` 4\n      (2 + 3) `shouldBe` 5\n\n    it "subtracts" $ do\n      (3 - 2) `shouldBe` 0\n      warn "This test might not be right..."\n\n  describe "Strings" $\n    it "concatenates" $\n      ("abc" <> "def") `shouldBe` "abcdef"\n\nmain :: IO ()\nmain = runSandwichWithCommandLineArgs defaultOptions basic\n')),Object(a.b)("h2",{id:"expectations"},"Expectations"),Object(a.b)("h2",{id:"tui-interface"},"TUI interface"),Object(a.b)("p",null,"Let's run this test from the command line, using the ",Object(a.b)("a",{parentName:"p",href:"/docs/formatters/tui"},"Terminal UI interface"),". This will allow us to move around and examine the tests. In particular, we can examine the failure and log message in the subtraction tests."),Object(a.b)("p",null,"Since we used ",Object(a.b)("inlineCode",{parentName:"p"},"runSandwichWithCommandLineArgs"),", we can pass flags to control the formatter:"),Object(a.b)("pre",null,Object(a.b)("code",{parentName:"pre",className:"language-bash"},"~/sandwich> stack run basic -- --tui\n")),Object(a.b)("video",{width:"100%",controls:!0,autoplay:"true",muted:"true"},Object(a.b)("source",{src:Object(s.a)("img/basic_tui.webm"),type:"video/webm"}),"Your browser does not support the video tag."),Object(a.b)("h2",{id:"on-disk-results"},"On-disk results"),Object(a.b)("p",null,"Unless configured otherwise, each test tree run produces a ",Object(a.b)("em",{parentName:"p"},"directory tree")," which exactly mirrors the test tree structure. For example, the test tree above would produce a tree like the following."),Object(a.b)("pre",null,Object(a.b)("code",{parentName:"pre",className:"language-bash"},"<test_root>\n\u251c\u2500 results\n\u2502  \u251c\u2500 Arithmetic\n\u2502  \u2502  \u251c\u2500 adds\n\u2502  \u2502  \u2514\u2500 subtracts\n\u2502  \u2502     \u2514\u2500 test_logs.txt # contains the log warning message\n\u2502  \u2514\u2500 Strings\n\u2502     \u2514\u2500 concatenates\n\u2514\u2500 errors\n   \u2514\u2500 subtracts --\x3e ../results/subtracts # failure symlink\n")),Object(a.b)("p",null,"Thus, every test tree node has a place where it can stash logs, screenshots, or other artifacts. This structure makes it easy to browse through your tests and check results. Also, the ",Object(a.b)("inlineCode",{parentName:"p"},"errors")," folder at the root provides a handy list of symlinks to all failures."),Object(a.b)("blockquote",null,Object(a.b)("p",{parentName:"blockquote"},"Check out the next sections to learn about contexts, hooks, and more!")))}u.isMDXComponent=!0},99:function(e,t,n){"use strict";n.d(t,"a",(function(){return d})),n.d(t,"b",(function(){return m}));var r=n(0),a=n.n(r);function s(e,t,n){return t in e?Object.defineProperty(e,t,{value:n,enumerable:!0,configurable:!0,writable:!0}):e[t]=n,e}function o(e,t){var n=Object.keys(e);if(Object.getOwnPropertySymbols){var r=Object.getOwnPropertySymbols(e);t&&(r=r.filter((function(t){return Object.getOwnPropertyDescriptor(e,t).enumerable}))),n.push.apply(n,r)}return n}function i(e){for(var t=1;t<arguments.length;t++){var n=null!=arguments[t]?arguments[t]:{};t%2?o(Object(n),!0).forEach((function(t){s(e,t,n[t])})):Object.getOwnPropertyDescriptors?Object.defineProperties(e,Object.getOwnPropertyDescriptors(n)):o(Object(n)).forEach((function(t){Object.defineProperty(e,t,Object.getOwnPropertyDescriptor(n,t))}))}return e}function c(e,t){if(null==e)return{};var n,r,a=function(e,t){if(null==e)return{};var n,r,a={},s=Object.keys(e);for(r=0;r<s.length;r++)n=s[r],t.indexOf(n)>=0||(a[n]=e[n]);return a}(e,t);if(Object.getOwnPropertySymbols){var s=Object.getOwnPropertySymbols(e);for(r=0;r<s.length;r++)n=s[r],t.indexOf(n)>=0||Object.prototype.propertyIsEnumerable.call(e,n)&&(a[n]=e[n])}return a}var l=a.a.createContext({}),u=function(e){var t=a.a.useContext(l),n=t;return e&&(n="function"==typeof e?e(t):i(i({},t),e)),n},d=function(e){var t=u(e.components);return a.a.createElement(l.Provider,{value:t},e.children)},b={inlineCode:"code",wrapper:function(e){var t=e.children;return a.a.createElement(a.a.Fragment,{},t)}},p=a.a.forwardRef((function(e,t){var n=e.components,r=e.mdxType,s=e.originalType,o=e.parentName,l=c(e,["components","mdxType","originalType","parentName"]),d=u(n),p=r,m=d["".concat(o,".").concat(p)]||d[p]||b[p]||s;return n?a.a.createElement(m,i(i({ref:t},l),{},{components:n})):a.a.createElement(m,i({ref:t},l))}));function m(e,t){var n=arguments,r=t&&t.mdxType;if("string"==typeof e||r){var s=n.length,o=new Array(s);o[0]=p;var i={};for(var c in t)hasOwnProperty.call(t,c)&&(i[c]=t[c]);i.originalType=e,i.mdxType="string"==typeof e?e:r,o[1]=i;for(var l=2;l<s;l++)o[l]=n[l];return a.a.createElement.apply(null,o)}return a.a.createElement.apply(null,n)}p.displayName="MDXCreateElement"}}]);
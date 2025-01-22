"use strict";(self.webpackChunksandwich_site=self.webpackChunksandwich_site||[]).push([[508],{1393:(e,t,n)=>{n.r(t),n.d(t,{assets:()=>c,contentTitle:()=>r,default:()=>m,frontMatter:()=>a,metadata:()=>l,toc:()=>d});var i=n(6070),s=n(6113),o=n(4125);const a={id:"timing",title:"Timing",sidebar_label:"Timing"},r=void 0,l={id:"timing",title:"Timing",description:"Sandwich has a built-in notion of test timing. Timing can be useful when you have a large test suite and want to understand where it is spending the most time.",source:"@site/docs/timing.md",sourceDirName:".",slug:"/timing",permalink:"/sandwich/docs/timing",draft:!1,unlisted:!1,editUrl:"https://github.com/facebook/docusaurus/edit/master/website/docs/timing.md",tags:[],version:"current",frontMatter:{id:"timing",title:"Timing",sidebar_label:"Timing"},sidebar:"docs",previous:{title:"Node Options",permalink:"/sandwich/docs/node_options"},next:{title:"Command line",permalink:"/sandwich/docs/command_line"}},c={},d=[{value:"Non-concurrent",id:"non-concurrent",level:3},{value:"Dealing with concurrency",id:"dealing-with-concurrency",level:2},{value:"Advanced configuration",id:"advanced-configuration",level:2}];function h(e){const t={a:"a",blockquote:"blockquote",code:"code",em:"em",h2:"h2",h3:"h3",p:"p",pre:"pre",strong:"strong",...(0,s.R)(),...e.components};return(0,i.jsxs)(i.Fragment,{children:[(0,i.jsxs)(t.p,{children:["Sandwich has a built-in notion of ",(0,i.jsx)(t.em,{children:"test timing"}),". Timing can be useful when you have a large test suite and want to understand where it is spending the most time."]}),"\n",(0,i.jsxs)(t.p,{children:["The goal of the timing system is to create a nice summary report, ideally in the form of a ",(0,i.jsx)(t.a,{href:"http://www.brendangregg.com/flamegraphs.html",children:"flame graph"}),"."]}),"\n",(0,i.jsxs)(t.p,{children:["You can select a test timer implementation you use in the Sandwich options. The default one targets ",(0,i.jsx)(t.a,{href:"https://www.speedscope.app/",children:"SpeedScope"}),", a nice web-based tool for visualizing flame graphs."]}),"\n",(0,i.jsx)(t.h3,{id:"non-concurrent",children:"Non-concurrent"}),"\n",(0,i.jsxs)(t.p,{children:["The simplest use of timing occurs when we don't have any ",(0,i.jsx)(t.code,{children:"parallel"})," stuff going on, so the tests all run in a single thread. To deal with multiple threads, see the ",(0,i.jsx)(t.a,{href:"#dealing-with-concurrency",children:"concurrency"})," section."]}),"\n",(0,i.jsxs)(t.p,{children:["First of all, ",(0,i.jsx)(t.strong,{children:"every node in the test tree is timed by default"}),'. Thus, the "describe" and "it" nodes in the example below will be timed automatically. You can prevent this by changing the ',(0,i.jsx)(t.a,{href:"/docs/node_options#timing",children:"node options"}),"."]}),"\n",(0,i.jsxs)(t.p,{children:["In addition, you can time arbitrary blocks of code using the ",(0,i.jsx)(t.code,{children:"timeAction"})," function. This function is a ",(0,i.jsx)(t.code,{children:"bracket_"})," style combinator that can be used to wrap an action. In the example below, we use it to wrap some sub-steps within a test."]}),"\n",(0,i.jsx)(t.pre,{children:(0,i.jsx)(t.code,{className:"language-haskell",metastring:'title="https://github.com/codedownio/sandwich/blob/master/demos/demo-timing/app/Main.hs"',children:'timingDemo :: TopSpec\ntimingDemo = describe "Dinner tests" $ do\n  it "Makes dinner" $ do\n    pauseSeconds 1\n    timeAction "Makes pasta" $ do\n      timeAction "Heats water" $ pauseSeconds 1\n      timeAction "Boils noodles" $ pauseSeconds 0.8\n      timeAction "Decants noodles" $ pauseSeconds 0.7\n\n  it "Cleans up" $ do\n    pauseSeconds 1\n'})}),"\n",(0,i.jsxs)(t.p,{children:["When you run this code using the default implementation, it will output a file ",(0,i.jsx)(t.code,{children:"speedscope.json"}),' in the root of the test results. If you drag and drop this file onto SpeedScope, you get a picture like the following. Note that the profile (in the center of the top bar) is "default." In the next section we\'ll explore using multiple threads (and thus multiple profiles).']}),"\n",(0,i.jsx)("img",{alt:"Simple timing example",src:(0,o.A)("img/dinner_timing.png"),style:{border:"solid 1px lightgray"}}),"\n",(0,i.jsx)(t.h2,{id:"dealing-with-concurrency",children:"Dealing with concurrency"}),"\n",(0,i.jsxs)(t.p,{children:["Flame graphs need to be properly ",(0,i.jsx)(t.em,{children:"nested"})," to be valid. If Frame A starts before Frame B, then Frame B must end before Frame A ends. When you run test subtrees in parallel, it's easy to violate this property and get stack frames that cross over each other. This will result in a malformed JSON file that makes the visualizer unhappy."]}),"\n",(0,i.jsxs)(t.p,{children:['The solution is to introduce "profiles" within the test timer to correspond to execution threads, and make sure you run in a single-threaded way ',(0,i.jsx)(t.em,{children:"within each profile"}),". Below is a simple example of this in action. Note that we use the ",(0,i.jsx)(t.code,{children:"parallel"})," keyword at the top level, to cause the two test trees underneath it to run in their own threads. Immediately underneath the ",(0,i.jsx)(t.code,{children:"parallel"})," keyword, we use ",(0,i.jsx)(t.code,{children:"withTimingProfile"})," to switch the profile for the rest of the sub-tree."]}),"\n",(0,i.jsx)(t.pre,{children:(0,i.jsx)(t.code,{className:"language-haskell",children:'timingParallelDemo :: TopSpec\ntimingParallelDemo = parallel $ do\n  withTimingProfile "italian" $\n    it "Makes Italian dinner" $ do\n      pauseSeconds 1\n      timeAction "Makes pasta" $ do\n        timeAction "Heats water" $ pauseSeconds 1\n        timeAction "Boils noodles" $ pauseSeconds 0.8\n        timeAction "Decants noodles" $ pauseSeconds 0.7\n\n  withTimingProfile "chinese" $\n    it "Makes Chinese dinner" $ do\n      pauseSeconds 0.1\n      timeAction "Makes rice" $ do\n        timeAction "Cooks rice" $ pauseSeconds 0.5\n        timeAction "Serves rice" $ pauseSeconds 0.2\n      pauseSeconds 0.3\n'})}),"\n",(0,i.jsxs)("video",{width:"100%",controls:!0,autoplay:"true",muted:"true",children:[(0,i.jsx)("source",{src:(0,o.A)("img/timing_parallel.webm"),type:"video/webm"}),(0,i.jsx)(t.p,{children:"Your browser does not support the video tag."})]}),"\n",(0,i.jsx)(t.h2,{id:"advanced-configuration",children:"Advanced configuration"}),"\n",(0,i.jsxs)(t.p,{children:["You can configure some settings for the test timer as part of the normal ",(0,i.jsx)(t.code,{children:"Options"})," object. To disable the test timer entirely, just switch to the ",(0,i.jsx)(t.code,{children:"NullTestTimer"}),":"]}),"\n",(0,i.jsx)(t.pre,{children:(0,i.jsx)(t.code,{className:"language-haskell",children:"myOptions = defaultOptions { optionsTestTimerType = NullTestTimerType }\n"})}),"\n",(0,i.jsxs)(t.p,{children:["Every test timer implementation has some implementation-specific options. For example, you can pass an arg with ",(0,i.jsx)(t.code,{children:"SpeedScopeTestTimerType"})," to cause it to emit a raw timing data file in addition to the JSON file. This is a simple event-based format with one timing event per line, and it can be useful to debug parallelism issues or convert to another format."]}),"\n",(0,i.jsxs)(t.blockquote,{children:["\n",(0,i.jsx)(t.p,{children:"If you'd like to support another timing format or visualizer, please open a PR! It should be easy to add more."}),"\n"]})]})}function m(e={}){const{wrapper:t}={...(0,s.R)(),...e.components};return t?(0,i.jsx)(t,{...e,children:(0,i.jsx)(h,{...e})}):h(e)}},6113:(e,t,n)=>{n.d(t,{R:()=>a,x:()=>r});var i=n(758);const s={},o=i.createContext(s);function a(e){const t=i.useContext(o);return i.useMemo((function(){return"function"==typeof e?e(t):{...t,...e}}),[t,e])}function r(e){let t;return t=e.disableParentContext?"function"==typeof e.components?e.components(s):e.components||s:a(e.components),i.createElement(o.Provider,{value:t},e.children)}}}]);
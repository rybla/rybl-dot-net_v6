// ${nodes}, ${edges}
function make_references_graph(node_list, edge_list) {
  if (node_list === undefined) node_list = [];
  if (edge_list === undefined) edge_list = [];

  console.log("[script] begin");
  // const nodes = new vis.DataSet([
  //   { id: 1, label: "Google", url: "https://www.google.com", shape: "box" },
  //   { id: 2, label: "GitHub", url: "https://www.github.com", shape: "box" },
  //   { id: 3, label: "Vis.js", url: "https://visjs.org/", shape: "box" },
  //   {
  //     id: 4,
  //     label: "Mozilla Developer Network Docs",
  //     url: "https://developer.mozilla.org/",
  //     shape: "box",
  //   },
  //   {
  //     id: 5,
  //     label: "Stack Overflow",
  //     url: "https://stackoverflow.com/",
  //     shape: "box",
  //   },
  //   { id: 6, label: "React", url: "https://react.dev/", shape: "box" },
  //   { id: 7, label: "Vue.js", url: "https://vuejs.org/", shape: "box" },
  //   { id: 8, label: "Angular", url: "https://angular.io/", shape: "box" },
  //   { id: 9, label: "Node.js", url: "https://nodejs.org/", shape: "box" },
  //   { id: 10, label: "npm", url: "https://www.npmjs.com/", shape: "box" },
  //   { id: 11, label: "Webpack", url: "https://webpack.js.org/", shape: "box" },
  //   { id: 12, label: "Babel", url: "https://babeljs.io/", shape: "box" },
  //   {
  //     id: 13,
  //     label: "TypeScript",
  //     url: "https://www.typescriptlang.org/",
  //     shape: "box",
  //   },
  //   { id: 14, label: "W3C", url: "https://www.w3.org/", shape: "box" },
  //   { id: 15, label: "D3.js", url: "https://d3js.org/", shape: "box" },
  // ]);
  const nodes = new vis.DataSet(node_list);

  // const edges = new vis.DataSet([
  //   { from: 1, to: 2 },
  //   { from: 2, to: 3 },
  //   { from: 1, to: 4 },
  //   { from: 4, to: 5 },
  //   { from: 1, to: 5 },
  //   { from: 3, to: 4 },
  //   { from: 6, to: 10 },
  //   { from: 7, to: 10 },
  //   { from: 8, to: 10 },
  //   { from: 9, to: 10 },
  //   { from: 6, to: 11 },
  //   { from: 7, to: 11 },
  //   { from: 8, to: 11 },
  //   { from: 11, to: 12 },
  //   { from: 6, to: 12 },
  //   { from: 7, to: 12 },
  //   { from: 8, to: 12 },
  //   { from: 6, to: 13 },
  //   { from: 8, to: 13 },
  //   { from: 4, to: 14 },
  //   { from: 3, to: 15 },
  //   { from: 1, to: 6 },
  //   { from: 1, to: 7 },
  //   { from: 1, to: 8 },
  //   { from: 2, to: 6 },
  //   { from: 2, to: 7 },
  //   { from: 2, to: 8 },
  //   { from: 2, to: 9 },
  //   { from: 5, to: 13 },
  // ]);
  const edges = new vis.DataSet(edge_list);

  const container = document.getElementById("references-graph");
  const data = {
    nodes: nodes,
    edges: edges,
  };
  const options = {
    nodes: {
      borderWidth: 2,
      font: {
        color: "#343434",
        size: 14,
      },
      color: {
        border: "#404040",
        background: "#e8e8e8",
        highlight: {
          border: "#2B7CE9",
          background: "#D2E5FF",
        },
        hover: {
          border: "#2B7CE9",
          background: "#D2E5FF",
        },
      },
      widthConstraint: 150,
    },
    edges: {
      color: "#404040",
      arrows: {
        to: { enabled: true, scaleFactor: 0.5 },
      },
    },
    physics: {
      enabled: true,
    },
    interaction: {
      hover: true,
      tooltipDelay: 200,
      dragNodes: false,
    },
  };

  const network = new vis.Network(container, data, options);

  network.on("click", function (params) {
    if (params.nodes.length > 0) {
      const nodeId = params.nodes[0];
      const node = nodes.get(nodeId);
      if (node.url) {
        window.open(node.url, "_blank");
      }
    }
  });

  network.on("hoverNode", function () {
    container.style.cursor = "pointer";
  });

  network.on("blurNode", function () {
    container.style.cursor = "default";
  });
  console.log("[script] end");
}

(function () {
  function renderMermaid() {
    if (!window.mermaid) return;

    document.querySelectorAll("pre.mermaid > code").forEach(function (code) {
      var pre = code.parentElement;
      var diagram = document.createElement("div");
      diagram.className = "mermaid";
      diagram.textContent = code.textContent;
      pre.replaceWith(diagram);
    });

    window.mermaid.initialize({
      startOnLoad: false,
      theme: document.body.getAttribute("data-md-color-scheme") === "slate" ? "dark" : "default"
    });

    window.mermaid.run({
      nodes: document.querySelectorAll(".mermaid")
    });
  }

  if (window.document$ && typeof window.document$.subscribe === "function") {
    window.document$.subscribe(renderMermaid);
  } else if (document.readyState === "loading") {
    document.addEventListener("DOMContentLoaded", renderMermaid);
  } else {
    renderMermaid();
  }
})();

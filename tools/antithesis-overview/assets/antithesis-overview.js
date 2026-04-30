// antithesis-overview runtime — depends on antithesis-triage.js being
// loaded first (it provides window.__antithesisTriage). Adds a
// window.__antithesisOverview namespace with multi-run helpers.
//
// Conventions match antithesis-triage.js:
// - methods throw on error so the playwright eval call returns non-zero
// - object shapes documented in the skill SKILL.md

(function () {
  function clean(text) {
    return (text || "").replace(/\s+/g, " ").trim();
  }

  function abort(details) {
    var msg =
      typeof details === "string"
        ? details
        : (details && details.error) || JSON.stringify(details);
    var err = new Error(msg);
    err.details = details;
    throw err;
  }

  // The runs page renders rows as JSON-blob-bearing divs. The blob is
  // embedded in the row's full text and looks like:
  //   cf - {"testRun":{"commitId":"...","try":N,"repository":{...},
  //                    "requester":"...","directory":"..."},
  //         "testRunId":"<hex>"}
  // We extract these via regex over the row's textContent rather than
  // hunting for specific class names — Antithesis ships UI changes
  // frequently and class selectors break.

  function parseRowMetadata(rowText) {
    var m = rowText.match(
      /\{\s*"testRun"\s*:\s*\{[^]*?"testRunId"\s*:\s*"([0-9a-f]+)"\s*\}/,
    );
    if (!m) return null;

    var blob = m[0];
    var testRunId = m[1];

    function pull(key) {
      var re = new RegExp('"' + key + '"\\s*:\\s*"([^"]+)"');
      var hit = blob.match(re);
      return hit ? hit[1] : null;
    }

    function pullInt(key) {
      var re = new RegExp('"' + key + '"\\s*:\\s*(\\d+)');
      var hit = blob.match(re);
      return hit ? parseInt(hit[1], 10) : null;
    }

    var commit = pull("commitId");
    var directory = pull("directory");
    var requester = pull("requester");
    var organization = pull("organization");
    var repo = pull("repo");
    var try_ = pullInt("try");

    return {
      commit: commit,
      commitShort: commit ? commit.slice(0, 8) : null,
      try: try_,
      testRunId: testRunId,
      requester: requester,
      repo: organization && repo ? organization + "/" + repo : null,
      directory: directory,
    };
  }

  function parseStatusBlock(rowText) {
    // Status is one of: "In progress", "Completed", "Failed", "Stopped"
    var m = rowText.match(/(In progress|Completed|Failed|Stopped)/);
    var status = m ? m[1] : null;

    // Duration is "Nh Nm" or "NNm" — captured immediately after status
    var dur = null;
    if (m) {
      var after = rowText.slice(m.index + m[0].length);
      var d = after.match(/^\s*((?:\d+h\s*)?\d+m)/);
      if (d) dur = d[1].trim();
    }

    return { status: status, duration: dur };
  }

  function parseFindings(rowText) {
    // Either "----" (no triage) or four counts:
    //   N new\nN ongoing\nN resolved\nN rare
    if (/----/.test(rowText.replace(/N\s*new/, ""))) {
      // crude: if there's no "N new" pattern, treat as no findings
      var hasFindings = /\d+\s*new/.test(rowText);
      if (!hasFindings) return null;
    }

    function pull(label) {
      var re = new RegExp("(\\d+)\\s*" + label);
      var hit = rowText.match(re);
      return hit ? parseInt(hit[1], 10) : null;
    }

    var n = pull("new");
    if (n === null) return null;

    return {
      new: n,
      ongoing: pull("ongoing") || 0,
      resolved: pull("resolved") || 0,
      rare: pull("rare") || 0,
    };
  }

  function parseStartedLabel(rowText) {
    // e.g. "Today 9:52 AM", "Yesterday 8:42 PM", "Apr 28, 2026 8:43 PM"
    var todayY = rowText.match(
      /(Today|Yesterday)\s*(\d{1,2}:\d{2}\s*[AP]M)/,
    );
    if (todayY) return todayY[1] + " " + todayY[2];

    var dated = rowText.match(
      /([A-Z][a-z]{2}\s+\d{1,2},\s+\d{4})\s*(\d{1,2}:\d{2}\s*[AP]M)/,
    );
    if (dated) return dated[1] + " " + dated[2];

    return null;
  }

  // Convert a "Today HH:MM AM/PM" / "Yesterday HH:MM AM/PM" / "Mon DD,
  // YYYY HH:MM AM/PM" label to a Date in the local browser timezone.
  // Returns null if the label can't be parsed. Used only for
  // hoursBack filtering — we don't surface this back to the user.
  function labelToDate(label) {
    if (!label) return null;
    var now = new Date();

    var m = label.match(/^(Today|Yesterday)\s+(\d{1,2}):(\d{2})\s*([AP]M)$/);
    if (m) {
      var d = new Date(now);
      if (m[1] === "Yesterday") d.setDate(d.getDate() - 1);
      var h = parseInt(m[2], 10) % 12;
      if (m[4] === "PM") h += 12;
      d.setHours(h, parseInt(m[3], 10), 0, 0);
      return d;
    }

    var ym = label.match(
      /^([A-Z][a-z]{2})\s+(\d{1,2}),\s+(\d{4})\s+(\d{1,2}):(\d{2})\s*([AP]M)$/,
    );
    if (ym) {
      var months = [
        "Jan",
        "Feb",
        "Mar",
        "Apr",
        "May",
        "Jun",
        "Jul",
        "Aug",
        "Sep",
        "Oct",
        "Nov",
        "Dec",
      ];
      var monIdx = months.indexOf(ym[1]);
      if (monIdx < 0) return null;
      var hh = parseInt(ym[4], 10) % 12;
      if (ym[6] === "PM") hh += 12;
      return new Date(
        parseInt(ym[3], 10),
        monIdx,
        parseInt(ym[2], 10),
        hh,
        parseInt(ym[5], 10),
        0,
        0,
      );
    }

    return null;
  }

  function rowsContainerCandidates() {
    // Each runs row is rendered as an `<a-row>` custom element. It
    // contains the row's JSON blob, status, duration, findings, and
    // both action anchors ("Triage results" and "Explore Logs").
    //
    // We also fall back to scanning every div/li/tr in case the UI
    // ever moves away from `<a-row>` — same heuristic: a single
    // testRunId hit AND the action text.
    var rows = Array.from(document.querySelectorAll("a-row"));
    if (rows.length > 0) return rows;

    var candidates = [];
    var all = document.querySelectorAll("div, li, tr");
    for (var i = 0; i < all.length; i++) {
      var el = all[i];
      var text = el.textContent || "";
      if (!/"testRunId"\s*:/.test(text)) continue;
      if (!/Explore Logs/.test(text)) continue;
      var hits = (text.match(/"testRunId"/g) || []).length;
      if (hits !== 1) continue;
      candidates.push(el);
    }
    return candidates;
  }

  function findActionLink(rowEl, label) {
    var anchors = rowEl.querySelectorAll("a");
    for (var i = 0; i < anchors.length; i++) {
      var t = anchors[i].textContent || "";
      if (clean(t) === label || t.indexOf(label) >= 0) {
        return anchors[i].href || null;
      }
    }
    return null;
  }

  function findExploreLogsLink(rowEl) {
    return findActionLink(rowEl, "Explore Logs");
  }

  function findTriageLink(rowEl) {
    return findActionLink(rowEl, "Triage results");
  }

  function getRecentRuns(opts) {
    opts = opts || {};
    var hoursBack = typeof opts.hoursBack === "number" ? opts.hoursBack : 48;
    var requesterFilter = opts.requester || null;
    var repoFilter = opts.repo || null;
    var onlyWithFindings = !!opts.onlyWithFindings;

    var rows = rowsContainerCandidates();
    if (rows.length === 0) {
      abort({
        error: "no run rows found — is the runs page loaded?",
        url: location.href,
      });
    }

    var cutoff = new Date(Date.now() - hoursBack * 3600 * 1000);
    var out = [];

    for (var i = 0; i < rows.length; i++) {
      var row = rows[i];
      var text = clean(row.textContent);

      var meta = parseRowMetadata(text);
      if (!meta) continue;

      var statusBlock = parseStatusBlock(text);
      var findings = parseFindings(text);
      var startedLabel = parseStartedLabel(text);
      var startedAt = labelToDate(startedLabel);

      if (startedAt && startedAt < cutoff) continue;

      if (requesterFilter && meta.requester) {
        if (
          meta.requester.toLowerCase().indexOf(requesterFilter.toLowerCase()) <
          0
        )
          continue;
      }

      if (repoFilter && meta.repo) {
        if (meta.repo.toLowerCase().indexOf(repoFilter.toLowerCase()) < 0)
          continue;
      }

      if (onlyWithFindings && (!findings || findings.new === 0)) continue;

      var commitUrl =
        meta.repo && meta.commit
          ? "https://github.com/" + meta.repo + "/commit/" + meta.commit
          : null;

      out.push({
        commit: meta.commit,
        commitShort: meta.commitShort,
        commitUrl: commitUrl,
        try: meta.try,
        testRunId: meta.testRunId,
        requester: meta.requester,
        repo: meta.repo,
        directory: meta.directory,
        startedLabel: startedLabel,
        status: statusBlock.status,
        duration: statusBlock.duration,
        findings: findings,
        triageReportUrl: findTriageLink(row),
        exploreLogsUrl: findExploreLogsLink(row),
        rowIndex: i,
      });
    }

    return {
      hoursBack: hoursBack,
      rowsAvailable: rows.length,
      runs: out,
      truncated: rows.length >= 20 && out.length === rows.length,
    };
  }

  // Click "Triage results" for the row at rowIndex (as returned by
  // getRecentRuns). Captures the new tab's URL after the click and
  // returns it, then asks the caller to close the tab themselves —
  // playwright manages tab lifecycle, not us.
  //
  // Returns the report URL synchronously by reading the button's
  // associated link, OR throws if the button doesn't exist.
  //
  // The actual click → new-tab flow needs to happen in playwright
  // (because eval'd JS can't observe new tabs reliably). Use this as
  // a marker step: eval `prepareTriageButtonForRow(idx)` to get the
  // button selector, then `await page.click(selector)` from
  // playwright while listening for the popup event.
  function prepareTriageButtonForRow(rowIndex) {
    var rows = rowsContainerCandidates();
    if (rowIndex < 0 || rowIndex >= rows.length) {
      abort({
        error: "row index out of range",
        rowIndex: rowIndex,
        available: rows.length,
      });
    }

    var row = rows[rowIndex];
    var btns = row.querySelectorAll("button");
    var target = null;
    for (var i = 0; i < btns.length; i++) {
      if (clean(btns[i].textContent) === "Triage results") {
        target = btns[i];
        break;
      }
    }
    if (!target) {
      abort({
        error: "no Triage results button on row — run may still be in progress",
        rowIndex: rowIndex,
      });
    }

    target.setAttribute("data-overview-triage-target", "1");
    return {
      rowIndex: rowIndex,
      selector: '[data-overview-triage-target="1"]',
    };
  }

  // Cleanup helper for after a triage button click sequence has
  // resolved its tab — clears the marker.
  function clearTriageMarkers() {
    var marked = document.querySelectorAll("[data-overview-triage-target]");
    for (var i = 0; i < marked.length; i++) {
      marked[i].removeAttribute("data-overview-triage-target");
    }
    return { cleared: marked.length };
  }

  window.__antithesisOverview = {
    __version: "1.0.0",
    getRecentRuns: getRecentRuns,
    prepareTriageButtonForRow: prepareTriageButtonForRow,
    clearTriageMarkers: clearTriageMarkers,
  };
})();

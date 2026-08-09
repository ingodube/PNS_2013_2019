#!/usr/bin/env node
const fs = require("fs");
const path = require("path");
const { pathToFileURL } = require("url");
const { chromium } = require("playwright");

function argument(name, fallback) {
  const index = process.argv.indexOf(name);
  return index >= 0 ? process.argv[index + 1] : fallback;
}

const target = process.argv[2];
if (!target) {
  throw new Error("Usage: node validate_responsive.cjs <html-or-url> [--widths 320,360,390,768,1024,1440]");
}

const widths = argument("--widths", "320,360,390,768,1024,1440")
  .split(",")
  .map(Number);
const url = /^https?:/i.test(target)
  ? target
  : pathToFileURL(path.resolve(target)).href;

const candidates = [
  process.env.EDGE_PATH,
  "C:/Program Files (x86)/Microsoft/Edge/Application/msedge.exe",
  "C:/Program Files/Microsoft/Edge/Application/msedge.exe",
].filter(Boolean);
const executablePath = candidates.find(fs.existsSync);

(async () => {
  const browser = await chromium.launch(executablePath ? { headless: true, executablePath } : { headless: true });
  const results = [];
  let failed = false;
  for (const width of widths) {
    const page = await browser.newPage({ viewport: { width, height: 1000 } });
    const errors = [];
    page.on("console", message => {
      if (message.type() === "error") errors.push(message.text());
    });
    page.on("pageerror", error => errors.push(error.message));
    const response = await page.goto(url, { waitUntil: "networkidle", timeout: 120000 });
    const metrics = await page.evaluate(async () => {
      const root = document.documentElement;
      const toc = document.querySelector(".report-toc");
      const list = document.querySelector("#TOC > ul");
      const pageBox = document.querySelector(".report-page")?.getBoundingClientRect();
      const tocBox = toc?.getBoundingClientRect();
      const offenders = [...document.querySelectorAll("body *")].filter(element => {
        const rect = element.getBoundingClientRect();
        const allowed = element.closest(".table-scroll, pre, .math.display");
        return !allowed && (rect.right > root.clientWidth + 1 || rect.left < -1);
      });
      const offenderDetails = offenders.slice(0, 8).map(element => {
        const rect = element.getBoundingClientRect();
        return {
          tag: element.tagName.toLowerCase(),
          id: element.id || null,
          className: typeof element.className === "string" ? element.className : null,
          left: Math.round(rect.left * 10) / 10,
          right: Math.round(rect.right * 10) / 10,
        };
      });
      const figureTitleOrder = [...document.querySelectorAll(".figure-title")].every(title => {
        let node = title.nextElementSibling;
        while (node && !node.matches("h1, h2, h3, .figure-title")) {
          if (node.matches("img") || node.querySelector("img")) return true;
          node = node.nextElementSibling;
        }
        return false;
      });
      const tableTitleOrder = [...document.querySelectorAll("table")].every(table => {
        const caption = table.querySelector(":scope > caption");
        return caption && table.firstElementChild === caption;
      });
      const tableAlignment = [...document.querySelectorAll("table")].every(table => {
        const headers = [...table.querySelectorAll("thead th")];
        const values = [...(table.querySelector("tbody tr")?.querySelectorAll("td") || [])];
        return headers.length === values.length && headers.every((header, index) =>
          getComputedStyle(header).textAlign === getComputedStyle(values[index]).textAlign
        );
      });
      const sourceAndNotesJustified = [...document.querySelectorAll(
        ".figure-source, .source-note, .source-after-note, .table-source, .table-note, .figure-note"
      )].every(element => getComputedStyle(element).textAlign === "justify");
      const referencesJustified = [...document.querySelectorAll(".references .reference-entry")]
        .every(element => getComputedStyle(element).textAlign === "justify");
      let tocToggle = true;
      if (toc instanceof HTMLDetailsElement && innerWidth <= 960) {
        toc.open = false;
        await new Promise(resolve => requestAnimationFrame(resolve));
        const closed = !toc.open;
        toc.open = true;
        await new Promise(resolve => requestAnimationFrame(resolve));
        tocToggle = closed && toc.open;
      }
      const referenceLink = document.querySelector('a[href^="#ref-"]');
      let targetHighlight = true;
      if (referenceLink) {
        const target = document.getElementById(referenceLink.hash.slice(1));
        location.hash = referenceLink.hash;
        await new Promise(resolve => requestAnimationFrame(resolve));
        targetHighlight = Boolean(target?.matches(":target")) &&
          getComputedStyle(target).backgroundColor !== "rgba(0, 0, 0, 0)";
        history.replaceState(null, "", location.href.split("#")[0]);
      }
      return {
        clientWidth: root.clientWidth,
        scrollWidth: root.scrollWidth,
        bleed: Math.max(0, root.scrollWidth - root.clientWidth),
        offenders: offenders.length,
        offenderDetails,
        tocOpen: toc instanceof HTMLDetailsElement ? toc.open : null,
        tocToggle,
        tocColumns: list ? getComputedStyle(list).columnCount : null,
        tocContained: Boolean(pageBox && tocBox && tocBox.left >= pageBox.left - 1 && tocBox.right <= pageBox.right + 1),
        figureTitleOrder,
        tableTitleOrder,
        tableAlignment,
        sourceAndNotesJustified,
        referencesJustified,
        targetHighlight,
        tables: document.querySelectorAll("table").length,
        figures: document.querySelectorAll(".report-content img").length,
        codeBlocks: document.querySelectorAll(".report-content pre").length,
        references: document.querySelectorAll(".references .reference-entry").length,
      };
    });
    const expectedColumns = width > 960 ? "2" : "1";
    const pass = (!response || response.ok()) && errors.length === 0 && metrics.bleed === 0 &&
      metrics.offenders === 0 && metrics.tocOpen === true && metrics.tocToggle &&
      metrics.tocColumns === expectedColumns && metrics.tocContained && metrics.figureTitleOrder &&
      metrics.tableTitleOrder && metrics.tableAlignment && metrics.sourceAndNotesJustified &&
      metrics.referencesJustified && metrics.targetHighlight;
    failed ||= !pass;
    results.push({ width, status: response ? response.status() : 200, errors, ...metrics, pass });
    await page.close();
  }
  await browser.close();
  console.log(JSON.stringify(results, null, 2));
  if (failed) process.exit(1);
})().catch(error => {
  console.error(error);
  process.exit(1);
});

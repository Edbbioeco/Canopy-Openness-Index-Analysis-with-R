 const toggleBodyColorMode = (bsSheetEl) => { const mode = bsSheetEl.getAttribute("data-mode"); const bodyEl = window.document.querySelector("body"); if (mode === "dark") { bodyEl.classList.add("quarto-dark"); bodyEl.classList.remove("quarto-light"); } else { bodyEl.classList.add("quarto-light"); bodyEl.classList.remove("quarto-dark"); } } const toggleBodyColorPrimary = () => { const bsSheetEl = window.document.querySelector("link#quarto-bootstrap:not([rel=disabled-stylesheet])"); if (bsSheetEl) { toggleBodyColorMode(bsSheetEl); } } const setColorSchemeToggle = (alternate) => { const toggles = window.document.querySelectorAll('.quarto-color-scheme-toggle'); for (let i=0; i < toggles.length; i++) { const toggle = toggles[i]; if (toggle) { if (alternate) { toggle.classList.add("alternate"); } else { toggle.classList.remove("alternate"); } } } }; const toggleColorMode = (alternate) => { // Switch the stylesheets const primaryStylesheets = window.document.querySelectorAll('link.quarto-color-scheme:not(.quarto-color-alternate)'); const alternateStylesheets = window.document.querySelectorAll('link.quarto-color-scheme.quarto-color-alternate'); manageTransitions('#quarto-margin-sidebar .nav-link', false); if (alternate) { // note: dark is layered on light, we don't disable primary! enableStylesheet(alternateStylesheets); for (const sheetNode of alternateStylesheets) { if (sheetNode.id === "quarto-bootstrap") { toggleBodyColorMode(sheetNode); } } } else { disableStylesheet(alternateStylesheets); enableStylesheet(primaryStylesheets) toggleBodyColorPrimary(); } manageTransitions('#quarto-margin-sidebar .nav-link', true); // Switch the toggles setColorSchemeToggle(alternate) // Hack to workaround the fact that safari doesn't // properly recolor the scrollbar when toggling (#1455) if (navigator.userAgent.indexOf('Safari') > 0 && navigator.userAgent.indexOf('Chrome') == -1) { manageTransitions("body", false); window.scrollTo(0, 1); setTimeout(() => { window.scrollTo(0, 0); manageTransitions("body", true); }, 40); } } const disableStylesheet = (stylesheets) => { for (let i=0; i < stylesheets.length; i++) { const stylesheet = stylesheets[i]; stylesheet.rel = 'disabled-stylesheet'; } } const enableStylesheet = (stylesheets) => { for (let i=0; i < stylesheets.length; i++) { const stylesheet = stylesheets[i]; if(stylesheet.rel !== 'stylesheet') { // for Chrome, which will still FOUC without this check stylesheet.rel = 'stylesheet'; } } } const manageTransitions = (selector, allowTransitions) => { const els = window.document.querySelectorAll(selector); for (let i=0; i < els.length; i++) { const el = els[i]; if (allowTransitions) { el.classList.remove('notransition'); } else { el.classList.add('notransition'); } } } const isFileUrl = () => { return window.location.protocol === 'file:'; } const hasAlternateSentinel = () => { let styleSentinel = getColorSchemeSentinel(); if (styleSentinel !== null) { return styleSentinel === "alternate"; } else { return false; } } const setStyleSentinel = (alternate) => { const value = alternate ? "alternate" : "default"; if (!isFileUrl()) { window.localStorage.setItem("quarto-color-scheme", value); } else { localAlternateSentinel = value; } } const getColorSchemeSentinel = () => { if (!isFileUrl()) { const storageValue = window.localStorage.getItem("quarto-color-scheme"); return storageValue != null ? storageValue : localAlternateSentinel; } else { return localAlternateSentinel; } } const toggleGiscusIfUsed = (isAlternate, darkModeDefault) => { const baseTheme = document.querySelector('#giscus-base-theme')?.value ?? 'light'; const alternateTheme = document.querySelector('#giscus-alt-theme')?.value ?? 'dark'; let newTheme = ''; if(authorPrefersDark) { newTheme = isAlternate ? baseTheme : alternateTheme; } else { newTheme = isAlternate ? alternateTheme : baseTheme; } const changeGiscusTheme = () => { // From: https://github.com/giscus/giscus/issues/336 const sendMessage = (message) => { const iframe = document.querySelector('iframe.giscus-frame'); if (!iframe) return; iframe.contentWindow.postMessage({ giscus: message }, 'https://giscus.app'); } sendMessage({ setConfig: { theme: newTheme } }); } const isGiscussLoaded = window.document.querySelector('iframe.giscus-frame') !== null; if (isGiscussLoaded) { changeGiscusTheme(); } }; const authorPrefersDark = false; const darkModeDefault = authorPrefersDark; document.querySelector('link#quarto-text-highlighting-styles.quarto-color-scheme-extra').rel = 'disabled-stylesheet'; document.querySelector('link#quarto-bootstrap.quarto-color-scheme-extra').rel = 'disabled-stylesheet'; let localAlternateSentinel = darkModeDefault ? 'alternate' : 'default'; // Dark / light mode switch window.quartoToggleColorScheme = () => { // Read the current dark / light value let toAlternate = !hasAlternateSentinel(); toggleColorMode(toAlternate); setStyleSentinel(toAlternate); toggleGiscusIfUsed(toAlternate, darkModeDefault); window.dispatchEvent(new Event('resize')); }; // Switch to dark mode if need be if (hasAlternateSentinel()) { toggleColorMode(true); } else { toggleColorMode(false); }


Tópicos
 
Canopy Openess Index in R

Packages

Data
Importing

Visualizing


Calculating Canopy Openness
Visualizing

Calculating






Canopy Openess Index in R

   
 

Author



Edson Silva-Júnior



   
   
 body { text-align: justify; font-size: 20px} blockquote { background-color: #FAEFA6; padding: 10px; font-size: 14.5px }
Canopy Openess Index in R


In ecology reasearchs about forest species, such leaf litter animals, plants and fungi, one useful environmental process is the canopy openness, which is the how much the tree canopy is open. This affects several ecological process, due the solar radiation rates achieving that ecosystems. For that, ecology researchers use fish eye lens to photograph the forest canopy, to better analyse those processes. Canopy may be measure by an index, where:



0: a canopy fully close;



1: there is not canopy.



Several image analysis softwares can generate canopy openness index analysis, such as ImageJ. Although, for many images, programming languages can automatize process, accelerating the research’s progress. In R, it is possible to analyse a set of canopy images and getting its canopy openness indexes.


Packages


For those analysis, we gonna use the packages:



terra: importing images as spatRast objets and get its propieties;



tidyverse: to assistant plots and loop processes;



tidyterra: to better plot the spatRast objets;



hemispheR: to binarize canopy imagens, for canopy openness indexes analysis.



Remind to confer whether the packages where previously installed to library them.



library(terra)


terra 1.8.50


library(tidyverse)


── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
✔ dplyr     1.1.4     ✔ readr     2.1.5
✔ forcats   1.0.0     ✔ stringr   1.5.1
✔ ggplot2   3.5.2     ✔ tibble    3.2.1
✔ lubridate 1.9.4     ✔ tidyr     1.3.1
✔ purrr     1.0.4     



── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
✖ tidyr::extract() masks terra::extract()
✖ dplyr::filter()  masks stats::filter()
✖ dplyr::lag()     masks stats::lag()
ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors


library(tidyterra)



Anexando pacote: 'tidyterra'

O seguinte objeto é mascarado por 'package:stats':

    filter


library(hemispheR)


Data

Importing


Our files are images taken by fish eye lens. Those images are archived on cropped-images folder. Initialy, using list.files() function, we get imagens path link.



images <- list.files(path = "cropped-images", 
                     pattern = ".png")

images


[1] "imagem1.png" "imagem2.png" "imagem3.png" "imagem4.png"



Visualizing


The next step is the importing and visualizing the images.



To importing, we use terra::rast() function, loading images as spatRast objects. Then, we use ggplot2 and tidyterra functions to plot. Moreover, we construct a loop for the every imagens, constructing a function and loading it with purrr package.



visualizing_canopy <- function(x){
  
  x <- paste0("cropped-images/", x)
  
  raster_bi <- terra::rast(x)
  
  ggplots <- ggplot() +
    tidyterra::geom_spatraster_rgb(data = raster_bi) +
    scale_fill_continuous(na.value = "transparent") +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) 
  
  print(ggplots)
  
}

purrr::walk(images, visualizing_canopy)


<SpatRaster> resampled to 501264 cells.









<SpatRaster> resampled to 501264 cells.









<SpatRaster> resampled to 501264 cells.









<SpatRaster> resampled to 501264 cells.









Calculating Canopy Openness

Visualizing


To calculating canopy openness index, we first binarize images, to pixels equivalent to canopy tree equals to 0 value and pixels equivalent to open sky equals to 1 value. For that, we use hemispheR::import_fisheye() to crop images to circle shape and then hemispheR::binarize_fisheye() to binarize circle shaped images.



canopy_visualizing <- function(x){ 
  
  x <- paste0("cropped-images/", x)
  
  analy <- stringr::str_glue("analysis for {x}") 
  
  file <- x  |>
    hemispheR::import_fisheye()  |>
    hemispheR::binarize_fisheye()

  ggplt <- ggplot() +
    tidyterra::geom_spatraster(data = file) +
    scale_fill_viridis_c(na.value = "transparent", breaks = seq(0, 1, 1)) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    labs(title = analy) +
    theme_bw()
  
  print(ggplt)
  
}

purrr::walk(images, canopy_visualizing)


It is a circular fisheye, where xc, yc and radius are 1485.5, 1485.5, 1483.5



<SpatRaster> resampled to 501264 cells.









It is a circular fisheye, where xc, yc and radius are 1499.5, 1499.5, 1497.5
<SpatRaster> resampled to 501264 cells.









It is a circular fisheye, where xc, yc and radius are 1499.5, 1499.5, 1497.5
<SpatRaster> resampled to 501264 cells.









It is a circular fisheye, where xc, yc and radius are 1500, 1500, 1498
<SpatRaster> resampled to 501264 cells.









Calculating


Finally, with binarize raster, we measuring the ration pixels = 1 (open sky) count by all pixels image count. For that, we use the terra::ncell() function.



canopy_Openess <- function(x){ 
  
  x <- paste0("cropped-images/", x)
  
  stringr::str_glue("analysis for {x}") |> message()
  
  raster <- x |>
    hemispheR::import_fisheye() |>
    hemispheR::binarize_fisheye()
  
  values_1 <- raster[raster > 0] |> terra::ncell()
  
  values_all <- raster |> terra::ncell()
  
  result <- values_1 / values_all
  
  print(result)
  
}

purrr::walk(images, canopy_Openess)


[1] 0.49774



[1] 0.5004268



[1] 0.2591377



[1] 0.1944832




Notice the images with more open sky presented higher canopy openness index values.


 window.document.addEventListener("DOMContentLoaded", function (event) { // Ensure there is a toggle, if there isn't float one in the top right if (window.document.querySelector('.quarto-color-scheme-toggle') === null) { const a = window.document.createElement('a'); a.classList.add('top-right'); a.classList.add('quarto-color-scheme-toggle'); a.href = ""; a.onclick = function() { try { window.quartoToggleColorScheme(); } catch {} return false; }; const i = window.document.createElement("i"); i.classList.add('bi'); a.appendChild(i); window.document.body.appendChild(a); } setColorSchemeToggle(hasAlternateSentinel()) const icon = ""; const anchorJS = new window.AnchorJS(); anchorJS.options = { placement: 'right', icon: icon }; anchorJS.add('.anchored'); const isCodeAnnotation = (el) => { for (const clz of el.classList) { if (clz.startsWith('code-annotation-')) {  return true; } } return false; } const onCopySuccess = function(e) { // button target const button = e.trigger; // don't keep focus button.blur(); // flash "checked" button.classList.add('code-copy-button-checked'); var currentTitle = button.getAttribute("title"); button.setAttribute("title", "Copied!"); let tooltip; if (window.bootstrap) { button.setAttribute("data-bs-toggle", "tooltip"); button.setAttribute("data-bs-placement", "left"); button.setAttribute("data-bs-title", "Copied!"); tooltip = new bootstrap.Tooltip(button,  { trigger: "manual",  customClass: "code-copy-button-tooltip", offset: [0, -8]}); tooltip.show();  } setTimeout(function() { if (tooltip) { tooltip.hide(); button.removeAttribute("data-bs-title"); button.removeAttribute("data-bs-toggle"); button.removeAttribute("data-bs-placement"); } button.setAttribute("title", currentTitle); button.classList.remove('code-copy-button-checked'); }, 1000); // clear code selection e.clearSelection(); } const getTextToCopy = function(trigger) { const codeEl = trigger.previousElementSibling.cloneNode(true); for (const childEl of codeEl.children) { if (isCodeAnnotation(childEl)) { childEl.remove(); } } return codeEl.innerText; } const clipboard = new window.ClipboardJS('.code-copy-button:not([data-in-quarto-modal])', { text: getTextToCopy }); clipboard.on('success', onCopySuccess); if (window.document.getElementById('quarto-embedded-source-code-modal')) { const clipboardModal = new window.ClipboardJS('.code-copy-button[data-in-quarto-modal]', { text: getTextToCopy, container: window.document.getElementById('quarto-embedded-source-code-modal') }); clipboardModal.on('success', onCopySuccess); } var localhostRegex = new RegExp(/^(?:http|https):\/\/localhost\:?[0-9]*\//); var mailtoRegex = new RegExp(/^mailto:/); var filterRegex = new RegExp('/' + window.location.host + '/'); var isInternal = (href) => { return filterRegex.test(href) || localhostRegex.test(href) || mailtoRegex.test(href); } // Inspect non-navigation links and adorn them if external var links = window.document.querySelectorAll('a[href]:not(.nav-link):not(.navbar-brand):not(.toc-action):not(.sidebar-link):not(.sidebar-item-toggle):not(.pagination-link):not(.no-external):not([aria-hidden]):not(.dropdown-item):not(.quarto-navigation-tool):not(.about-link)'); for (var i=0; i<links.length; i++) { const link = links[i]; if (!isInternal(link.href)) { // undo the damage that might have been done by quarto-nav.js in the case of // links that we want to consider external if (link.dataset.originalHref !== undefined) { link.href = link.dataset.originalHref; } } } function tippyHover(el, contentFn, onTriggerFn, onUntriggerFn) { const config = { allowHTML: true, maxWidth: 500, delay: 100, arrow: false, appendTo: function(el) { return el.parentElement; }, interactive: true, interactiveBorder: 10, theme: 'quarto', placement: 'bottom-start', }; if (contentFn) { config.content = contentFn; } if (onTriggerFn) { config.onTrigger = onTriggerFn; } if (onUntriggerFn) { config.onUntrigger = onUntriggerFn; } window.tippy(el, config);  } const noterefs = window.document.querySelectorAll('a[role="doc-noteref"]'); for (var i=0; i<noterefs.length; i++) { const ref = noterefs[i]; tippyHover(ref, function() { // use id or data attribute instead here let href = ref.getAttribute('data-footnote-href') || ref.getAttribute('href'); try { href = new URL(href).hash; } catch {} const id = href.replace(/^#\/?/, ""); const note = window.document.getElementById(id); if (note) { return note.innerHTML; } else { return ""; } }); } const xrefs = window.document.querySelectorAll('a.quarto-xref'); const processXRef = (id, note) => { // Strip column container classes const stripColumnClz = (el) => { el.classList.remove("page-full", "page-columns"); if (el.children) { for (const child of el.children) { stripColumnClz(child); } } } stripColumnClz(note) if (id === null || id.startsWith('sec-')) { // Special case sections, only their first couple elements const container = document.createElement("div"); if (note.children && note.children.length > 2) { container.appendChild(note.children[0].cloneNode(true)); for (let i = 1; i < note.children.length; i++) { const child = note.children[i]; if (child.tagName === "P" && child.innerText === "") { continue; } else { container.appendChild(child.cloneNode(true)); break; } } if (window.Quarto?.typesetMath) { window.Quarto.typesetMath(container); } return container.innerHTML } else { if (window.Quarto?.typesetMath) { window.Quarto.typesetMath(note); } return note.innerHTML; } } else { // Remove any anchor links if they are present const anchorLink = note.querySelector('a.anchorjs-link'); if (anchorLink) { anchorLink.remove(); } if (window.Quarto?.typesetMath) { window.Quarto.typesetMath(note); } if (note.classList.contains("callout")) { return note.outerHTML; } else { return note.innerHTML; } } } for (var i=0; i<xrefs.length; i++) { const xref = xrefs[i]; tippyHover(xref, undefined, function(instance) { instance.disable(); let url = xref.getAttribute('href'); let hash = undefined;  if (url.startsWith('#')) { hash = url; } else { try { hash = new URL(url).hash; } catch {} } if (hash) { const id = hash.replace(/^#\/?/, ""); const note = window.document.getElementById(id); if (note !== null) { try { const html = processXRef(id, note.cloneNode(true)); instance.setContent(html); } finally { instance.enable(); instance.show(); } } else { // See if we can fetch this fetch(url.split('#')[0]) .then(res => res.text()) .then(html => { const parser = new DOMParser(); const htmlDoc = parser.parseFromString(html, "text/html"); const note = htmlDoc.getElementById(id); if (note !== null) { const html = processXRef(id, note); instance.setContent(html); }  }).finally(() => { instance.enable(); instance.show(); }); } } else { // See if we can fetch a full url (with no hash to target) // This is a special case and we should probably do some content thinning / targeting fetch(url) .then(res => res.text()) .then(html => { const parser = new DOMParser(); const htmlDoc = parser.parseFromString(html, "text/html"); const note = htmlDoc.querySelector('main.content'); if (note !== null) { // This should only happen for chapter cross references // (since there is no id in the URL) // remove the first header if (note.children.length > 0 && note.children[0].tagName === "HEADER") { note.children[0].remove(); } const html = processXRef(null, note); instance.setContent(html); }  }).finally(() => { instance.enable(); instance.show(); }); } }, function(instance) { }); } let selectedAnnoteEl; const selectorForAnnotation = ( cell, annotation) => { let cellAttr = 'data-code-cell="' + cell + '"'; let lineAttr = 'data-code-annotation="' + annotation + '"'; const selector = 'span[' + cellAttr + '][' + lineAttr + ']'; return selector; } const selectCodeLines = (annoteEl) => { const doc = window.document; const targetCell = annoteEl.getAttribute("data-target-cell"); const targetAnnotation = annoteEl.getAttribute("data-target-annotation"); const annoteSpan = window.document.querySelector(selectorForAnnotation(targetCell, targetAnnotation)); const lines = annoteSpan.getAttribute("data-code-lines").split(","); const lineIds = lines.map((line) => { return targetCell + "-" + line; }) let top = null; let height = null; let parent = null; if (lineIds.length > 0) { //compute the position of the single el (top and bottom and make a div) const el = window.document.getElementById(lineIds[0]); top = el.offsetTop; height = el.offsetHeight; parent = el.parentElement.parentElement; if (lineIds.length > 1) { const lastEl = window.document.getElementById(lineIds[lineIds.length - 1]); const bottom = lastEl.offsetTop + lastEl.offsetHeight; height = bottom - top; } if (top !== null && height !== null && parent !== null) { // cook up a div (if necessary) and position it  let div = window.document.getElementById("code-annotation-line-highlight"); if (div === null) { div = window.document.createElement("div"); div.setAttribute("id", "code-annotation-line-highlight"); div.style.position = 'absolute'; parent.appendChild(div); } div.style.top = top - 2 + "px"; div.style.height = height + 4 + "px"; div.style.left = 0; let gutterDiv = window.document.getElementById("code-annotation-line-highlight-gutter"); if (gutterDiv === null) { gutterDiv = window.document.createElement("div"); gutterDiv.setAttribute("id", "code-annotation-line-highlight-gutter"); gutterDiv.style.position = 'absolute'; const codeCell = window.document.getElementById(targetCell); const gutter = codeCell.querySelector('.code-annotation-gutter'); gutter.appendChild(gutterDiv); } gutterDiv.style.top = top - 2 + "px"; gutterDiv.style.height = height + 4 + "px"; } selectedAnnoteEl = annoteEl; } }; const unselectCodeLines = () => { const elementsIds = ["code-annotation-line-highlight", "code-annotation-line-highlight-gutter"]; elementsIds.forEach((elId) => { const div = window.document.getElementById(elId); if (div) { div.remove(); } }); selectedAnnoteEl = undefined; }; // Handle positioning of the toggle window.addEventListener( "resize", throttle(() => { elRect = undefined; if (selectedAnnoteEl) { selectCodeLines(selectedAnnoteEl); } }, 10) ); function throttle(fn, ms) { let throttle = false; let timer; return (...args) => { if(!throttle) { // first call gets through fn.apply(this, args); throttle = true; } else { // all the others get throttled if(timer) clearTimeout(timer); // cancel #2 timer = setTimeout(() => { fn.apply(this, args); timer = throttle = false; }, ms); } }; } // Attach click handler to the DT const annoteDls = window.document.querySelectorAll('dt[data-target-cell]'); for (const annoteDlNode of annoteDls) { annoteDlNode.addEventListener('click', (event) => { const clickedEl = event.target; if (clickedEl !== selectedAnnoteEl) { unselectCodeLines(); const activeEl = window.document.querySelector('dt[data-target-cell].code-annotation-active'); if (activeEl) { activeEl.classList.remove('code-annotation-active'); } selectCodeLines(clickedEl); clickedEl.classList.add('code-annotation-active'); } else { // Unselect the line unselectCodeLines(); clickedEl.classList.remove('code-annotation-active'); } }); } const findCites = (el) => { const parentEl = el.parentElement; if (parentEl) { const cites = parentEl.dataset.cites; if (cites) { return { el, cites: cites.split(' ') }; } else { return findCites(el.parentElement) } } else { return undefined; } }; var bibliorefs = window.document.querySelectorAll('a[role="doc-biblioref"]'); for (var i=0; i<bibliorefs.length; i++) { const ref = bibliorefs[i]; const citeInfo = findCites(ref); if (citeInfo) { tippyHover(citeInfo.el, function() { var popup = window.document.createElement('div'); citeInfo.cites.forEach(function(cite) { var citeDiv = window.document.createElement('div'); citeDiv.classList.add('hanging-indent'); citeDiv.classList.add('csl-entry'); var biblioDiv = window.document.getElementById('ref-' + cite); if (biblioDiv) { citeDiv.innerHTML = biblioDiv.innerHTML; } popup.appendChild(citeDiv); }); return popup.innerHTML; }); } } });
    !function(t,e){"function"==typeof define&&define.amd?define([],e()):"object"==typeof module&&module.exports?module.exports=e():function n(){document&&document.body?t.zenscroll=e():setTimeout(n,9)}()}(this,function(){"use strict";var t=function(t){return t&&"getComputedStyle"in window&&"smooth"===window.getComputedStyle(t)["scroll-behavior"]};if("undefined"==typeof window||!("document"in window))return{};var e=function(e,n,o){n=n||999,o||0===o||(o=9);var i,r=function(t){i=t},u=function(){clearTimeout(i),r(0)},c=function(t){return Math.max(0,e.getTopOf(t)-o)},a=function(o,i,c){if(u(),0===i||i&&i<0||t(e.body))e.toY(o),c&&c();else{var a=e.getY(),f=Math.max(0,o)-a,s=(new Date).getTime();i=i||Math.min(Math.abs(f),n),function t(){r(setTimeout(function(){var n=Math.min(1,((new Date).getTime()-s)/i),o=Math.max(0,Math.floor(a+f*(n<.5?2*n*n:n*(4-2*n)-1)));e.toY(o),n<1&&e.getHeight()+o<e.body.scrollHeight?t():(setTimeout(u,99),c&&c())},9))}()}},f=function(t,e,n){a(c(t),e,n)},s=function(t,n,i){var r=t.getBoundingClientRect().height,u=e.getTopOf(t)+r,s=e.getHeight(),l=e.getY(),d=l+s;c(t)<l||r+o>s?f(t,n,i):u+o>d?a(u-s+o,n,i):i&&i()},l=function(t,n,o,i){a(Math.max(0,e.getTopOf(t)-e.getHeight()/2+(o||t.getBoundingClientRect().height/2)),n,i)};return{setup:function(t,e){return(0===t||t)&&(n=t),(0===e||e)&&(o=e),{defaultDuration:n,edgeOffset:o}},to:f,toY:a,intoView:s,center:l,stop:u,moving:function(){return!!i},getY:e.getY,getTopOf:e.getTopOf}},n=document.documentElement,o=function(){return window.scrollY||n.scrollTop},i=e({body:document.scrollingElement||document.body,toY:function(t){window.scrollTo(0,t)},getY:o,getHeight:function(){return window.innerHeight||n.clientHeight},getTopOf:function(t){return t.getBoundingClientRect().top+o()-n.offsetTop}});if(i.createScroller=function(t,o,i){return e({body:t,toY:function(e){t.scrollTop=e},getY:function(){return t.scrollTop},getHeight:function(){return Math.min(t.clientHeight,window.innerHeight||n.clientHeight)},getTopOf:function(t){return t.offsetTop}},o,i)},"addEventListener"in window&&!window.noZensmooth&&!t(document.body)){var r="history"in window&&"pushState"in history,u=r&&"scrollRestoration"in history;u&&(history.scrollRestoration="auto"),window.addEventListener("load",function(){u&&(setTimeout(function(){history.scrollRestoration="manual"},9),window.addEventListener("popstate",function(t){t.state&&"zenscrollY"in t.state&&i.toY(t.state.zenscrollY)},!1)),window.location.hash&&setTimeout(function(){var t=i.setup().edgeOffset;if(t){var e=document.getElementById(window.location.href.split("#")[1]);if(e){var n=Math.max(0,i.getTopOf(e)-t),o=i.getY()-n;0<=o&&o<9&&window.scrollTo(0,n)}}},9)},!1);var c=new RegExp("(^|\\s)noZensmooth(\\s|$)");window.addEventListener("click",function(t){for(var e=t.target;e&&"A"!==e.tagName;)e=e.parentNode;if(!(!e||1!==t.which||t.shiftKey||t.metaKey||t.ctrlKey||t.altKey)){if(u){var n=history.state&&"object"==typeof history.state?history.state:{};n.zenscrollY=i.getY();try{history.replaceState(n,"")}catch(t){}}var o=e.getAttribute("href")||"";if(0===o.indexOf("#")&&!c.test(e.className)){var a=0,f=document.getElementById(o.substring(1));if("#"!==o){if(!f)return;a=i.getTopOf(f)}t.preventDefault();var s=function(){window.location=o},l=i.setup().edgeOffset;l&&(a=Math.max(0,a-l),r&&(s=function(){history.pushState({},"",o)})),i.toY(a,null,s)}}},!1)}return i});

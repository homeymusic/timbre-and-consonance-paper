let baseURL = window.location.pathname;
let separator = ";"

$( document ).ready(() => {
  let searchParams = new URLSearchParams(window.location.search)
  
  if (searchParams.has("select")) {
    let selectEncoded = searchParams.get("select");
    let selectDecoded = decodeURI(selectEncoded);
    let selectSplit = selectDecoded.split(separator);

    selectSplit.forEach((select) => {
      console.log("Seeking " + select);
      $("li").find(`[data-value='` + select + `']`).click()
    });
  }
  
  $("li").click(() => setTimeout(updateURL, 10));
})

function updateURL() {
  let active = [];
  //$("li[class='active']").filter(":visible").children().each((index, element) => {
  //$("a[aria-expanded='true']").each((index, element) => {
  $("div").filter(".tab-pane").filter(".active").filter(":visible").each((index, element) => {
    if (isActive(element)) {
      active.push(element.getAttribute("data-value"));    
    }
  })
  let joined = active.join(separator);
  let encoded = encodeURI(joined);
  window.history.pushState({}, "", baseURL + "?select=" + encoded);
}

function isActive(elt) {
  if (elt.classList.contains("main-container")) {
    return true
  }
  let parentIsActive = isActive(elt.parentElement);
  let eltIsInactive = elt.classList.contains("tab-pane") && !elt.classList.contains("active");
  
  return parentIsActive && !eltIsInactive
}

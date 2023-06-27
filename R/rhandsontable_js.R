rhot_set_visual_colheaders <- function(hot, colheaders) {
  htmlwidgets::onRender(hot, "
    function(el, x, data) {
      var hot = this.hot;
      hot.updateSettings({'colHeaders': data});
    }
  ", data = colheaders)
}

rhot_disable_context_menu <- function(hot) {
  htmlwidgets::onRender(hot, "
    function(el, x, data) {
      var hot = this.hot;
      hot.addHook('beforeOnCellContextMenu', (e) => {
        e.stopImmediatePropagation();
      });
    }
  ")
}

rhot_renderer_gray_bg_on_read_only <- "
function (instance, td, row, col, prop, value, cellProperties) {
  Handsontable.renderers.TextRenderer.apply(this, arguments);

  if (cellProperties.readOnly) {
    td.style.background = 'lightgray';
    td.style.color = 'gray';
    if (value === null || value === '') {
      td.innerText = '-';
    }
  } else {
    customRenderer(instance, td, row, col, prop, value, cellProperties)
  }
}"

rhot_renderer_validate_provid_gray_bg_on_read_only <- "
function (instance, td, row, col, prop, value, cellProperties) {
  Handsontable.renderers.TextRenderer.apply(this, arguments);

  if (cellProperties.readOnly) {
    td.style.background = 'lightgray';
    td.style.color = 'gray';
    if (value === null || value === '') {
      td.innerText = '-';
    }
  } else {
    re = /^Q[0-9]{4}\\-[0-9]{5}$/;
    if (value === null || value.match(re) === null) {
      td.style.background = 'red';
    } else {
      td.style.background = 'white';
    }
  }
}"

rhot_renderer_validate_accnr <- "
function (instance, td, row, col, prop, value, cellProperties) {
  Handsontable.renderers.TextRenderer.apply(this, arguments);
  re = /^[ABCDGHLXP][0-9]{4}\\/?[0-9]{5}$/;
  if (value !== null && value.match(re) === null) {
    td.style.background = 'red';
  } else {
    td.style.background = 'white';
  }
}"

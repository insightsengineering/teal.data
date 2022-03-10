// allow a button to be pressed by pressing the enter key see
// https://github.com/daattali/advanced-shiny/blob/master/proxy-click/app.R
$(function () {
  var $els = $("[data-proxy-click]");
  $.each(
    $els,
    function (idx, el) {
      var $el = $(el);
      var $proxy = $("#" + $el.data("proxyClick"));
      $el.keydown(function (e) {
        if (e.keyCode == 13) {
          $proxy.click();
        }
      });
    }
  );
});

import $ from "jquery";

console.log('imported wells_fargo_content_script');

$('body').on('mouseenter', 'a', (event) => {
  const a = $(event.currentTarget);
  if (a.attr('data-pdf') === 'true') {
    const url = a.attr('data-url');
    const text = a.text();
    const [fullMatch, month, day, year] = /(\d\d)\/(\d\d)\/(\d\d)/.exec(text);

    const accountName = $("select#statements_selectAccount option:selected").text();

    const filename = `financial-statements/wells_fargo/${accountName}/20${year}/${month}-${day}.pdf`;

    chrome.runtime.sendMessage({
      type: 'requestDownload',
      url,
      filename,
      waitMs: 0,
    });
  }
});

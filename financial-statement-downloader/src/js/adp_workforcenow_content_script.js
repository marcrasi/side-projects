import $ from "jquery";

console.log('imported adp_workforcenow_content_script');

const pad2 = (numstr) => `00${numstr}`.slice(-2);

/**
 * We wait a bit for the PDF file to be actually ready.
 * Is there a more robust way to do this?
 * It would be nice to somehow validate that the PDF actually
 * downloaded correctly.
 */
const WAIT_FOR_DOWNLOAD_MS = 5000;

setInterval(() => {
  $('iframe').each((index, element) => {
    const src = $(element).prop('src');
    if (src.indexOf('getStatementDocument') !== -1) {
      const container = $(element).parent().parent();
      const statementInfoLis = container.find('li');

      var payDate;
      var checkNumber;

      statementInfoLis.each((index, li) => {
        if ($(li).text().indexOf('Pay Date') !== -1) {
          payDate = $(li).text().split(':')[1].trim();
        }
        if ($(li).text().indexOf('Check Number') !== -1) {
          checkNumber = $(li).text().split(':')[1].trim();
        }
      });

      const [month, day, year] = payDate.split('/');
      const filename = `financial-statements/adp_workforcenow/${year}/${pad2(month)}-${pad2(day)}-${checkNumber}.pdf`;

      chrome.runtime.sendMessage({
        type: 'requestDownload',
        url: src,
        filename,
        waitMs: WAIT_FOR_DOWNLOAD_MS,
      });
    }
  });
}, 100);

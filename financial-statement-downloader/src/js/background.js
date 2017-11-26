import '../img/icon-128.png'
import '../img/icon-34.png'

const downloads = {};

/**
 * We wait a bit for the PDF file to be actually ready.
 * Is there a more robust way to do this?
 * It would be nice to somehow validate that the PDF actually
 * downloaded correctly.
 */
const WAIT_FOR_DOWNLOAD_MS = 5000;

chrome.runtime.onMessage.addListener((request) => {
  if (request.type === 'requestDownload') {
    const {filename, url} = request;
    if (downloads[filename] === undefined) {
      downloads[filename] = {
        state: 'waiting',
        filename,
        url,
      };
      setTimeout(() => {
        chrome.downloads.download(
          {
            url,
            filename,
          },
          (downloadId) => {
            downloads[filename] = {
              ...downloads[filename],
              state: 'issued',
              downloadId,
            };
          }
        );
      }, WAIT_FOR_DOWNLOAD_MS);
    }
  }
});

setInterval(() => {
  console.log(downloads);
}, 1000);

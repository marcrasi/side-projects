import '../img/icon-128.png'
import '../img/icon-34.png'

const downloads = {};

chrome.runtime.onMessage.addListener((request) => {
  if (request.type === 'requestDownload') {
    const {filename, url, waitMs} = request;
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
      }, waitMs || 0);
    }
  }
});

setInterval(() => {
  console.log(downloads);
}, 1000);

// @format
import 'sanitize.css';
import Elm from './Main.elm';

const app = Elm.Main.embed(document.getElementById('main'));

app.ports.scrollToId.subscribe(id => {
  const element = document.getElementById(id);

  if (!element) {
    return;
  }

  // Delaying scroll to let Elm render elements
  setTimeout(() => {
    const viewTop = window.pageYOffset;
    const viewBottom = window.pageYOffset + window.innerHeight;
    const viewHeight = window.innerHeight;

    const elementTop = element.offsetTop;
    const elementBottom = element.offsetTop + element.offsetHeight;

    const offset = viewHeight / 10;

    if (elementTop < viewTop + offset) {
      window.scroll(0, elementTop - offset);
    } else if (elementBottom > viewBottom - offset) {
      window.scroll(0, elementBottom - viewHeight + offset);
    }
  }, 20);
});

app.ports.localStorageSet.subscribe(({key, value}) => {
  window.localStorage.setItem(key, value);
});

app.ports.localStorageGet.subscribe(key => {
  const value = window.localStorage.getItem(key);
  app.ports.onLocalStorageResponse.send({key, value});
});

app.ports.localStorageRemove.subscribe(key => {
  window.localStorage.removeItem(key);
});

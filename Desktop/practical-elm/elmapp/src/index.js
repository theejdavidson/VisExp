import './main.css';
import { Elm } from './Main.elm';
import * as serviceWorker from './serviceWorker';

let app = Elm.Main.init({
  flags: {sessionId: localStorage.getItem("sessionId")} })
  app.ports.saveSessionId.subscribe((sessionId) => {
    if (sessionId === null) localStorage.removeItem("sessionId")
    else
                localStorage.setItem("sessionId", sessionId)
    })
    

// If you want your app to work offline and load faster, you can change
// unregister() to register() below. Note this comes with some pitfalls.
// Learn more about service workers: https://bit.ly/CRA-PWA
serviceWorker.unregister();

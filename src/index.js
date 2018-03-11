import 'sanitize.css'
import Elm from './Main.elm'

const app = Elm.Main.embed(document.getElementById('main'))

app.ports.export.subscribe((text) => {
    const data = new Blob([text], {type: 'text/plain'})
    const objURL = window.URL.createObjectURL(data)

    const panel = window.open()
    panel.document.write(`<a href=${objURL} download="export.md">Download link</a>`)
})

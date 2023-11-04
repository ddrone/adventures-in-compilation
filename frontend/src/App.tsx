import { useRef, useState } from "react";

function App() {
  const [msg, setMsg] = useState('');
  const [lastText, setLastText] = useState('');
  const textarea = useRef<HTMLTextAreaElement|null>(null);

  async function handleClick() {
    const text = textarea.current!.value;
    setLastText(text);
    const response = await fetch('/api/parse', {
      method: 'POST',
      body: text
    });
    const json = await response.json();
    setMsg(JSON.stringify(json));
  }

  return (
    <>
      <textarea ref={textarea} rows={10} cols={80}>
      </textarea><br />
      <button onClick={handleClick}>
        Click me
      </button><br />
      <div className="output">
        <div className="text">
          <pre>
            {lastText}
          </pre>
        </div>
        <div className="result">
          {msg}
        </div>
      </div>
    </>
  )
}

export default App

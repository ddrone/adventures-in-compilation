import { KeyboardEvent, useRef, useState } from "react";

function App() {
  const [msg, setMsg] = useState('');
  const [lastText, setLastText] = useState('');
  const textarea = useRef<HTMLTextAreaElement|null>(null);

  async function handleClick() {
    const text = textarea.current!.value;
    setLastText(text);
    setMsg('');
    const response = await fetch('/api/parse', {
      method: 'POST',
      body: text
    });
    const json = await response.json();
    setMsg(JSON.stringify(json));
  }

  function handleKeyUp(e: KeyboardEvent): boolean {
    if (e.key === 'Enter' && e.ctrlKey) {
      handleClick();
      return true;
    }

    return false;
  }

  return (
    <>
      <textarea ref={textarea} rows={10} cols={80} onKeyUp={handleKeyUp}>
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

import { KeyboardEvent, useRef, useState } from "react";
import { Resp, description, tokenInfo } from "./api";
import ErrorHighlight from "./ErrorHighlight";

function App() {
  const [resp, setResp] = useState<Resp>({tag: "RespOK"});
  const [lastText, setLastText] = useState('');
  const textarea = useRef<HTMLTextAreaElement|null>(null);

  async function handleClick() {
    const text = textarea.current!.value;
    setLastText(text);
    setResp({tag: "RespOK"});
    const response = await fetch('/api/parse', {
      method: 'POST',
      body: text
    });
    const json = await response.json();
    setResp(json as Resp);
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
        <ErrorHighlight
          text={lastText}
          info={tokenInfo(resp)} />
        {description(resp)}
      </div>
    </>
  )
}

export default App

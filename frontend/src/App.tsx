import { KeyboardEvent, useEffect, useRef, useState } from "react";
import { Resp, description, emptyResponse, emptyTests, getTests, processParseForest, singleParse, tokenInfo } from "./api";
import ErrorHighlight from "./ErrorHighlight";
import TreeView from "./TreeView";
import TestList from "./TestList";

function App() {
  const [resp, setResp] = useState<Resp>(emptyResponse);
  const [lastText, setLastText] = useState('');
  const [tests, setTests] = useState(emptyTests);
  const textarea = useRef<HTMLTextAreaElement|null>(null);

  useEffect(() => {
    getTests().then(setTests);
  }, []);

  async function handleClick() {
    parseText(textarea.current!.value);
  }

  async function parseText(text: string) {
    setLastText(text);
    setResp(emptyResponse);
    setResp(await singleParse(text));
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
        {resp.tag === 'RespOK' && <>
          <TreeView text={lastText} forest={processParseForest(resp.contents.spParseForest)} />
          <pre className={resp.contents.spCompileResult.tag === 'CRFailure' ? 'error' : ''}>
            {resp.contents.spCompileResult.contents}
          </pre>
        </>}
        <ErrorHighlight
          text={lastText}
          info={tokenInfo(resp)} />
        {description(resp)}
      </div>
      <TestList tests={tests} onLoad={parseText} />
    </>
  )
}

export default App

import { ReactElement } from "react";
import { Resp, TestFile, TestResponse } from "./api"

interface TestListProps {
  tests: TestResponse;
  onLoad: (text: string) => void;
}

function TestList(props: TestListProps) {
  if (props.tests.trFiles.length === 0) {
    return null;
  }

  function renderParseStatus(status: Resp): ReactElement {
    switch (status.tag) {
      case 'RespOK':
        return <span className="good">OK</span>;
      case 'RespLexerError':
        return <span className="error">Lexer error</span>;
      case 'RespParserError':
        return <span className="error">Parser error</span>;
    }
  }

  function renderRow(file: TestFile) {
    return (
      <tr>
        <td>{file.tfName}</td>
        <td>
          {renderParseStatus(file.tfParseResult)}
        </td>
        <td>
          <button onClick={() => props.onLoad(file.tfContents)}>
            Load
          </button>
        </td>
      </tr>
    );
  }

  return (
    <table>
      {props.tests.trFiles.map(renderRow)}
    </table>
  );
}

export default TestList

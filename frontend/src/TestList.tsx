import { TestResponse } from "./api"

interface TestListProps {
  tests: TestResponse;
}

function TestList(props: TestListProps) {
  if (props.tests.trFiles.length === 0) {
    return null;
  }

  return (
    <ul>
      {props.tests.trFiles.map(file => <li>{file.tfName}</li>)}
    </ul>
  );
}

export default TestList

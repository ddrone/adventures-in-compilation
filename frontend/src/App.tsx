import { useState } from "react";

function App() {
  const [msg, setMsg] = useState('');

  async function handleClick() {
    const response = await fetch('/api/parse', {
      method: 'POST',
      body: 'Program to parse'
    });
    const json = await response.json();
    setMsg(JSON.stringify(json));
  }

  return (
    <>
      <button onClick={handleClick}>
        Click me
      </button><br />
      {msg}
    </>
  )
}

export default App

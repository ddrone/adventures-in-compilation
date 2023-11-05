import { TokenInfo } from "./api";

interface ErrorHighlightProps {
  text: string;
  info?: TokenInfo;
}

function ErrorHighlight(props: ErrorHighlightProps) {
  function showRange(text: string, info: TokenInfo) {
    const prefix = text.substring(0, info.tokOffset);
    const middle = text.substring(info.tokOffset, info.tokEnd);
    const suffix = text.substring(info.tokEnd);

    return (
      <>
        {prefix}
        <span className={middle.length === 0 ? 'error-marker' : 'error'}>
          {middle.length === 0 ? '|' : middle}
        </span>
        {suffix}
      </>
    )
  }

  return (
    <pre>
      {props.info === undefined && props.text}
      {props.info !== undefined && showRange(props.text, props.info)}
    </pre>
  )
}

export default ErrorHighlight

import { ReactElement, useState } from "react";
import { ProcParseForest, ProcParseTree } from "./api";

interface TreeViewProps {
  text: string;
  forest: ProcParseForest;
}

function TreeView(props: TreeViewProps) {
  const [highlightId, setHighlightId] = useState(-1);

  function resetHighlight() {
    setHighlightId(-1);
  }

  function go(tree: ProcParseTree) {
    const className = tree.id === highlightId ? 'highlight' : '';
    return (
      <li>
        <span
          className={className}
          onMouseEnter={() => setHighlightId(tree.id)}
          onMouseLeave={resetHighlight}>
          {tree.name}
        </span>
        {tree.children.length > 0 && goForest(tree.children)}
      </li>
    )
  }

  function goForest(forest: ProcParseForest) {
    return (
      <ul>
        {forest.map(go)}
      </ul>
    )
  }

  let lastAdded = 0;
  function goText(tree: ProcParseTree, parentId: number): ReactElement {
    const prefix = lastAdded < tree.tokenInfo.tokOffset && (
      <span onMouseEnter={() => setHighlightId(parentId)} onMouseLeave={resetHighlight}>
        {props.text.substring(lastAdded, tree.tokenInfo.tokOffset)}
      </span>
    );
    lastAdded = tree.tokenInfo.tokOffset;

    const className = tree.id === highlightId ? 'highlight' : '';
    const children: ReactElement[] = [];

    for (const child of tree.children) {
      children.push(goText(child, tree.id));
      lastAdded = child.tokenInfo.tokEnd;
    }

    const suffix = lastAdded < tree.tokenInfo.tokEnd && props.text.substring(lastAdded, tree.tokenInfo.tokEnd);
    lastAdded = tree.tokenInfo.tokEnd;

    return (
      <>
        {prefix}
        <span className={className}>
          {children}
          <span onMouseEnter={() => setHighlightId(tree.id)} onMouseLeave={resetHighlight}>
            {suffix}
          </span>
        </span>
      </>
    )
  }

  function topLevelText(forest: ProcParseForest): ReactElement {
    const children = forest.map(child => goText(child, -1));
    const suffix = lastAdded < props.text.length && props.text.substring(lastAdded);

    return (
      <>
        {children}
        {suffix}
      </>
    );
  }

  return (
    <>
      <pre>
        {topLevelText(props.forest)}
      </pre>
      {goForest(props.forest)}
    </>
  );
}

export default TreeView

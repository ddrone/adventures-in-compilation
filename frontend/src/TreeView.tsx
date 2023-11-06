import { ReactElement, useState } from "react";
import { ParseForest, ParseTree, TokenInfo } from "./api";

interface ProcParseTree {
  id: number;
  name: string;
  tokenInfo: TokenInfo;
  children: ProcParseForest;
}

export type ProcParseForest = ProcParseTree[];

export function processParseForest(forest: ParseForest): ProcParseForest {
  let id = 0;

  function go(tree: ParseTree): ProcParseTree {
    return {
      id: id++,
      name: tree.ptName,
      tokenInfo: tree.ptTokenInfo,
      children: tree.ptChildren.map(go)
    }
  }

  return forest.map(go);
}

interface TreeViewProps {
  text: string;
  forest: ProcParseForest;
}

interface TextTreeChunk {
  tag: 'chunk';
  contents: string;
  id?: number;
}

interface TextTreeNode {
  tag: 'node';
  children: TextTree[];
}

type TextTree = TextTreeChunk | TextTreeNode;

// This function has to be recursive, therefore it needs to accept the start position to maintain
// necessary invariant.
function toTextTree(text: string, start: number, forest: ProcParseForest, parentId?: number): TextTreeNode {
  const children: TextTree[] = [];
  let lastAdded = start;

  function addChunk(from: number, to: number, id?: number) {
    if (from !== lastAdded) {
      throw new Error(`missing chunks from ${lastAdded} to ${from}`);
    }
    children.push({
      tag: 'chunk',
      contents: text.substring(from, to),
      id
    });
    lastAdded = to;
  }

  function go(node: ProcParseTree, parentNode?: ProcParseTree) {
    // This if might not be needed
    if (node.tokenInfo.tokOffset > lastAdded) {
      addChunk(lastAdded, node.tokenInfo.tokOffset);
      children.push({
        tag: 'chunk',
        contents: text.substring(lastAdded, node.tokenInfo.tokOffset),
        id: parentNode === undefined ? undefined : parentNode.id
      });
      lastAdded = node.tokenInfo.tokOffset;
    }

    if (node.children.length === 0) {
      children.push({
        tag: 'chunk',
        contents: text.substring(node.tokenInfo.tokOffset, node.tokenInfo.tokEnd),
        id: node.id
      });
    }
    for (const child of node.children) {
      if (child.tokenInfo.tokOffset > lastAdded) {
      }
      go(child, node);
    }

    if (lastAdded < node.tokenInfo.tokEnd) {
      children.push({
        tag: 'chunk',
        contents: text.substring(lastAdded, node.tokenInfo.tokEnd),
        id: node.id
      });
      lastAdded = node.tokenInfo.tokEnd;
    }
  }
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
  function goText(tree: ProcParseTree): ReactElement {
    const prefix = lastAdded < tree.tokenInfo.tokOffset && props.text.substring(lastAdded, tree.tokenInfo.tokOffset);
    lastAdded = tree.tokenInfo.tokOffset;

    const className = tree.id === highlightId ? 'highlight' : '';
    const children: ReactElement[] = [];

    for (const child of tree.children) {
      children.push(goText(child));
      lastAdded = child.tokenInfo.tokEnd;
    }

    const suffix = lastAdded < tree.tokenInfo.tokEnd && props.text.substring(lastAdded, tree.tokenInfo.tokEnd);
    lastAdded = tree.tokenInfo.tokEnd;

    return (
      <>
        {prefix}
        <span
          className={className}
          onMouseEnter={() => setHighlightId(tree.id)}
          onMouseLeave={resetHighlight}>
          {children}
          {suffix}
        </span>
      </>
    )
  }

  function topLevelText(forest: ProcParseForest): ReactElement {
    const children = forest.map(goText);
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

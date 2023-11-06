import { useState } from "react";
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
  children: [TextTree];
}

type TextTree = TextTreeChunk | TextTreeNode;

function toTextTree(text: string, forest: ProcParseForest): TextTree {
  throw new Error('TODO: implement me');
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

  return goForest(props.forest);
}

export default TreeView

export interface RespOK {
  tag: "RespOK";
  contents: ParseForest;
}

export interface TokenInfo {
  tokOffset: number;
  tokRow: number;
  tokColumn: number;
  tokEnd: number;
}

export interface RespParserError {
  tag: "RespParserError";
  contents: [string, TokenInfo]
}

export interface RespLexerError {
  tag: "RespLexerError";
  contents: [string, TokenInfo]
}

export type Resp = RespOK | RespLexerError | RespParserError;

export const emptyResponse: RespOK = {
  tag: 'RespOK',
  contents: []
}

export function tokenInfo(resp: Resp): TokenInfo | undefined {
  switch (resp.tag) {
    case "RespOK":
      return undefined;
    case "RespLexerError":
      return resp.contents[1];
    case "RespParserError":
      return resp.contents[1];
  }
}

export function description(resp: Resp): string {
  switch (resp.tag) {
    case "RespOK":
      return '';
    case "RespLexerError":
      return `Lexer error: ${resp.contents[0]}`;
    case "RespParserError":
      return `Parser error: ${resp.contents[0]}`;
  }
}

export interface ParseTree {
  ptName: string;
  ptTokenInfo: TokenInfo;
  ptChildren: ParseForest;
}

export type ParseForest = ParseTree[];

export interface ProcParseTree {
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

export interface RespOK {
  tag: "RespOK"
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

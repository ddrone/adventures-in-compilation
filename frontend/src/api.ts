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

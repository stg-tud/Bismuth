package loreCompilerPlugin.lsp

import upickle.default.ReadWriter
import ujson.Value

/** Various Data structures for parsing any kind of message sent from the language server to the client.
  * Not intended be used for constructing messages to send to the Dafny Language Server, such usage may break.
  */
object LSPDataTypes {
  /* ------------------- Sealed traits for case classes --------------------- */

  // All JSON-RPC messages contain must the "jsonrpc" property, in this case version 2.0 by default
  // Reference: https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#abstractMessage
  sealed trait LSPMessage derives ReadWriter {
    val jsonrpc: String = "2.0"
  }

  /* ------------------- Generic message case classes --------------------- */

  // Reference: https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#requestMessage
  case class LSPRequest(
      method: String,
      // Params can have different forms, so store them generically on initial deserialization
      params: Option[Value] = None,
      id: Int
  ) extends LSPMessage

  // Reference: https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#notificationMessage
  case class LSPNotification(
      method: String,
      // Params can have different forms, so store them generically on initial deserialization
      params: Option[Value] = None
  ) extends LSPMessage

  /** LSP Responses may contain a result _or_ an error, but never both at the same time. */
  // Reference: https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#responseMessage
  case class LSPResponse(
      // Result can have different forms, so store it generically on initial deserialization
      result: Option[Value] = None,
      error: Option[ResponseError] = None,
      id: Int
  ) extends LSPMessage

  // Reference: https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#responseError
  case class ResponseError(
      code: ErrorCode,
      message: String,
      // Error data can have varying structure, so store it generically on initial deserialization
      data: Option[Value] = None
  ) derives ReadWriter

  // Reference: https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#errorCodes
  enum ErrorCode(val code: Int) {
    case ParseError     extends ErrorCode(-32700)
    case InvalidRequest extends ErrorCode(-32600)
    case MethodNotFound extends ErrorCode(-32601)
    case InvalidParams  extends ErrorCode(-32602)
    case InternalError  extends ErrorCode(-32603)
    // Further cases are implementation-specific
  }

  object ErrorCode {
    // Custom pickler for upickle so we can deserialize integers into error codes
    implicit val rw: ReadWriter[ErrorCode] = upickle.default.readwriter[Int].bimap[ErrorCode](
      // Serialization: From ErrorCode enum to integer
      error => error.code,
      // Deserialization: From integer to ErrorCode enum
      {
        case -32700 => ErrorCode.ParseError
        case -32600 => ErrorCode.InvalidRequest
        case -32601 => ErrorCode.MethodNotFound
        case -32602 => ErrorCode.InvalidParams
        case -32603 => ErrorCode.InternalError
        // Can't have a catch-all "Unknown Error" enum case since that loses the int value
        case i => throw new Error(s"Unknown LSP error code: $i")
      }
    )
  }

  // Reference: https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#range
  case class LSPRange(
      start: LSPPosition,
      end: LSPPosition
  ) derives ReadWriter

  // Reference: https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#position
  case class LSPPosition(
      line: Int,
      character: Int
  ) derives ReadWriter

  // Reference: https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#location
  case class LSPLocation(
      uri: String,
      range: LSPRange
  ) derives ReadWriter

  /* ------------------- Method-specific case classes --------------------- */

  case class SymbolStatusNotification(
      method: String = "dafny/textDocument/symbolStatus",
      params: SymbolStatusParams
  ) extends LSPMessage

  // Reference: https://github.com/dafny-lang/dafny/blob/2473d8da29c2b934b5465ef92073305f34910596/Source/DafnyLanguageServer/Workspace/FileVerificationStatus.cs#L11
  case class SymbolStatusParams(
      uri: String,
      version: Int,
      namedVerifiables: List[NamedVerifiable]
  ) derives ReadWriter

  // Reference: https://github.com/dafny-lang/dafny/blob/54c5496adf569aff9568e9ef963573f115fafee0/Source/DafnyLanguageServer/Workspace/FileVerificationStatus.cs#L48
  case class NamedVerifiable(
      nameRange: LSPRange,
      status: VerificationStatus
  ) derives ReadWriter

  // Reference: https://github.com/dafny-lang/dafny/blob/2473d8da29c2b934b5465ef92073305f34910596/Source/DafnyLanguageServer/Workspace/FileVerificationStatus.cs#L66
  enum VerificationStatus(val status: Int) {
    case Stale   extends VerificationStatus(0)
    case Queued  extends VerificationStatus(1)
    case Running extends VerificationStatus(2)
    // Status 3 does not exist, see above reference
    case Error   extends VerificationStatus(4)
    case Correct extends VerificationStatus(5)
  }

  object VerificationStatus {
    // Custom pickler for upickle so we can deserialize integers into verification statuses
    implicit val rw: ReadWriter[VerificationStatus] = upickle.default.readwriter[Int].bimap[VerificationStatus](
      // Serialization: From VerificationStatus enum to integer
      error => error.status,
      // Deserialization: From integer to VerificationStatus enum
      {
        case 0 => VerificationStatus.Stale
        case 1 => VerificationStatus.Queued
        case 2 => VerificationStatus.Running
        case 4 => VerificationStatus.Error
        case 5 => VerificationStatus.Correct
        case i => throw new Error(s"Unknown Dafny verification status: $i")
      }
    )
  }

  case class CompilationStatusNotification(
      method: String = "dafny/compilation/status",
      params: CompilationStatusParams
  ) extends LSPMessage

  // Reference: https://github.com/dafny-lang/dafny/blob/master/Source/DafnyLanguageServer/Workspace/Notifications/CompilationStatusParams.cs
  case class CompilationStatusParams(
      uri: String,
      version: Int,
      status: CompilationStatus,
      message: String
  ) derives ReadWriter

  // Reference: https://github.com/dafny-lang/dafny/blob/master/Source/DafnyLanguageServer/Workspace/Notifications/CompilationStatus.cs
  enum CompilationStatus(val code: String) {
    case Parsing             extends CompilationStatus("Parsing")
    case InternalException   extends CompilationStatus("InternalException")
    case ParsingFailed       extends CompilationStatus("ParsingFailed")
    case ResolutionStarted   extends CompilationStatus("ResolutionStarted")
    case ResolutionFailed    extends CompilationStatus("ResolutionFailed")
    case ResolutionSucceeded extends CompilationStatus("ResolutionSucceeded")
  }

  object CompilationStatus {
    // Custom pickler for upickle so we can deserialize integers into compilation statuses
    implicit val rw: ReadWriter[CompilationStatus] = upickle.default.readwriter[String].bimap[CompilationStatus](
      // Serialization: From CompilationStatus enum to string
      status => status.code,
      // Deserialization: From string to CompilationStatus enum
      {
        case "Parsing"             => CompilationStatus.Parsing
        case "InternalException"   => CompilationStatus.InternalException
        case "ParsingFailed"       => CompilationStatus.ParsingFailed
        case "ResolutionStarted"   => CompilationStatus.ResolutionStarted
        case "ResolutionFailed"    => CompilationStatus.ResolutionFailed
        case "ResolutionSucceeded" => CompilationStatus.ResolutionSucceeded
        case str                   => throw new Error(s"Unknown Dafny compilation status: $str")
      }
    )
  }

  case class PublishDiagnosticsNotification(
      method: String = "textDocument/publishDiagnostics",
      params: PublishDiagnosticsParams
  ) extends LSPMessage

  // Reference: https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#publishDiagnosticsParams
  case class PublishDiagnosticsParams(
      uri: String,
      version: Int,
      diagnostics: List[Diagnostic]
  ) derives ReadWriter

  // Reference: https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#diagnostic
  case class Diagnostic(
      range: LSPRange,
      severity: Option[DiagnosticSeverity] = None,
      code: Option[String] = None,
      codeDescription: Option[CodeDescription] = None,
      source: Option[String] = None,
      message: String,
      tags: Option[List[DiagnosticTag]] = None,
      relatedInformation: Option[List[DiagnosticRelatedInformation]] = None,
      data: Option[Value] = None
  ) derives ReadWriter

  // Reference: https://github.com/dafny-lang/dafny/blob/master/Source/DafnyLanguageServer/Workspace/Notifications/CompilationStatus.cs
  enum DiagnosticSeverity(val code: Int) {
    case Error       extends DiagnosticSeverity(1)
    case Warning     extends DiagnosticSeverity(2)
    case Information extends DiagnosticSeverity(3)
    case Hint        extends DiagnosticSeverity(4)
  }

  object DiagnosticSeverity {
    // Custom pickler for upickle so we can deserialize integers into diagnostic severities
    implicit val rw: ReadWriter[DiagnosticSeverity] = upickle.default.readwriter[Int].bimap[DiagnosticSeverity](
      // Serialization: From DiagnosticSeverity enum to string
      severity => severity.code,
      // Deserialization: From int to DiagnosticSeverity enum
      {
        case 1 => DiagnosticSeverity.Error
        case 2 => DiagnosticSeverity.Warning
        case 3 => DiagnosticSeverity.Information
        case 4 => DiagnosticSeverity.Hint
        case i => throw new Error(s"Unknown diagnostic severity: $i")
      }
    )
  }

  // Reference: https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#codeDescription
  case class CodeDescription(
      href: String
  ) derives ReadWriter

  // Reference: https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#diagnosticTag
  enum DiagnosticTag(val code: Int) {
    case Unnecessary extends DiagnosticTag(1)
    case Deprecated  extends DiagnosticTag(2)
  }

  object DiagnosticTag {
    // Custom pickler for upickle so we can deserialize integers into diagnostic tags
    implicit val rw: ReadWriter[DiagnosticTag] = upickle.default.readwriter[Int].bimap[DiagnosticTag](
      // Serialization: From DiagnosticTag enum to string
      severity => severity.code,
      // Deserialization: From int to DiagnosticTag enum
      {
        case 1 => DiagnosticTag.Unnecessary
        case 2 => DiagnosticTag.Deprecated
        case i => throw new Error(s"Unknown diagnostic tag: $i")
      }
    )
  }

  // Reference: https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#diagnosticTag
  case class DiagnosticRelatedInformation(
      location: LSPLocation,
      message: String
  ) derives ReadWriter
}

module Cowlaser.HTTP where

import Control.Monad.Aff (Aff)
import Data.List (List)
import Data.Map (Map)
import Data.NonEmpty (NonEmpty)
import Data.String.CaseInsensitive (CI)
import Node.HTTP (HTTP)
import Node.Stream (Readable, Writable)
import Prelude

type Request eff =
  { method :: CI
  , uri :: String
  , headers :: Map CI (NonEmpty List String)
  , body :: Readable () (http :: HTTP | eff)
  }

type Response eff =
  { status :: {code :: Int, message :: String}
  , headers :: Map CI (NonEmpty List String)
  , body :: Writable () (http :: HTTP | eff) -> Aff (http :: HTTP | eff) Unit
  }

statusContinue :: {code :: Int, message :: String}
statusContinue = {code: 100, message: "Continue"}

statusSwitchingProtocols :: {code :: Int, message :: String}
statusSwitchingProtocols = {code: 101, message: "Switching Protocols"}

statusOK :: {code :: Int, message :: String}
statusOK = {code: 200, message: "OK"}

statusCreated :: {code :: Int, message :: String}
statusCreated = {code: 201, message: "Created"}

statusAccepted :: {code :: Int, message :: String}
statusAccepted = {code: 202, message: "Accepted"}

statusNonAuthoritativeInformation :: {code :: Int, message :: String}
statusNonAuthoritativeInformation = {code: 203, message: "Non-Authoritative Information"}

statusNoContent :: {code :: Int, message :: String}
statusNoContent = {code: 204, message: "No Content"}

statusResetContent :: {code :: Int, message :: String}
statusResetContent = {code: 205, message: "Reset Content"}

statusPartialContent :: {code :: Int, message :: String}
statusPartialContent = {code: 206, message: "Partial Content"}

statusMultipleChoices :: {code :: Int, message :: String}
statusMultipleChoices = {code: 300, message: "Multiple Choices"}

statusMovedPermanently :: {code :: Int, message :: String}
statusMovedPermanently = {code: 301, message: "Moved Permanently"}

statusFound :: {code :: Int, message :: String}
statusFound = {code: 302, message: "Found"}

statusSeeOther :: {code :: Int, message :: String}
statusSeeOther = {code: 303, message: "See Other"}

statusNotModified :: {code :: Int, message :: String}
statusNotModified = {code: 304, message: "Not Modified"}

statusUseProxy :: {code :: Int, message :: String}
statusUseProxy = {code: 305, message: "Use Proxy"}

statusTemporaryRedirect :: {code :: Int, message :: String}
statusTemporaryRedirect = {code: 307, message: "Temporary Redirect"}

statusBadRequest :: {code :: Int, message :: String}
statusBadRequest = {code: 400, message: "Bad Request"}

statusUnauthorized :: {code :: Int, message :: String}
statusUnauthorized = {code: 401, message: "Unauthorized"}

statusPaymentRequired :: {code :: Int, message :: String}
statusPaymentRequired = {code: 402, message: "Payment Required"}

statusForbidden :: {code :: Int, message :: String}
statusForbidden = {code: 403, message: "Forbidden"}

statusNotFound :: {code :: Int, message :: String}
statusNotFound = {code: 404, message: "Not Found"}

statusMethodNotAllowed :: {code :: Int, message :: String}
statusMethodNotAllowed = {code: 405, message: "Method Not Allowed"}

statusNotAcceptable :: {code :: Int, message :: String}
statusNotAcceptable = {code: 406, message: "Not Acceptable"}

statusProxyAuthenticationRequired :: {code :: Int, message :: String}
statusProxyAuthenticationRequired = {code: 407, message: "Proxy Authentication Required"}

statusRequestTimeout :: {code :: Int, message :: String}
statusRequestTimeout = {code: 408, message: "Request Timeout"}

statusConflict :: {code :: Int, message :: String}
statusConflict = {code: 409, message: "Conflict"}

statusGone :: {code :: Int, message :: String}
statusGone = {code: 410, message: "Gone"}

statusLengthRequired :: {code :: Int, message :: String}
statusLengthRequired = {code: 411, message: "Length Required"}

statusPreconditionFailed :: {code :: Int, message :: String}
statusPreconditionFailed = {code: 412, message: "Precondition Failed"}

statusPayloadTooLarge :: {code :: Int, message :: String}
statusPayloadTooLarge = {code: 413, message: "Payload Too Large"}

statusURITooLong :: {code :: Int, message :: String}
statusURITooLong = {code: 414, message: "URI Too Long"}

statusUnsupportedMediaType :: {code :: Int, message :: String}
statusUnsupportedMediaType = {code: 415, message: "Unsupported Media Type"}

statusRangeNotSatisfiable :: {code :: Int, message :: String}
statusRangeNotSatisfiable = {code: 416, message: "Range Not Satisfiable"}

statusExpectationFailed :: {code :: Int, message :: String}
statusExpectationFailed = {code: 417, message: "Expectation Failed"}

statusI'mateapot :: {code :: Int, message :: String}
statusI'mateapot = {code: 418, message: "I'm a teapot"}

statusUpgradeRequired :: {code :: Int, message :: String}
statusUpgradeRequired = {code: 426, message: "Upgrade Required"}

statusInternalServerError :: {code :: Int, message :: String}
statusInternalServerError = {code: 500, message: "Internal Server Error"}

statusNotImplemented :: {code :: Int, message :: String}
statusNotImplemented = {code: 501, message: "Not Implemented"}

statusBadGateway :: {code :: Int, message :: String}
statusBadGateway = {code: 502, message: "Bad Gateway"}

statusServiceUnavailable :: {code :: Int, message :: String}
statusServiceUnavailable = {code: 503, message: "Service Unavailable"}

statusGatewayTimeOut :: {code :: Int, message :: String}
statusGatewayTimeOut = {code: 504, message: "Gateway Time-out"}

statusHTTPVersionNotSupported :: {code :: Int, message :: String}
statusHTTPVersionNotSupported = {code: 505, message: "HTTP Version Not Supported"}

statusProcessing :: {code :: Int, message :: String}
statusProcessing = {code: 102, message: "Processing"}

statusMultiStatus :: {code :: Int, message :: String}
statusMultiStatus = {code: 207, message: "Multi-Status"}

statusIMUsed :: {code :: Int, message :: String}
statusIMUsed = {code: 226, message: "IM Used"}

statusPermanentRedirect :: {code :: Int, message :: String}
statusPermanentRedirect = {code: 308, message: "Permanent Redirect"}

statusUnprocessableEntity :: {code :: Int, message :: String}
statusUnprocessableEntity = {code: 422, message: "Unprocessable Entity"}

statusLocked :: {code :: Int, message :: String}
statusLocked = {code: 423, message: "Locked"}

statusFailedDependency :: {code :: Int, message :: String}
statusFailedDependency = {code: 424, message: "Failed Dependency"}

statusPreconditionRequired :: {code :: Int, message :: String}
statusPreconditionRequired = {code: 428, message: "Precondition Required"}

statusTooManyRequests :: {code :: Int, message :: String}
statusTooManyRequests = {code: 429, message: "Too Many Requests"}

statusRequestHeaderFieldsTooLarge :: {code :: Int, message :: String}
statusRequestHeaderFieldsTooLarge = {code: 431, message: "Request Header Fields Too Large"}

statusUnavailableForLegalReasons :: {code :: Int, message :: String}
statusUnavailableForLegalReasons = {code: 451, message: "Unavailable For Legal Reasons"}

statusVariantAlsoNegotiates :: {code :: Int, message :: String}
statusVariantAlsoNegotiates = {code: 506, message: "Variant Also Negotiates"}

statusInsufficientStorage :: {code :: Int, message :: String}
statusInsufficientStorage = {code: 507, message: "Insufficient Storage"}

statusNetworkAuthenticationRequired :: {code :: Int, message :: String}
statusNetworkAuthenticationRequired = {code: 511, message: "Network Authentication Required"}

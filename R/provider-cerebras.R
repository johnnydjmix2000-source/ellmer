#' @include provider-openai-compatible.R
NULL

#' Chat with a model hosted on Cerebras
#'
#' @description
#' Sign up at <https://www.cerebras.ai>.
#'
#' Built on top of [chat_openai_compatible()].
#'
#' @export
#' @family chatbots
#' @inheritParams chat_openai
#' @param api_key `r lifecycle::badge("deprecated")` Use `credentials` instead.
#' @param credentials `r api_key_param("CEREBRAS_API_KEY")`
#' @param base_url The base URL to the endpoint; the default uses Cerebras.
#' @param model `r param_model("openai/gpt-oss-120b")`
#' @param params Common model parameters, usually created by [params()].
#' @inherit chat_openai return
#' @examples
#' \dontrun{
#' chat <- chat_cerebras()
#' chat$chat("Tell me three jokes about statisticians")
#' }
chat_cerebras <- function(
  system_prompt = NULL,
  base_url = "https://api.cerebras.ai/v1",
  api_key = NULL,
  credentials = NULL,
  model = NULL,
  params = NULL,
  api_args = list(),
  echo = NULL,
  api_headers = character()
) {
  model <- set_default(model, "openai/gpt-oss-120b")
  echo <- check_echo(echo)

  credentials <- as_credentials(
    "chat_cerebras",
    function() cerebras_key(),
    credentials = credentials,
    api_key = api_key
  )

  params <- params %||% params()

  provider <- ProviderCerebras(
    name = "Cerebras",
    base_url = base_url,
    model = model,
    params = params,
    extra_args = api_args,
    credentials = credentials,
    extra_headers = api_headers
  )
  Chat$new(provider = provider, system_prompt = system_prompt, echo = echo)
}

ProviderCerebras <- new_class(
  "ProviderCerebras",
  parent = ProviderOpenAICompatible
)

cerebras_key <- function() {
  key_get("CEREBRAS_API_KEY")
}

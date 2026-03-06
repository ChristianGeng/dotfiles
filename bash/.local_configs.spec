;; Credential specification for local_configs generation
;; Maps pass store entries to environment variables
;; Format: (name :pass "pass/path" :env "ENV_VAR_NAME")

((perplexity         :pass "code/perplexity_api_key"         :env "PERPLEXITY_API_KEY")
  (openai-personal   :pass "code/openai_api_key"             :env "OPENAI_API_KEY")
  (anthropic-personal :pass "code/anthropic_api_key_personal" :env "ANTHROPIC_API_KEY")
  (xai               :pass "code/xai_api_key"                :env "XAI_API_KEY")
  (pplx              :pass "code/perplexity_api_key"         :env "PPLX_API_KEY")

  ;; Add additional credentials here as needed
  ;; (example-api       :pass "api/example_key"              :env "EXAMPLE_API_KEY")
  )

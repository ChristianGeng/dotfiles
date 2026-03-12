;; Credential specification for local_configs generation
;; Maps pass store entries to environment variables
;; Format: (name :pass "pass/path" :env "ENV_VAR_NAME")

((openai-work        :pass "work/aud/api/openai/openai_api_key"       :env "OPENAI_API_KEY")
  (anthropic-work     :pass "work/aud/api/anthropic/anthropic_api_key_aud" :env "ANTHROPIC_API_KEY")
  (perplexity-personal :pass "personal/api/perplexity/perplexity_api_key" :env "PERPLEXITY_API_KEY")
  (xai-personal       :pass "personal/api/xai/xai_api_key"                :env "XAI_API_KEY")
  (brave-personal     :pass "personal/api/brave/BRAVE_API_KEY"            :env "BRAVE_API_KEY")
  (deepgram-personal  :pass "personal/api/deepgram/main"                  :env "DEEPGRAM_API_KEY")
  (gradium-personal   :pass "personal/api/gradium/gradium_api_key"        :env "GRADIUM_API_KEY")
  (gitlab-work        :pass "work/aud/api/gitlab/gitlab_api_token"         :env "GITLAB_API_TOKEN")

  ;; Add additional credentials here as needed
  ;; Example format:
  ;; (service-name :pass "work/aud/api/service/api_key" :env "SERVICE_API_KEY")
  )

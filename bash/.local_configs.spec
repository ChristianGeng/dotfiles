;; Credential specification for local_configs generation
;; Maps pass store entries to environment variables
;; Format: (name :pass "pass/path" :env "ENV_VAR_NAME")

;; NOTE: company/work creds (OPENAI_API_KEY, GITLAB_API_TOKEN, ...) now live in
;; audeering-dotfiles/audeering-config/.audeering_configs.spec, generated via
;; `generate-local-configs --profile audeering`. Keep only personal creds here.
((perplexity-personal :pass "personal/api/perplexity/perplexity_api_key" :env "PERPLEXITY_API_KEY")
  (xai-personal       :pass "personal/api/xai/xai_api_key"                :env "XAI_API_KEY")
  (brave-personal     :pass "personal/api/brave/BRAVE_API_KEY"            :env "BRAVE_API_KEY")
  (deepgram-personal  :pass "personal/api/deepgram/main"                  :env "DEEPGRAM_API_KEY")
  (gradium-personal   :pass "personal/api/gradium/gradium_api_key"        :env "GRADIUM_API_KEY")
  (vlh-pdf            :pass "personal/passwords/vlh-pdf-secret"             :env "PDF_PASSWORD")

  (huggingface-personal :pass "personal/huggingface/api_key"                    :env "HF_TOKEN")

  ;; Add additional credentials here as needed
  ;; Example format:
  ;; (service-name :pass "work/aud/api/service/api_key" :env "SERVICE_API_KEY")
  )

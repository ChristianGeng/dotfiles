;;; cg-ai.el --- AI/LLM helper functions -*- lexical-binding: t; -*-

;;; Commentary:
;; AI Rules system and workflow functions for enhanced AI interactions

;;; Code:

;;; AI Rules System (Similar to .cursorrules)

(defvar cg/ai-global-rules
  "You are an expert software developer assistant. Follow these global rules:

1. CODING STANDARDS:
   - Write clean, readable, and maintainable code
   - Follow language-specific best practices and idioms
   - Use meaningful variable and function names
   - Add comments for complex logic only
   - Prefer composition over inheritance
   - Write self-documenting code

2. SECURITY:
   - Never expose API keys or sensitive data
   - Validate all inputs
   - Use secure coding practices
   - Consider potential security vulnerabilities

3. PERFORMANCE:
   - Write efficient algorithms
   - Avoid premature optimization
   - Consider memory usage and time complexity
   - Use appropriate data structures

4. TESTING:
   - Suggest testable code structure
   - Include error handling
   - Consider edge cases
   - Write defensive code

5. DOCUMENTATION:
   - Keep documentation concise but clear
   - Update documentation when changing code
   - Use consistent formatting"
  "Global AI rules applied to all AI interactions.")

(defvar cg/ai-project-rules nil
  "Buffer to store project-specific AI rules loaded from .aiderrules file.")

(defun cg/load-project-ai-rules ()
  "Load AI rules from .aiderrules file in project root."
  (let ((rules-file (expand-file-name ".aiderrules" (project-root (project-current)))))
    (when (file-exists-p rules-file)
      (setq cg/ai-project-rules
            (with-temp-buffer
              (insert-file-contents rules-file)
              (buffer-string)))
      (message "Loaded project AI rules from %s" rules-file))))

(defun cg/get-combined-ai-rules ()
  "Combine global and project-specific AI rules."
  (concat cg/ai-global-rules
          (when cg/ai-project-rules
            (concat "\n\nPROJECT-SPECIFIC RULES:\n" cg/ai-project-rules))))

(defun cg/create-aiderrules-template ()
  "Create a template .aiderrules file in project root."
  (interactive)
  (let* ((project-root (project-root (project-current)))
         (rules-file (expand-file-name ".aiderrules" project-root))
         (template-content "# Project-specific AI rules for this codebase
# This file defines how AI assistants should behave in this project

## Framework/Technology Stack
- Language: [e.g., Python, JavaScript, Rust]
- Framework: [e.g., React, Django, Actix]
- Architecture: [e.g., MVC, microservices, monolith]

## Code Style Preferences
- Indentation: [e.g., 2 spaces, 4 spaces, tabs]
- Line length: [e.g., 80, 100, 120 characters]
- Naming convention: [e.g., camelCase, snake_case, PascalCase]

## Project-Specific Guidelines
- Use our custom error handling pattern
- Follow our API response format
- Implement proper logging using our logger
- Add type hints/annotations where applicable
- Follow our testing patterns and file structure

## Dependencies and Libraries
- Prefer [specific libraries] for [specific tasks]
- Avoid [specific libraries] due to [reasons]
- Use our internal utilities instead of [alternatives]

## File Organization
- Follow our directory structure conventions
- Use consistent file naming patterns
- Group related functionality appropriately

## Additional Instructions
- Always consider backward compatibility
- Optimize for readability over cleverness
- Include proper error messages
- Consider internationalization where applicable"))
    (if (file-exists-p rules-file)
        (message ".aiderrules already exists in %s" project-root)
      (with-temp-file rules-file
        (insert template-content))
      (find-file rules-file)
      (message "Created .aiderrules template in %s" project-root))))

;;; Enhanced AI functions with rules support

(defun cg/ai-send-with-rules (content prompt-type)
  "Send content to AI with appropriate rules prepended."
  (let ((full-prompt (concat (cg/get-combined-ai-rules)
                           "\n\n=== TASK ===\n"
                           prompt-type
                           "\n\n=== CODE ===\n"
                           content)))
    (with-current-buffer (get-buffer-create "*AI Assistant*")
      (erase-buffer)
      (insert full-prompt)
      (gptel-mode)
      (goto-char (point-max))
      (gptel-send))))

;;; Integrated AI workflow functions

(defun cg/ai-code-review ()
  "Send current buffer to GPTel for code review with rules."
  (interactive)
  (cg/ai-send-with-rules
   (buffer-string)
   "Please review this code for:
- Code quality and best practices
- Potential bugs or issues
- Performance improvements
- Security considerations
- Adherence to the specified rules and conventions"))

(defun cg/ai-explain-code ()
  "Explain selected code or function at point using GPTel with rules."
  (interactive)
  (let ((code (if (region-active-p)
                  (buffer-substring-no-properties (region-beginning) (region-end))
                (thing-at-point 'defun t))))
    (when code
      (cg/ai-send-with-rules
       code
       "Please explain this code in detail, considering the project context and rules."))))

(defun cg/ai-refactor-with-aider ()
  "Start aidermacs and suggest refactoring for current file with rules."
  (interactive)
  (cg/load-project-ai-rules)  ; Ensure rules are loaded
  (aidermacs-start)
  (sleep-for 2)  ; Wait for aider to start
  (let ((prompt (concat (cg/get-combined-ai-rules)
                       "\n\nPlease review and suggest refactoring improvements for "
                       (buffer-file-name)
                       ". Focus on code quality, maintainability, and adherence to the specified rules.")))
    (aidermacs-send-prompt prompt)))

(defun cg/ai-generate-code ()
  "Generate code based on user prompt with project rules."
  (interactive)
  (let ((user-prompt (read-string "Describe what code you need: ")))
    (cg/ai-send-with-rules
     (format "Current file: %s\nContext: %s"
             (or (buffer-file-name) "New file")
             (if (region-active-p)
                 (buffer-substring-no-properties (region-beginning) (region-end))
               "No specific context"))
     (concat "Generate code based on this request: " user-prompt))))

(defun cg/ai-fix-code ()
  "Fix code issues in current selection or buffer."
  (interactive)
  (let ((code (if (region-active-p)
                  (buffer-substring-no-properties (region-beginning) (region-end))
                (buffer-string))))
    (cg/ai-send-with-rules
     code
     "Please identify and fix any issues in this code. Provide the corrected version with explanations.")))

(defun cg/ai-optimize-code ()
  "Optimize selected code or buffer for performance."
  (interactive)
  (let ((code (if (region-active-p)
                  (buffer-substring-no-properties (region-beginning) (region-end))
                (buffer-string))))
    (cg/ai-send-with-rules
     code
     "Please optimize this code for better performance while maintaining readability and following the specified rules.")))

(defun cg/ai-add-tests ()
  "Generate tests for current function or class."
  (interactive)
  (let ((code (if (region-active-p)
                  (buffer-substring-no-properties (region-beginning) (region-end))
                (thing-at-point 'defun t))))
    (when code
      (cg/ai-send-with-rules
       code
       "Please generate comprehensive tests for this code. Include unit tests, edge cases, and error scenarios."))))

(defun cg/ai-add-documentation ()
  "Generate documentation for current function or class."
  (interactive)
  (let ((code (if (region-active-p)
                  (buffer-substring-no-properties (region-beginning) (region-end))
                (thing-at-point 'defun t))))
    (when code
      (cg/ai-send-with-rules
       code
       "Please generate appropriate documentation for this code. Include docstrings, parameter descriptions, and usage examples."))))

(defun cg/ai-toggle-all-tools ()
  "Toggle all AI tools on/off."
  (interactive)
  (when (fboundp 'copilot-mode)
    (copilot-mode 'toggle))
  (message "AI tools toggled: Copilot %s"
           (if (and (boundp 'copilot-mode) copilot-mode) "ON" "OFF")))

(provide 'cg-ai)
;;; cg-ai.el ends here

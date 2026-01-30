; SPDX-License-Identifier: PMPL-1.0-or-later
; FormBD Studio - Ecosystem Position
; Media-Type: application/vnd.ecosystem+scm

(ecosystem
  (version "1.0")
  (name "formbd-studio")
  (type "application")
  (purpose "Zero-friction GUI for FormBD with dependently-typed FBQL")

  (position-in-ecosystem
    (role "User-facing GUI for FormBD ecosystem")
    (layer "Application layer")
    (users
      "Journalists - Visual evidence entry with PROMPT scores"
      "Researchers - Reproducibility proofs and provenance tracking"
      "Compliance Officers - Reversibility guarantees and audit trails"
      "Developers - Learning dependent types through gradual revelation"))

  (related-projects
    (project "formbd"
      (relationship sibling-standard)
      (url "https://github.com/hyperpolymath/formbd")
      (description "The narrative-first database that Studio provides a GUI for")
      (integration
        "HTTP API for queries and mutations"
        "Form.Normalizer for FD discovery"
        "Provenance tracking and audit trails"))

    (project "fdql-dt"
      (relationship sibling-standard)
      (url "https://github.com/hyperpolymath/fdql-dt")
      (description "Dependently-typed query language that Studio generates")
      (integration
        "Lean 4 type checker for constraint validation"
        "Proof generation for schema changes"
        "PROMPT score type safety (BoundedNat 0 100)"))

    (project "formbd-debugger"
      (relationship sibling-standard)
      (url "https://github.com/hyperpolymath/formbd-debugger")
      (description "Proof-carrying database recovery tool")
      (integration
        "Studio can launch Debugger for recovery operations"
        "Shared proof visualization concepts"))

    (project "bofig"
      (relationship potential-consumer)
      (url "https://github.com/hyperpolymath/bofig")
      (description "Evidence graph for investigative journalism")
      (integration "Primary use case for PROMPT scores and evidence tracking"))

    (project "zotero-formbd"
      (relationship potential-consumer)
      (url "https://github.com/hyperpolymath/zotero-formbd")
      (description "Reference manager with PROMPT scores")
      (integration "Production pilot for refinement types"))

    (project "formbase"
      (relationship potential-consumer)
      (url "https://github.com/hyperpolymath/formbase")
      (description "Open-source Airtable alternative")
      (integration "Could use Studio components for verified data entry"))

    (project "tauri"
      (relationship dependency)
      (url "https://github.com/tauri-apps/tauri")
      (description "Cross-platform desktop app framework")
      (integration "Rust backend + web frontend architecture"))

    (project "rescript"
      (relationship dependency)
      (url "https://github.com/rescript-lang/rescript-compiler")
      (description "Type-safe language that compiles to JavaScript")
      (integration "All UI components written in ReScript")))

  (what-this-is
    "A zero-friction GUI for creating and querying FormBD databases"
    "A visual FBQLdt generator that hides dependent type complexity"
    "A schema builder with automatic proof generation"
    "A normalization assistant with plain English explanations"
    "A provenance-aware data entry interface"
    "A cross-platform desktop app (Mac, Windows, Linux)")

  (what-this-is-not
    "Not a general-purpose database GUI"
    "Not a replacement for command-line FBQL"
    "Not a web application (desktop only for now)"
    "Not a collaboration platform (single-user focus for MVP)"))

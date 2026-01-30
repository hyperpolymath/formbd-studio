; SPDX-License-Identifier: PMPL-1.0-or-later
; FormBD Studio - Meta Information
; Media-Type: application/vnd.meta+scm

(meta
  (version "1.0")
  (name "formbd-studio")
  (governance "hyperpolymath")

  (architecture-decisions
    (adr
      (id "ADR-001")
      (title "Use Tauri 2.0 for desktop app")
      (status accepted)
      (date "2025-01-11")
      (context "Need cross-platform desktop app with native performance")
      (decision "Use Tauri 2.0 with Rust backend instead of Electron")
      (consequences
        "Smaller bundle size than Electron"
        "Native performance for type checking calls"
        "Rust backend can integrate with FormBD's Zig bridge"
        "ReScript/JS frontend provides reactive UI"))

    (adr
      (id "ADR-002")
      (title "Use ReScript for frontend")
      (status accepted)
      (date "2025-01-11")
      (context "Need type-safe UI that compiles to JavaScript")
      (decision "Use ReScript instead of TypeScript (per RSR language policy)")
      (consequences
        "Type-safe without runtime overhead"
        "Better integration with React ecosystem"
        "Aligns with hyperpolymath language standards"
        "Learning curve for contributors unfamiliar with ReScript"))

    (adr
      (id "ADR-003")
      (title "Generate FBQLdt from visual builder")
      (status accepted)
      (date "2025-01-11")
      (context "Users shouldn't need to understand dependent types")
      (decision "Visual builders generate FBQLdt code automatically")
      (consequences
        "Users get verification benefits without learning Lean 4"
        "Advanced users can see/edit generated code"
        "Proof tactics auto-selected for common patterns"
        "Complex proofs may still require manual intervention"))

    (adr
      (id "ADR-004")
      (title "Deno for build tooling")
      (status accepted)
      (date "2025-01-11")
      (context "Need JavaScript runtime for build tasks")
      (decision "Use Deno instead of Node.js (per RSR language policy)")
      (consequences
        "No npm/node_modules"
        "Secure by default"
        "Import maps in deno.json"
        "Some ReScript tooling may need npx wrappers")))

  (development-practices
    (code-style
      "ReScript: Use @rescript/core, open RescriptCore by default"
      "Rust: Standard rustfmt, Clippy lints enabled"
      "All files: SPDX license header required")
    (security
      "Tauri CSP enabled (currently null for dev)"
      "No eval() or dynamic code execution"
      "User credentials never stored in app")
    (testing
      "ReScript: Unit tests with @rescript/test (TBD)"
      "Rust: Cargo test for Tauri commands"
      "E2E: Tauri driver tests (TBD)")
    (versioning
      "Semantic versioning (major.minor.patch)"
      "Version synced across package.json, Cargo.toml, tauri.conf.json")
    (documentation
      "README.adoc as primary documentation"
      "Code comments for complex logic"
      "STATE.scm for progress tracking")
    (branching
      "main: stable releases"
      "develop: integration branch"
      "feature/*: feature branches"))

  (design-rationale
    (why-tauri
      "Electron too heavy (200MB+ bundles)"
      "Need Rust for calling FormBD's Zig FFI"
      "Tauri 2.0 has mobile support for future iOS/Android")
    (why-rescript
      "TypeScript banned per RSR"
      "ReScript provides sound type system"
      "Better React integration than Reason")
    (why-desktop-first
      "Web requires server-side FormBD deployment"
      "Desktop can embed FormBD locally"
      "Mobile via Tauri 2.0 after desktop MVP")
    (why-proof-hiding
      "Target users are journalists/researchers, not type theorists"
      "Proofs are implementation detail, verification is the benefit"
      "Advanced mode can reveal proofs for expert users")))

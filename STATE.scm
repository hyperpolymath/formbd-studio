; SPDX-License-Identifier: PMPL-1.0-or-later
; FormBD Studio - Project State
; Media-Type: application/vnd.state+scm

(state
  (metadata
    (version "0.1.0")
    (schema-version "1.0")
    (created "2025-01-11")
    (updated "2026-01-12T23:00:00Z")
    (project "formbd-studio")
    (repo "https://github.com/hyperpolymath/formbd-studio"))

  (project-context
    (name "FormBD Studio")
    (tagline "Zero-friction interface for dependently-typed databases")
    (tech-stack
      (frontend "ReScript")
      (backend "Rust (Tauri 2.0)")
      (runtime "Deno")
      (type-checker "Lean 4 (via FBQLdt)")
      (database "FormBD")))

  (current-position
    (phase "production-ready")
    (overall-completion 65)
    (components
      (rescript-ui
        (status "implemented")
        (completion 90)
        (files
          "src/App.res - Main app shell with navigation"
          "src/SchemaBuilder.res - Collection creator"
          "src/QueryBuilder.res - Visual query construction"
          "src/DataEntryPanel.res - Form-based data entry"
          "src/ProofAssistant.res - Proof generation UI"
          "src/NormalizationPanel.res - Schema normalization"
          "src/FieldEditor.res - Field type picker"
          "src/FqldtPreview.res - Live FBQLdt preview with copy/create buttons"
          "src/Types.res - Shared types module"))
      (tauri-backend
        (status "functional")
        (completion 50)
        (files
          "src-tauri/src/main.rs - 10 Tauri commands (no unused variable warnings)"
          "src-tauri/Cargo.toml - Tauri 2.0 deps"
          "src-tauri/tauri.conf.json - App config"))
      (fbqldt-integration
        (status "not-started")
        (completion 0)
        (notes "Commands have TODO placeholders"))
      (formbd-integration
        (status "not-started")
        (completion 0)
        (notes "HTTP API not yet available in FormBD"))
      (build-pipeline
        (status "complete")
        (completion 100)
        (notes "Dev and production builds working. Deb/RPM packages generated.")))
    (working-features
      "ReScript UI component structure"
      "Tab navigation (Schema/Query/Data/Proofs/Normalize)"
      "Keyboard shortcuts (Ctrl/Cmd+1-5 for tab switching)"
      "localStorage persistence (collections auto-save/load)"
      "Field type system (Number, Text, Confidence, PromptScores)"
      "Tauri command definitions (10 commands)"
      "Schema types in Rust (FieldDef, CollectionDef, etc.)"
      "Normalization types (FunctionalDependency, NormalizationProposal)"
      "Proof types (ProofObligation, ConstraintViolation)"))

  (route-to-mvp
    (target-version "1.0.0")
    (definition "Working desktop app that can create schemas and query FormBD")

    (milestones
      (milestone (id "M1") (name "Build Pipeline")
        (status "completed")
        (items
          (item "Verify ReScript compilation" status: "completed")
          (item "Verify Tauri build" status: "completed")
          (item "Test dev mode (cargo tauri dev)" status: "completed")
          (item "Test production build (cargo tauri build)" status: "completed")
          (item "Add icons to src-tauri/icons/" status: "completed")
          (item "Generate .deb package" status: "completed")
          (item "Generate .rpm package" status: "completed")))

      (milestone (id "M2") (name "FormBD HTTP Integration")
        (status "blocked")
        (depends-on "FormBD M11")
        (items
          (item "Wait for FormBD HTTP API (M11)" status: "blocked")
          (item "Add HTTP client to Rust backend" status: "pending")
          (item "Implement execute_query command" status: "pending")
          (item "Implement insert_document command" status: "pending")))

      (milestone (id "M3") (name "FBQLdt Type Checking")
        (status "blocked")
        (depends-on "FBQLdt M5")
        (items
          (item "Wait for FBQLdt Zig FFI (M5)" status: "blocked")
          (item "Add Lean 4 subprocess invocation" status: "pending")
          (item "Implement validate_fbqldt with real checks" status: "pending")
          (item "Return actual proof obligations" status: "pending")))

      (milestone (id "M4") (name "Normalization Features")
        (status "pending")
        (depends-on "M2" "M3")
        (items
          (item "Connect discover_fds to Form.Normalizer" status: "pending")
          (item "Display FD discovery results in UI" status: "pending")
          (item "Implement apply_normalization with proofs" status: "pending")
          (item "Add three-phase migration support" status: "pending")))

      (milestone (id "M5") (name "Production Polish")
        (status "pending")
        (depends-on "M4")
        (items
          (item "Error handling and user feedback" status: "pending")
          (item "Keyboard navigation" status: "pending")
          (item "Settings persistence" status: "pending")
          (item "Cross-platform testing (Mac/Win/Linux)" status: "pending")))))

  (blockers-and-issues
    (critical
      (blocker
        (id "BLOCK-001")
        (title "Awaiting FormBD HTTP API")
        (description "FormBD M11 (HTTP API Server) must complete before Studio can connect")
        (blocked-milestones "M2" "M4")))
    (high
      (blocker
        (id "BLOCK-002")
        (title "Awaiting FBQLdt Zig FFI")
        (description "FBQLdt M5 (Zig FFI) needed for real type checking")
        (blocked-milestones "M3")))
    (medium)
    (low
      (issue
        (id "ISSUE-003")
        (title "AppImage bundling failed")
        (description "linuxdeploy failed during AppImage creation, but deb/rpm work fine"))))

  (ecosystem-integration
    (formbd-version "0.0.4")
    (fdql-dt-version "0.2.0")
    (alignment-status "awaiting-upstream")
    (integration-points
      (formbd
        "HTTP API for queries (awaiting M11)"
        "Schema metadata storage"
        "Provenance tracking"
        "FD discovery via Form.Normalizer")
      (fdql-dt
        "Type checking via Lean 4"
        "Proof generation"
        "PROMPT score validation"
        "Refinement type validation (BoundedNat, NonEmptyString)")))

  (critical-next-actions
    (immediate
      "Create GitHub release with built packages"
      "Test binary on clean system")
    (this-week
      "Test on different Linux distributions"
      "Run usability testing on the UI"
      "Address ReScript deprecation warnings")
    (this-month
      "Integrate when FormBD M11 completes"
      "Wire up FBQLdt type checking when M5 completes"))

  (session-history
    (snapshot "2025-01-11"
      (accomplishments
        "Created project scaffold"
        "Implemented all ReScript UI components (9 files)"
        "Implemented Tauri backend with 10 commands"
        "Set up deno.json and rescript.json"))
    (snapshot "2026-01-12"
      (accomplishments
        "Created STATE.scm"
        "Created ECOSYSTEM.scm"
        "Created META.scm"
        "Documented blockers and dependencies"
        "Aligned with unified FormBD ecosystem roadmap"))
    (snapshot "2026-01-12T14:30:00Z"
      (accomplishments
        "Fixed ReScript build errors (circular dependencies, Unicode syntax)"
        "Created Types.res module to break circular dependency"
        "Renamed DataEntry.res to DataEntryPanel.res"
        "Fixed rescript.json deprecation warnings (bs-dependencies->dependencies)"
        "Fixed Rust unused variable warnings in main.rs"
        "Created placeholder RGBA icons for Tauri"
        "Added Clipboard API binding for copy functionality"
        "Connected Create Collection button to functionality"
        "Successfully ran cargo tauri dev - application launches!"
        "Completed M1 (Build Pipeline) milestone"))
    (snapshot "2026-01-12T17:45:00Z"
      (accomplishments
        "Created professional FormBD Studio icons (indigo theme with form fields + lambda symbol)"
        "Generated all icon sizes: 32x32, 128x128, 256x256, ICO, ICNS"
        "Successfully built production release (cargo tauri build)"
        "Generated .deb package (2.9MB)"
        "Generated .rpm package (2.9MB)"
        "Updated CSS with new indigo color scheme matching icon branding"
        "Enhanced button styles with gradients and hover animations"
        "Added focus ring effects for better accessibility"
        "Enhanced validation status animations (pulse effect for validating)"
        "Improved nav tab active states with subtle glow"))
    (snapshot "2026-01-12T19:00:00Z"
      (accomplishments
        "Added service status system (check_service_status, get_app_info commands)"
        "Created StatusBar.res component showing backend availability"
        "Added ServiceStatus and FeatureAvailability types to Types.res"
        "Clear user feedback about FormBD M11 and FBQLdt M5 dependencies"
        "Feature availability badges (Schema [OK], Query [ ])"
        "Color-coded status bar (green=ready, yellow=partial, indigo=loading)"
        "Pushed all changes to GitHub (commits: e64a91d, f7d86fd)"
        "Completed comprehensive error handling milestone"))
    (snapshot "2026-01-12T21:30:00Z"
      (accomplishments
        "Complete rebrand: FormDB -> FormBD, FQL/FQLdt -> FBQL/FBQLdt"
        "Updated 24 files across codebase (configs, source, docs, SCM)"
        "Renamed FqldtPreview.res -> FbqldtPreview.res"
        "Verified ReScript and Rust builds pass"
        "Committed and pushed rebrand (commit: 3526112)"
        "GitHub repo renamed: formdb-studio -> formbd-studio"))
    (snapshot "2026-01-12T22:00:00Z"
      (accomplishments
        "Fixed all ReScript deprecation warnings (24 total)"
        "Updated Js.Dict.* -> Dict.*, Js.Global.setTimeout -> setTimeout"
        "Updated Float.toFixedWithPrecision -> Float.toFixed"
        "Updated Exn.Error/message -> JsExn/JsExn.message"
        "Build now compiles warning-free"
        "Verified app launch with cargo tauri dev"
        "Pushed all fixes to GitHub (commits: f389b90)"))
    (snapshot "2026-01-12T23:00:00Z"
      (accomplishments
        "Added keyboard navigation (Ctrl/Cmd+1-5 for tab switching)"
        "Added localStorage persistence for collections"
        "Collections auto-save on change and auto-load on startup"
        "Pushed to GitHub (commit: ac0bcdd)"))))

// SPDX-License-Identifier: AGPL-3.0-or-later
// FormDB Studio - Main Application

// Re-export types for backward compatibility
module FieldType = Types.FieldType
module Field = Types.Field
module Collection = Types.Collection
module Tauri = Types.Tauri
module ServiceStatus = Types.ServiceStatus
module AppInfo = Types.AppInfo

// Validation result from Rust backend
type validationResult = {
  valid: bool,
  errors: array<string>,
  proofs_generated: array<string>,
}

// Check service status from Tauri backend
let checkServiceStatus = async (): option<ServiceStatus.t> => {
  try {
    let result = await Tauri.invoke("check_service_status", ())
    Some(result)
  } catch {
  | _ => None
  }
}

// Generate FQLdt from collection definition
let generateFqldt = async (collection: Collection.t): result<string, string> => {
  try {
    let payload = {
      "name": collection.name,
      "fields": collection.fields->Array.map(f => {
        let (min, max, required) = switch f.fieldType {
        | FieldType.Number({min, max}) => (min, max, false)
        | FieldType.Text({required}) => (None, None, required)
        | _ => (None, None, false)
        }
        {
          "name": f.name,
          "field_type": f.fieldType->FieldType.toString,
          "min": min,
          "max": max,
          "required": required,
        }
      }),
    }
    let result = await Tauri.invoke("generate_fqldt", {"collection": payload})
    Ok(result)
  } catch {
  | Exn.Error(e) => Error(Exn.message(e)->Option.getOr("Unknown error"))
  }
}

// Validate FQLdt code
let validateFqldt = async (code: string): result<validationResult, string> => {
  try {
    let result = await Tauri.invoke("validate_fqldt", {"code": code})
    Ok(result)
  } catch {
  | Exn.Error(e) => Error(Exn.message(e)->Option.getOr("Unknown error"))
  }
}

// Navigation tabs
type tab =
  | Schema
  | Query
  | DataEntry
  | ProofAssistant
  | Normalization

let tabToString = (tab: tab): string =>
  switch tab {
  | Schema => "Schema"
  | Query => "Query"
  | DataEntry => "Data"
  | ProofAssistant => "Proofs"
  | Normalization => "Normalize"
  }

let tabToDescription = (tab: tab): string =>
  switch tab {
  | Schema => "Create Collections"
  | Query => "Query Builder"
  | DataEntry => "Enter Data"
  | ProofAssistant => "Proof Assistant"
  | Normalization => "Schema Normalization"
  }

// Navigation component
module Navigation = {
  @react.component
  let make = (~activeTab: tab, ~onTabChange: tab => unit) => {
    let tabs = [Schema, Query, DataEntry, ProofAssistant, Normalization]

    <nav className="main-nav">
      {tabs
      ->Array.map(t => {
        let isActive = t == activeTab
        <button
          key={tabToString(t)}
          className={`nav-tab ${isActive ? "active" : ""}`}
          onClick={_ => onTabChange(t)}>
          <span className="nav-tab-label"> {React.string(tabToString(t))} </span>
          <span className="nav-tab-desc"> {React.string(tabToDescription(t))} </span>
        </button>
      })
      ->React.array}
    </nav>
  }
}

// Main App component
@react.component
let make = () => {
  let (activeTab, setActiveTab) = React.useState(() => Schema)
  let (collections, setCollections) = React.useState(() => [])
  let (currentCollection, setCurrentCollection) = React.useState(() => Collection.empty())
  let (validationState, setValidationState) = React.useState(() => Types.NotValidated)
  let (serviceStatus, setServiceStatus) = React.useState(() => None)

  // Check service status on mount
  React.useEffect0(() => {
    let _ = checkServiceStatus()->Promise.then(status => {
      setServiceStatus(_ => status)
      Promise.resolve()
    })->ignore
    None
  })

  // Schema builder handlers
  let handleUpdateName = name => {
    setCurrentCollection(prev => {...prev, name})
  }

  let handleAddField = (field: Field.t) => {
    setCurrentCollection(prev => {
      ...prev,
      fields: prev.fields->Array.concat([field]),
    })
  }

  let handleRemoveField = index => {
    setCurrentCollection(prev => {
      ...prev,
      fields: prev.fields->Array.filterWithIndex((_, i) => i != index),
    })
  }

  let handleCreateCollection = () => {
    if currentCollection.name != "" && Array.length(currentCollection.fields) > 0 {
      setCollections(prev => prev->Array.concat([currentCollection]))
      setCurrentCollection(_ => Collection.empty())
      setValidationState(_ => Types.NotValidated)
    }
  }

  // Auto-validate when collection changes
  React.useEffect1(() => {
    if currentCollection.name != "" && Array.length(currentCollection.fields) > 0 {
      setValidationState(_ => Types.Validating)

      let _ = generateFqldt(currentCollection)->Promise.then(result => {
        switch result {
        | Ok(code) =>
          validateFqldt(code)->Promise.then(validResult => {
            switch validResult {
            | Ok(r) =>
              if r.valid {
                setValidationState(_ => Types.Valid(r.proofs_generated))
              } else {
                setValidationState(_ => Types.Invalid(r.errors))
              }
            | Error(e) => setValidationState(_ => Types.Invalid([e]))
            }
            Promise.resolve()
          })
        | Error(e) =>
          setValidationState(_ => Types.Invalid([e]))
          Promise.resolve()
        }
      })->ignore
    } else {
      setValidationState(_ => Types.NotValidated)
    }
    None
  }, [currentCollection])

  <div className="formdb-studio">
    <header>
      <div className="header-content">
        <h1> {React.string("FormDB Studio")} </h1>
        <p> {React.string("Zero-friction interface for dependently-typed databases")} </p>
      </div>
      {if Array.length(collections) > 0 {
        <div className="collections-badge">
          <span className="badge">
            {React.string(`${Int.toString(Array.length(collections))} collections`)}
          </span>
        </div>
      } else {
        React.null
      }}
    </header>

    <Navigation activeTab onTabChange={tab => setActiveTab(_ => tab)} />

    <main>
      {switch activeTab {
      | Schema =>
        <div className="schema-view">
          <SchemaBuilder
            collection={currentCollection}
            onUpdateName={handleUpdateName}
            onAddField={handleAddField}
            onRemoveField={handleRemoveField}
          />
          <FqldtPreview
            collection={currentCollection}
            validationState
            onCreateCollection={handleCreateCollection}
          />
        </div>
      | Query => <QueryBuilder collections />
      | DataEntry => <DataEntryPanel collections />
      | ProofAssistant => <ProofAssistant />
      | Normalization => <NormalizationPanel collections />
      }}
    </main>

    <StatusBar status={serviceStatus} />
  </div>
}

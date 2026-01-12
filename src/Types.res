// SPDX-License-Identifier: AGPL-3.0-or-later
// FormDB Studio - Shared Types

module FieldType = {
  type t =
    | Number({min: option<int>, max: option<int>})
    | Text({required: bool})
    | Confidence
    | PromptScores

  let toString = (t: t) =>
    switch t {
    | Number(_) => "number"
    | Text(_) => "text"
    | Confidence => "confidence"
    | PromptScores => "prompt_scores"
    }
}

module Field = {
  type t = {
    name: string,
    fieldType: FieldType.t,
  }
}

module Collection = {
  type t = {
    name: string,
    fields: array<Field.t>,
  }

  let empty = () => {
    name: "",
    fields: [],
  }
}

// Tauri command bindings
module Tauri = {
  type invokeResult<'a>

  @module("@tauri-apps/api/core")
  external invoke: (string, 'a) => promise<'b> = "invoke"
}

// Validation state for FQLdt preview
type validationState =
  | NotValidated
  | Validating
  | Valid(array<string>)
  | Invalid(array<string>)

// Service status types
module ServiceStatus = {
  type serviceInfo = {
    name: string,
    available: bool,
    version: option<string>,
    message: string,
    blocking_milestone: option<string>,
  }

  type featureAvailability = {
    schema_builder: bool,
    fqldt_generation: bool,
    fqldt_validation: bool,
    query_execution: bool,
    data_entry: bool,
    normalization: bool,
    proof_assistant: bool,
  }

  type t = {
    formdb: serviceInfo,
    fqldt: serviceInfo,
    overall_ready: bool,
    features: featureAvailability,
  }
}

// App info type
module AppInfo = {
  type t = {
    name: string,
    version: string,
    description: string,
    license: string,
    repository: string,
  }
}

// SPDX-License-Identifier: AGPL-3.0-or-later
//! FormDB Studio - Zero-friction interface for FormDB with FQLdt
//!
//! This is the Tauri backend that bridges the ReScript UI to FormDB.

#![cfg_attr(not(debug_assertions), windows_subsystem = "windows")]

use serde::{Deserialize, Serialize};

// ============================================================================
// Schema Types
// ============================================================================

/// Schema field definition from the UI
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FieldDef {
    pub name: String,
    pub field_type: String,
    pub min: Option<i64>,
    pub max: Option<i64>,
    pub required: bool,
}

/// Collection definition from the UI
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CollectionDef {
    pub name: String,
    pub fields: Vec<FieldDef>,
}

/// Validation result
#[derive(Debug, Serialize, Deserialize)]
pub struct ValidationResult {
    pub valid: bool,
    pub errors: Vec<String>,
    pub proofs_generated: Vec<String>,
}

// ============================================================================
// Query Types
// ============================================================================

/// Query filter
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct QueryFilter {
    pub field: String,
    pub operator: String,
    pub value: String,
}

/// Query definition
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct QueryDef {
    pub collection: String,
    pub filters: Vec<QueryFilter>,
    pub limit: Option<i64>,
    pub include_provenance: bool,
}

/// Query result row
#[derive(Debug, Serialize, Deserialize)]
pub struct QueryRow {
    pub data: std::collections::HashMap<String, String>,
}

/// Query execution result
#[derive(Debug, Serialize, Deserialize)]
pub struct QueryResult {
    pub rows: Vec<QueryRow>,
    pub total: i64,
    pub execution_time_ms: i64,
}

// ============================================================================
// Data Entry Types
// ============================================================================

/// Document with provenance
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DocumentWithProvenance {
    pub collection: String,
    pub data: std::collections::HashMap<String, String>,
    pub provenance: ProvenanceInfo,
}

/// Provenance metadata
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ProvenanceInfo {
    pub source: String,
    pub rationale: String,
    pub confidence: i32,
}

/// Insert result
#[derive(Debug, Serialize, Deserialize)]
pub struct InsertResult {
    pub success: bool,
    pub document_id: Option<String>,
    pub message: String,
    pub proofs: Vec<String>,
}

// ============================================================================
// Normalization Types
// ============================================================================

/// Functional dependency
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FunctionalDependency {
    pub determinant: Vec<String>,
    pub dependent: Vec<String>,
    pub confidence: f64,
    pub discovered: bool,
}

/// Normal form level
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum NormalForm {
    First,
    Second,
    Third,
    BCNF,
    Fourth,
    Fifth,
}

/// Normalization proposal
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct NormalizationProposal {
    pub id: String,
    pub current_nf: String,
    pub target_nf: String,
    pub violating_fds: Vec<FunctionalDependency>,
    pub proposed_tables: Vec<TableChange>,
    pub narrative: String,
    pub is_lossless: bool,
    pub preserves_fds: bool,
}

/// Proposed table change
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TableChange {
    pub name: String,
    pub fields: Vec<String>,
    pub reason: String,
}

/// FD discovery result
#[derive(Debug, Serialize, Deserialize)]
pub struct DiscoveryResult {
    pub fds: Vec<FunctionalDependency>,
    pub current_nf: String,
    pub proposals: Vec<NormalizationProposal>,
}

// ============================================================================
// Proof Types
// ============================================================================

/// Proof obligation
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ProofObligation {
    pub id: String,
    pub description: String,
    pub formal_statement: String,
    pub status: String,
    pub suggested_tactic: Option<String>,
    pub explanation: String,
}

/// Constraint violation
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ConstraintViolation {
    pub field: String,
    pub constraint: String,
    pub value: String,
    pub severity: String,
    pub explanation: String,
    pub suggested_fixes: Vec<SuggestedFix>,
}

/// Suggested fix for a violation
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SuggestedFix {
    pub description: String,
    pub code: String,
    pub confidence: i32,
}

// ============================================================================
// Tauri Commands - Schema
// ============================================================================

/// Generate FQLdt code from a visual collection definition
#[tauri::command]
fn generate_fqldt(collection: CollectionDef) -> Result<String, String> {
    let mut fql = format!(
        "CREATE COLLECTION {} (\n  id : UUID",
        collection.name
    );

    for field in &collection.fields {
        let type_str = match field.field_type.as_str() {
            "number" => {
                if let (Some(min), Some(max)) = (field.min, field.max) {
                    format!("BoundedNat {} {}", min, max)
                } else {
                    "Int".to_string()
                }
            }
            "text" => {
                if field.required {
                    "NonEmptyString".to_string()
                } else {
                    "Option String".to_string()
                }
            }
            "confidence" => "Confidence".to_string(),
            "prompt_scores" => "PromptScores".to_string(),
            _ => "String".to_string(),
        };

        fql.push_str(&format!(",\n  {} : {}", field.name, type_str));
    }

    fql.push_str("\n) WITH DEPENDENT_TYPES, PROVENANCE_TRACKING;");

    Ok(fql)
}

/// Validate FQLdt code using Lean 4 type checker
#[tauri::command]
fn validate_fqldt(code: String) -> Result<ValidationResult, String> {
    // TODO: Call Lean 4 via subprocess or FFI
    // For now, return a placeholder
    let proofs = if code.contains("BoundedNat") {
        vec!["bounds_valid".to_string()]
    } else {
        vec![]
    };

    Ok(ValidationResult {
        valid: true,
        errors: vec![],
        proofs_generated: proofs,
    })
}

// ============================================================================
// Tauri Commands - Query
// ============================================================================

/// Execute a query
#[tauri::command]
fn execute_query(query: QueryDef) -> Result<QueryResult, String> {
    // TODO: Connect to FormDB and execute query
    // For now, return placeholder
    Ok(QueryResult {
        rows: vec![],
        total: 0,
        execution_time_ms: 5,
    })
}

/// Explain a query plan
#[tauri::command]
fn explain_query(query: QueryDef) -> Result<String, String> {
    // TODO: Generate query explanation
    Ok(format!(
        "EXPLAIN for {} with {} filters",
        query.collection,
        query.filters.len()
    ))
}

// ============================================================================
// Tauri Commands - Data Entry
// ============================================================================

/// Insert a document with provenance
#[tauri::command]
fn insert_document(doc: DocumentWithProvenance) -> Result<InsertResult, String> {
    // TODO: Connect to FormDB and insert
    // For now, return placeholder
    let doc_id = format!("doc_{}", uuid::Uuid::new_v4());

    Ok(InsertResult {
        success: true,
        document_id: Some(doc_id),
        message: "Document inserted with provenance tracking".to_string(),
        proofs: vec!["constraints_satisfied".to_string()],
    })
}

/// Validate a document against schema constraints
#[tauri::command]
fn validate_document(
    collection: String,
    data: std::collections::HashMap<String, String>,
) -> Result<Vec<ConstraintViolation>, String> {
    // TODO: Validate against actual schema
    // For now, return empty (no violations)
    Ok(vec![])
}

// ============================================================================
// Tauri Commands - Normalization
// ============================================================================

/// Discover functional dependencies from data
#[tauri::command]
fn discover_fds(
    collection: String,
    confidence_threshold: f64,
) -> Result<DiscoveryResult, String> {
    // TODO: Connect to Form.Normalizer and discover FDs
    // For now, return placeholder with example FDs
    let fds = vec![
        FunctionalDependency {
            determinant: vec!["id".to_string()],
            dependent: vec!["name".to_string(), "email".to_string()],
            confidence: 1.0,
            discovered: true,
        },
    ];

    Ok(DiscoveryResult {
        fds,
        current_nf: "2NF".to_string(),
        proposals: vec![],
    })
}

/// Apply a normalization proposal
#[tauri::command]
fn apply_normalization(proposal_id: String) -> Result<bool, String> {
    // TODO: Apply normalization with rollback support
    Ok(true)
}

// ============================================================================
// Tauri Commands - Proofs
// ============================================================================

/// Get proof obligations for a schema
#[tauri::command]
fn get_proof_obligations(collection: String) -> Result<Vec<ProofObligation>, String> {
    // TODO: Get actual proof obligations from Lean 4
    Ok(vec![])
}

/// Apply a proof tactic
#[tauri::command]
fn apply_tactic(obligation_id: String, tactic: String) -> Result<bool, String> {
    // TODO: Apply tactic via Lean 4
    Ok(true)
}

// ============================================================================
// Main
// ============================================================================

fn main() {
    tauri::Builder::default()
        .invoke_handler(tauri::generate_handler![
            // Schema
            generate_fqldt,
            validate_fqldt,
            // Query
            execute_query,
            explain_query,
            // Data entry
            insert_document,
            validate_document,
            // Normalization
            discover_fds,
            apply_normalization,
            // Proofs
            get_proof_obligations,
            apply_tactic,
        ])
        .run(tauri::generate_context!())
        .expect("error while running FormDB Studio");
}

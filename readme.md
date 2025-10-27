# SAP S/4HANA Clean Core Extension Proof-of-Concept (BTP RAP)

## 1. Problem Statement

Modern SAP S/4HANA implementations mandate a **"Keep the Core Clean"** strategy. This means custom extensions must be built **side-by-side** on the **SAP Business Technology Platform (BTP)**, rather than modifying the core S/4HANA system directly.

This project demonstrates a common requirement: extending a standard S/4HANA business object (Business Partner) with custom fields (e.g., ESG Score, Contract-Specific ID) using a modern, cloud-native approach on BTP.

---

## 2. Solution Overview & Architecture

This proof-of-concept utilizes the **SAP BTP, ABAP Environment** (also known as *Steampunk*) and the **ABAP RESTful Application Programming Model (RAP)** to build a side-by-side extension.

### Architecture Highlights:
- Consumes the **S/4HANA Cloud Business Partner (A2X) API** (`API_BUSINESS_PARTNER`) via the **SAP API Business Hub sandbox**.
- Stores custom fields in a **dedicated database table** within the BTP ABAP Environment.
- Builds a **RAP Business Object** to manage this custom data.
- Exposes the custom data via an **OData V4 service** for consumption by UI or external systems.

---

## 3. Key Components Implemented

This repository contains the ABAP Development Tools (ADT) source code for the core backend components:

| Component | Object Name | Description |
|------------|--------------|-------------|
| **Database Table** | `ZESG_SCORES` | Stores the custom ESG score linked to the Business Partner UUID. |
| **Interface View (CDS)** | `ZI_ESG_SCORES` | Defines the core data structure based on the table. |
| **Service Definition** | `ZUI_ESG_SCORES` | Defines the OData service exposure for the Interface View. |
| **Service Binding** | `ZUI_ESG_SCORES` | Publishes the OData V4 service endpoint. |
| **Connection Test Class** | `ZCL_TEST_S4_CONNECTION` | Demonstrates connectivity and authentication to the external S/4HANA API sandbox. |
| **Backend Logic Simulation Class** | `ZCL_ESG_SCORE_MANAGER` | Contains ABAP SQL logic (Create, Update, Delete methods) for managing data in `ZESG_SCORES`. |

---

## 4. BTP Trial Environment Limitations & Workarounds

This project was developed within the constraints of the **SAP BTP ABAP Environment Trial**. Several restrictions prevented activation of the standard RAP Behavior Definition for `ZI_ESG_SCORES` — a key element for enabling transactional behavior and Fiori Elements previews.

### Key Limitations Encountered:
- RAP annotations (`@ObjectModel.modelCategory`, `@ObjectModel.writeActivePersistence`, etc.) required for marking a CDS View as a root Business Object were **not released** in the trial environment.
- Attempts to create the Behavior Definition failed since the Interface View was not recognized as a valid root entity.
- Attempts to create a transactional Projection View also failed due to the same dependency.

### Workaround Implemented:
- Core business logic (INSERT, UPDATE, DELETE) normally handled by a RAP Behavior Implementation class was instead simulated within the class **`ZCL_ESG_SCORE_MANAGER`**.
- The OData service **`ZUI_ESG_SCORES`** was published directly on the Interface View **`ZI_ESG_SCORES`**, providing **read-only** access.

---

## 5. Project Status

| Area | Status | Notes |
|------|---------|-------|
| **Backend** | ✅ Complete | Data model, API connectivity, and simulated business logic implemented. |
| **Frontend (UI)** | ⚠️ Limited | Fiori Elements UI could not be generated due to RAP restrictions in the BTP Trial. |

---

## 6. Summary

This project successfully demonstrates the **Clean Core Extension Architecture** on SAP BTP using RAP principles, within the constraints of a BTP Trial tenant. While certain RAP features (Behavior Definitions, UI generation) are unavailable in the trial, the project validates:

- Secure external S/4HANA API integration.  
- Clean side-by-side extension with isolated persistence.  
- RAP-based OData exposure and service binding.  

> ⚙️ For full RAP-managed transactional behavior and Fiori Elements UI generation, deploy this project on a licensed **SAP BTP ABAP Environment (Enterprise)** system.

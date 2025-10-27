SAP S/4HANA Clean Core Extension Proof-of-Concept (BTP RAP)

1. Problem Statement

Modern SAP S/4HANA implementations mandate a "Keep the Core Clean" strategy. This means custom extensions must be built "side-by-side" on the SAP Business Technology Platform (BTP), rather than modifying the core S/4HANA system directly.

This project demonstrates a common requirement: extending a standard S/4HANA business object (Business Partner) with custom fields (e.g., ESG Score, Contract-Specific ID) using a modern, cloud-native approach on BTP.

2. Solution Overview & Architecture

This proof-of-concept utilizes the SAP BTP, ABAP Environment (also known as "Steampunk") and the ABAP RESTful Application Programming Model (RAP) to build a side-by-side extension.

The architecture involves:

Consuming the standard S/4HANA Cloud Business Partner (A2X) API (API_BUSINESS_PARTNER) via the SAP API Business Hub sandbox.

Storing custom fields in a dedicated table within the BTP ABAP Environment.

Building a RAP Business Object to manage this custom data.

Exposing the custom data via an OData V4 service.

3. Key Components Implemented

This repository contains the ABAP Development Tools (ADT) source code for the core backend components:

Database Table: ZESG_SCORES - Stores the custom ESG score linked to the Business Partner UUID.

Interface View (CDS): ZI_ESG_SCORES - Defines the core data structure based on the table.

Service Definition: ZUI_ESG_SCORES - Defines the OData service exposure for the Interface View.

Service Binding: ZUI_ESG_SCORES - Publishes the OData V4 service endpoint.

Connection Test Class: ZCL_TEST_S4_CONNECTION - Demonstrates successful connectivity and authentication to the external S/4HANA API sandbox using the IF_WEB_HTTP_CLIENT interface.

Backend Logic Simulation Class: ZCL_ESG_SCORE_MANAGER - Contains the core ABAP SQL logic (Create, Update, Delete methods) required to manage data in the ZESG_SCORES table. This class serves as a demonstration of the intended business logic implementation.

4. BTP Trial Environment Limitations & Workarounds

This project was developed within the constraints of the SAP BTP ABAP Environment Trial. Significant restrictions were encountered, preventing the activation of the standard RAP Behavior Definition for ZI_ESG_SCORES. This core component is required for the RAP framework to manage transactional operations (Create, Update, Delete) and enable features like Fiori Elements UI previews.

Key Limitations Encountered:

Standard RAP annotations (@ObjectModel.modelCategory, @ObjectModel.writeActivePersistence, etc.) required to designate a CDS View as a Business Object root were not released/permitted in the trial environment.

Attempts to create the Behavior Definition failed due to the inability to recognize the Interface View as a valid root entity.

Attempts to create a transactional Projection View also failed due to the underlying root entity issue.

Workaround Implemented:

The core backend logic (INSERT, UPDATE, DELETE) that would normally reside in the RAP Behavior Implementation class was instead implemented and demonstrated in the standalone class ZCL_ESG_SCORE_MANAGER.

The OData service (ZUI_ESG_SCORES) was successfully published based directly on the Interface View (ZI_ESG_SCORES), enabling read-only access but lacking the transactional capabilities provided by an active Behavior Definition.

5. Project Status

Backend: Core data model, external API connectivity, and essential business logic (simulated) are complete.

Frontend (UI): A standard Fiori Elements UI preview could not be generated or activated due to the BTP Trial limitations preventing the activation of the required RAP Behavior Definition. Full UI functionality would require deployment to a licensed BTP ABAP Environment where the necessary framework components are released.

This project successfully demonstrates the fundamental architecture and backend development required for a Clean Core S/4HANA extension using the BTP ABAP Environment and RAP principles, adapted to navigate specific trial system constraints.

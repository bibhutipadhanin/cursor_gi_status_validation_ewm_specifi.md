Below is the complete and formal Functional Specification Document for the three stated requirements.
It is structured in a standard SAP FS format with clear detailing of inputs, outputs, validations, and end-to-end logic.



Functional Specification: GI Status Validation in EWM

1. Document Control

Version

Date

Author

Description

1.0

11-Dec-2025

ChatGPT

Initial FS



2. Business Requirement Overview

Goods Issue (GI) posting in EWM is not always synchronized with ECC processes that depend on GI completion. Various downstream processes—Invoice Generation, Posting Check, and Reporting Number-based processing—should not proceed until GI is fully completed.

To ensure GI synchronization between ECC and EWM, a new RFC-enabled function module will be introduced in EWM to check GI status for one or more deliveries. Additional logic will be embedded in relevant ECC function modules to validate GI completion before proceeding.



3. Requirements Summary

Requirement 1

Create a new RFC-enabled FM ZSCM_GI_STATUS_CHECK in EWM to validate GI status for multiple deliveries.

Requirement 2

Embed GI status validation using RFC call in FM Z_FIORI_SWM_OB_INV_GENRATE before calling FM Z_SCM_RPU_GET_AND_POST.

Requirement 3

Embed GI status validation using RFC call in FM ZSCM_POST_CHK_BILLING before calling FM Z_SCM_RPU_POSTING_CHK.



4. Detailed Functional Design



**4.1 Requirement 1

Create RFC-Enabled Function Module: ZSCM_GI_STATUS_CHECK**

4.1.1 Objective

To check GI status of one or more deliveries in EWM and return only those entries where GI is not yet completed.

4.1.2 FM Name

## ZSCM_GI_STATUS_CHECK
(Type: RFC-enabled, Remote Callable)

4.1.3 Input Parameters

Parameter

Type

Description

## IT_DELIVERY

Table Type (List of VBELN)

Multiple delivery numbers for which GI status needs to be checked

4.1.4 Output Parameters

Field

Description

## VBELN

Delivery Number

## STATUS_GI

Converted GI Status (Values: “Not Started”, “In Process”, “Partially Completed”, “Completed”)

## RAW_STATUS

Status_GI value from /SCDL/DB_PROCI_O

4.1.5 Processing Logic

Step 1: Fetch DOCID & ITEMID for each delivery

From table: /SCDL/DB_REFDOC

## Selection criteria:

## REFDOCCAT = 'ERP'

REFDOCNO = Delivery number

Store DOCID and ITEMID.

Step 2: Fetch GI Status from /SCDL/DB_PROCI_O

## Selection criteria:

DOCID = DOCID from Step 1

ITEMID = ITEMID from Step 1

## Fetch:

## STATUS_GI

Step 3: Filter Records

## Remove entries where:

STATUS_GI = 0 (No Status)

STATUS_GI = 9 (Completed/Finalized)

## Retain records with STATUS_GI in:

1 → Not Started

2 → In Process

3 → Partially Completed

4–8 → Intermediate statuses (if applicable)

Step 4: Map Status

## STATUS_GI

Meaning to Return

1 or 2

“Not Started”

3

“In Process”

Others (but not 0 or 9)

“In Process”

Step 5: Prepare Output

## For each qualifying delivery, return:

Delivery Number

Status description

Raw Status_GI

If no entry remains after filtering → return blank output.



**4.2 Requirement 2

GI Status Validation in FM: Z_FIORI_SWM_OB_INV_GENRATE**

4.2.1 Change Point

Before line no. 300 (before calling Z_SCM_RPU_GET_AND_POST)

4.2.2 Logic

Step 1: Read activation flag

From table: ZLOG_EXEC_VAR

## Selection:

PARAM = 'ZSCM_GI_STATUS_Check'

## ACTIVE = 'X'

If no such record → skip validation and continue existing logic.

Step 2: Get Delivery from YTTSTX0002

## Selection criteria:

AREA = input AREA of current process

REPORT_NO = Reporting Number passed during process

If no record found → proceed as usual.

Step 3: Get RFC Destination

From table: ZWMSPARA

## Selection:

## PARAM1 = 'ZEWM_DEST'

## ACTIVE_FLAG = 'X'

Store VALUE1 as RFCDEST.

If RFCDEST not found → proceed without stopping (fallback to default).

Step 4: Call RFC

## Call ZSCM_GI_STATUS_CHECK using:

Delivery (single or multiple) from YTTSTX0002

RFC Destination = VALUE1

Step 5: Validate Output

If RFC returns any rows, it means GI is not completed.

## Then:

Raise error message
“GI for the reporting number (&Report_no) is not yet completed in EWM”

Stop further processing

Do NOT call Z_SCM_RPU_GET_AND_POST

If output is empty → GI is completed → proceed normally.



**4.3 Requirement 3

GI Status Validation in FM: ZSCM_POST_CHK_BILLING**

4.3.1 Change Point

At line no. 32 (before calling Z_SCM_RPU_POSTING_CHK)

4.3.2 Logic

Same logic as Requirement 2, repeated here for completeness.

Step 1: Check activation flag

## From ZLOG_EXEC_VAR:

PARAM = "ZSCM_GI_Status_Check"

## ACTIVE = 'X'

If not maintained → continue existing logic.

Step 2: Get Delivery using Reporting Number

Table: YTTSTX0002
## Criteria:

## AREA

## REPORTING NUMBER

If not found → continue existing logic.

Step 3: Get RFC Destination

## From ZWMSPARA:

## PARAM1 = 'ZEWM_DEST'

## ACTIVE_FLAG = 'X'

If VALUE1 not found → continue without stopping.

Step 4: RFC Call to ZSCM_GI_STATUS_CHECK

## Input:

Delivery number

## RFCDEST = VALUE1

Step 5: Evaluate Response

## If RFC output contains any record:

## Display message:
“GI for the reporting number (&Report_no) is not yet completed in EWM”

Abort further processing

Do NOT call Z_SCM_RPU_POSTING_CHK

If empty output → proceed with the standard flow.



5. Error Handling

Scenario

Action

RFC destination unavailable

Proceed with default logic (failsafe mode)

ZSCM_GI_STATUS_CHECK not reachable

Error message “Unable to reach EWM System for GI validation”

Delivery not found in YTTSTX0002

Skip GI validation

ZLOG_EXEC_VAR param not maintained

Skip GI validation



6. Technical Objects Summary

Object

Description

## ZSCM_GI_STATUS_CHECK

New RFC-enabled FM in EWM

## Z_FIORI_SWM_OB_INV_GENRATE

GI check embedded before invoice posting logic

## ZSCM_POST_CHK_BILLING

GI check embedded before billing posting check

## ZLOG_EXEC_VAR

Control Table

## YTTSTX0002

Reporting number ↔ Delivery mapping

## ZWMSPARA

RFC Destination parameter storage



7. Authorization Impact

No new authorization objects required.
Existing role must allow RFC calls to EWM.
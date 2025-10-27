-- @AbapCatalog.sqlViewName: 'ZIESGSCORES' --<= 16 chars -- REMOVED (Not allowed in View Entity)
-- @AbapCatalog.preserveKey: true -- REMOVED (Obsolete/Not allowed in View Entity)
@AccessControl.authorizationCheck: #NOT_REQUIRED -- For simplicity in trial, adjust for real apps
@EndUserText.label: 'Interface View for ESG Scores'
-- @ObjectModel.modelCategory: #BUSINESS_OBJECT  -- REMOVED (Not released for View Entity)

-- --- Core RAP Annotations (Minimal Set) ---
@ObjectModel.representativeKey: 'BpUuid'
@ObjectModel.semanticKey: [ 'BpUuid' ]
-- @ObjectModel.writeActivePersistence: 'zesg_scores' -- Removed (Defined in Behavior Definition)
-- @ObjectModel.createEnabled: true -- Removed (Defined in Behavior Definition)
-- @ObjectModel.updateEnabled: true -- Removed (Defined in Behavior Definition)
-- @ObjectModel.deleteEnabled: true -- Removed (Defined in Behavior Definition)

define view entity ZI_ESG_SCORES
  as select from zesg_scores
{
      -- --- Key Fields ---
  key bp_uuid         as BpUuid,

      -- --- Data Fields ---
      @EndUserText.label: 'ESG Score'
      esg_score       as EsgScore,

      -- --- Admin Fields (Read Only) ---
      @UI.hidden: true -- Usually hidden in UIs, managed by framework
      created_by      as CreatedBy,
      @UI.hidden: true
      created_at      as CreatedAt,
      @UI.hidden: true
      last_changed_by as LastChangedBy,
      @UI.hidden: true
      last_changed_at as LastChangedAt
      -- local_last_changed_at field omitted as per table definition
}


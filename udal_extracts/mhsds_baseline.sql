-- =================================================================================
-- Set-up
-- =================================================================================

IF OBJECT_ID('TempDB..#AdmOrder') IS NOT NULL DROP TABLE #AdmOrder
IF OBJECT_ID('TempDB..#DDOrder') IS NOT NULL DROP TABLE #DDOrder
IF OBJECT_ID('TempDB..#DDReasons') IS NOT NULL DROP TABLE #DDReasons
IF OBJECT_ID('TempDB..#DD') IS NOT NULL DROP TABLE #DD
IF OBJECT_ID('TempDB..#CRDOrder') IS NOT NULL DROP TABLE #CRDOrder
IF OBJECT_ID('TempDB..#CRDReasons') IS NOT NULL DROP TABLE #CRDReasons
IF OBJECT_ID('TempDB..#CRD') IS NOT NULL DROP TABLE #CRD
IF OBJECT_ID('TempDB..#CRD_DD_DC') IS NOT NULL DROP TABLE #CRD_DD_DC
IF OBJECT_ID('TempDB..#PDIAGOrder') IS NOT NULL DROP TABLE #PDIAGOrder
IF OBJECT_ID('TempDB..#PDIAG_F_L') IS NOT NULL DROP TABLE #PDIAG_F_L
IF OBJECT_ID('TempDB..#MHActOrder') IS NOT NULL DROP TABLE #MHActOrder
IF OBJECT_ID('TempDB..#MHAct') IS NOT NULL DROP TABLE #MHAct
IF OBJECT_ID('TempDB..#MHActRP') IS NOT NULL DROP TABLE #MHActRP
IF OBJECT_ID('TempDB..#WStayOrder') IS NOT NULL DROP TABLE #WStayOrder
IF OBJECT_ID('TempDB..#WStay') IS NOT NULL DROP TABLE #WStay
IF OBJECT_ID('TempDB..#HLOrder') IS NOT NULL DROP TABLE #HLOrder
IF OBJECT_ID('TempDB..#HLWSDays') IS NOT NULL DROP TABLE #HLWSDays
IF OBJECT_ID('TempDB..#HLPSDays') IS NOT NULL DROP TABLE #HLPSDays
IF OBJECT_ID('TempDB..#WStayLOS') IS NOT NULL DROP TABLE #WStayLOS
IF OBJECT_ID('TempDB..#LDA_Flag_MHS007') IS NOT NULL DROP TABLE #LDA_Flag_MHS007
IF OBJECT_ID('TempDB..#LDA_Flag_MHS101') IS NOT NULL DROP TABLE #LDA_Flag_MHS101
IF OBJECT_ID('TempDB..#LDA_Flag_MHS103') IS NOT NULL DROP TABLE #LDA_Flag_MHS103
IF OBJECT_ID('TempDB..#LDA_Flag_MHS102') IS NOT NULL DROP TABLE #LDA_Flag_MHS102
IF OBJECT_ID('TempDB..#LDA_Flag_MHS601') IS NOT NULL DROP TABLE #LDA_Flag_MHS601
IF OBJECT_ID('TempDB..#LDA_Flag_MHS604') IS NOT NULL DROP TABLE #LDA_Flag_MHS604
IF OBJECT_ID('TempDB..#LDA_Flag_MHS605') IS NOT NULL DROP TABLE #LDA_Flag_MHS605
IF OBJECT_ID('TempDB..#LDA_Flag_Combined') IS NOT NULL DROP TABLE #LDA_Flag_Combined
IF OBJECT_ID('TempDB..#LDA_Flag_SU') IS NOT NULL DROP TABLE #LDA_Flag_SU
IF OBJECT_ID('TempDB..#AdmOutput') IS NOT NULL DROP TABLE #AdmOutput
IF OBJECT_ID('TempDB..#WStayOutput') IS NOT NULL DROP TABLE #WStayOutput

DECLARE @StartDate DATE;
DECLARE @EndDate DATE;
 
SET @StartDate = '2023-07-01'
SET @EndDate =  '2024-06-30'

-- =================================================================================
-- Extract Provider Spells AND order for latest submission version
-- =================================================================================

SELECT HSP.[UniqMonthID]
	  ,HSP.[NHSEUniqSubmissionID]
      ,SF.[ReportingPeriodStartDate]
      ,SF.[ReportingPeriodEndDate]
	  ,ROW_NUMBER() OVER(PARTITION BY HSP.[Der_Person_ID], HSP.[UniqServReqID], HSP.[UniqHospProvSPellID] ORDER BY HSP.[UniqMonthID] DESC) AS [Der_Spell_Order]
	  ,HSP.[MHS501UniqID]
	  ,HSP.[RowNumber]
	  ,HSP.[Der_Person_ID]
	  ,HSP.[RecordNumber]
	  ,HSP.[HospProvSpellID]
	  ,HSP.[OrgIDProv]
	  ,SP_PROS.[Provider_Name]
	  ,SP_PROS.[Region_Name] AS [Provider_Region_Name]
	  ,SP_PROS.[Region_Code] AS [Provider_Region_Code]
	  ,ICB.[ICB_Code] AS [Provider_ICB_Code]
	  ,ICB.[ICB_Name] AS [Provider_ICB_Name]
	  ,CASE
			WHEN HSP.[OrgIDProv] IS NULL THEN NULL
			WHEN LEFT(HSP.[OrgIDProv], 1) = 'R' THEN 'NHS'
			ELSE 'Independent'
	   END AS [Provider_Type]
	  ,HSP.[ServiceRequestId]
	  ,HSP.[StartDateHospProvSpell]
	  ,HSP.[SourceAdmMHHospProvSpell]
	  ,ASRC.[Main_Description] AS [AdmissionSourceDesc]
	  ,HSP.[MethAdmMHHospProvSpell]
	  ,AMET.[Main_Description] AS [AdmissionMethodDesc]
	  ,HSP.[DischDateHospProvSpell]
	  ,DATEDIFF(DAY, HSP.[StartDateHospProvSpell], COALESCE(HSP.[DischDateHospProvSpell], SF.[ReportingPeriodEndDate])) AS [Der_HospProvSpell_LOS]
	  ,CASE
			WHEN HSP.[StartDateHospProvSpell] <=  @EndDate AND COALESCE(HSP.[DischDateHospProvSpell], CASE WHEN SF.[ReportingPeriodEndDate] >=  @EndDate THEN  @EndDate ELSE SF.[ReportingPeriodEndDate] END) >= @StartDate THEN 
				DATEDIFF(DAY,
					CASE WHEN HSP.[StartDateHospProvSpell] > @StartDate THEN HSP.[StartDateHospProvSpell] ELSE @StartDate END,
					CASE WHEN HSP.[DischDateHospProvSpell] <  @EndDate THEN HSP.[DischDateHospProvSpell] ELSE  @EndDate END
				)
			ELSE 0
		END AS [Reporting_HosProvSpell_LOS]
	  ,HSP.[MethOfDischMHHospProvSpell]
	  ,DMET.[Main_Description] AS [DischargeMethodDesc]
	  ,HSP.[DestOfDischHospProvSpell]
	  ,DDEST.[Main_Description] AS [DischargeDestDesc]
	  ,HSP.[UniqHospProvSPellID]
	  ,HSP.[UniqServReqID]
	  ,HSP.[TransformingCareInd]
	  ,HSP.[TransformingCareCategory]
	  ,HSP.[Der_Age_at_StartDateHospProvSpell]
	  ,CASE WHEN HSP.[Der_Age_at_StartDateHospProvSpell] BETWEEN 0 AND 17 THEN '00-17'
			WHEN HSP.[Der_Age_at_StartDateHospProvSpell] BETWEEN 18 AND 24 THEN '18-24'
			WHEN HSP.[Der_Age_at_StartDateHospProvSpell] BETWEEN 25 AND 64 THEN '25-64'
			ELSE '65+' END AS [Age_Group_Admission]
	  ,MPI.[Gender]
	  ,MPI.[EthnicCategory]
	  ,ETH.[Category] AS [Ethnic_Category]
	  ,MPI.[LSOA2011]
	  ,MPI.[LSOA2021]
	  ,MPI.[Der_Postcode_LSOA_Code]
	  ,ICB_LSOA.[ICB_Code] AS [Residence_ICB_Code]
	  ,ICB_LSOA.[ICB_Name] AS [Residence_ICB_Name]
	  ,IMD.[IMD_Decile]
	  ,MPI.[LDAFlag]
	  ,PIND.[AutismStatus]
	  ,PIND.[LDStatus]

INTO #AdmOrder	 
FROM [Reporting_MESH_MHSDS].[MHS501HospProvSpell_Published] AS HSP

LEFT JOIN [Reporting_MESH_MHSDS].[MHS001MPI_Published] AS MPI
ON HSP.[Der_Person_ID] = MPI.[Der_Person_ID]
AND HSP.[RecordNumber] = MPI.[RecordNumber]

LEFT JOIN [UKHD_Data_Dictionary].[Ethnic_Category_Code_SCD] AS ETH
ON MPI.[EthnicCategory] = ETH.[Main_Code_Text]
AND ETH.[Is_Latest] = 1

LEFT JOIN [Reporting_MESH_MHSDS].[MHS005PatInd_Published] AS [PIND]
ON HSP.[Der_Person_ID] = PIND.[Der_Person_ID]
AND HSP.[RecordNumber] = PIND.[RecordNumber]

INNER JOIN [Reporting_MESH_MHSDS].[MHSDS_SubmissionFlags_Published] AS SF
ON HSP.[NHSEUniqSubmissionID] = SF.[NHSEUniqSubmissionID]
AND SF.[Der_IsLatest] = 'Y'

LEFT JOIN [Internal_Reference].[CurrentProvider] AS SP_PROS
ON HSP.[OrgIDProv] = SP_PROS.[Provider_Code]

LEFT JOIN  (SELECT [ICB_Code]
				  ,[ICB_Name]
			FROM [Internal_Reference].[CCGToICB_2425]
			GROUP BY [ICB_Code], [ICB_Name]) AS ICB
ON SP_PROS.[STP_Code] = ICB.[ICB_Code] 

LEFT JOIN [UKHF_Demography].[Domains_Of_Deprivation_By_LSOA1] AS IMD
ON MPI.[LSOA2011] = IMD.[LSOA_Code]
AND IMD.[Effective_Snapshot_Date] = '2019-12-31'

LEFT JOIN [Internal_Reference].[LSOAs_to_Higher_Geographies] AS LSOA
ON MPI.[LSOA2011] = LSOA.[LSOA11CD]

LEFT JOIN [Internal_Reference].[CCGToICB_2425] AS ICB_LSOA
ON LSOA.[CCG16CDH] = ICB_LSOA.[Org_Code]

LEFT JOIN [UKHD_Data_Dictionary].[Source_Of_Admission_SCD] AS ASRC
ON HSP.[SourceAdmMHHospProvSpell] = ASRC.[Main_Code_Text]
AND ASRC.[Is_Latest] = 1

LEFT JOIN [UKHD_Data_Dictionary].[Admission_Method_SCD] AS AMET
ON HSP.[MethAdmMHHospProvSpell] = AMET.[Main_Code_Text]
AND AMET.[Is_Latest] = 1

LEFT JOIN [UKHD_Data_Dictionary].[Discharge_Method_SCD] AS DMET
ON HSP.[MethOfDischMHHospProvSpell] = DMET.[Main_Code_Text]
AND DMET.[Is_Latest] = 1

LEFT JOIN [UKHD_Data_Dictionary].[Discharge_Destination_SCD] AS DDEST
ON HSP.[DestOfDischHospProvSpell] = DDEST.[Main_Code_Text]
AND DDEST.[Is_Latest] = 1

WHERE ICB_LSOA.[ICB_Code] IN ('QGH', 'QHL', 'QJ2', 'QJM',
							    'QK1', 'QNC', 'QOC', 'QPM',
								'QT1', 'QUA', 'QWU')
AND (HSP.[StartDateHospProvSpell] BETWEEN @StartDate AND @EndDate
	 OR
	 HSP.[DischDateHospProvSpell] BETWEEN @StartDate AND @EndDate)

-- =================================================================================
-- Extract Discharge Delays Information
-- =================================================================================

SELECT  DD.[UniqHospProvSpellID]
	   ,DD.[UniqMonthID]
	   ,SF.[ReportingPeriodEndDate]
	   ,DD.[DelayDischReason]
	   ,DDR.[Main_Description] AS [DelayReasonDesc]
	   ,DD.[AttribToIndic]
	   ,DDA.[Main_Description] AS [DelayReasonAttr]
	   ,DD.[StartDateDelayDisch]
	   ,DD.[EndDateDelayDisch]
	   ,ADM.[DischDateHospProvSpell]
	   ,ROW_NUMBER() OVER (PARTITION BY DD.[UniqHospProvSpellID], DD.[StartDateDelayDisch] ORDER BY DD.[UniqMonthID] DESC) AS [Der_DD_Order]
	   ,DATEDIFF(DAY, DD.[StartDateDelayDisch], COALESCE(DD.[EndDateDelayDisch], COALESCE(SF.[ReportingPeriodEndDate], ADM.[DischDateHospProvSpell]))) AS [Total_DD_Days]
	   ,CASE
			WHEN DD.[StartDateDelayDisch] <=  @EndDate AND DD.[EndDateDelayDisch] >= @StartDate THEN
				DATEDIFF(DAY,
					CASE WHEN DD.[StartDateDelayDisch] > @StartDate THEN DD.[StartDateDelayDisch] ELSE @StartDate END,
					CASE WHEN DD.[EndDateDelayDisch] <  @EndDate THEN DD.[EndDateDelayDisch] ELSE  @EndDate END
				)
			ELSE 0
		END AS [Reporting_DD_Days]
INTO #DDOrder
FROM [Reporting_MESH_MHSDS].[MHS504DelayedDischarge_Published] AS DD

LEFT JOIN #AdmOrder AS [ADM]
ON DD.[UniqHospProvSpellID] = ADM.[UniqHospProvSpellID]
AND ADM.[Der_Spell_Order] = 1

INNER JOIN [Reporting_MESH_MHSDS].[MHSDS_SubmissionFlags_Published] AS SF
ON DD.[NHSEUniqSubmissionID] = SF.[NHSEUniqSubmissionID]
AND SF.[Der_IsLatest] = 'Y'

LEFT JOIN [UKHD_Data_Dictionary].[Mental_Health_Delayed_Discharge_Reason_SCD] AS DDR
ON DD.[DelayDischReason] = DDR.[Main_Code_Text]
AND DDR.[Is_Latest] = 1

LEFT JOIN [UKHD_Data_Dictionary].[Mental_Health_Delayed_Discharge_Attributable_To_Indication_Code_SCD] AS DDA
ON DD.[AttribToIndic] = DDA.[Main_Code_Text]
AND DDA.[Is_Latest] = 1

WHERE DD.[UniqHospProvSpellID] IN (SELECT [UniqHospProvSpellID] FROM #AdmOrder)

-- =================================================================================
-- Group Delays by Delay Reason for each Provider Spell
-- =================================================================================

SELECT [UniqHospProvSpellID]
	  ,[DelayDischReason]
	  ,[DelayReasonDesc]
	  ,[AttribToIndic]
	  ,[DelayReasonAttr]
	  ,SUM([Total_DD_Days]) AS [Total_DD_Days]
	  ,SUM([Reporting_DD_Days]) AS [Reporting_DD_Days]
INTO #DDReasons
FROM #DDOrder
WHERE [Der_DD_Order] = 1

GROUP BY [UniqHospProvSpellID]
	  ,[DelayDischReason]
	  ,[DelayReasonDesc]
	  ,[AttribToIndic]
	  ,[DelayReasonAttr]

-- =================================================================================
-- Aggregate delays for each Provider Spell
-- =================================================================================

SELECT [UniqHospProvSpellID]
	  ,SUM([Total_DD_Days]) AS [Total_DD_Days]
	  ,SUM([Reporting_DD_Days]) AS [Reporting_DD_Days]
INTO #DD
FROM #DDOrder
WHERE [Der_DD_Order] = 1
GROUP BY [UniqHospProvSpellID]

-- =================================================================================
-- Extract Clinically Ready for Discharge Days
-- =================================================================================

SELECT CRD.[UniqHospProvSpellID]
	  ,CRD.[UniqMonthID]
	  ,SF.[ReportingPeriodEndDate]
	  ,CRD.[ClinReadyforDischDelayReason]
	  ,CASE
			WHEN CRD.[ClinReadyforDischDelayReason] = '01' THEN 'Awaiting care coordinator allocation'
			WHEN CRD.[ClinReadyforDischDelayReason] = '02' THEN 'Awaiting allocation of community psychiatrist'
			WHEN CRD.[ClinReadyforDischDelayReason] = '03' THEN 'Awaiting allocation of social worker'
			WHEN CRD.[ClinReadyforDischDelayReason] = '04' THEN 'Awaiting public funding or decision from funding panel'
			WHEN CRD.[ClinReadyforDischDelayReason] = '05' THEN 'Awaiting further community or mental health NHS Services not delivered in an acute setting including intermediate care, rehabilitation services, step down service'
			WHEN CRD.[ClinReadyforDischDelayReason] = '06' THEN 'Awaiting availability of placement in prison or Immigration Removal Centre'
			WHEN CRD.[ClinReadyforDischDelayReason] = '07' THEN 'Awaiting availability of placement in care home without nursing'
			WHEN CRD.[ClinReadyforDischDelayReason] = '08' THEN 'Awaiting availability of placement in care home with nursing'
			WHEN CRD.[ClinReadyforDischDelayReason] = '09' THEN 'Awaiting commencement of care package in usual or temporary place of residence'
			WHEN CRD.[ClinReadyforDischDelayReason] = '11' THEN 'Patient or Family choice'
			WHEN CRD.[ClinReadyforDischDelayReason] = '12' THEN 'Disputes relating to responsible commissioner for post-discharge care'
			WHEN CRD.[ClinReadyforDischDelayReason] = '13' THEN 'Disputes relating to post-discharge care pathway between clinical teams and/or care panels'
			WHEN CRD.[ClinReadyforDischDelayReason] = '14' THEN 'Housing - awaiting availability of private rented accommodation'
			WHEN CRD.[ClinReadyforDischDelayReason] = '15' THEN 'Housing - awaiting availability of social rented housing via council housing waiting list'
			WHEN CRD.[ClinReadyforDischDelayReason] = '16' THEN 'Housing - awaiting purchase/sourcing of own home'
			WHEN CRD.[ClinReadyforDischDelayReason] = '17' THEN 'Housing - Patient NOT eligible for funded care or support'
			WHEN CRD.[ClinReadyforDischDelayReason] = '18' THEN 'Housing - Awaiting supported accommodation'
			WHEN CRD.[ClinReadyforDischDelayReason] = '19' THEN 'Housing - Awaiting temporary accommodation from the Local Authority under housing legislation'
			WHEN CRD.[ClinReadyforDischDelayReason] = '20' THEN 'Awaiting availability of residential children''s home (non-secure)'
			WHEN CRD.[ClinReadyforDischDelayReason] = '21' THEN 'Awaiting availability of secure children''s home (welfare or non-welfare)'
			WHEN CRD.[ClinReadyforDischDelayReason] = '22' THEN 'Awaiting availability of placement in Youth Offender Institution'
			WHEN CRD.[ClinReadyforDischDelayReason] = '23' THEN 'Child or young person awaiting foster placement'
			WHEN CRD.[ClinReadyforDischDelayReason] = '24' THEN 'Awaiting Ministry of Justice agreement to proposed placement'
			WHEN CRD.[ClinReadyforDischDelayReason] = '25' THEN 'Awaiting outcome of legal proceedings under relevant Mental Health legislation'
			WHEN CRD.[ClinReadyforDischDelayReason] = '26' THEN 'Awaiting Court of Protection proceedings'
			WHEN CRD.[ClinReadyforDischDelayReason] = '27' THEN 'Awaiting Deprivation of Liberty Safeguards (DOLS) Application'
			WHEN CRD.[ClinReadyforDischDelayReason] = '28' THEN 'Delay due to consideration of specific court judgements'
			WHEN CRD.[ClinReadyforDischDelayReason] = '29' THEN 'Awaiting residential special school or college placement'
			WHEN CRD.[ClinReadyforDischDelayReason] = '30' THEN 'Lack of local education support'
			WHEN CRD.[ClinReadyforDischDelayReason] = '31' THEN 'Public safety concern unrelated to clinical treatment need (care team and/or ministry of justice)'
			WHEN CRD.[ClinReadyforDischDelayReason] = '32' THEN 'Highly bespoke housing and/or care arrangements not available in the community'
			WHEN CRD.[ClinReadyforDischDelayReason] = '33' THEN 'No lawful support available in the community excluding social care'
			WHEN CRD.[ClinReadyforDischDelayReason] = '34' THEN 'No social care support including social care funded placement'
			WHEN CRD.[ClinReadyforDischDelayReason] = '35' THEN 'Delays to NHS-led assessments in the community'
			WHEN CRD.[ClinReadyforDischDelayReason] = '36' THEN 'Hospital staff shortages'
			WHEN CRD.[ClinReadyforDischDelayReason] = '37' THEN 'Delays to non-NHS led assessments in the community'
			WHEN CRD.[ClinReadyforDischDelayReason] = '98' THEN 'Reason not known'
			ELSE NULL
	  END AS [CRDReason] -- Awaiting Lookup Table
	  ,CRD.[AttribToIndic]
	  ,DDA.[Main_Description] AS [DelayReasonAttr]
	  ,CRD.[StartDateClinReadyforDisch]
	  ,CRD.[EndDateClinReadyforDisch]
	  ,ADM.[DischDateHospProvSpell]
	  ,ROW_NUMBER() OVER (PARTITION BY CRD.[UniqHospProvSpellID], CRD.[StartDateClinReadyforDisch] ORDER BY CRD.[UniqMonthID] DESC) AS [Der_CRD_Order]
	   ,DATEDIFF(DAY, CRD.[StartDateClinReadyforDisch], COALESCE(CRD.[EndDateClinReadyforDisch], COALESCE(SF.[ReportingPeriodEndDate], ADM.[DischDateHospProvSpell]))) AS [Total_CRD_Days]
	   ,CASE
			WHEN CRD.[StartDateClinReadyforDisch] <=  @EndDate AND CRD.[EndDateClinReadyforDisch] >= @StartDate THEN
				DATEDIFF(DAY,
					CASE WHEN CRD.[StartDateClinReadyforDisch] > @StartDate THEN CRD.[StartDateClinReadyforDisch] ELSE @StartDate END,
					CASE WHEN CRD.[EndDateClinReadyforDisch] <  @EndDate THEN CRD.[EndDateClinReadyforDisch] ELSE  @EndDate END
				)
			ELSE 0
		END AS [Reporting_CRD_Days]
INTO #CRDOrder
FROM [Reporting_MESH_MHSDS].[MHS518ClinReadyforDischarge_Published] AS CRD

LEFT JOIN #AdmOrder AS [ADM]
ON CRD.[UniqHospProvSpellID] = ADM.[UniqHospProvSpellID]
AND ADM.[Der_Spell_Order] = 1

INNER JOIN [Reporting_MESH_MHSDS].[MHSDS_SubmissionFlags_Published] AS SF
ON CRD.[NHSEUniqSubmissionID] = SF.[NHSEUniqSubmissionID]
AND SF.[Der_IsLatest] = 'Y'

LEFT JOIN [UKHD_Data_Dictionary].[Mental_Health_Delayed_Discharge_Attributable_To_Indication_Code_SCD] AS DDA
ON CRD.[AttribToIndic] = DDA.[Main_Code_Text]
AND DDA.[Is_Latest] = 1

WHERE CRD.[UniqHospProvSpellID] IN (SELECT [UniqHospProvSpellID] FROM #AdmOrder)

-- =================================================================================
-- Aggregate delays by reason for each Provider Spell
-- =================================================================================

SELECT [UniqHospProvSpellID]
	  ,[ClinReadyforDischDelayReason]
	  ,[CRDReason]
	  ,[AttribToIndic]
	  ,[DelayReasonAttr]
	  ,SUM([Total_CRD_Days]) AS [Total_CRD_Days]
	  ,SUM([Reporting_CRD_Days]) AS [Reporting_CRD_Days]

INTO #CRDReasons
FROM #CRDOrder
WHERE [Der_CRD_Order] = 1

GROUP BY [UniqHospProvSpellID]
	  ,[ClinReadyforDischDelayReason]
	  ,[CRDReason]
	  ,[AttribToIndic]
	  ,[DelayReasonAttr]

-- =================================================================================
-- Aggregate delays for each Provider Spell
-- =================================================================================

SELECT [UniqHospProvSpellID]
	  ,SUM([Total_CRD_Days]) AS [Total_CRD_Days]
	  ,SUM([Reporting_CRD_Days]) AS [Reporting_CRD_Days]
INTO #CRD
FROM #CRDOrder
WHERE [Der_CRD_Order] = 1
GROUP BY [UniqHospProvSpellID]

-- =================================================================================
-- Find provider spells with both CRD and DD records. To remove and avoid double counting
-- =================================================================================

SELECT CRD.[UniqHospProvSpellID]
	  ,SUM(CRD.[Total_CRD_Days]) AS [Total_CRD_Days]
	  ,SUM(CRD.[Reporting_CRD_Days]) AS [Reporting_CRD_Days]
	  ,SUM(DD.[Total_DD_Days]) AS [Total_DD_Days]
	  ,SUM(DD.[Reporting_DD_Days]) AS [Reporting_DD_Days]
	  ,-1 * SUM(DD.[Total_DD_Days]) AS [Total_DC_Days]
	  ,-1 * SUM(DD.[Reporting_DD_Days]) AS [Reporting_DC_Days]
INTO #CRD_DD_DC
FROM #CRDOrder AS CRD

LEFT JOIN #DDOrder AS DD
ON CRD.[UniqHospProvSpellID] = DD.[UniqHospProvSpellID]
AND CRD.[StartDateClinReadyforDisch] = DD.[StartDateDelayDisch]

WHERE CRD.[Der_CRD_Order] = 1
AND DD.[Der_DD_Order] = 1

GROUP BY CRD.[UniqHospProvSpellID]

-- =================================================================================
-- Extract Primary Diagnoses recorded during Provider Spell
-- =================================================================================

SELECT PDIAG.[Der_Person_ID]
	  ,PDIAG.[UniqMonthID]
	  ,PDIAG.[CodedDiagTimestamp]
	  ,ADM.[UniqHospProvSpellID]
	  ,ADM.[StartDateHospProvSpell]
	  ,ADM.[DischDateHospProvSPell]
	  ,PDIAG.[DiagSchemeInUse]
	  ,PDIAG.[PrimDiag]
	  ,ICD10.[Alt_Code]
	  ,ICD10.[Description] AS [ICD_10_Description]
	  ,ICD10.[Category_3_Description]
	  ,ICD10.[Category_2_Description]
	  ,PDIAG.[RecordStartDate]
	  ,PDIAG.[RecordEndDate]
	  ,ROW_NUMBER() OVER (PARTITION BY PDIAG.[Der_Person_ID], PDIAG.[UniqMonthID] ORDER BY PDIAG.[CodedDiagTimestamp] DESC) AS [Der_PDIAG_Submission_Order]
INTO #PDIAGOrder
FROM [Reporting_MESH_MHSDS].[MHS604PrimDiag_Published] AS [PDIAG]

INNER JOIN [Reporting_MESH_MHSDS].[MHSDS_SubmissionFlags_Published] AS SF
ON PDIAG.[NHSEUniqSubmissionID] = SF.[NHSEUniqSubmissionID]
AND SF.[Der_IsLatest] = 'Y'

INNER JOIN #AdmOrder AS ADM
ON PDIAG.[Der_Person_ID] = ADM.[Der_Person_ID]
AND PDIAG.[CodedDiagTimestamp] BETWEEN ADM.[StartDateHospProvSpell] AND ADM.[DischDateHospProvSpell]

LEFT JOIN [UKHD_ICD10].[Codes_And_Titles_And_MetaData] AS ICD10
ON LEFT(REPLACE(PDIAG.[PrimDiag], '.', ''), 4) = ICD10.[Alt_Code]
AND ICD10.[Effective_To] IS NULL

WHERE PDIAG.[Der_Person_ID] IN (SELECT [Der_Person_ID] FROM #AdmOrder)


-- =================================================================================
-- Order Diagnoses for First AND Last in Provider Spell
-- =================================================================================

SELECT *
	  ,ROW_NUMBER() OVER (PARTITION BY [Der_Person_ID] ORDER BY [CodedDiagTimestamp] ASC) AS [Der_First_Diag]
	  ,ROW_NUMBER() OVER (PARTITION BY [Der_Person_ID] ORDER BY [CodedDiagTimestamp] DESC) AS [Der_Last_Diag]
INTO #PDIAG_F_L
FROM #PDIAGOrder

WHERE [Der_PDIAG_Submission_Order] = 1

-- =================================================================================
-- Extract MH Act Assignments during Provider Spells
-- =================================================================================

SELECT MHACT.[Der_Person_ID]
	  ,MHACT.[MHS401UniqID]
	  ,MHACT.[UniqMonthID]
	  ,SF.[ReportingPeriodStartDate]
	  ,SF.[ReportingPeriodEndDate]
	  ,MHACT.[UniqMHActEpisodeID]
	  ,MHACT.[MentalCat]
	  ,MHACT_MCAT.[Main_Description] AS [MentalCatDesc]
	  ,MHACT.[StartDateMHActLegalStatusClass]
	  ,MHACT.[EndDateMHActLegalStatusClass]
	  ,MHACT.[InactTimeMHAPeriod]
	  ,MHACT.[ExpiryDateMHActLegalStatusClass]
	  ,MHACT.[LegalStatusCode]
	  ,MHACT_LC.[Main_Description] AS [LegalStatusDesc]
	  ,MHACT.[LegalStatusClassPeriodStartReason]
	  ,MHACT_SR.[Main_Description] AS [LegalStatusClassPSRDesc]
	  ,MHACT.[LegalStatusClassPeriodEndReason]
	  ,MHACT_ER.[Main_Description] AS [LegalStatusClassPERDesc]
	  ,MHACT.[MHActLegalStatusClassPeriodId]
	  ,MHACT.[NHSDLegalStatus]
	  ,ADM.[UniqHospProvSpellID]
	  ,ADM.[StartDateHospProvSpell]
	  ,ADM.[DischDateHospProvSpell]
	  ,CASE WHEN MHACT.[StartDateMHActLegalStatusClass] BETWEEN @StartDate AND  @EndDate THEN 1 ELSE 0 END AS [In_RP]
	  ,ROW_NUMBER() OVER (PARTITION BY MHACT.[UniqMHActEpisodeID] ORDER BY MHACT.[UniqMonthID] DESC) AS [Der_MHActEpisode_Submission_Order]

INTO #MHActOrder
FROM [Reporting_MESH_MHSDS].[MHS401MHActPeriod_Published] AS [MHACT]

INNER JOIN [Reporting_MESH_MHSDS].[MHSDS_SubmissionFlags_Published] AS SF
ON MHACT.[NHSEUniqSubmissionID] = SF.[NHSEUniqSubmissionID]
AND SF.[Der_IsLatest] = 'Y'

INNER JOIN #AdmOrder AS [ADM]
ON MHACT.[Der_Person_ID] = ADM.[Der_Person_ID]
AND MHACT.[StartDateMHActLegalStatusClass] BETWEEN ADM.[StartDateHospProvSpell] AND ADM.[DischDateHospProvSpell]
AND ADM.[Der_Spell_Order] = 1

LEFT JOIN [UKHD_Data_Dictionary].[Mental_Health_Act_2007_Mental_Category_SCD] AS [MHACT_MCAT]
ON MHACT.[MentalCat] = MHACT_MCAT.[Main_Code_Text]
AND MHACT_MCAT.[Is_Latest] = 1

LEFT JOIN [UKHD_Data_Dictionary].[Mental_Health_Act_Legal_Status_Classification_Code_SCD] AS [MHACT_LC]
ON MHACT.[LegalStatusCode] = MHACT_LC.[Main_Code_Text]
AND MHACT_LC.[Is_Latest] = 1

LEFT JOIN [UKHD_Data_Dictionary].[Mental_Health_Act_Legal_Status_Classification_Assignment_Period_Start_Reason_SCD] AS [MHACT_SR]
ON MHACT.[LegalStatusClassPeriodStartReason] = MHACT_SR.[Main_Code_Text]
AND MHACT_SR.[Is_Latest] = 1

LEFT JOIN [UKHD_Data_Dictionary].[Mental_Health_Act_Legal_Status_Classification_Assignment_Period_End_Reason_SCD] AS [MHACT_ER]
ON MHACT.[LegalStatusClassPeriodEndReason] = MHACT_ER.[Main_Code_Text]
AND MHACT_ER.[Is_Latest] = 1

WHERE MHACT.[Der_Person_ID] IN (SELECT [Der_Person_ID] FROM #AdmOrder)

-- =================================================================================
-- Order MH Act Assignments within Provider Spells
-- =================================================================================

SELECT *
	  ,ROW_NUMBER() OVER (PARTITION BY [UniqHospProvSpellID] ORDER BY [StartDateMHActLegalStatusClass] ASC) AS [MH_Episode_Order_First]
	  ,ROW_NUMBER() OVER (PARTITION BY [UniqHospProvSpellID] ORDER BY [StartDateMHActLegalStatusClass] DESC) AS [MH_Episode_Order_Last]
INTO #MHAct
FROM #MHActOrder
WHERE [Der_MHActEpisode_Submission_Order] = 1

-- =================================================================================
-- Order MH Act Assignments within Provider Spells for those in RP
-- =================================================================================

SELECT *
	  ,ROW_NUMBER() OVER (PARTITION BY [UniqHospProvSpellID] ORDER BY [StartDateMHActLegalStatusClass] ASC) AS [MH_Episode_Order_RP_First]
	  ,ROW_NUMBER() OVER (PARTITION BY [UniqHospProvSpellID] ORDER BY [StartDateMHActLegalStatusClass] DESC) AS [MH_Episode_Order_RP_Last]
INTO #MHActRP
FROM #MHActOrder
WHERE [Der_MHActEpisode_Submission_Order] = 1
AND [In_RP] = 1

-- =================================================================================
-- LDA Flags: Disab Code from MHS007
-- =================================================================================

SELECT DISTYPE.[Der_Person_ID]
	  ,DISTYPE.[RecordNumber]
	  ,DISTYPE.[UniqMonthID]
	  ,CAST('MHS007-DisabCode' AS VARCHAR(100)) AS [Table_Source]
	  ,1 AS [LDA_Flag]
	  ,1 AS [LD_Flag]
	  ,NULL AS [Autism_Flag]
	  ,ROW_NUMBER() OVER (PARTITION BY DISTYPE.[Der_Person_ID] ORDER BY DISTYPE.[UniqMonthID] DESC) AS [Der_LD_Submission_Order]
INTO #LDA_Flag_MHS007
FROM [Reporting_MESH_MHSDS].[MHS007DisabilityType_Published] AS [DISTYPE]

INNER JOIN [Reporting_MESH_MHSDS].[MHSDS_SubmissionFlags_Published] AS SF
ON DISTYPE.[NHSEUniqSubmissionID] = SF.[NHSEUniqSubmissionID]
AND SF.[Der_IsLatest] = 'Y'

WHERE [Der_Person_ID] IN (SELECT [Der_Person_ID] FROM #AdmOrder)
AND [DisabCode] = '04'

-- =================================================================================
-- LDA Flags: Primary Referral Reason from MHS101
-- =================================================================================

SELECT REF.[Der_Person_ID]
	  ,REF.[RecordNumber]
	  ,REF.[UniqMonthID]
	  ,CAST('MHS101-PrimRefReason' AS VARCHAR(100)) AS [Table_Source]
	  ,1 AS [LDA_Flag]
	  ,CASE WHEN REF.[PrimReasonReferralMH] IN ('30', '24') THEN 1 ELSE 0 END AS [LD_Flag]
	  ,CASE WHEN REF.[PrimReasonReferralMH] IN ('17', '25','26') THEN 1 ELSE 0 END AS [Autism_Flag]
	  ,ROW_NUMBER() OVER (PARTITION BY REF.[Der_Person_ID] ORDER BY REF.[UniqMonthID] DESC) AS [Der_LD_Submission_Order]
INTO #LDA_Flag_MHS101
FROM [Reporting_MESH_MHSDS].[MHS101Referral_Published] AS [REF]

INNER JOIN [Reporting_MESH_MHSDS].[MHSDS_SubmissionFlags_Published] AS SF
ON REF.[NHSEUniqSubmissionID] = SF.[NHSEUniqSubmissionID]
AND SF.[Der_IsLatest] = 'Y'

WHERE REF.[Der_Person_ID] IN (SELECT [Der_Person_ID] FROM #AdmOrder)
AND REF.[PrimReasonReferralMH] IN ('17', '30', '24','25','26')

-- =================================================================================
-- LDA Flags: Other Referral Reason from MHS103
-- =================================================================================

SELECT OREF.[Der_Person_ID]
	  ,OREF.[RecordNumber]
	  ,OREF.[UniqMonthID]
	  ,CAST('MHS103-ReferReasonOther' AS VARCHAR(100)) AS [Table_Source]
	  ,1 AS [LDA_Flag]
	  ,CASE WHEN OREF.[OtherReasonReferMH] IN ('30', '24') THEN 1 ELSE 0 END AS [LD_Flag]
	  ,CASE WHEN OREF.[OtherReasonReferMH] IN ('17', '25','26') THEN 1 ELSE 0 END AS [Autism_Flag]
	  ,ROW_NUMBER() OVER (PARTITION BY OREF.[Der_Person_ID] ORDER BY OREF.[UniqMonthID] DESC) AS [Der_LD_Submission_Order]
INTO #LDA_Flag_MHS103
FROM [Reporting_MESH_MHSDS].[MHS103OtherReasonReferral_Published] AS [OREF]

INNER JOIN [Reporting_MESH_MHSDS].[MHSDS_SubmissionFlags_Published] AS SF
ON OREF.[NHSEUniqSubmissionID] = SF.[NHSEUniqSubmissionID]
AND SF.[Der_IsLatest] = 'Y'

WHERE OREF.[Der_Person_ID] IN (SELECT [Der_Person_ID] FROM #AdmOrder)
AND OREF.[OtherReasonReferMH] IN ('17', '30', '24','25','26')

-- =================================================================================
-- LDA Flags: Service referred to from MHS102
-- =================================================================================

SELECT SERV.[Der_Person_ID]
	  ,SERV.[RecordNumber]
	  ,SERV.[UniqMonthID]
	  ,'MHS102-ReferService' AS [Table_Source]
	  ,1 AS [LDA_Flag]
	  ,CASE WHEN SERV.[ServTeamTypeRefToMH] IN ('B02', 'E01') THEN 1 ELSE 0 END AS [LD_Flag]
	  ,CASE WHEN SERV.[ServTeamTypeRefToMH] IN ('C01', 'C04') THEN 1 ELSE 0 END AS [Autism_Flag]
	  ,ROW_NUMBER() OVER (PARTITION BY SERV.[Der_Person_ID] ORDER BY SERV.[UniqMonthID] DESC) AS [Der_LD_Submission_Order]
INTO #LDA_Flag_MHS102
FROM [Reporting_MESH_MHSDS].[MHS102ServiceTypeReferredTo_Published] AS [SERV]

INNER JOIN [Reporting_MESH_MHSDS].[MHSDS_SubmissionFlags_Published] AS SF
ON SERV.[NHSEUniqSubmissionID] = SF.[NHSEUniqSubmissionID]
AND SF.[Der_IsLatest] = 'Y'

WHERE SERV.[Der_Person_ID] IN (SELECT [Der_Person_ID] FROM #AdmOrder)
AND SERV.[RecordNumber] IN (SELECT [RecordNumber] FROM #AdmOrder)
AND SERV.[ServTeamTypeRefToMH] IN ('B02', 'C01', 'C04', 'E01')

-- =================================================================================
-- LDA Flags: Previous diagnoses from MHS601
-- =================================================================================

SELECT PREV_DIAG.[Der_Person_ID]
	  ,PREV_DIAG.[RecordNumber]
	  ,PREV_DIAG.[UniqMonthID]
	  ,'MHS601-PrevDiag' AS [Table_Source]
	  ,1 AS [LDA_Flag]
	  ,CASE WHEN LEFT(PREV_DIAG.[PrevDiag], 2) = 'F7' OR LEFT(PREV_DIAG.[PrevDiag], 3) IN ('F80', 'F81', 'F82') THEN 1 ELSE 0 END AS [LD_Flag]
	  ,CASE WHEN LEFT(PREV_DIAG.[PrevDiag], 3) = 'F84' THEN 1 ELSE 0 END AS [Autism_Flag]
	  ,ROW_NUMBER() OVER (PARTITION BY PREV_DIAG.[Der_Person_ID] ORDER BY PREV_DIAG.[UniqMonthID] DESC) AS [Der_LD_Submission_Order]
INTO #LDA_Flag_MHS601
FROM [Reporting_MESH_MHSDS].[MHS601MedHistPrevDiag_Published] AS [PREV_DIAG]

INNER JOIN [Reporting_MESH_MHSDS].[MHSDS_SubmissionFlags_Published] AS SF
ON PREV_DIAG.[NHSEUniqSubmissionID] = SF.[NHSEUniqSubmissionID]
AND SF.[Der_IsLatest] = 'Y'

WHERE PREV_DIAG.[Der_Person_ID] IN (SELECT [Der_Person_ID] FROM #AdmOrder)
AND PREV_DIAG.[RecordNumber] IN (SELECT [RecordNumber] FROM #AdmOrder)
AND PREV_DIAG.[DiagSchemeInUse] = '02'
AND (LEFT(PREV_DIAG.[PrevDiag] ,2) = 'F7' OR
	 LEFT(PREV_DIAG.[PrevDiag], 3) IN ('F80', 'F81', 'F83', 'F84'))

-- =================================================================================
-- LDA Flags: Current Primary Diagnosis from MHS604
-- =================================================================================

SELECT CURR_PDIAG.[Der_Person_ID]
	  ,CURR_PDIAG.[RecordNumber]
	  ,CURR_PDIAG.[UniqMonthID]
	  ,'MHS604-CurrPDiag' AS [Table_Source]
	  ,1 AS [LDA_Flag]
	  ,CASE WHEN LEFT(CURR_PDIAG.[PrimDiag], 2) = 'F7' OR LEFT(CURR_PDIAG.[PrimDiag], 3) IN ('F80', 'F81', 'F82') THEN 1 ELSE 0 END AS [LD_Flag]
	  ,CASE WHEN LEFT(CURR_PDIAG.[PrimDiag], 3) = 'F84' THEN 1 ELSE 0 END AS [Autism_Flag]
	  ,ROW_NUMBER() OVER (PARTITION BY CURR_PDIAG.[Der_Person_ID] ORDER BY  CURR_PDIAG.[UniqMonthID] DESC, CURR_PDIAG.[CodedDiagTimestamp] DESC) AS [Der_LD_Submission_Order]
INTO #LDA_Flag_MHS604
FROM [Reporting_MESH_MHSDS].[MHS604PrimDiag_Published] AS [CURR_PDIAG]

INNER JOIN [Reporting_MESH_MHSDS].[MHSDS_SubmissionFlags_Published] AS SF
ON CURR_PDIAG.[NHSEUniqSubmissionID] = SF.[NHSEUniqSubmissionID]
AND SF.[Der_IsLatest] = 'Y'

INNER JOIN #AdmOrder AS ADM
ON CURR_PDIAG.[Der_Person_ID] = ADM.[Der_Person_ID]
AND CURR_PDIAG.[CodedDiagTimestamp] BETWEEN ADM.[StartDateHospProvSpell] AND ADM.[DischDateHospProvSpell]

WHERE CURR_PDIAG.[DiagSchemeInUse] = '02'
AND (LEFT(CURR_PDIAG.[PrimDiag] ,2) = 'F7' OR
	 LEFT(CURR_PDIAG.[PrimDiag], 3) IN ('F80', 'F81', 'F83', 'F84'))

-- =================================================================================
-- LDA Flags: Current Secondary Diagnosis from MHS605
-- =================================================================================

SELECT CURR_SDIAG.[Der_Person_ID]
	  ,CURR_SDIAG.[RecordNumber]
	  ,CURR_SDIAG.[UniqMonthID]
	  ,'MHS605-CurrSDiag' AS [Table_Source]
	  ,1 AS [LDA_Flag]
	  ,CASE WHEN LEFT(CURR_SDIAG.[SecDiag], 2) = 'F7' OR LEFT(CURR_SDIAG.[SecDiag], 3) IN ('F80', 'F81', 'F82') THEN 1 ELSE 0 END AS [LD_Flag]
	  ,CASE WHEN LEFT(CURR_SDIAG.[SecDiag], 3) = 'F84' THEN 1 ELSE 0 END AS [Autism_Flag]
	  ,ROW_NUMBER() OVER (PARTITION BY CURR_SDIAG.[Der_Person_ID] ORDER BY  CURR_SDIAG.[UniqMonthID] DESC, CURR_SDIAG.[CodedDiagTimestamp] DESC) AS [Der_LD_Submission_Order]
INTO #LDA_Flag_MHS605
FROM [Reporting_MESH_MHSDS].[MHS605SecDiag_Published] AS [CURR_SDIAG]

INNER JOIN [Reporting_MESH_MHSDS].[MHSDS_SubmissionFlags_Published] AS SF
ON CURR_SDIAG.[NHSEUniqSubmissionID] = SF.[NHSEUniqSubmissionID]
AND SF.[Der_IsLatest] = 'Y'

INNER JOIN #AdmOrder AS ADM
ON CURR_SDIAG.[Der_Person_ID] = ADM.[Der_Person_ID]
AND CURR_SDIAG.[CodedDiagTimestamp] BETWEEN ADM.[StartDateHospProvSpell] AND ADM.[DischDateHospProvSpell]

WHERE CURR_SDIAG.[DiagSchemeInUse] = '02'
AND (LEFT(CURR_SDIAG.[SecDiag] ,2) = 'F7' OR
	 LEFT(CURR_SDIAG.[SecDiag], 3) IN ('F80', 'F81', 'F83', 'F84'))

-- =================================================================================
-- LDA Flags: Create Combined LDA Table
-- =================================================================================

SELECT *
INTO #LDA_Flag_Combined
FROM #LDA_Flag_MHS007
WHERE [Der_LD_Submission_Order] = 1

INSERT INTO  #LDA_Flag_Combined
SELECT *
FROM #LDA_Flag_MHS101
WHERE [Der_LD_Submission_Order] = 1

INSERT INTO  #LDA_Flag_Combined
SELECT *
FROM #LDA_Flag_MHS103
WHERE [Der_LD_Submission_Order] = 1

INSERT INTO  #LDA_Flag_Combined
SELECT *
FROM #LDA_Flag_MHS102
WHERE [Der_LD_Submission_Order] = 1

INSERT INTO  #LDA_Flag_Combined
SELECT *
FROM #LDA_Flag_MHS601
WHERE [Der_LD_Submission_Order] = 1

INSERT INTO  #LDA_Flag_Combined
SELECT *
FROM #LDA_Flag_MHS604
WHERE [Der_LD_Submission_Order] = 1

INSERT INTO  #LDA_Flag_Combined
SELECT *
FROM #LDA_Flag_MHS605
WHERE [Der_LD_Submission_Order] = 1

-- =================================================================================
-- LDA Flags: Create LDA Flag Table
-- =================================================================================


SELECT [Der_Person_ID]
	  ,CASE WHEN SUM([LDA_Flag]) > 0 THEN 1 ELSE 0 END AS [LDA_Flag]
	  ,CASE WHEN SUM([LD_Flag]) > 0 THEN 1 ELSE 0 END AS [LD_Flag]
	  ,CASE WHEN SUM([Autism_Flag]) > 0 THEN 1 ELSE 0 END AS [Autism_Flag]
INTO #LDA_Flag_SU
FROM #LDA_Flag_Combined
GROUP BY [Der_Person_ID]

-- =================================================================================
-- Extract Ward Stays for Provider Spells
-- =================================================================================

SELECT ADM.*
	  ,WSTAY.[MHS502UniqID]
	  ,WSTAY.[NHSEUniqSubmissionID] AS [NHSEUniqSumbissionID_502]
	  ,WSTAY.[UniqWardStayID]
	  ,WSTAY.[StartDateWardStay]
	  ,WSTAY.[EndDateWardStay]
	  ,WSTAY.[HospitalBedTypeMH] AS [HospitalBedTypeMH_502]
	  ,WSTAY.[HospitalBedTypeName] AS [HospitalBedTypeName_502]
	  ,WSTAY.[MHAdmittedPatientClass]
	  ,MHAPC.[Main_Description] AS [HospitalBedType_MHAPC]
	  ,MHAPCT.[Main_Description] AS [HospitalBedType_MHAPCT]
	  ,COALESCE(COALESCE(WSTAY.[HospitalBedTypeName], MHAPC.[Main_Description]), MHAPCT.[Main_Description]) AS [HospitalBedType]
	  ,WSTAY.[SpecialisedMHServiceCode]
	  ,WSTAY.[SiteIDOfTreat]
	  ,WSTAY.[SiteIDOfWard]
	  ,TSITE.[Organisation_Name] AS [Site_Name]
	  ,TSITE.[SourceReference] AS [Site_Reference]
	  ,CASE
			WHEN WSTAY.[SiteIDOfTreat] IS NULL THEN NULL
			WHEN LEFT(WSTAY.[SiteIDOfTreat], 1) = 'R' THEN 'NHS'
			ELSE 'Independent'
	   END AS [Site_Type]
	  ,WSTAY.[WardType] AS [WardType_502]
	  ,WSTAY.[WardAge] AS [WardAge_502]
	  ,WSTAY.[WardLocDistanceHome]
	  ,WSTAY.[WardSecLevel] AS [WardSecLevel_502]
	  ,WSTAY.[WardStayBedTypesLkup] AS [WardStayBedTypesLkup_502]
	  ,WSTAY.[WardCode] AS [WardCode_502]
	  ,WSTAY.[UniqWardCode] AS [UniqWardCode_502]
	  ,ROW_NUMBER() OVER (PARTITION BY WSTAY.[UniqWardStayID] ORDER BY WSTAY.[UniqMonthID] DESC) AS [Der_WSTAY_Submission_Order]
	  ,WDET.[WardCode] AS [WardCode_903]
	  ,WDET.[UniqWardCode] AS [UniqWardCode_903] 
	  ,WDET.[WardType] AS [WardType_903]
	  ,WDET.[WardAge] AS [WardAge_903]
	  ,WDET.[WardSecLevel] AS [WardSecLevel_903]
	  ,COALESCE(WSTAY.[WardType], WDET.[WardType]) AS [Der_WardType]
	  ,WTYPE.[Main_Description] AS [Der_WardTypeDesc]
	  ,COALESCE(WSTAY.[WardAge], WDET.[WardAge]) AS [Der_WardAge]
	  ,CASE
			WHEN COALESCE(WSTAY.[WardAge], WDET.[WardAge]) = '10' THEN 'Child only'
			WHEN COALESCE(WSTAY.[WardAge], WDET.[WardAge]) = '11' THEN 'Adolescent only'
			WHEN COALESCE(WSTAY.[WardAge], WDET.[WardAge]) = '12' THEN 'Child and Adolescent'
			WHEN COALESCE(WSTAY.[WardAge], WDET.[WardAge]) = '13' THEN 'Adult only'
			WHEN COALESCE(WSTAY.[WardAge], WDET.[WardAge]) = '14' THEN 'Older Adult only'
			WHEN COALESCE(WSTAY.[WardAge], WDET.[WardAge]) = '15' THEN 'Adult and Older Adult'
			WHEN COALESCE(WSTAY.[WardAge], WDET.[WardAge]) = '99' THEN 'Any age'
			END AS [Der_WardAgeDesc]
	  ,COALESCE(WSTAY.[WardSecLevel], WDET.[WardSecLevel]) AS [Der_WardSecLevel]
	  ,WSEC.[Main_Description] AS [Der_WardSecLevelDesc]
	  ,COALESCE(WSTAY.[WardCode], WDET.[WardCode]) AS [Der_WardCode]
	  ,COALESCE(WSTAY.[UniqWardCode], WDET.[UniqWardCode]) AS [Der_UniqWardCode]

INTO #WStayOrder
FROM #AdmOrder AS ADM

LEFT JOIN [Reporting_MESH_MHSDS].[MHS502WardStay_Published] AS WSTAY
ON ADM.[Der_Person_ID] = WSTAY.[Der_Person_ID]
AND ADM.[UniqHospProvSPellID] = WSTAY.[UniqHospProvSPellID]

INNER JOIN [Reporting_MESH_MHSDS].[MHSDS_SubmissionFlags_Published] AS SF
ON WSTAY.[NHSEUniqSubmissionID] = SF.[NHSEUniqSubmissionID]
AND SF.[Der_IsLatest] = 'Y'

LEFT JOIN [Reporting_MESH_MHSDS].[MHS903WardDetails_Published] AS WDET
ON WSTAY.[WardCode] = WDET.[WardCode]
AND WSTAY.[OrgIDProv] = WDET.[OrgIDProv]
AND WSTAY.[NHSEUniqSubmissionID] = WDET.[NHSEUniqSubmissionID]

LEFT JOIN [UKHD_Data_Dictionary].[Mental_Health_Admitted_Patient_Classification_Type_SCD] AS MHAPCT
ON WSTAY.[MHAdmittedPatientClass] = MHAPCT.[Main_Code_Text]

LEFT JOIN [UKHD_Data_Dictionary].[Mental_Health_Admitted_Patient_Classification_SCD] AS MHAPC
ON WSTAY.[MHAdmittedPatientClass] = MHAPC.[Main_Code_Text]
AND MHAPC.[Is_Latest] = 1

LEFT JOIN [UKHD_Data_Dictionary].[Ward_Setting_Type_For_Mental_Health_SCD] AS WTYPE
ON COALESCE(WSTAY.[WardType], WDET.[WardType]) = WTYPE.[Main_Code_Text]
AND WTYPE.[Is_Latest] = 1

LEFT JOIN [UKHD_Data_Dictionary].[Ward_Security_Level_SCD] AS WSEC
ON COALESCE(WSTAY.[WardSecLevel], WDET.[WardSecLevel]) = WSEC.[Main_Code_Text]
AND WSEC.[Is_Latest] = 1

LEFT JOIN [Internal_Reference].[Site] as TSite
ON WSTAY.[SiteIDOfTreat] = TSite.[Organisation_Code]
AND TSite.[is_latest] = 1

WHERE ADM.[Der_Spell_Order] = 1
AND WSTAY.[SpecialisedMHServiceCode] IS NULL

-- =================================================================================
-- Remove duplicate Ward Stays
-- =================================================================================

SELECT *
	,DENSE_RANK() OVER (PARTITION BY [UniqHospProvSPellID] ORDER BY [StartDateWardStay]) AS [Der_WSTAY_Order]
INTO #WStay
FROM #WStayOrder
WHERE [Der_WSTAY_Submission_Order] = 1

-- =================================================================================
-- Extract Home Leave for relevant Ward Stays
-- =================================================================================

SELECT HL.[UniqMonthID]
	  ,HL.[UniqWardStayID]
	  ,HL.[StartDateHomeLeave]
	  ,HL.[EndDateHomeLeave]
	  ,#WSTAY.[StartDateWardStay]
	  ,#WSTAY.[EndDateWardStay]
	  ,SF.[ReportingPeriodEndDate]
	  ,ROW_NUMBER() OVER (PARTITION BY HL.[UniqWardStayID], HL.[StartDateHomeLeave] ORDER BY HL.[UniqMonthID] DESC) AS [Der_HL_Submission_Order]
	  
INTO #HLOrder
FROM [Reporting_MESH_MHSDS].[MHS509HomeLeave_Published] AS [HL]

INNER JOIN [Reporting_MESH_MHSDS].[MHSDS_SubmissionFlags_Published] AS SF
ON HL.[NHSEUniqSubmissionID] = SF.[NHSEUniqSubmissionID]
AND SF.[Der_IsLatest] = 'Y'

LEFT JOIN #WStay
ON HL.[UniqWardStayID] = #WSTAY.[UniqWardStayID]

WHERE HL.[UniqWardStayID] IN (SELECT [UniqWardStayID] FROM #WStay)

-- =================================================================================
-- Aggregate Home Leave for each Ward Stay
-- =================================================================================

SELECT [UniqWardStayID]
	 ,SUM(DATEDIFF(DAY, [StartDateHomeLeave], COALESCE([EndDateHomeLeave], COALESCE([EndDateWardStay], [ReportingPeriodEndDate])))) AS [HL_Days]
	 ,SUM(CASE
		  WHEN [StartDateHomeLeave] <  @EndDate AND COALESCE([EndDateHomeLeave], COALESCE([EndDateWardStay], [ReportingPeriodEndDate])) >= @StartDate THEN
		  DATEDIFF(DAY,
					CASE WHEN [StartDateHomeLeave] > @StartDate THEN [StartDateHomeLeave] ELSE @StartDate END,
					CASE WHEN COALESCE([EndDateHomeLeave], COALESCE([EndDateWardStay], [ReportingPeriodEndDate])) <  @EndDate THEN COALESCE([EndDateHomeLeave], COALESCE([EndDateWardStay], [ReportingPeriodEndDate])) ELSE  @EndDate END
				)
		  ELSE 0
	   END) AS [HL_Days_In_RP]

INTO #HLWSDays
FROM #HLOrder
WHERE [Der_HL_Submission_Order] = 1
GROUP BY [UniqWardStayID]

-- =================================================================================
-- Aggregate Home Leave for each Provider Spell
-- =================================================================================

SELECT WSTAY.[UniqHospProvSpellID]
	  ,SUM(HLW.[HL_Days]) AS [HL_Days]
	  ,SUM(HLW.[HL_Days_In_RP]) AS [HL_Days_In_RP]
INTO #HLPSDays
FROM #HLWSDays AS HLW

LEFT JOIN #WStay AS WSTAY
ON HLW.[UniqWardStayID] = WSTAY.[UniqWardStayID]

GROUP BY WSTAY.[UniqHospProvSpellID]

-- =================================================================================
-- Combined Ward Stays AND Home Leave to derive LOS Days
-- =================================================================================

SELECT WSTAY.[UniqWardStayID]
	  ,WSTAY.[UniqHospProvSpellID]
	  ,WSTAY.[StartDateWardStay]
	  ,WSTAY.[EndDateWardStay]
	  ,WSTAY.[DischDateHospProvSpell]
	  ,WSTAY.[ReportingPeriodEndDate]
	  ,COALESCE(WSTAY.[EndDateWardStay], COALESCE(WSTAY.[DischDateHospProvSpell], WSTAY.[ReportingPeriodEndDate])) AS [Der_EndDateWardStay]
	  ,DATEDIFF(DAY, WSTAY.[StartDateWardStay], COALESCE(WSTAY.[EndDateWardStay], COALESCE(WSTAY.[DischDateHospProvSpell], COALESCE(WSTAY.[ReportingPeriodEndDate],  @EndDate)))) AS [Der_WardStayLOS]
	  ,CASE
			WHEN WSTAY.[StartDateWardStay] <=  @EndDate AND COALESCE(WSTAY.[EndDateWardStay], COALESCE(WSTAY.[DischDateHospProvSpell], WSTAY.[ReportingPeriodEndDate])) >= @StartDate THEN
				DATEDIFF(DAY,
					CASE WHEN WSTAY.[StartDateWardStay] > @StartDate THEN WSTAY.[StartDateWardStay] ELSE @StartDate END,
					CASE WHEN COALESCE(WSTAY.[EndDateWardStay], COALESCE(WSTAY.[DischDateHospProvSpell], WSTAY.[ReportingPeriodEndDate])) <  @EndDate THEN COALESCE(WSTAY.[EndDateWardStay], COALESCE(WSTAY.[DischDateHospProvSpell], WSTAY.[ReportingPeriodEndDate])) ELSE  @EndDate END
				)
			ELSE 0
		END AS [Der_Reporting_WardStayLOS]
	  ,COALESCE(HL.[HL_Days], 0) AS [HL_Days]
	  ,COALESCE(HL.[HL_Days_In_RP], 0) AS [HL_Days_In_RP]

INTO #WStayLOS
FROM #WStay AS WSTAY

LEFT JOIN #HLWSDays AS HL
ON WSTAY.[UniqWardStayID] = HL.[UniqWardStayID]

-- =================================================================================
-- Admission Level Output
-- =================================================================================

SELECT ADM.*
      ,CASE WHEN ADM.[IMD_Decile] IS NULL THEN NULL
			WHEN ADM.[IMD_Decile] IN ('1', '2') THEN '1'
			WHEN ADM.[IMD_Decile] IN ('3', '4') THEN '2'
			WHEN ADM.[IMD_Decile] IN ('5', '6') THEN '3'
			WHEN ADM.[IMD_Decile] IN ('7', '8') THEN '4'
			WHEN ADM.[IMD_Decile] IN ('9', '10') THEN '5'
			ELSE NULL END AS [IMD_Quartile]
	  ,DATEADD(MONTH, DATEDIFF(MONTH, 0, ADM.[StartDateHospProvSpell]), 0) AS [Admission_Month]
	  ,DATEADD(MONTH, DATEDIFF(MONTH, 0, ADM.[DischDateHospProvSpell]), 0) AS [Discharge_Month]
	  ,COALESCE(DD.[Total_DD_Days], 0) + COALESCE(CRD.[Total_CRD_Days], 0) + COALESCE(CRD_DD_DC.[Total_DC_Days], 0) AS [Adj_Delay_Days]
	  ,COALESCE(DD.[Reporting_DD_Days], 0) + COALESCE(CRD.[Reporting_CRD_Days], 0) + COALESCE(CRD_DD_DC.[Reporting_DC_Days], 0) AS [Adj_Reporting_Delay_Days]
	  ,COALESCE(HL.[HL_Days], 0) AS [HL_Days]
	  ,COALESCE(HL.[HL_Days_In_RP], 0) AS [HL_Days_In_RP]
	  ,PDIAG_First.[PrimDiag] AS [PrimDiag_First]
	  ,PDIAG_First.[ICD_10_Description] AS [ICD_10_Description_First]
	  ,PDIAG_First.[Category_3_Description] AS [Category_3_Description_First]
	  ,PDIAG_Last.[PrimDiag] AS [PrimDiag_Last]
	  ,PDIAG_Last.[ICD_10_Description] AS [ICD_10_Description_Last]
	  ,PDIAG_Last.[Category_3_Description] AS [Category_3_Description_Last]
	  ,MHACT_RP_First.[StartDateMHActLegalStatusClass]
	  ,MHACT_RP_First.[EndDateMHActLegalStatusClass]
	  ,MHACT_RP_First.[LegalStatusCode]
	  ,MHACT_RP_First.[LegalStatusDesc]
	  ,CASE WHEN MHACT_RP_First.[LegalStatusCode] IS NULL THEN 'Not formally detained'
			WHEN MHACT_RP_First.[LegalStatusCode] IN ('98', '99') THEN 'Not formally detained'
			WHEN MHACT_RP_First.[LegalStatusCode] = '01' THEN 'Informal'
			ELSE 'Formally detained' END AS [legal_status_group]
	  ,ICB_H.[Region_Code] AS [Residence_ICB_Region_Code]
	  ,ICB_H.[Region_Name] AS [Residence_ICB_Region_Name]
	  ,CASE WHEN ICB_H.[Region_Code] = 'Y60' AND ADM.[Provider_Region_Code] = 'Y60' THEN 'MidlandsRes-MidlandsProv'
			WHEN ICB_H.[Region_Code] = 'Y60' AND ADM.[Provider_Region_Code] != 'Y60' THEN 'MidlandsRes-NonMidlandsProv'
			WHEN ICB_H.[Region_Code] != 'Y60' AND ADM.[Provider_Region_Code] = 'Y60' THEN 'NonMidlandsRes-MidlandsProv'
			WHEN ICB_H.[Region_Code] != 'Y60' AND ADM.[Provider_Region_Code] != 'Y60' THEN 'NonMidlandsRes-NonMidlandsProv'
			ELSE NULL END AS [OOA_Group]
	  ,WStay_First.[UniqWardStayID] AS [UniqWardStayID_First]
	  ,WStay_First.[HospitalBedType] AS [HospitalBedType_First]
	  ,WStay_First.[Der_WardType] AS [Der_WardType_First]
	  ,WStay_First.[Der_WardTypeDesc] AS [Der_WardTypeDesc_First]
	  ,WStay_First.[Der_WardAge] AS [Der_WardAge_First]
	  ,WStay_First.[Der_WardAgeDesc] AS [Der_WardAgeDesc_First]
	  ,WStay_First.[Der_WardSecLevel] AS [Der_WardSecLevel_First]
	  ,WStay_First.[Der_WardSecLevelDesc] AS [Der_WardSecLevelDesc_First]
	  ,LDA_Flag.[LDA_Flag] AS [LDA_Flag_SU]
	  ,LDA_Flag.[LD_Flag] AS [LD_Flag_SU]
	  ,LDA_Flag.[Autism_Flag] AS [Autism_Flag_SU]

INTO #AdmOutput
FROM #AdmOrder AS [ADM]

LEFT JOIN #DD AS [DD]
ON ADM.[UniqHospProvSpellID] = DD.[UniqHospProvSpellID]

LEFT JOIN #CRD AS [CRD]
ON ADM.[UniqHospProvSpellID] = CRD.[UniqHospProvSpellID]

LEFT JOIN #CRD_DD_DC AS [CRD_DD_DC]
ON ADM.[UniqHospProvSpellID] = CRD_DD_DC.[UniqHospProvSpellID]

LEFT JOIN #PDIAG_F_L AS [PDIAG_First]
ON ADM.[UniqHospProvSpellID] = PDIAG_First.[UniqHospProvSpellID]
AND PDIAG_First.[Der_First_Diag] = 1

LEFT JOIN #PDIAG_F_L AS [PDIAG_Last]
ON ADM.[UniqHospProvSpellID] = PDIAG_Last.[UniqHospProvSpellID]
AND PDIAG_Last.[Der_First_Diag] = 1

LEFT JOIN #MHActRP AS [MHACT_RP_First]
ON ADM.[UniqHospProvSpellID] = MHACT_RP_First.[UniqHospProvSpellID]
AND MHACT_RP_First.[In_RP] = 1
AND MHACT_RP_First.[MH_Episode_Order_RP_First] = 1

LEFT JOIN #HLPSDays AS [HL]
ON ADM.[UniqHospProvSpellID] = HL.[UniqHospProvSpellID]

LEFT JOIN #WStay as [WStay_First]
ON ADM.[UniqHospProvSpellID] = WStay_First.[UniqHospProvSpellID]
AND WStay_First.[Der_WSTAY_Order] = 1

LEFT JOIN #LDA_Flag_SU AS LDA_Flag
ON ADM.[Der_Person_ID] = LDA_Flag.[Der_Person_ID]

LEFT JOIN (SELECT [ICB_Code]
      ,[Integrated_Care_Board_Name]
	  ,[Region_Code]
	  ,[Region_Name]
  FROM [Reporting_UKHD_ODS].[Commissioner_Hierarchies_ICB]

  GROUP BY [ICB_Code]
      ,[Integrated_Care_Board_Name]
	  ,[Region_Code]
	  ,[Region_Name]) AS [ICB_H]
ON ADM.[Residence_ICB_Code] = ICB_H.[ICB_Code]

WHERE ADM.[Der_Spell_Order] = 1
AND WStay_First.[UniqWardStayID] IS NOT NULL

-- =================================================================================
-- Ward Stay Level Output
-- =================================================================================

SELECT WStay.*
	  ,WStay_LOS.[Der_WardStayLOS]
	  ,WStay_LOS.[Der_Reporting_WardStayLOS]
	  ,WStay_LOS.[HL_Days]
	  ,WStay_LOS.[HL_Days_In_RP]
INTO #WStayOutput
FROM #WStay AS [WStay]

LEFT JOIN #WStayLOS AS [WStay_LOS]
ON WStay.[UniqWardStayID] = WStay_LOS.[UniqWardStayID]

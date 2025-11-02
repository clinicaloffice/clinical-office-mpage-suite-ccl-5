/*************************************************************************
 
        Script Name:    1co5_mpage_template.prg
 
        Description:    Clinical Office - MPage Developer
                        Custom CCL Pre/Post Blank Template
 
        Date Written:   September 9, 2025
        Written by:     John Simpson
                        Precision Healthcare Solutions
 
 *************************************************************************
            Copyright (c) 2025 Precision Healthcare Solutions
 
 NO PART OF THIS CODE MAY BE COPIED, MODIFIED OR DISTRIBUTED WITHOUT
 PRIOR WRITTEN CONSENT OF PRECISION HEALTHCARE SOLUTIONS EXECUTIVE
 LEADERSHIP TEAM.
 
 FOR LICENSING TERMS PLEASE VISIT www.clinicaloffice.com/mpage/license
 
 *************************************************************************
                            Special Instructions
 *************************************************************************
 Called from 1co5_mpage_entry. Do not attempt to run stand alone. If you
 wish to test the development of your custom script from the CCL back-end,
 please run with 1co_mpage_test.
 
 Possible Payload values:
 
    "customScript": {
        "script": [
            "name": "your custom script name:GROUP1",
            "id": "identifier for your output, omit if you won't be returning data",
            "run": "pre or post",
            "parameters": {
                "your custom parameters for your job"
            }
        ]
    }
 
 *************************************************************************
                            Revision Information
 *************************************************************************
 Rev    Date     By             Comments
 ------ -------- -------------- ------------------------------------------
 001    09/06/25 J. Simpson     Initial Development
 *************************************************************************/
drop program 1co5_mpage_template:group1 go
create program 1co5_mpage_template:group1
 
/*
	The parameters for your script are stored in the PAYLOAD record structure. This
	structure contains the entire payload for the current CCL execution so parameters
	for other Clinical Office jobs may be present (e.g. person, encounter, etc.).
 
	Your payload parameters are stored in payload->customscript->script[script number].parameters.
 
	The script number for your script has been assigned to a variable called nSCRIPT.
 
	For example, if you had a parameter called fromDate in your custom parameters for your script
	you would access it as follows:
 
	set dFromDate = payload->customscript->script[nscript]->parameters.fromdate
 
	**** NOTE ****
	If you plan on running multiple pre/post scripts in the same payload, please ensure that
	you do not have the same parameter with different data types between jobs. For example, if
	you ran two pre/post jobs at the same time with a parameter called fromDate and in one job
	you passed a valid JavaScript date such as  "fromDate": "2018-05-07T14:44:51.000+00:00" and
	in the other job you passed "fromDate": "05-07-2018" the second instance of the parameter
	would cause an error.
*/
  
; This is the point where you would add your custom CCL code to collect data. If you did not
; choose to clear the patient source, you will have the encounter/person data passed from the
; mpage available for use in the PATIENT_SOURCE record structure.
;
; There are two branches you can use, either VISITS or PATIENTS. The format of the
; record structure is:
;
; 1 patient_source
;	2 visits[*]
;		3 person_id			= f8
;		3 encntr_id			= f8
;	2 patients[*]
;		3 person_id			= f8
;
; Additionally, you can alter the contents of the PATIENT_SOURCE structure to allow encounter
; or person records to be available for standard Clinical Office scripts. For example, your custom
; script may collect a list of visits you wish to have populated in your mPage. Instead of
; manually collecting your demographic information, simply add your person_id/encntr_id combinations
; to the PATIENT_SOURCE record structure and ensure that the standard Clinical Office components
; are being called within your payload. (If this is a little unclear, please see the full
; documentation on http://www.clinicaloffice.com).
 
; ------------------------------------------------------------------------------------------------
;								BEGIN YOUR CUSTOM CODE HERE
; ------------------------------------------------------------------------------------------------
 
; Define the custom record structure you wish to have sent back in the JSON to the MPage. The name
; of the record structure can be anything you want but you must make sure it matches the structure
; name used in the add_custom_output subroutine at the bottom of this script.
free record rCustom
record rCustom (
	1 data[*]
		2 custom_string				= vc
		2 custom_date               = dq8
		2 custom_cd                 = f8
		2 custom_prsnl[*]
		  3 person_id               = f8
		  3 name_full_formatted     = vc
		  3 position_cd             = f8
		  3 position                = vc
)
 
; Alternate reference structure to be used with ReferenceService. If you are not implementing 
; ReferenceService, simply remove this entire IF block from your script. If you plan on using the
; ReferenceService, please ensure that you populate your entire record structure with one
; empty row for each dynamic element.
if (run_stats->reference_ind = 1)
    set stat = alterlist(rCustom->data, 1)
    set stat = alterlist(rCustom->data[1]->custom_prsnl, 1)
        
    ; Populate the code sets to be matched up to your record structure. This will send the entire
    ; reference for all code values for a table unless you specify column_name filters as done
    ; in the example below.
    select into "nl:"
    from    dm_columns_doc      dcd
    plan dcd
        where dcd.table_name in ("PRSNL")
        and dcd.code_set > 0
        and dcd.column_name in ("POSITION_CD") ; Filter only the fields you need
    detail
        call add_ref_code_set(payload->customscript->script[nscript].id, 
                    camel_field(dcd.column_name), dcd.description, dcd.code_set)        
    with counter        
        
    go to skip_logic
endif 

; Do something (e.g. collect orders, appointments, etc.)
 
 
; ------------------------------------------------------------------------------------------------
;								END OF YOUR CUSTOM CODE
; ------------------------------------------------------------------------------------------------
 
; If you wish to return output back to the MPage, you need to run the ADD_CUSTOM_OUTPUT function.
; Any valid JSON format is acceptable including the CNVTRECTOJSON function. If using
; CNVTRECTOJSON be sure to use parameters 4 and 1 as shown below.
; If you plan on creating your own JSON string rather than converting a record structure, be
; sure to have it in the format of {"name":{your custom json data}} as the ADD_CUSTOM_OUTPUT
; subroutine will extract the first sub-object from the JSON. (e.g. {"name":{"personId":123}} will
; be sent to the output stream as {"personId": 123}.

#skip_logic

call add_custom_output(cnvtrectojson(rCustom, 4, 1))
 
#end_program
 
end go

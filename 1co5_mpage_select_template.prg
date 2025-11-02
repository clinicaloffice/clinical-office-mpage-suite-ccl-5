/*************************************************************************
 
        Script Name:    1co5_mpage_select_template.prg
 
        Description:    Clinical Office - MPage Developer
                        MPage Select Component - Custom CCL Pre/Post Blank Template
 
        Date Written:   October 10, 2025
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
 Called from 1co_mpage_entry. Do not attempt to run stand alone. If you
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
        ],
        "clearPatientSource": true
    }
 
 *************************************************************************
                            Revision Information
 *************************************************************************
 Rev    Date     By             Comments
 ------ -------- -------------- ------------------------------------------
 001    10/10/25 J. Simpson     Initial Development
 *************************************************************************/
drop program 1co5_mpage_select_template:group1 go
create program 1co5_mpage_select_template:group1
  
; Check to see if we have cleared the patient source and if not, do we have data in the patient source
; to run our custom CCL against.
if (validate(payload->customscript->clearpatientsource, 0) = 0)
	if (size(patient_source->patients, 5) = 0)
		go to end_program
	endif
endif
 
; Clear and define rCustom structure
free record rCustom
record rCustom (
    1 status
        2 error_ind             = i4
        2 message               = vc
        2 count                 = i4
    1 data[*]
        2 key                   = f8
        2 value                 = vc      
)

; Define and populate the parameters structure
free record rParam
record rParam (
    1 search_ind                = i4
    1 search_limit              = i4
    1 physician_ind             = i4
    1 code_set                  = i4
    1 value_type                = vc
    1 search_value              = vc
    1 default[*]                = f8
)

set rParam->search_ind = payload->customscript->script[nscript]->parameters->search
set rParam->search_limit = payload->customscript->script[nscript]->parameters->searchlimit
set rParam->physician_ind = payload->customscript->script[nscript]->parameters->physicianind
set rParam->code_set= payload->customscript->script[nscript]->parameters->codeset
set rParam->value_type = cnvtupper(payload->customscript->script[nscript]->parameters->valuetype)
set rParam->search_value = cnvtupper(payload->customscript->script[nscript]->parameters->searchvalue)

if (validate(payload->customscript->script[nScript]->parameters->default) = 1)
    if (size(payload->customscript->script[nScript]->parameters->default, 5) > 0)
        set stat = alterlist(rParam->default, size(payload->customscript->script[nScript]->parameters->default, 5))
        for (nLoop = 1 to size(rParam->default, 5))   
            set rParam->default[nLoop] = cnvtreal(payload->customscript->script[nScript]->parameters->default[nLoop])
        endfor        
    endif
endif
 
; Declare variables and subroutines
declare nNum = i4
declare nDefault = i4
 
; ------------------------------------------------------------------------------------------------
;								BEGIN YOUR CUSTOM CODE HERE
; ------------------------------------------------------------------------------------------------

; Please make sure you replace "***COMPARISON DB FIELD***" with the database field you are 
; searching against in your SQL. For example, if you are using the code_value table
; "***COMPARISON DB FIELD***" would be replaced with cv.code_value if your code_value table
; is aliased as "cv".
 
; Custom parser declarations
declare cParser = vc with noconstant("1=1")

; Build the parser for user search
if (rParam->search_value != "")

    set cParser = concat(^cnvtupper( ***COMPARISON DB FIELD*** ) = patstring(|^, rParam->search_value, ^*|)^)
                                    
; Build a parser for default values                                    
elseif (rParam->search_limit > 0 and size(rParam->default, 5) > 0)

    set cParser = ^expand(nNum, 1, size(rParam->default, 5), ***COMPARISON DB FIELD***, rParam->default[nNum])^
    set nDefault = 1

endif

; Perform a limit check to determine if too many values exist to upload
; ---------------------------------------------------------------------
if (rParam->search_limit > 0)
    
    ; Perform your select to count the results you are after
    select into "nl:"
        row_count   = count(***COMPARISON DB FIELD***)
    from    ***YOUR TABLES *** ***YOUR ALIAS***
    plan ***YOUR ALIAS***
        and parser(cParser)
        ... remaining custom code

    ; WARNING: Avoid modifying the detail section below or your code may fail
    detail
        if (row_count > rParam->search_limit)
            rCustom->status->error_ind = 1
            rCustom->status->message = concat(build(cnvtint(row_count)), " records retrieved. Limit is ", 
                                        build(rParam->search_limit), ".")
        endif
        rCustom->status->count = row_count
    with nocounter        
    
endif

; Perform the load if search limit does not fail
if (rCustom->status->error_ind = 0 or nDefault = 1)

    set rCustom->status.message = "No records qualified."

    select into "nl:"
    from    ***YOUR TABLES *** ***YOUR ALIAS***
    plan ***YOUR ALIAS***
        and parser(cParser)
        ... remaining custom code
    order ***YOUR SORT FIELD***        
    head report
        rCustom->status.message = "Ok."
        nCount = 0
        
    ; WARNING: Detail section must write to rCustom->data[].key and rCustom->data[].value        
    detail
        nCount = nCount + 1
        stat = alterlist(rCustom->data, nCount)
        rCustom->data[nCount].key = ***COMPARISON DB FIELD***
        rCustom->data[nCount].value = ***COMPARISON DB DISPLAY FIELD***
        
    with counter, expand=2

endif
 
; ------------------------------------------------------------------------------------------------
;								END OF YOUR CUSTOM CODE
; ------------------------------------------------------------------------------------------------
 
; If you wish to return output back to the mPage, you need to run the ADD_CUSTOM_OUTPUT function.
; Any valid JSON format is acceptable including the CNVTRECTOJSON function. If using
; CNVTRECTOJSON be sure to use parameters 4 and 1 as shown below.
; If you plan on creating your own JSON string rather than converting a record structure, be
; sure to have it in the format of {"name":{your custom json data}} as the ADD_CUSTOM_OUTPUT
; subroutine will extract the first sub-object from the JSON. (e.g. {"name":{"personId":123}} will
; be sent to the output stream as {"personId": 123}.
call add_custom_output(cnvtrectojson(rCustom, 4, 1))
 
#end_program
 
end go

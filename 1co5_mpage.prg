/*************************************************************************
 
        Script Name:    1co5_code_value_search.prg
 
        Description:    Clinical Office - MPage Developer
                        Code Value Search Component CCL Support Script
 
        Date Written:   May 3, 2025
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
 Called from select component
 *************************************************************************
                            Revision Information
 *************************************************************************
 Rev    Date     By             Comments
 ------ -------- -------------- ------------------------------------------
 001    03/03/22 J. Simpson     Initial Development
 002    09/21/22 J. Simpson     Added code set limits to allow initial filtering of list
 003    07/24/23 J. Simpson     Fixed search values with comma to prevent split of data
 004    05/03/25 J. Simpson     Converted to MPage Developer v5 standards
 *************************************************************************/
drop program 1co5_code_value_search:group1 go
create program 1co5_code_value_search:group1
 
; Declare variables and subroutines
declare nNum = i4
declare nDefault = i4
  
; Clear and define rCustom structure
free record rCustom
record rCustom (
    1 status
        2 error_ind                 = i4
        2 message                   = vc
        2 limit_met                 = i4
        2 full_data_set_loaded      = i4
    1 data[*]
        2 key                       = f8
        2 value                     = vc      
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
    1 limit_type                = vc
    1 limits[*]                 = vc
    1 default[*]                = f8
)

set rParam->search_ind = payload->customscript->script[nscript]->parameters->search
set rParam->search_limit = payload->customscript->script[nscript]->parameters->searchlimit
set rParam->physician_ind = payload->customscript->script[nscript]->parameters->physicianind
set rParam->code_set= payload->customscript->script[nscript]->parameters->codeset
set rParam->value_type = cnvtupper(payload->customscript->script[nscript]->parameters->valuetype)
set rParam->search_value = cnvtupper(payload->customscript->script[nscript]->parameters->searchvalue)
set rParam->limit_type = cnvtupper(payload->customscript->script[nscript]->parameters->codeSetLimitType)

if (size(payload->customscript->script[nscript]->parameters->codeSetLimits, 5) > 0)
    set stat = alterlist(rParam->limits, size(payload->customscript->script[nscript]->parameters->codeSetLimits, 5))
    for (nLoop = 1 to size(rParam->limits, 5))
        set rParam->limits[nLoop] = payload->customscript->script[nscript]->parameters->codeSetLimits[nLoop]
    endfor
endif

if (validate(payload->customscript->script[nscript]->parameters->default) = 1)
    if (size(payload->customscript->script[nscript]->parameters->default, 5) > 0)
        set stat = alterlist(rParam->default, size(payload->customscript->script[nscript]->parameters->default, 5))
        for (nLoop = 1 to size(rParam->default, 5))
            set rParam->default[nLoop] = cnvtreal(payload->customscript->script[nscript]->parameters->default[nLoop])
        endfor
    endif
endif    
 
; Custom parser declarations
declare cParser = vc with noconstant("1=1")
if (rParam->search_value != "")

    ; Build the parser
    set cParser = concat(^cnvtupper(cv.^, rParam->value_type,  ^) = patstring(|^,
                                    trim(rParam->search_value), ^*|)^)

endif

; Build a parser for default values                                    
declare cDefaultParser = vc with noconstant("1=1")
if (size(rParam->default, 5) > 0)
    set cDefaultParser = ^expand(nNum, 1, size(rParam->default, 5), cv.code_value, cnvtreal(rParam->default[nNum]))^
    set nDefault = 1
endif

declare cLimitParser = vc with noconstant("1=1")
if (size(rParam->limits, 5) > 0 and rParam->limit_type in 
        ("CDF_MEANING","DISPLAY","DISPLAY_KEY","DESCRIPTION","DEFINITION","CKI","CONCEPT_CKI"))
    set cLimitParser = concat(^expand(nNum, 1, size(rParam->limits, 5), cv.^, rParam->limit_type, 
                        ^, rParam->limits[nNum])^)
endif

; Perform a limit check to determine if too many values exist to upload
; ---------------------------------------------------------------------
if (rParam->search_limit > 0)
    
    ; Perform your select to count the results you are after
    select into "nl:"
        row_count   = count(cv.code_value)
    from    code_value      cv
    plan cv
        where cv.code_set = rParam->code_set
        and parser(cParser)
        and parser(cLimitParser)
        and cv.active_ind = 1
        and cv.end_effective_dt_tm > sysdate
        
    ; WARNING: Avoid modifying the detail section below or your code may fail
    detail
        if (row_count > rParam->search_limit)
            rCustom->status->limit_met = 1
        elseif (cParser = "1=1")
            rCustom->status->full_data_set_loaded = 1
        endif
        
        if (row_count > rParam->search_limit and size(rParam->default, 5) = 0)
            rCustom->status->error_ind = 1
            rCustom->status->message = concat(build(cnvtint(row_count)), " records retrieved. Limit is ", 
                                        build(rParam->search_limit), ".")
        endif            
    with expand=1, nocounter    
endif

; Perform the load if search limit does not fail
if (rCustom->status->error_ind = 0 or nDefault = 1)
    if (rCustom->status->limit_met = 0)
        set cDefaultParser = "1=1"
    endif

    set rCustom->status.message = "No records qualified."

    select into "nl:"
        display_value   = if (rParam->value_type = "DISPLAY_KEY")
                            cv.display_key
                          elseif (rParam->value_type = "DESCRIPTION")
                            cv.description
                          elseif (rParam->value_type = "DEFINITION")
                            cv.definition
                          else
                            cv.display
                          endif
    from    code_value      cv
    plan cv
        where cv.code_set = rParam->code_set
        and parser(cParser)
        and parser(cLimitParser)
        and parser(cDefaultParser)
        and cv.active_ind = 1
        and cv.end_effective_dt_tm > sysdate
    order display_value        
    head report
        rCustom->status.message = "Ok."
        
        nCount = 0
        
    ; WARNING: Detail section must write to rCustom->data[].key and rCustom->data[].value        
    detail
        nCount = nCount + 1
        stat = alterlist(rCustom->data, nCount)
        rCustom->data[nCount].key = cv.code_value
        rCustom->data[nCount].value = display_value
        
    with expand=1, counter        

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
/*************************************************************************
 
        Script Name:    1co5_convert_blob.prg
 
        Description:    Clinical Office - MPage Developer
        				Uses TDBExecute statement to convert Blobs between
        				formats (e.g. Postscript to RTF to HTML
 
        Date Written:   November 10, 2021
        Written by:     John Simpson
                        Precision Healthcare Solutions
 
 *************************************************************************
		   Copyright (c) 2021 Precision Healthcare Solutions
 
 NO PART OF THIS CODE MAY BE COPIED, MODIFIED OR DISTRIBUTED WITHOUT
 PRIOR WRITTEN CONSENT OF PRECISION HEALTHCARE SOLUTIONS EXECUTIVE
 LEADERSHIP TEAM.
 
 FOR LICENSING TERMS PLEASE VISIT www.clinicaloffice.com/mpage/license
 
 *************************************************************************
                            Special Instructions
 *************************************************************************
 1. Execute this script in your CCL script.
 2. Populate co_blob_events from inside your CCL script.
 3. call convert_blob(format) where format is your desired format (e.g. HTML, PDF, RTF)
 4. Copy the contents of co_blob_events[]->converted_text to where you need it.
 
 Any format other than AS (ASCII text) will be Hex Encoded
 *************************************************************************
                            Revision Information
 *************************************************************************
 Rev    Date     By             Comments
 ------ -------- -------------- ------------------------------------------
 001    11/10/21 J. Simpson     Initial Development
 002    05/12/22 J. Simpson     Created convert_text routine to allow passing non-blob content
 *************************************************************************/
 
drop program 1co5_convert_blob:group1 go
create program 1co5_convert_blob:group1
 
; Set the absolute maximum possible variable length
set modify maxvarlen 268435456
 
; Define temporary structure
free record co_blob_events
record co_blob_events (
    1 data[*]
        2 event_id              = f8
        2 original_format_cd    = f8
        2 original_text         = vc
        2 converted_text        = vc
) with persist
 
; Misc variables
free define nNum
declare nNum = i4 with persist
 
; Define the conversion function
declare convert_blob(cFormat=vc)=null with persist
declare convert_text(cFormatTo=vc)=null with persist

; Convert CE_BLOB data 
subroutine convert_blob(cFormat)
 
    ; Declare blob working variables
    declare good_blob = vc
    declare outbuf = c32768
    declare blobout = vc
    declare retlen = i4
    declare offset = i4
    declare newsize = i4
    declare finlen = i4
    declare xlen=i4 
 
    ; Declare Code Values
    declare cv8_Auth = f8 with noconstant(uar_get_code_by("MEANING", 8, "AUTH"))
    declare cv8_Modified = f8 with noconstant(uar_get_code_by("MEANING", 8, "MODIFIED"))
    declare cv15751_True = f8 with noconstant(uar_get_code_by("MEANING", 15751, "T"))
 
 
    ; Read the blob results
    if (size(co_blob_events->data, 5) > 0)
 
        select into "nl:"
        from	ce_blob_result          cbr,
                ce_blob	                cb
        plan cbr
            where expand(nNum, 1, size(co_blob_events->data, 5), cbr.event_id, co_blob_events->data[nNum].event_id)
            and cbr.valid_until_dt_tm > sysdate
        join cb
	       where cb.event_id = cbr.event_id
	       and cb.valid_until_dt_tm > sysdate
        order cb.event_id, cb.blob_seq_num
        head cb.event_id
            for (x = 1 to (cb.blob_length/32768))
                blobout = notrim(concat(notrim(blobout),notrim(fillstring(32768, " "))))
            endfor
            finlen = mod(cb.blob_length,32768)
 
            blobout = notrim(concat(notrim(blobout),notrim(substring(1,finlen,fillstring(32768, " ")))))
 
            good_blob = " "	; Clear the last value
        detail
            retlen = 1
            offset = 0
 
            while (retlen > 0)
                retlen = blobget(outbuf, offset, cb.blob_contents)
                offset = offset + retlen
                if(retlen != 0)
                    xlen = findstring("OCF_BLOB",outbuf,1)-1
 
                    if(xlen<1)
                        xlen = retlen
                    endif
 
                    good_blob = notrim(concat(notrim(good_blob), notrim(substring(1,xlen,outbuf))))
                endif
            endwhile
        foot cb.event_id
            newsize = 0
            good_blob = concat(notrim(good_blob),"OCF_BLOB")
            blob_un = uar_ocf_uncompress(good_blob, size(good_blob),
                            blobout, size(blobout), newsize)
 
            ; Write to memory
            nPos = locateval(nNum, 1, size(co_blob_events->data, 5), cbr.event_id, co_blob_events->data[nNum].event_id)
            co_blob_events->data[nPos].original_format_cd = cbr.format_cd
            co_blob_events->data[nPos].original_text = substring(1, newsize, blobout)
        with expand=1, rdbarrayfetch = 1

        ; Convert the text
        call convert_text(cFormat)
 
    endif
end

; Convert the text
subroutine convert_text(cFormat)

    declare cEncoded = vc
    declare iBase64Size = i4
 
    declare uar_si_encode_base64((p1=vc(ref)),(p2=i4(ref)),(p3=i4(ref)))=vc


    free record 969553_request
    record 969553_request (
        1 desired_format_cd = f8
        1 origin_format_cd = f8
        1 origin_text = gvc
        1 page_height = vc
        1 page_width = vc
        1 page_margin_top = vc
        1 page_margin_bottom = vc
        1 page_margin_left = vc
        1 page_margin_right = vc
    )
 
    free record 969553_reply
    record 969553_reply (
        1 converted_text = gvc
        1 status_data
            2 status = c1
            2 subeventstatus [*]
                3 OperationName = c25
                3 OperationStatus = c1
                3 TargetObjectName = c25
                3 TargetObjectValue = vc
    )

    ; Loop through each result and perform the conversion
    for (nLoop = 1 to size(co_blob_events->data, 5))
        set stat = initrec(969553_request)
        set stat = initrec(969553_reply)
 
        set 969553_request->origin_text = trim(co_blob_events->data[nLoop].original_text)
        set 969553_request->origin_format_cd = co_blob_events->data[nLoop].original_format_cd
        set 969553_request->desired_format_cd = value(uar_get_code_by("MEANING", 23, cnvtupper(cFormat)))
 
        ; Execute the conversion
        set stat = tdbexecute(3202004, 3202004, 969553, "REC", 969553_request, "REC", 969553_reply)
 
        ; Write the reply back
        if (969553_reply->status_data->status = "S")
            if (cnvtupper(cFormat) = "AS")
                set co_blob_events->data[nLoop].converted_text = 969553_reply->converted_text
            else
                ; Convert non-text to hex to prevent html send errors
                set co_blob_events->data[nLoop].converted_text = cnvtrawhex(969553_reply->converted_text)
            endif
        endif
    endfor

end
 
end go
/*************************************************************************
 
        Script Name:    1co_enc_search.prg
 
        Description:    Clinical Office - mPage Edition
                        Person/Encounter Search Component CCL Support Script
 
        Date Written:   August 12, 2025
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
 Called from Patient/Encounter search component
 *************************************************************************
                            Revision Information
 *************************************************************************
 Rev    Date     By             Comments
 ------ -------- -------------- ------------------------------------------
 001    08/12/25 J. Simpson     Initial Development
 *************************************************************************/
drop program 1co5_enc_search:group1 go
create program 1co5_enc_search:group1
 
; Clear and define rCustom structure
free record rCustom
record rCustom (
    1 status
        2 type                  = vc
        2 message               = vc
        2 code                  = i4
) with persistscript

free record rTemp
record rTemp (
    1 qual_match                = i4       ; Global Qualification Match Level
    1 data[*]
        2 person_id             = f8
        2 encntr_id             = f8
        2 qual_match            = i4       ; Person/Encounter specific Qualification Match Level
)
 
; Declare variables and subroutines
declare nNum = i4
set rCustom->status->type = "person"
declare addTemp(nPersonId=f8, nEncntrId=f8, nForceAdd=i4)=null
subroutine addTemp(nPersonId, nEncntrId, nForceAdd)
    if (nForceAdd = 1)
        set nNum = 0
    else
        set nNum = locateval(nNum, 1, size(rTemp->data, 5), nPersonId, rTemp->data[nNum].person_id)
    endif
            
    if (nNum > 0)
        call echorecord(rTemp->data[nNum])
        if (nEncntrId in (0.0, rTemp->data[nNum].encntr_id))
            set rTemp->data[nNum].qual_match = rTemp->data[nNum].qual_match + 1
        endif
    else
        set stat = alterlist(rTemp->data, size(rTemp->data, 5)+1)
        set rTemp->data[size(rTemp->data, 5)].person_id = nPersonId
        set rTemp->data[size(rTemp->data, 5)].encntr_id = nEncntrId
        set rTemp->data[size(rTemp->data, 5)].qual_match = rTemp->qual_match
    endif
end
 
; Collect code values
declare cv48_Active = f8 with noconstant(uar_get_code_by("MEANING", 48, "ACTIVE"))
declare cv302_Person = f8 with noconstant(uar_get_code_by("MEANING", 302, "PERSON"))

call echorecord(payload->customscript->script[nscript]->parameters)

; Search ENCNTR_ALIAS
if (validate(payload->customscript->script[nscript]->parameters->encntrAlias) > 0)
    if (size(payload->customscript->script[nscript]->parameters->encntrAlias, 5) > 0)
        set rTemp->qual_match = rTemp->qual_match + 
            size(payload->customscript->script[nscript]->parameters->encntrAlias, 5)   ; Update the qualification check level
    
        select into "nl:"
            person_id           = e.person_id
        from    encntr_alias            ea,
                encounter               e
        plan ea
            where expand(nNum, 1, size(payload->customscript->script[nscript]->parameters->encntrAlias, 5), ea.alias, 
                payload->customscript->script[nscript]->parameters->encntrAlias[nNum].alias,
                ea.encntr_alias_type_cd, 
                uar_get_code_by("MEANING", 319, payload->customscript->script[nscript]->parameters->encntrAlias[nNum].cdfMeaning))
            and ea.active_status_cd = cv48_Active
            and ea.active_ind = 1
            and ea.end_effective_dt_tm > sysdate
        join e
            where e.encntr_id = ea.encntr_id        
        order person_id        
        detail        
            call addTemp(person_id, ea.encntr_id, 0)
        with expand=1, counter        
    endif
endif

; Search PERSON_ALIAS
if (validate(payload->customscript->script[nscript]->parameters->personAlias) > 0)
    if (size(payload->customscript->script[nscript]->parameters->personAlias, 5) > 0)
        set rTemp->qual_match = rTemp->qual_match + 
            size(payload->customscript->script[nscript]->parameters->personAlias, 5)   ; Update the qualification check level
    
        select into "nl:"
            person_id           = pa.person_id
        from    person_alias            pa
        plan pa
            where expand(nNum, 1, size(payload->customscript->script[nscript]->parameters->personAlias, 5), pa.alias, 
                payload->customscript->script[nscript]->parameters->personAlias[nNum].alias,
                pa.person_alias_type_cd, 
                uar_get_code_by("MEANING", 4, payload->customscript->script[nscript]->parameters->personAlias[nNum].cdfMeaning))
            and pa.active_status_cd = cv48_Active
            and pa.active_ind = 1
            and pa.end_effective_dt_tm > sysdate
        order person_id        
        detail        
            call addTemp(person_id, 0.0, 0)
        with expand=1, counter        
    endif        
endif

; Search the PERSON table
declare cFirstName = vc
declare cLastName = vc
declare dBirthDate = dq8
declare cPersonSearchParser = vc with noconstant("1=1")
declare nPersonCount = i4

; Setup name values
if (validate(payload->customscript->script[nscript]->parameters->fullname, "") != char(42))
    set cLastName = trim(cnvtupper(piece(payload->customscript->script[nscript]->parameters->fullname, "," , 1,
                            payload->customscript->script[nscript]->parameters->fullname)),3)
    set cFirstName = trim(cnvtupper(piece(payload->customscript->script[nscript]->parameters->fullname, "," , 2, "")),3)
endif

if (validate(payload->customscript->script[nscript]->parameters->lastname, "") != char(42))
    set cLastName = cnvtupper(payload->customscript->script[nscript]->parameters->lastname)
endif

if (validate(payload->customscript->script[nscript]->parameters->firstname, "") != char(42))
    set cFirstName = cnvtupper(payload->customscript->script[nscript]->parameters->firstname)
endif

if (size(cFirstName) > 0)
    set cPersonSearchParser = "p.name_first_key = patstring(cFirstName)"
endif

; Setup DOB values
if (validate(payload->customscript->script[nscript]->parameters->birthdate, "") != "")
    set dBirthDate = cnvtdate2(payload->customscript->script[nscript]->parameters->birthdate, "yyyy-mm-dd")

    set cPersonSearchParser = concat(trim(cPersonSearchParser), 
            ^ and p.birth_dt_tm between cnvtdatetime("^, format(dBirthDate, ^DD-MMM-YYYY;;D^),
            ^") and cnvtdatetime("^, format(dBirthDate, ^DD-MMM-YYYY 23:59:59;;D^), ^")^)
endif

; Setup SexCd values
if (validate(payload->customscript->script[nscript]->parameters->sexCd) > 0)
    if (payload->customscript->script[nscript]->parameters->sexCd > 0)
        set cPersonSearchParser = concat(trim(cPersonSearchParser), 
            ^ and p.sex_cd = ^, cnvtstring(cnvtreal(payload->customscript->script[nscript]->parameters->sexCd)))
    endif            
endif

call echo("test zone")
call echo(cLastName)
call echo(cFirstName)
call echo(cPersonSearchParser)

; Collect a count of the last name
if (size(cLastName) > 1)
    set rTemp->qual_match = rTemp->qual_match + 1

    select into "nl:"
        personCount = count(p.person_id)
    from    person          p
    plan    p
        where p.name_last_key = patstring(cLastName)
        and parser(cPersonSearchParser)
        and exists (select pp.person_id from person_patient pp where pp.person_id = p.person_id)
        and p.person_type_cd = cv302_Person
        and p.active_ind = 1
        and p.active_status_cd = cv48_Active
    detail
        nPersonCount = personCount
        if (personCount > 1000)
            rCustom->status.code = 1000
            rCustom->status.message = concat("More than 1000 patients returned. Please modify your search.")
        endif
    with counter        

    ; Only process up to 1000 patients.
    if (nPersonCount <= 1000)
    
    select into "nl:"
    from    person          p
    plan    p
        where p.name_last_key = patstring(cLastName)
        and parser(cPersonSearchParser)
        and p.person_type_cd = cv302_Person
        and exists (select pp.person_id from person_patient pp where pp.person_id = p.person_id)
        and p.active_ind = 1
        and p.active_status_cd = cv48_Active
    detail
        call addTemp(p.person_id, 0.0, 0)        
    with counter        
    
    endif
endif

; Collect encounters for the selected person_id
if (validate(payload->customscript->script[nscript]->parameters->personId, 0) > 0)

    call echo( payload->customscript->script[nscript]->parameters->personId)

;    if (payload->customscript->script[nscript]->parameters->personId > 0)
        set rCustom->status->type = "encounter"
;    endif        
    
    select ;into "nl:"
        e.*
    from    encounter           e
    plan e
        where e.person_id = payload->customscript->script[nscript]->parameters->personId
        and e.active_status_cd = cv48_Active
        and e.active_ind = 1
        and e.end_effective_dt_tm > sysdate
    head report
        rTemp->qual_match = rTemp->qual_match + 1        
    detail
        call addTemp(e.person_id, e.encntr_id, 1)
    with counter
    
endif

call echorecord(rtemp)

; Update the Patient Source with the valid qualifiers
for (nLoop = 1 to size(rTemp->data, 5))
    if (rTemp->data[nLoop].qual_match = rTemp->qual_match)
        ; Person Level
        set nNum = locateval(nNum, 1, size(patient_source->patients, 5), rTemp->data[nLoop].person_id, 
                                    patient_source->patients[nNum].person_id)
        if (nNum = 0)
            set stat = alterlist(patient_source->patients, size(patient_source->patients, 5) + 1)
            set patient_source->patients[size(patient_source->patients, 5)].person_id = rTemp->data[nLoop].person_id
        endif            
        
        ; Encounter Level
        if (rTemp->data[nLoop].encntr_id > 0.0)
            set stat = alterlist(patient_source->visits, size(patient_source->visits, 5) + 1)
            set patient_source->visits[size(patient_source->visits, 5)].person_id = rTemp->data[nLoop].person_id
            set patient_source->visits[size(patient_source->visits, 5)].encntr_id = rTemp->data[nLoop].encntr_id
        endif
    endif
endfor



; Update the status values
if (rCustom->status.code = 0)
    if (size(patient_source->patients, 5) = 0 or 
        (payload->customscript->script[nscript]->parameters->personId > 0 and size(patient_source->visits, 5) = 0))
        set rCustom->status.code = 99
        set rCustom->status.message = "No records qualified."
    endif        
endif

call add_custom_output(cnvtrectojson(rCustom, 4, 1))

call echo("Done First Search")
 
#end_program
 
end go
/*************************************************************************
 
        Script Name:    1co5_enc_search_data.prg
 
        Description:    Clinical Office - MPage Developer
                        Person/Encounter Search Component Data CCL Support Script
 
        Date Written:   March 3, 2022
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
 Called from Patient/Encounter search component and used to display the
 output for collected person/encounter records.
 *************************************************************************
                            Revision Information
 *************************************************************************
 Rev    Date     By             Comments
 ------ -------- -------------- ------------------------------------------
 001    03/03/22 J. Simpson     Initial Development
 002    09/13/25 J. Simpson     Added VIP, Address and Phone numbers
 *************************************************************************/
drop program 1co5_enc_search_data:group1 go
create program 1co5_enc_search_data:group1
  
; Clear and define rCustom structure
free record rCustom
record rCustom (
    1 person[*]
        2 person_id             = f8
        2 name                  = vc
        2 vip                   = vc
        2 mrn                   = vc
        2 sex                   = vc
        2 birth_date            = dq8
        2 age                   = vc
        2 home_address          = vc
        2 phone_numbers         = vc
        2 ethnic_group          = vc
    1 encounter[*]
        2 encntr_id             = f8
        2 person_id             = f8
        2 fin_number            = vc
        2 facility              = vc
        2 nurse_unit            = vc
        2 room_bed              = vc
        2 reg_dt_tm             = dq8
        2 disch_dt_tm           = dq8
        2 medical_service       = vc
        2 encounter_type        = vc
        2 attending_physician   = vc
)
 
; Declare variables and subroutines
declare nNum = i4
declare cAddress = vc
declare cPhone = vc
 
; Collect code values
declare cv4_MRN = f8 with noconstant(uar_get_code_by("MEANING", 4, "MRN"))
declare cv43_Home = f8 with noconstant(uar_get_code_by("MEANING", 43, "HOME"))
declare cv43_Mobile = f8 with noconstant(uar_get_code_by("MEANING", 43, "MOBILE"))
declare cv43_Business = f8 with noconstant(uar_get_code_by("MEANING", 43, "BUSINESS"))
declare cv48_Active = f8 with noconstant(uar_get_code_by("MEANING", 48, "ACTIVE"))
declare cv212_Home = f8 with noconstant(uar_get_code_by("MEANING", 212, "HOME"))
declare cv319_FinNbr = f8 with noconstant(uar_get_code_by("MEANING", 319, "FIN NBR"))
declare cv333_AttendDoc = f8 with noconstant(uar_get_code_by("MEANING", 333, "ATTENDDOC"))

; Collect the person level data
if (size(patient_source->patients, 5) > 0)

    select into "nl:"
        name            = p.name_full_formatted
    from    person              p,
            person_alias        pa
    plan p
        where expand(nNum, 1, size(patient_source->patients, 5), p.person_id, patient_source->patients[nNum].person_id)
        and p.active_status_cd = cv48_Active
    join pa
        where pa.person_id = outerjoin(p.person_id)
        and pa.person_alias_type_cd = outerjoin(cv4_MRN)
        and pa.active_status_cd = outerjoin(cv48_Active)
        and pa.active_ind = outerjoin(1)
        and pa.end_effective_dt_tm > outerjoin(sysdate)
    order name, p.person_id
    head report
        nCount = 0
    head name
        x = 0
    head p.person_id
        nCount = nCount + 1
        stat = alterlist(rCustom->person, nCount)
        
        rCustom->person[nCount].person_id = p.person_id
        rCustom->person[nCount].name = p.name_full_formatted
        rCustom->person[nCount].vip = uar_get_code_display(p.vip_cd)
        rCustom->person[nCount].mrn = cnvtalias(pa.alias, pa.alias_pool_cd)
        rCustom->person[nCount].sex = uar_get_code_display(p.sex_cd)
        rCustom->person[nCount].birth_date = p.birth_dt_tm
        rCustom->person[nCount].age = trim(cnvtage(p.birth_dt_tm),3)
        rCustom->person[nCount].ethnic_group = uar_get_code_display(p.ethnic_grp_cd)
    with expand=1
    
    ; Address
    select into "nl:"
    from    address         a
    plan a
        where expand(nNum, 1, size(rCustom->person, 5), a.parent_entity_id, rCustom->person[nNum].person_id)
        and a.parent_entity_name = "PERSON"
        and a.address_type_cd = cv212_Home
        and a.active_ind = 1
        and a.active_status_cd = cv48_Active
        and a.beg_effective_dt_tm < sysdate
        and a.end_effective_dt_tm > sysdate
    detail
        cAddress = a.street_addr
        nPos = locateval(nNum, 1, size(rCustom->person, 5), a.parent_entity_id, rCustom->person[nNum].person_id)
        cAddress = combine_strings(cAddress, a.street_addr2, ", ")
        cAddress = combine_strings(cAddress, a.street_addr3, ", ")
        cAddress = combine_strings(cAddress, a.street_addr4, ", ")
        cAddress = combine_strings(cAddress, a.city, ", ")
        cAddress = combine_strings(cAddress, evaluate(a.state_cd, 0, a.state, uar_get_code_display(a.state_cd)), ", ")
        cAddress = combine_strings(cAddress, evaluate(a.country_cd, 0, a.country, uar_get_code_display(a.country_cd)), ", ")
        cAddress = combine_strings(cAddress, a.zipcode, " ")
        
        rCustom->person[nNum].home_address = cAddress
    with expand=1
    
    ; Phone
    select into "nl:"
        parent_entity_id    = p.parent_entity_id,
        phone_type          = uar_get_code_display(p.phone_type_cd)
    from    phone           p
    plan p
        where expand(nNum, 1, size(rCustom->person, 5), p.parent_entity_id, rCustom->person[nNum].person_id)
        and p.parent_entity_name = "PERSON"
        and p.phone_type_cd in (cv43_Business, cv43_Home, cv43_Mobile)
        and p.active_ind = 1
        and p.active_status_cd = cv48_Active
        and p.beg_effective_dt_tm < sysdate
        and p.end_effective_dt_tm > sysdate
    order parent_entity_id, phone_type desc
    head parent_entity_id        
        cPhone = ""
    head phone_type
        cPhone = combine_strings(cPhone, build(phone_type, ":"), ", ")
        cPhone = combine_strings(cPhone, cnvtphone(p.phone_num, p.phone_format_cd), " ")
    foot parent_entity_id
        nPos = locateval(nNum, 1, size(rCustom->person, 5), p.parent_entity_id, rCustom->person[nNum].person_id)
        rCustom->person[nNum].phone_numbers = cPhone
    with expand=1
endif

; Collect the encounter level data
if (size(patient_source->visits, 5) > 0)

    select into "nl:"
        person_id             = e.person_id,
        encntr_id             = e.encntr_id,
        sort_key_1            = format(e.disch_dt_tm, "yyyymmddhhmm;;d"),
        sort_key_2            = format(e.reg_dt_tm, "yyyymmddhhmm;;d")
    from    encounter           e,
            encntr_alias        ea
    plan e
        where expand(nNum, 1, size(patient_source->visits, 5), e.encntr_id, patient_source->visits[nNum].encntr_id)
        and e.active_status_cd = cv48_Active
    join ea
        where ea.encntr_id = outerjoin(e.encntr_id)
        and ea.encntr_alias_type_cd = outerjoin(cv319_FinNbr)
        and ea.active_status_cd = outerjoin(cv48_Active)
        and ea.active_ind = outerjoin(1)
        and ea.end_effective_dt_tm > outerjoin(sysdate)
    order person_id, sort_key_1, sort_key_2 desc, encntr_id
    head report
        nCount = 0
    head person_id
        x = 0
    head sort_key_1
        x = 0
    head sort_key_2
        x = 0                
    head encntr_id
        nCount = nCount + 1
        stat = alterlist(rCustom->encounter, nCount)
        
        rCustom->encounter[nCount].encntr_id = e.encntr_id
        rCustom->encounter[nCount].person_id = e.person_id
        rCustom->encounter[nCount].medical_service = uar_get_code_display(e.med_service_cd)
        rCustom->encounter[nCount].facility = uar_get_code_display(e.loc_facility_cd)
        rCustom->encounter[nCount].nurse_unit = uar_get_code_display(e.loc_nurse_unit_cd)
        if (e.loc_bed_cd > 0.0)
            rCustom->encounter[nCount].room_bed = concat(trim(uar_get_code_display(e.loc_room_cd)), "-",
                                    trim(uar_get_code_display(e.loc_bed_cd)))
        else                                    
            rCustom->encounter[nCount].room_bed = uar_get_code_display(e.loc_room_cd)
        endif
        rCustom->encounter[nCount].reg_dt_tm = e.reg_dt_tm
        rCustom->encounter[nCount].disch_dt_tm = e.disch_dt_tm
        rCustom->encounter[nCount].fin_number = cnvtalias(ea.alias, ea.alias_pool_cd)
        rCustom->encounter[nCount].encounter_type = uar_get_code_display(e.encntr_type_cd)
    with expand=1
    
    ; Collect the attending physician
    select into "nl:"
    from    encntr_prsnl_reltn          epr,
            prsnl                       p
    plan epr
        where expand(nNum, 1, size(rCustom->encounter, 5), epr.encntr_id, rCustom->encounter[nNum].encntr_id)
        and epr.encntr_prsnl_r_cd = cv333_AttendDoc
        and epr.active_ind = 1
        and epr.active_status_cd = cv48_Active
        and epr.end_effective_dt_tm > sysdate
    join p
        where p.person_id = epr.prsnl_person_id
    detail
        nPos = locateval(nNum, 1, size(rCustom->encounter, 5), epr.encntr_id, rCustom->encounter[nNum].encntr_id)
        rCustom->encounter[nPos].attending_physician = p.name_full_formatted
    with expand=1        
     
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
/*************************************************************************
 
        Script Name:    1co5_event_set_search.prg
 
        Description:    Clinical Office - MPage Developer
                        Event Set Search Component CCL Support Script
 
        Date Written:   May 3, 2025
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
 Called from select component
 *************************************************************************
                            Revision Information
 *************************************************************************
 Rev    Date     By             Comments
 ------ -------- -------------- ------------------------------------------
 001    06/13/25 J. Simpson     Initial Development
 *************************************************************************/
drop program 1co5_event_set_search:group1 go
create program 1co5_event_set_search:group1
 
; Declare variables and subroutines
declare nNum = i4
declare nDefault = i4
  
; Clear and define rCustom structure
free record rCustom
record rCustom (
    1 status
        2 error_ind                 = i4
        2 message                   = vc
        2 limit_met                 = i4
        2 full_data_set_loaded      = i4
    1 data[*]
        2 key                       = f8
        2 value                     = vc      
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
    1 limit_type                = vc
    1 limits[*]                 = vc
    1 default[*]                = f8
    1 parent_loc_cd[*]          = f8
    1 display_field             = vc
)

set rParam->search_ind = payload->customscript->script[nscript]->parameters->search
set rParam->search_limit = payload->customscript->script[nscript]->parameters->searchlimit
set rParam->physician_ind = payload->customscript->script[nscript]->parameters->physicianind
set rParam->code_set= payload->customscript->script[nscript]->parameters->codeset
set rParam->value_type = cnvtupper(payload->customscript->script[nscript]->parameters->valuetype)
set rParam->search_value = cnvtupper(payload->customscript->script[nscript]->parameters->searchvalue)
set rParam->limit_type = cnvtupper(payload->customscript->script[nscript]->parameters->codeSetLimitType)

if (size(payload->customscript->script[nscript]->parameters->codeSetLimits, 5) > 0)
    set stat = alterlist(rParam->limits, size(payload->customscript->script[nscript]->parameters->codeSetLimits, 5))
    for (nLoop = 1 to size(rParam->limits, 5))
        set rParam->limits[nLoop] = payload->customscript->script[nscript]->parameters->codeSetLimits[nLoop]
    endfor
endif

if (validate(payload->customscript->script[nscript]->parameters->default) = 1)
    if (size(payload->customscript->script[nscript]->parameters->default, 5) > 0)
        set stat = alterlist(rParam->default, size(payload->customscript->script[nscript]->parameters->default, 5))
        for (nLoop = 1 to size(rParam->default, 5))
            set rParam->default[nLoop] = cnvtreal(payload->customscript->script[nscript]->parameters->default[nLoop])
        endfor
    endif
endif

if (validate(payload->customscript->script[nscript]->parameters->parentcd) = 1)
    if (size(payload->customscript->script[nscript]->parameters->parentcd, 5) > 0)
        set stat = alterlist(rParam->parent_loc_cd, size(payload->customscript->script[nscript]->parameters->parentcd, 5))
        for (nLoop = 1 to size(rParam->parent_loc_cd, 5))
            set rParam->parent_loc_cd[nLoop] = payload->customscript->script[nscript]->parameters->parentcd[nLoop]
        endfor
    endif
endif

set rParam->display_field = "display"
if (validate(payload->customscript->script[nscript]->parameters->displayfield) = 1)
    set rParam->display_field = payload->customscript->script[nscript]->parameters->displayfield
endif

call echorecord(rParam)
;call echorecord(payload->customscript->script[nscript]->parameters)
 
; Custom parser declarations
declare cParser = vc with noconstant("1=1")
if (rParam->search_value != "")

    ; Build the parser
    set cParser = concat(^cnvtupper(cv2.^, rParam->value_type,  ^) = patstring(|^,
                                    trim(rParam->search_value), ^*|)^)

endif


; Build a parser for default values                                    
declare cDefaultParser = vc with noconstant("1=1")
if (size(rParam->default, 5) > 0)
    set cDefaultParser = ^expand(nNum, 1, size(rParam->default, 5), cv2.code_value, cnvtreal(rParam->default[nNum]))^
    set nDefault = 1
endif

declare cParentParser = vc with noconstant("1=1")
if (size(rParam->parent_loc_cd, 5) > 0)
    set cParentParser = ^expand(nNum, 1, size(rParam->parent_loc_cd, 5), cv.code_value, cnvtreal(rParam->parent_loc_cd[nNum]))^
endif

; Perform a limit check to determine if too many values exist to upload
; ---------------------------------------------------------------------
if (rParam->search_limit > 0)

    select into "nl:"
        row_count   = count(cv.code_value)
    from	code_value					cv,
    		v500_event_set_explode		vese,
    		code_value					cv2
    plan cv
		where cv.code_set = 93
        and parser(cParentParser)
        and cv.active_ind = 1
        and cv.end_effective_dt_tm > sysdate
	join vese
		where vese.event_set_cd = cv.code_value
	join cv2
		where cv2.code_value = vese.event_cd		
		and parser(cParser)

;		and vese.event_set_level = 1
    ; WARNING: Avoid modifying the detail section below or your code may fail
    detail
        if (row_count > rParam->search_limit)
            rCustom->status->limit_met = 1
        elseif (cParser = "1=1")
            rCustom->status->full_data_set_loaded = 1
        endif
        
        if (row_count > rParam->search_limit and size(rParam->default, 5) = 0)
            rCustom->status->error_ind = 1
            rCustom->status->message = concat(build(cnvtint(row_count)), " records retrieved. Limit is ", 
                                        build(rParam->search_limit), ".")
        endif
    with expand=1, nocounter    
endif

; Perform the load if search limit does not fail
if (rCustom->status->error_ind = 0 or nDefault = 1)
    if (rCustom->status->limit_met = 0)
        set cDefaultParser = "1=1"
    endif

    set rCustom->status.message = "No records qualified."

    select distinct into "nl:"
        display_value       = if (cnvtlower(rParam->display_field) = "description")
                                cv2.description
                              elseif (cnvtlower(rParam->display_field) = "definition")
                                cv2.definition
                              else
                                cv2.display
                              endif
    from	code_value					cv,
    		v500_event_set_explode		vese,
    		code_value					cv2
    plan cv
		where cv.code_set = 93
        and parser(cParentParser)
        and cv.active_ind = 1
        and cv.end_effective_dt_tm > sysdate
	join vese
		where vese.event_set_cd = cv.code_value
	join cv2
		where cv2.code_value = vese.event_cd		
		and parser(cParser)
		and parser(cDefaultParser)
    order display_value
    head report
        rCustom->status.message = "Ok."
        
        nCount = 0
        
    ; WARNING: Detail section must write to rCustom->data[].key and rCustom->data[].value        
    detail
        nCount = nCount + 1
        stat = alterlist(rCustom->data, nCount)
        rCustom->data[nCount].key = cv2.code_value
        rCustom->data[nCount].value = display_value
    with expand=1, counter        

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

/*************************************************************************
 
        Script Name:    1co_get_patient_list.prg
 
        Description:    Populate PATIENT_SOURCE structure with a patient list
        				based on the input of a PATIENT_LIST_ID value.
 
        Date Written:   July 22, 2019
        Written by:     John Simpson
                        Precision Healthcare Solutions
 
 *************************************************************************
                            Special Instructions
 *************************************************************************
 1. None
 *************************************************************************
                            Revision Information
 *************************************************************************
 Rev    Date     By             Comments
 ------ -------- -------------- ------------------------------------------
 001    07/22/19 J. Simpson     Initial Development
 *************************************************************************/
 
drop program 1co5_get_patient_list:group1 go
create program 1co5_get_patient_list:group1
 
prompt
	"PatientListId" = ""   ;* Enter or select the printer or file name to send this report to.
 
with PatientListId
 
declare nNum = i4
 
; Clear the patient source
set stat = initrec(patient_source)
 
; Define record structures
free record 600144_request
record 600144_request (
	1 patient_list_id					= f8
	1 prsnl_id							= f8
	1 definition_version				= i4
)
 
free record 600123_request
record 600123_request (
	1 patient_list_id					= f8
	1 patient_list_type_cd				= f8
	1 best_encntr_flag 					= i2
	1 arguments[*]
		2 argument_name					= vc
		2 argument_value				= vc
		2 parent_entity_name			= vc
		2 parent_entity_id				= f8
	1 encntr_type_filters[*]
		2 encntr_type_cd				= f8
		2 encntr_class_cd				= f8
	1 patient_list_name					= vc
	1 mv_flag							= i2
	1 rmv_pl_rows_flag					= i2
)
 
; Assign values to 600144_request
set 600144_request->patient_list_id = cnvtreal($PatientListId)
set 600144_request->prsnl_id = nPRSNL_ID
set 600144_request->definition_version = 1
 
; Retrieve the patient list argument data
set stat = tdbexecute(600005, 600024, 600144, "REC", 600144_request, "REC", 600144_reply)
 
; Assign values to 600123_request
select into "nl:"
	patient_list_id	 		= d.patient_list_id,
	patient_list_type_cd	= d.patient_list_type_cd,
	patient_list_name		= d.name
from	dcp_patient_list			d
plan d
	where d.patient_list_id = 600144_reply->patient_list_id
detail
	600123_request->patient_list_id = patient_list_id
	600123_request->patient_list_type_cd = patient_list_type_cd
	600123_request->best_encntr_flag = 1
	600123_request->patient_list_name = patient_list_name
	600123_request->mv_flag = -1
	600123_request->rmv_pl_rows_flag = 0
with nocounter
 
; Move the arguments from 600144 to 600123
set stat = moverec(600144_reply->arguments, 600123_request->arguments)
 
; Execute the patient list retrieval
set stat = tdbexecute(600005, 600024, 600123, "REC", 600123_request, "REC", 600123_reply)
 
; Populate the patient source structure
if (600123_reply->status_data->status = "S")
 
	select into "nl:"
		person_id		= 600123_reply->patients[d.seq].person_id,
		encntr_id		= 600123_reply->patients[d.seq].encntr_id
	from	(dummyt				d with seq=value(size(600123_reply->patients, 5)))
	order person_id, encntr_id
	head person_id
		stat = alterlist(patient_source->patients, size(patient_source->patients, 5)+1)
		patient_source->patients[size(patient_source->patients,5)].person_id = person_id
	head encntr_id
		stat = alterlist(patient_source->visits, size(patient_source->visits, 5)+1)
		patient_source->visits[size(patient_source->visits,5)].person_id = person_id
		patient_source->visits[size(patient_source->visits,5)].encntr_id = encntr_id
	with counter
 
endif
 
#end_program
 
end go
 
/*************************************************************************
 
        Script Name:    1co5_load_document.prg
 
        Description:    Clinical Office - MPage Developer
                        Returns document or reference to external document usable in MPages
 
        Date Written:   August 31, 2022
        Written by:     John Simpson
                        Precision Healthcare Solutions
 
 *************************************************************************
            Copyright (c) 2022 Precision Healthcare Solutions
 
 NO PART OF THIS CODE MAY BE COPIED, MODIFIED OR DISTRIBUTED WITHOUT
 PRIOR WRITTEN CONSENT OF PRECISION HEALTHCARE SOLUTIONS EXECUTIVE
 LEADERSHIP TEAM.
 
 FOR LICENSING TERMS PLEASE VISIT www.clinicaloffice.com/mpage/license
 
 *************************************************************************
                            Special Instructions
 *************************************************************************
 None
 *************************************************************************
                            Revision Information
 *************************************************************************
 Rev    Date     By             Comments
 ------ -------- -------------- ------------------------------------------
 001    03/31/22 J. Simpson     Initial Development
 *************************************************************************/
drop program 1co5_load_document:group1 go
create program 1co5_load_document:group1
  
; Clear and define rCustom structure
free record rCustom
record rCustom (
    1 status
        2 message               = vc
    1 document[*]
        2 event_id              = f8
        2 event_end_dt_tm       = dq8
        2 event_title_text      = vc
        2 event_cd              = f8
        2 event                 = vc
        2 event_tag             = vc
        2 result_status_cd      = f8
        2 result_status         = vc
        2 storage_cd            = f8
        2 storage               = vc
        2 format_cd             = f8
        2 format                = vc
        2 doc_content           = vc
        2 signature             = vc
        2 blob_handle           = vc
        2 image_url             = vc
)

set rCustom->status->message = "Invalid Parent Event Id"

; Declare variables and subroutines
if (validate(payload->customscript->script[nscript]->parameters->parentEventId) = 0)
    call echo("Problem in parameters")
    call echorecord(payload->customscript->script[nscript])
    go to end_program
endif
    
declare nParentEventId = f8 with noconstant(cnvtreal(payload->customscript->script[nscript]->parameters->parentEventId))

declare nNum = i4
declare cTemp = vc

; Collect code values
declare cv23_RTF = f8 with noconstant(uar_get_code_by("MEANING", 23, "RTF"))
declare cv23_AH = f8 with noconstant(uar_get_code_by("MEANING", 23, "AH"))
declare cv25_Blob = f8 with noconstant(uar_get_code_by("MEANING", 25, "BLOB"))
declare cv48_Active = f8 with noconstant(uar_get_code_by("MEANING", 48, "ACTIVE"))
declare cv53_Doc = f8 with noconstant(uar_get_code_by("MEANING", 53, "DOC"))
declare cv120_OCFCompression = f8 with noconstant(uar_get_code_by("MEANING", 120, "OCFCOMP"))
declare cv120_NoCompression = f8 with noconstant(uar_get_code_by("MEANING", 120, "NOCOMP"))

execute 1co5_convert_blob:group1

; Collect the document events
select into "nl:"
from    clinical_event      ce,
        ce_blob_result      cbr
plan ce
    where ce.parent_event_id = nParentEventId
    and ce.valid_until_dt_tm > sysdate
    and ce.event_class_cd = cv53_Doc
join cbr
    where cbr.event_id = ce.event_id
    and cbr.valid_until_dt_tm > sysdate
head report
    nCount = 0
    nBCount = 0
detail
    nCount = nCount + 1
    stat = alterlist(rCustom->document, nCount)
    
    rCustom->document[nCount].event_id = ce.event_id
    rCustom->document[nCount].event_end_dt_tm = ce.event_end_dt_tm
    rCustom->document[nCount].event_title_text = ce.event_title_text
    rCustom->document[nCount].event_cd = ce.event_cd
    rCustom->document[nCount].event = uar_get_code_display(ce.event_cd)
    rCustom->document[nCount].event_tag = ce.event_tag
    rCustom->document[nCount].result_status_cd = ce.result_status_cd
    rCustom->document[nCount].result_status = uar_get_code_display(ce.result_status_cd)
    rCustom->document[nCount].storage_cd = cbr.storage_cd
    rCustom->document[nCount].storage = uar_get_code_display(cbr.storage_cd)
    rCustom->document[nCount].format_cd = cbr.format_cd
    rCustom->document[nCount].format = uar_get_code_display(cbr.format_cd)
    rCustom->document[nCount].blob_handle = cbr.blob_handle
    if (cbr.storage_cd != cv25_Blob)
        rCustom->document[nCount].image_url = "Image"
    endif

    if (cbr.storage_cd = cv25_Blob)
        nBCount = nBCount + 1
        stat = alterlist(co_blob_events->data, nBCount)
        co_blob_events->data[nBCount].event_id = ce.event_id
        co_blob_events->data[nBCount].original_format_cd = cbr.format_cd
    endif
    
    rCustom->status.message = "Document Loaded."
with counter


; Convert any blob items
call convert_blob("HTML")

; Blend the results of the blob conversion into rCustom
if (size(co_blob_events->data, 5) > 0)

    select into "nl:"
    from    (dummyt         d with seq=value(size(rCustom->document, 5))),
            (dummyt         d2 with seq=value(size(co_blob_events->data, 5)))
    plan d
    join d2
        where rCustom->document[d.seq].event_id = co_blob_events->data[d2.seq].event_id
    detail
        rCustom->document[d.seq].doc_content = co_blob_events->data[d2.seq].converted_text
    with counter        

endif

; Look for signature lines
declare blobout = vc
declare blobNoRtf = vc
declare bSize = i4

; Clear the blob events
set stat = initrec(co_blob_events)

select into "nl:"
from    ce_event_note       cen,
        long_blob           lb
plan cen
    where expand(nNum, 1, size(rCustom->document, 5), cen.event_id, rCustom->document[nNum].event_id)
    and cen.valid_until_dt_tm > sysdate
join lb
    where lb.parent_entity_id = cen.ce_event_note_id
    and lb.parent_entity_name = "CE_EVENT_NOTE"
    and lb.active_ind = 1
head report
    nCount = 0    
detail
    blobout = notrim(fillstring(32768, " "))
        
    nCount = nCount + 1
    
    stat = alterlist(co_blob_events->data, nCount)    
    co_blob_events->data[nCount].event_id = cen.event_id

    if (cen.compression_cd = cv120_OCFCompression)
        unCompSize = 0
        blob_un = uar_ocf_uncompress(lb.long_blob, size(lb.long_blob), blobout, size(blobout), unCompSize)
        co_blob_events->data[nCount].original_format_cd = cv23_AH ;cv23_RTF
    else
        co_blob_events->data[nCount].original_format_cd = cv23_AH
        blobout = lb.long_blob
    endif

    co_blob_events->data[nCount].original_text = blobout    
    
with expand=1

call convert_text("HTML")

; Blend the signatures into rCustom
if (size(co_blob_events->data, 5) > 0)

    select into "nl:"
    from    (dummyt         d with seq=value(size(rCustom->document, 5))),
            (dummyt         d2 with seq=value(size(co_blob_events->data, 5)))
    plan d
    join d2
        where rCustom->document[d.seq].event_id = co_blob_events->data[d2.seq].event_id
    detail
        rCustom->document[d.seq].signature = co_blob_events->data[d2.seq].converted_text
    with counter        

endif

#end_program

call add_custom_output(cnvtrectojson(rCustom, 4, 1))
  
end go
/*************************************************************************
 
        Script Name:    1co5_location_routines
 
        Description:    Clinical Office - MPage Developer
        				Location Tree data source code
 
        Date Written:   September 1, 2022
        Written by:     John Simpson
                        Precision Healthcare Solutions
 
 *************************************************************************
		   Copyright (c) 2022 Precision Healthcare Solutions
 
 NO PART OF THIS CODE MAY BE COPIED, MODIFIED OR DISTRIBUTED WITHOUT
 PRIOR WRITTEN CONSENT OF PRECISION HEALTHCARE SOLUTIONS EXECUTIVE
 LEADERSHIP TEAM.
 
 FOR LICENSING TERMS PLEASE VISIT www.clinicaloffice.com/mpage/license
 
 *************************************************************************
                            Special Instructions
 *************************************************************************
 Used by 1co_location_tree and your custom CCL scripts to provide location
 tree functionality.
 *************************************************************************
                            Revision Information
 *************************************************************************
 Rev    Date     By             Comments
 ------ -------- -------------- ------------------------------------------
 001    09/01/22 J. Simpson     Initial Development
 *************************************************************************/

drop program 1co5_location_routines:group1 go
create program 1co5_location_routines:group1
 
prompt 
	"JsonParam" = "MINE"
	, "Field Name" = "" 

with JsonParam, fieldName
 
; Declare internal subroutines
declare LocationBranch(cBranchId = vc) = null with persist
declare GetLocationParent(nChildCd = f8, cParentType = vc) = f8 with persist
declare SelectedCount(cParentType = vc) = i4 with persist
declare AddFilteredLocation(cDisplayKey = vc, cCdfMeaning = vc) = null with persist
 
; Required Code values
declare cvFacility = f8 with noconstant(uar_get_code_by("MEANING", 222, "FACILITY"))
declare cvBuilding = f8 with noconstant(uar_get_code_by("MEANING", 222, "BUILDING"))
 
; Misc Variables
declare nRow = i4
declare nNum = i4
 
; Record structure to contain Full Location Hierarchy (some data excluded based on filters)
free record rAllLocations
record rAllLocations (
	1 data[*]
	    2 organization_id       = f8
		2 facility_cd			= f8
		2 building_cd           = f8
		2 unit_cd				= f8
		2 unit_type_cd			= f8
		2 room_cd				= f8
		2 bed_cd				= f8
		2 census_ind			= i4
	1 children[*]
		2 location_cd			= f8
		2 children				= i4
) with persist
 
; Record structure containing list of filtered locations
free record rFilteredLocations
record rFilteredLocations (
	1 data[*]
		2 location_cd			= f8
		2 meaning				= vc
		2 expand				= i4
) with persist
 
; Temporary record structure to handle exclusions
free record rLocationExclusion
record rLocationExclusion (
	1 data[*]
		2 location_cd			= f8
)
 
; Temporary structure to handle location group root values
free record rRoot
record rRoot (
	1 data[*]
		2 root_loc_cd			= f8
)
; Always need a 0 entry
set stat = alterlist(rRoot->data, 1)
set rRoot->data[1].root_loc_cd = 0
  
; Convert the incoming parameter string to a record structure and create appropriate parser strings
declare cJsonParam = vc with noconstant(concat(^{"param":{^, trim($JsonParam), ^}}^))
set stat = cnvtjsontorec(cJsonParam)
 
; Process the parameter of unit types to allow (e.g. NURSEUNIT, AMBULATORY, etc.)
declare cUnitParser = vc with noconstant("1=1")
declare cFaclParser = vc with noconstant("1=1")
declare cBldParser = vc with noconstant("bld.root_loc_cd = 0.0")
 
if (validate(param->showUnits)=1)
	set cUnitParser = ""
	for (nLoop = 1 to size(param->showUnits, 5))
		if (trim(cUnitParser) != "")
			set cUnitParser = concat(trim(cUnitParser),",")
		endif
		set cUnitParser = concat(trim(cUnitParser), ^"^, param->showUnits[nLoop], ^"^)
 
		; Populate the root location with HIM Root values
		if (param->showUnits[nLoop] = "HIM")
			select into "nl:"
			from	code_value			cv
			plan cv
				where cv.code_set = 220
				and cv.cdf_meaning = "HIMROOT"
				and cv.active_ind = 1
			detail
				stat = alterlist(rRoot->data, size(rRoot->data, 5) + 1)
				rRoot->data[size(rRoot->data, 5)].root_loc_cd = cv.code_value
			with counter
 
			set cBldParser = "expand(nRow, 1, size(rRoot->data, 5), bld.root_loc_cd, rRoot->data[nRow].root_loc_cd)"
		endif
 
	endfor
	if (trim(cUnitParser) != "")
		set cUnitParser = concat(^cv.cdf_meaning in (^, trim(cUnitParser), ^)^) 
	endif
endif
  
declare cMaxViewLevel = vc with noconstant(validate(param->maxViewLevel,"ALL")), persist
declare nOrgSecurity = i4 with noconstant(cnvtint(validate(param->orgSecurity,"1"))), persist	; Default to org security
declare nCensusInd = i4 with noconstant(cnvtint(validate(param->censusInd,"0"))), persist		; Default to not check census ind
declare cRootValue = vc with noconstant(validate(param->rootValue, "x")), persist
declare nRootCode = f8 with noconstant(1.0), persist
  
; Facility Parser
if (nOrgSecurity = 1 and reqinfo->updt_id > 0)
    set cFaclParser = concat("exists (select por.prsnl_org_reltn_id from prsnl_org_reltn por ",
                    "where por.organization_id = facl.organization_id and por.person_id = reqinfo->updt_id ",
                    "and por.active_ind=1 and por.end_effective_dt_tm > sysdate)")
endif
  
; Collect the all of the locations into memory
select into "nl:"
	organization_id		= facl.organization_id,
	fac_cd				= facl.location_cd,
	building_cd         = bld.parent_loc_cd,
	unit_cd				= unit.location_cd,
	unit_type_cd		= unit.location_type_cd,
	room_cd				= if (cMaxViewLevel = "ALL")
							room.child_loc_cd
						  endif,
	bed_cd				= if (cMaxViewLevel = "ALL")
							bed.child_loc_cd
						  endif,
	census_ind			= unit.census_ind
from	location			facl,       ; FACILITY
        location_group      fac,
		location_group		bld,
		location			unit,		; UNIT
		code_value			cv,
		location_group		room,
		location_group		bed
plan facl
    where facl.location_type_cd = cvFacility
	and facl.active_ind = 1
	and facl.end_effective_dt_tm > sysdate
	and parser(cFaclParser)
join fac
	where fac.parent_loc_cd = facl.location_cd
	and fac.location_group_type_cd = cvFacility
	and fac.root_loc_cd = 0.0
	and fac.active_ind = 1
	and fac.end_effective_dt_tm > sysdate
join bld
	where bld.parent_loc_cd = fac.child_loc_cd
	and bld.location_group_type_cd = cvBuilding
	and parser(cBldParser)
	and bld.active_ind = 1
	and bld.end_effective_dt_tm > sysdate
join unit
	where unit.location_cd = bld.child_loc_cd
	and unit.active_ind = 1
	and unit.end_effective_dt_tm > sysdate
join cv
	where cv.code_value = unit.location_cd
	and (bld.root_loc_cd = 0 or cv.cdf_meaning = "HIM")
	and parser(cUnitParser)
join room
	where room.parent_loc_cd = outerjoin(unit.location_cd)
	and room.root_loc_cd = outerjoin(0)
	and room.parent_loc_cd > outerjoin(0)
	and room.active_ind = outerjoin(1)
	and room.end_effective_dt_tm > outerjoin(sysdate)
join bed
	where bed.parent_loc_cd = outerjoin(room.child_loc_cd)
	and bed.root_loc_cd = outerjoin(0)
	and bed.parent_loc_cd > outerjoin(0)
	and bed.active_ind = outerjoin(1)
	and bed.end_effective_dt_tm > outerjoin(sysdate)
order fac_cd, building_cd, unit_cd, room_cd, bed_cd
;/*
head report
	nCount = 0
	nChildCount = 0
 
	; Used to count children at each level
	subroutine addChild(nLocCd, nChildren)
		nChildCount = nChildCount + 1
 
		stat = alterlist(rAllLocations->children, nChildCount)
		rAllLocations->children[nChildCount].location_cd = nLocCd
		rAllLocations->children[nChildCount].children = nChildren
	end
head fac_cd
	nFacCount = 0
head building_cd
    nBldCount = 0
    nFacCount = nFacCount + 1
head unit_cd
	nBldCount = nBldCount + 1
head room_cd
	x = 0
head bed_cd
    if (nCensusInd = 0 or census_ind = 1)
        nCount = nCount + 1
        stat = alterlist(rAllLocations->data, nCount)
 
        rAllLocations->data[nCount].organization_id = organization_id
        rAllLocations->data[nCount].facility_cd = fac_cd
        rAllLocations->data[nCount].building_cd = building_cd
        rAllLocations->data[nCount].unit_cd = unit_cd
        rAllLocations->data[nCount].unit_type_cd = unit_type_cd
        rAllLocations->data[nCount].census_ind = census_ind
        rAllLocations->data[nCount].room_cd = room_cd
        rAllLocations->data[nCount].bed_cd = bed_cd
    endif
foot room_cd
	x = 0
foot unit_cd
	x = 0
foot building_cd
	stat = addChild(building_cd, nBldCount)	
foot fac_cd
	stat = addChild(fac_cd, nFacCount)
;*/
with expand=1, counter
 
; If the ValueList prompt value contains a list of locations, populate the rFilteredLocations structure
; with either beds or units.
if (trim($fieldName) != "")
    call echo("-----------------------")
    call echo("Start")
    call echo("-----------------------")

    declare cFilterLocations = vc
    if (findstring(".", $fieldName) > 0 or findstring(">", $fieldName) > 0)
        set cFilterLocations = concat(^expand(nNum, 1, ^,
                                ^size(^, trim($fieldName), ^,5),^,
                                ^cv.code_value, cnvtreal(^, trim($fieldName),
                                ^[nNum]))^)
    else
        set cFilterLocations = concat(^expand(nNum, 1, ^,
                                ^size(payload->customscript->script[nscript]->parameters.^, trim($fieldName), ^,5),^,
                                ^cv.code_value, cnvtreal(payload->customscript->script[nscript]->parameters.^, trim($fieldName),
                                ^[nNum]))^)
    endif                                
    call echo(  cFilterLocations)                              

 
    ; Step 1 - Find the exclusion list to prevent entire branches from being collected
    select distinct into "nl:"
    	exclude_cd		= if (cv.code_value = rAllLocations->data[d.seq].building_cd)
                            rAllLocations->data[d.seq].facility_cd
					      elseif (cv.code_value = rAllLocations->data[d.seq].unit_cd)
                            rAllLocations->data[d.seq].building_cd
	        			  elseif (cv.code_value = rAllLocations->data[d.seq].room_cd)
                            rAllLocations->data[d.seq].unit_cd
                          elseif (cv.code_value = rAllLocations->data[d.seq].bed_cd)
                            rAllLocations->data[d.seq].room_cd
                          endif
        from	code_value			cv,
                (dummyt				d with seq=value(size(rAllLocations->data, 5)))
    plan cv
        where parser(cFilterLocations)
    ;	where cv.code_value = $ValueList
	   and cv.code_value != 0
    join d
    	where cv.code_value in (
		  rAllLocations->data[d.seq].facility_cd,
		  rAllLocations->data[d.seq].building_cd,
    	  rAllLocations->data[d.seq].unit_cd,
		  rAllLocations->data[d.seq].room_cd,
		  rAllLocations->data[d.seq].bed_cd
	   )
    order exclude_cd
    detail
    	if (exclude_cd > 0)
		  stat = alterlist(rLocationExclusion->data, size(rLocationExclusion->data, 5)+1)
    		rLocationExclusion->data[size(rLocationExclusion->data, 5)].location_cd = exclude_cd
	   endif
    with expand=1, counter
 
    set stat = initrec(rFilteredLocations)
  
    ; Collect the values that haven't been excluded
    select into "nl:"
        facility        = uar_get_code_description(rAllLocations->data[d.seq].facility_cd),
        building        = uar_get_code_description(rAllLocations->data[d.seq].building_cd),
        unit            = uar_get_code_description(rAllLocations->data[d.seq].unit_cd),
        room            = uar_get_code_description(rAllLocations->data[d.seq].room_cd),
        bed             = uar_get_code_description(rAllLocations->data[d.seq].bed_cd),
    	location_cd		= if (cMaxViewLevel = "ALL")
    						  rAllLocations->data[d.seq].bed_cd
					      else
					          rAllLocations->data[d.seq].unit_cd
					      endif
    from    code_value			cv,
		    (dummyt				d with seq=value(size(rAllLocations->data, 5)))
    plan cv
;	where cv.code_value = $ValueList
        where parser(cFilterLocations)
    	and cv.code_value != 0
	    and not exists (
		  select cv.code_value from code_value
		      where expand(nRow, 1, size(rLocationExclusion->data,5), cv.code_value, rLocationExclusion->data[nRow].location_cd)
		  )
    join d
	   where cv.code_value in (
            rAllLocations->data[d.seq].facility_cd,
            rAllLocations->data[d.seq].building_cd,
    		rAllLocations->data[d.seq].unit_cd,
            rAllLocations->data[d.seq].room_cd,
            rAllLocations->data[d.seq].bed_cd
	   )
    order facility, building, unit, room, bed, location_cd
    head report
	   nCount = 0 
    head facility
        x = 0
    head building
        x = 0    
    head unit
        x = 0
    head room
        x = 0
    head bed
    	x = 0
    head location_cd
    	if (location_cd > 0)
		  nCount = nCount + 1
    	  stat = alterlist(rFilteredLocations->data, nCount)
 
		  rFilteredLocations->data[nCount].location_cd = location_cd
		  rFilteredLocations->data[nCount].meaning = uar_get_code_meaning(location_cd)
	   endif
    with expand=1

endif

 
 ; Used to return a single level of location code values from the hierarchy
subroutine LocationBranch(cBranchId)
 
	set nBranchId = cnvtreal(cBranchId)
	set cBranchDisplay = uar_get_code_display(nBranchId)
	set cBranchMeaning = uar_get_code_meaning(nBranchId)
 
	set stat = initrec(rFilteredLocations)
 
	select into "nl:"
	nBranchId,cBranchMeaning,
		root_code			= if (nBranchId = 1.0)
								rAllLocations->data[d.seq].facility_cd
							  elseif (cBranchMeaning = "FACILITY")
							  	rAllLocations->data[d.seq].building_cd
							  elseif (cBranchMeaning = "BUILDING")
						  		rAllLocations->data[d.seq].unit_cd
							  elseif (cBranchMeaning in ("NURSEUNIT","AMBULATORY"))
							  	rAllLocations->data[d.seq].room_cd
							  elseif (cBranchMeaning = "ROOM")
							  	rAllLocations->data[d.seq].bed_cd
							  endif,
        expandable          = if ((nBranchId = 1.0 and rAllLocations->data[d.seq].building_cd > 0.0)
							     or (cBranchMeaning = "FACILITY" and rAllLocations->data[d.seq].unit_cd > 0.0)
							     or (cBranchMeaning = "BUILDING" and rAllLocations->data[d.seq].room_cd > 0.0)
							     or (cBranchMeaning in ("NURSEUNIT","AMBULATORY") and rAllLocations->data[d.seq].bed_cd > 0.0))							  	
                                1							  	
							  endif
	from	(dummyt				d with seq=value(size(rAllLocations->data, 5)))
	plan d
		where nBranchId in (
				1.0,
				rAllLocations->data[d.seq].facility_cd,
				rAllLocations->data[d.seq].building_cd,
				rAllLocations->data[d.seq].unit_cd,
				rAllLocations->data[d.seq].room_cd,
				rAllLocations->data[d.seq].bed_cd
		)
		and nBranchId > 0.0
	order root_code
;/*	
	head report
		nCount = 0
	head root_code
		nExpand = 0
		if (nBranchId = 1.0 or cBranchMeaning in ("FACILITY", "BUILDING")
			or (nBranchId = rAllLocations->data[d.seq].unit_cd and rAllLocations->data[d.seq].room_cd > 0))
			nExpand = 2
		endif
	detail
		nExpand = nExpand + 1
	foot root_code
		; Limit the maximum view level if passed in the parameters
		if (cMaxViewLevel != "FACILITY" and nBranchId = 1.0)
			x = 0
		elseif (
			(cMaxViewLevel = "FACILITY" and nBranchId = 1.0) or
			(cMaxViewLevel = "BUILDING" and uar_get_code_meaning(root_code)
									!= "FACILITY") or
			(cMaxViewLevel = "UNIT" and uar_get_code_meaning(root_code)
									not in ("FACILITY","BUILDING"))
			)
			nExpand = 0
		endif
 
		; Write the new record to the record structure
		nCount = nCount + 1
		stat = alterlist(rFilteredLocations->data, nCount)
 
		rFilteredLocations->data[nCount].location_cd = root_code
 
		if (nExpand > 1 and expandable = 1)
			rFilteredLocations->data[nCount].expand = 1
		endif ;*/
	with counter
end
 
end go
/*************************************************************************
 
        Script Name:    1co5_location_search.prg
 
        Description:    Clinical Office - MPage Developer
                        Location Search Component CCL Support Script
 
        Date Written:   May 6, 2025
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
 Called from select component
 *************************************************************************
                            Revision Information
 *************************************************************************
 Rev    Date     By             Comments
 ------ -------- -------------- ------------------------------------------
 001    05/06/25 J. Simpson     Initial Development
 *************************************************************************/

drop program 1co5_location_search:group1 go
create program 1co5_location_search:group1

; Declare variables and subroutines
declare nNum = i4
declare nDefault = i4
  
; Clear and define rCustom structure
free record rCustom
record rCustom (
    1 status
        2 error_ind                 = i4
        2 message                   = vc
        2 limit_met                 = i4
        2 full_data_set_loaded      = i4
    1 data[*]
        2 key                       = f8
        2 value                     = vc      
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
    1 limit_type                = vc
    1 limits[*]                 = vc
    1 default[*]                = f8
    1 parent_loc_cd[*]          = f8
    1 display_field             = vc
)

set rParam->search_ind = payload->customscript->script[nscript]->parameters->search
set rParam->search_limit = payload->customscript->script[nscript]->parameters->searchlimit
set rParam->physician_ind = payload->customscript->script[nscript]->parameters->physicianind
set rParam->code_set= payload->customscript->script[nscript]->parameters->codeset
set rParam->value_type = cnvtupper(payload->customscript->script[nscript]->parameters->valuetype)
set rParam->search_value = cnvtupper(payload->customscript->script[nscript]->parameters->searchvalue)
set rParam->limit_type = cnvtupper(payload->customscript->script[nscript]->parameters->codeSetLimitType)

if (size(payload->customscript->script[nscript]->parameters->codeSetLimits, 5) > 0)
    set stat = alterlist(rParam->limits, size(payload->customscript->script[nscript]->parameters->codeSetLimits, 5))
    for (nLoop = 1 to size(rParam->limits, 5))
        set rParam->limits[nLoop] = payload->customscript->script[nscript]->parameters->codeSetLimits[nLoop]
    endfor
endif

if (validate(payload->customscript->script[nscript]->parameters->default) = 1)
    if (size(payload->customscript->script[nscript]->parameters->default, 5) > 0)
        set stat = alterlist(rParam->default, size(payload->customscript->script[nscript]->parameters->default, 5))
        for (nLoop = 1 to size(rParam->default, 5))
            set rParam->default[nLoop] = cnvtreal(payload->customscript->script[nscript]->parameters->default[nLoop])
        endfor
    endif
endif

if (validate(payload->customscript->script[nscript]->parameters->parentcd) = 1)
    if (size(payload->customscript->script[nscript]->parameters->parentcd, 5) > 0)
        set stat = alterlist(rParam->parent_loc_cd, size(payload->customscript->script[nscript]->parameters->parentcd, 5))
        for (nLoop = 1 to size(rParam->parent_loc_cd, 5))
            set rParam->parent_loc_cd[nLoop] = payload->customscript->script[nscript]->parameters->parentcd[nLoop]
        endfor
    endif
endif

set rParam->display_field = "display"
if (validate(payload->customscript->script[nscript]->parameters->displayfield) = 1)
    set rParam->display_field = payload->customscript->script[nscript]->parameters->displayfield
endif

call echorecord(rParam)
;call echorecord(payload->customscript->script[nscript]->parameters)
 
; Custom parser declarations
declare cParser = vc with noconstant("1=1")
if (rParam->search_value != "")

    ; Build the parser
    set cParser = concat(^cnvtupper(parent.^, rParam->value_type,  ^) = patstring(|^,
                                    trim(rParam->search_value), ^*|)^)

endif


; Build a parser for default values                                    
declare cDefaultParser = vc with noconstant("1=1")
if (size(rParam->default, 5) > 0)
    set cDefaultParser = ^expand(nNum, 1, size(rParam->default, 5), cv.code_value, cnvtreal(rParam->default[nNum]))^
    set nDefault = 1
endif

declare cLimitParser = vc with noconstant("1=1")
if (size(rParam->limits, 5) > 0 and rParam->limit_type in 
        ("CDF_MEANING","DISPLAY","DISPLAY_KEY","DESCRIPTION","DEFINITION","CKI","CONCEPT_CKI"))
    set cLimitParser = concat(^expand(nNum, 1, size(rParam->limits, 5), parent.^, rParam->limit_type, 
                        ^, rParam->limits[nNum])^)
endif

declare cParentParser = vc with noconstant("1=1")
if (size(rParam->parent_loc_cd, 5) > 0)
    set cParentParser = ^expand(nNum, 1, size(rParam->parent_loc_cd, 5), cv.code_value, cnvtreal(rParam->parent_loc_cd[nNum]))^
endif

call echo(cParser)
call echo(cDefaultParser)
call echo(cLimitParser)
call echo(cParentParser)

; Perform a limit check to determine if too many values exist to upload
; ---------------------------------------------------------------------
if (rParam->search_limit > 0)

    select into "nl:"
        row_count   = count(cv.code_value)
    from    ((
                select parent_loc_cd=parent.code_value, child_loc_cd=parent.code_value, set_id=parent.code_value, set_level=1
                from    code_value      parent
                where  parent.code_set = 220
                and parser(cParser)
                and parser(cLimitParser)
                and parent.active_ind = 1
                and parent.end_effective_dt_tm > sysdate
                
                union all
 
            (   select lg.parent_loc_cd, lg.child_loc_cd, set_id=p.set_id, set_level=p.set_level+1
                from recursive_parent p, location_group lg
                where lg.child_loc_cd = p.parent_loc_cd
                and lg.root_loc_cd = 0.0
                and lg.active_ind = 1
 
                recursive (select parent_loc_cd, child_loc_cd, set_id, set_level from recursive_parent)
            ) with sqltype("F8","F8","F8","I4"),
            
            recursive = recursive_parent(parent_loc_cd, child_loc_cd, set_id, set_level)) vp),
            code_value  cv
    plan vp
    join cv 
        where cv.code_value = vp.parent_loc_cd
        and parser(cParentParser)
    order vp.set_id        
    ; WARNING: Avoid modifying the detail section below or your code may fail
    detail
        if (row_count > rParam->search_limit)
            rCustom->status->limit_met = 1
        elseif (cParser = "1=1")
            rCustom->status->full_data_set_loaded = 1
        endif
                
        if (row_count > rParam->search_limit and size(rParam->default, 5) = 0)
            rCustom->status->error_ind = 1
            rCustom->status->message = concat(build(cnvtint(row_count)), " records retrieved. Limit is ", 
                                        build(rParam->search_limit), ".")
        endif            
    with expand=1, nocounter    
endif

; Perform the load if search limit does not fail
if (rCustom->status->error_ind = 0 or nDefault = 1)

    if (rCustom->status->limit_met = 0)
        set cDefaultParser = "1=1"
    endif

    set rCustom->status.message = "No records qualified."

    select distinct into "nl:"
        display_value       = if (cnvtlower(rParam->display_field) = "description")
                                uar_get_code_description(vp.set_id)
                              elseif (cnvtlower(rParam->display_field) = "definition")
                                uar_get_definition(vp.set_id)
                              else
                                uar_get_code_display(vp.set_id)
                              endif
    from    ((
                select parent_loc_cd=parent.code_value, child_loc_cd=parent.code_value, set_id=parent.code_value, set_level=1
                from    code_value      parent
                where  parent.code_set = 220
                and parser(cParser)
                and parser(cLimitParser)
                and parent.active_ind = 1
                and parent.end_effective_dt_tm > sysdate
                
                union all
 
            (   select lg.parent_loc_cd, lg.child_loc_cd, set_id=p.set_id, set_level=p.set_level+1
                from recursive_parent p, location_group lg
                where lg.child_loc_cd = p.parent_loc_cd
                and lg.root_loc_cd = 0.0
                and lg.active_ind = 1
 
                recursive (select parent_loc_cd, child_loc_cd, set_id, set_level from recursive_parent)
            ) with sqltype("F8","F8","F8","I4"),
            
            recursive = recursive_parent(parent_loc_cd, child_loc_cd, set_id, set_level)) vp),
            code_value  cv
    plan vp
    join cv 
        where cv.code_value = vp.parent_loc_cd
        and cv.active_ind = 1
        and parser(cParentParser)
        and parser(cDefaultParser)
    order display_value, vp.set_id        
    head report
        rCustom->status.message = "Ok."
        
        nCount = 0
        
    ; WARNING: Detail section must write to rCustom->data[].key and rCustom->data[].value        
    detail
        nCount = nCount + 1
        stat = alterlist(rCustom->data, nCount)
        rCustom->data[nCount].key = vp.set_id
        rCustom->data[nCount].value = display_value
        
    with expand=1, counter        

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
/*************************************************************************
 
        Script Name:    1co5_location_tree.prg
 
        Description:    Clinical Office - MPage Developer
        				Location Tree
 
        Date Written:   September 1, 2022
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
 Used by 1co_location_tree and your custom CCL scripts to provide location
 tree functionality.
 *************************************************************************
                            Revision Information
 *************************************************************************
 Rev    Date     By             Comments
 ------ -------- -------------- ------------------------------------------
 001    09/01/22 J. Simpson     Initial Development
 *************************************************************************/

drop program 1co5_location_tree:group1 go
create program 1co5_location_tree:group1

; Define output structure
free record rCustom
record rCustom (
    1 data[*]
        2 parent_id     = f8
        2 id            = f8
        2 display       = vc
        2 expandable    = i4
        2 selected      = i4
        2 indeterminate = i4
        2 expanded      = i4
        2 dirty         = i4
        2 create_zone   = i4
)    

; Required variables
declare nBranchId = f8 with noconstant(payload->customscript->script[nscript]->parameters.branchid)
declare nNum = i4
declare cSearchMode = vc with noconstant("")
declare cParser = vc
declare qualifySearchRow(nRow = i4) = vc

call echorecord(payload->customscript->script[nscript]->parameters)

; Execute the code to load the locations
execute 1co5_location_routines:group1 payload->customscript->script[nscript]->parameters.scriptparams, ""

declare cSearchText = vc with noconstant("")
if (nBranchId = 0 and trim(payload->customscript->script[nscript]->parameters->searchText) != "")
    set cSearchText = cnvtlower(concat(trim(payload->customscript->script[nscript]->parameters->searchText),"*"))
    set cSearchMode = "search"
elseif (nBranchId = 1 and size(payload->customscript->script[nscript]->parameters->default,5) > 0)
    set cSearchMode = "value"
endif

; Normal drill down
;/*
if (nBranchId > 0)
    call LocationBranch(cnvtstring(nBranchId))
    
	select into "nl:"
		item_display		= uar_get_code_description(rFilteredLocations->data[d.seq].location_cd),
		item_keyvalue		= rFilteredLocations->data[d.seq].location_cd,
		item_expand_flag	= rFilteredLocations->data[d.seq].expand
	from	(dummyt				d with seq=value(size(rFilteredLocations->data, 5)))
	order item_display
	head report
	   nCount = 0
	detail
	   nCount = nCount + 1
	   stat = alterlist(rCustom->data, nCount)

       rCustom->data[nCount].parent_id = nBranchId
	   rCustom->data[nCount].id = item_keyvalue
	   rCustom->data[nCount].display = item_display
	   rCustom->data[nCount].expandable = item_expand_flag
	   rCustom->data[nCount].create_zone = 1
	with counter
endif
;*/

; Search	
if (nBranchId in (0,1) and cSearchMode in ("value","search"))

    ; Create entries for the branches that match the search
    select ; into "nl:"
        sort_key        = substring(1,50,qualifySearchRow(d.seq)),
        facility        = uar_get_code_description(rAllLocations->data[d.seq].facility_cd),
        building        = uar_get_code_description(rAllLocations->data[d.seq].building_cd),
        unit            = uar_get_code_description(rAllLocations->data[d.seq].unit_cd),
        room            = uar_get_code_description(rAllLocations->data[d.seq].room_cd),
        bed             = uar_get_code_description(rAllLocations->data[d.seq].bed_cd)
    from    (dummyt         d with seq=value(size(rAllLocations->data, 5)))
    plan d
        where qualifySearchRow(d.seq) != "XXXNOTFOUNDXXX"
;        or cSearchMode = "value"
    order sort_key, facility, building, unit, room, bed
;/*    
    head report
        position = 0
 
        subroutine addBranch(nParent, cBranchDisplay)
          cBranchMeaning = uar_get_code_meaning(nParent)
 
          nChild = 0.0
          nGrandChild = 0.0
           
		  if (cBranchMeaning = "FACILITY")
            nChild = rAllLocations->data[d.seq].building_cd
            nGrandChild = rAllLocations->data[d.seq].unit_cd
          elseif (cBranchMeaning = "BUILDING")          		      
            nChild = rAllLocations->data[d.seq].unit_cd
            nGrandChild = rAllLocations->data[d.seq].room_cd
          elseif (cBranchMeaning in ("NURSEUNIT","AMBULATORY"))
            nChild = rAllLocations->data[d.seq].room_cd
            nGrandChild = rAllLocations->data[d.seq].bed_cd
          elseif (cBranchMeaning = "ROOM")
            nChild = rAllLocations->data[d.seq].bed_cd
          endif
 
          ; Determine if next level down should be collected
          cChildMeaning = uar_get_code_meaning(nChild)
          case (cChildMeaning)
            of "BED": nChildLevel = 1
            of "ROOM": nChildLevel = 2
            of "NURSEUNIT": nChildLevel = 3
            of "AMBULATORY": nChildLevel = 3
            of "BUILDING": nChildLevel = 4
            of "FACILITY": nChildLevel = 5
          endcase
          
          col 10, cBranchMeaning, " child:", nChild, " childLevel:",  nChildLevel, " pos: ", position, row + 1
 
          if (nChildLevel >= position and nChild > 0.0)
 
            cChildDisplay = uar_get_code_description(nChild)
 
            nExpand = 0
            if ((cMaxViewLevel in ("ALL", "FACILITY") or
        	   (cMaxViewLevel = "UNIT" and nChildLevel > 3) or
        	   (cMaxViewLevel = "BUILDING"  and nChildLevel > 4) ; or
        	   ;(cMaxViewLevel = "FACILITY"  and nChildLevel > 5)
        	   ) and nGrandChild > 0)
                nExpand = 1
            endif
 
 
            ; Add the row if not previously added
            if (locateval(nNum, 1, size(rCustom->data, 5), nChild, rCustom->data[nNum].id) = 0)
            
                col 20, "***WRITE***", rCustom->data[nNum].id, " child: ", nChild, " gc: ", nGrandChild, row + 1
            
                nCount = size(rCustom->data, 5) + 1
                stat = alterlist(rCustom->data, nCount)
                rCustom->data[nCount].parent_id = nParent
                rCustom->data[nCount].id = nChild
                rCustom->data[nCount].display = cChildDisplay                
                rCustom->data[nCount].expandable = nExpand
                if (nChildLevel = position and cSearchMode = "value")
                    rCustom->data[nCount].selected = 1
                endif
                if (nChildLevel != position and nChildLevel > 0 and cSearchMode = "value")  
                    rCustom->data[nCount].indeterminate = 1
                endif
                if (nChildLevel > position)
                    rCustom->data[nCount].expanded = nExpand
                endif
                rCustom->data[nCount].dirty = 1
            endif
 
            call addBranch(nChild, cChildDisplay)
          endif
        end
    head sort_key
        position = cnvtint(piece(sort_key, "|", 1, "0"))
        
        ; Add the facility level if it doesn't exist already
        nFoundFAC = locateval(nNum, 1, size(rCustom->data, 5), rAllLocations->data[d.seq].facility_cd,
                    rCustom->data[nNum].id)
        if (nFoundFAC = 0) ; or rCustom->data[nFoundFac].create_zone)
            nCount = size(rCustom->data, 5) + 1
            stat = alterlist(rCustom->data, nCount)
                        
            rCustom->data[nCount].parent_id = 1.0
            rCustom->data[nCount].id = rAllLocations->data[d.seq].facility_cd
            rCustom->data[nCount].display = facility
            rCustom->data[nCount].expandable = 1
            rCustom->data[nCount].expanded = 1
            rCustom->data[nCount].dirty = 1
            if (position = 5)
                rCustom->data[nCount].selected = 1
            endif
            if (position between 1 and 4)
                rCustom->data[nCount].indeterminate = 1
            endif
        else
            rCustom->data[nFoundFAC].dirty = 1
            if (position between 1 and 4)
                rCustom->data[nFoundFAC].indeterminate = 1
            else
                rCustom->data[nFoundFAC].selected = 1
            endif
        endif

        if (position > 0)
            call addBranch(rAllLocations->data[d.seq].facility_cd, facility)
        endif
    with counter
;*/
endif

subroutine qualifySearchRow(nRow)
    set nReturnValue = 0
    set nQualValue = 0.0
 
    for (nSearchLoop = 5 to 1 by -1)  ; 1 = Bed ---> 5 = Facility
        if (cMaxViewLevel = "ALL" or
        	(cMaxViewLevel = "UNIT" and nSearchLoop > 2) or
        	(cMaxViewLevel = "BUILDING" and nSearchLoop > 3) or
        	(cMaxViewLevel = "FACILITY"  and nSearchLoop > 4))
 
            set nValue = 0.0
 
            ; Pick the correct search string
            case (nSearchLoop)
                of 1:   set cString = uar_get_code_description(rAllLocations->data[nRow].bed_cd)
                        set nValue = rAllLocations->data[nRow].bed_cd
                of 2:   set cString = uar_get_code_description(rAllLocations->data[nRow].room_cd)
                        set nValue = rAllLocations->data[nRow].room_cd
                of 3:   set cString = uar_get_code_description(rAllLocations->data[nRow].unit_cd)
                        set nValue = rAllLocations->data[nRow].unit_cd
                of 4:   set cString = uar_get_code_description(rAllLocations->data[nRow].building_cd)
                        set nValue = rAllLocations->data[nRow].building_cd
                of 5:   set cString = uar_get_code_description(rAllLocations->data[nRow].facility_cd)
                        set nValue = rAllLocations->data[nRow].facility_cd
            endcase

            ; Perform text search
            if (cSearchMode = "search" and trim(cSearchText) != "" 
                            and cnvtlower(cstring) = patstring(concat(trim(cSearchText), "*")))
                set nReturnValue = nSearchLoop
                set nQualValue = nValue
            elseif (cSearchMode = "value" and nValue > 0.0)
                if (locateval(nNum, 1, size(payload->customscript->script[nscript]->parameters->default, 5), nValue,
                    payload->customscript->script[nscript]->parameters->default[nNum]) > 0) 
                    set nReturnValue = nSearchLoop
                    set nQualValue = nValue
                endif
            endif

        endif
    endfor

    if (nQualValue = 0)
        return ("XXXNOTFOUNDXXX")
    else
        return (build(nReturnValue, "|", cnvtstring(nQualValue)))
    endif
end


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
/*************************************************************************
 
        Script Name:    1co5_mpage_allergy.prg
 
        Description:    Clinical Office - mPage Edition
        				Allergy Data Retrieval
 
        Date Written:   January 31, 2025
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
 Called from 1CO5_MPAGE_ENTRY. Do not attempt to run stand alone.
 
 Possible Payload values:
 
	"patientSource":[
		{"personId": value, "encntrId": value}
	],
	"allergy": {
		"reactions": true
		"comments": true
	},
	"typeList": [
		{"codeSet": value, "type": "value", "typeCd": value}
	]
 
 
 
 *************************************************************************
                            Revision Information
 *************************************************************************
 Rev    Date     By             Comments
 ------ -------- -------------- ------------------------------------------
 001    31/05/25 J. Simpson     Initial Development
 *************************************************************************/
 
drop program 1co5_mpage_allergy:group1 go
create program 1co5_mpage_allergy:group1
 
; Check to see if running from mPage entry script
if (validate(payload->allergy) = 0 or size(patient_source->patients, 5) = 0)
	go to end_program
endif

free record rAllergy 
record rAllergy (
	1 allergies[*]
		2 person_id						= f8
		2 encntr_id						= f8
		2 allergy_id					= f8
		2 allergy_instance_id			= i8
		2 substance						= vc
		2 substance_identifier			= vc
		2 substance_ft_desc				= vc
		2 substance_type                = vc
		2 substance_type_meaning        = vc
		2 reaction_class                = vc
		2 severity                      = vc
		2 source_of_info                = vc
		2 source_of_info_ft				= vc
		2 onset_dt_tm					= dq8
		2 reaction_status               = vc
		2 created_dt_tm					= dq8
		2 created_prsnl_id				= f8
		2 cancel_reason                 = vc
		2 cancel_dt_tm					= dq8
		2 cancel_prsnl_id				= f8
		2 verified_status_flag			= i4
		2 rec_src_vocab                 = vc
		2 rec_src_identifer				= vc
		2 rec_src_string				= vc
		2 onset_precision               = vc
		2 onset_precision_flag			= i4
		2 reviewed_dt_tm				= dq8
		2 reviewed_prsnl_id				= f8
		2 orig_prsnl_id					= f8
		2 reaction_status_dt_tm			= dq8
		2 substance_type_cd				= f8
		2 reaction_class_cd				= f8
		2 severity_cd					= f8
		2 source_of_info_cd				= f8
		2 reaction_status_cd			= f8
		2 cancel_reason_cd				= f8
		2 rec_src_vocab_cd				= f8
		2 onset_precision_cd			= f8
		2 reaction[*]
			3 reaction					= vc
			3 reaction_identifier		= vc
			3 reaction_ftdesc			= vc
		2 comment[*]
			3 comment_dt_tm				= dq8
			3 comment_prsnl_id			= f8
			3 allergy_comment			= vc
) with persist

; Alternate reference structure - Populate all structures with empty values
if (run_stats->reference_ind = 1)
    set stat = alterlist(rAllergy->allergies, 1)
    set stat = alterlist(rAllergy->allergies[1]->reaction, 1)
    set stat = alterlist(rAllergy->allergies[1]->comment, 1)
        
    ; Populate the code sets
    select into "nl:"
    from    dm_columns_doc      dcd
    plan dcd
        where dcd.table_name = "ALLERGY"
        and dcd.code_set > 0
    detail
        call add_ref_code_set("allergy", camel_field(dcd.column_name), dcd.description, dcd.code_set)        
    with counter        
        
    go to skip_logic
endif 


declare nNum = i4 
declare cParser = vc
declare cParser2= vc
 
; Set the Parser
call type_parser("a.reaction_status_cd", 12025)
set cParser2 = cParser
call type_parser("a.substance_type_cd", 12020)
 
; Collect the allergies
select into "nl:"
    person_id           = a.person_id,
	sort_key			= cnvtupper(n.source_string)
from	allergy				a,
		nomenclature		n
plan a
	where expand(nNum, 1, size(patient_source->patients, 5), a.person_id, patient_source->patients[nNum].person_id)
	and parser(cParser)
	and parser(cParser2)
	and a.active_ind = 1
	and a.end_effective_dt_tm > sysdate
join n
	where n.nomenclature_id = a.substance_nom_id
order by person_id, sort_key
head report
	nCount = 0
detail
	nCount = nCount + 1
	stat = alterlist(rAllergy->allergies, nCount)
 
	rAllergy->allergies[nCount].person_id = a.person_id
	rAllergy->allergies[nCount].encntr_id = a.encntr_id
	rAllergy->allergies[nCount].allergy_id = a.allergy_id
	rAllergy->allergies[nCount].allergy_instance_id = a.allergy_instance_id
	rAllergy->allergies[nCount].substance = n.source_string
	rAllergy->allergies[nCount].substance_identifier = n.source_identifier
	rAllergy->allergies[nCount].substance_ft_desc = a.substance_ftdesc
	rAllergy->allergies[nCount].substance_type = uar_get_code_display(a.substance_type_cd)
	rAllergy->allergies[nCount].substance_type_cd = a.substance_type_cd
	rAllergy->allergies[nCount].source_of_info = uar_get_code_display(a.source_of_info_cd)
	rAllergy->allergies[nCount].source_of_info_cd = a.source_of_info_cd
	rAllergy->allergies[nCount].source_of_info_ft = a.source_of_info_ft
	rAllergy->allergies[nCount].onset_dt_tm = a.onset_dt_tm
	rAllergy->allergies[nCount].reaction_status = uar_get_code_display(a.reaction_status_cd)
	rAllergy->allergies[nCount].reaction_status_cd = a.reaction_status_cd
	rAllergy->allergies[nCount].created_dt_tm = a.created_dt_tm
	rAllergy->allergies[nCount].created_prsnl_id = a.created_prsnl_id
	rAllergy->allergies[nCount].cancel_reason = uar_get_code_display(a.cancel_reason_cd)
	rAllergy->allergies[nCount].cancel_reason_cd = a.cancel_reason_cd
	rAllergy->allergies[nCount].cancel_dt_tm = a.cancel_dt_tm
	rAllergy->allergies[nCount].cancel_prsnl_id = a.cancel_prsnl_id
	rAllergy->allergies[nCount].verified_status_flag = a.verified_status_flag
	rAllergy->allergies[nCount].rec_src_vocab = uar_get_code_display(a.rec_src_vocab_cd)
	rAllergy->allergies[nCount].rec_src_vocab_cd = a.rec_src_vocab_cd
	rAllergy->allergies[nCount].rec_src_identifer = a.rec_src_identifer
	rAllergy->allergies[nCount].rec_src_string = a.rec_src_string
	rAllergy->allergies[nCount].onset_precision = uar_get_code_display(a.onset_precision_cd)
	rAllergy->allergies[nCount].onset_precision_cd = a.onset_precision_cd
	rAllergy->allergies[nCount].onset_precision_flag = a.onset_precision_flag
	rAllergy->allergies[nCount].reviewed_dt_tm = a.reviewed_dt_tm
	rAllergy->allergies[nCount].reviewed_prsnl_id = a.reviewed_prsnl_id
	rAllergy->allergies[nCount].orig_prsnl_id = a.orig_prsnl_id
	rAllergy->allergies[nCount].reaction_status_dt_tm = a.reaction_status_dt_tm
with expand=2, nocounter
 
; Collect the reactions
if (validate(payload->allergy->reactions, 0) = 1)
	select into "nl:"
		allergy_id          = r.allergy_id,
		allergy_instance_id = r.allergy_instance_id,
		sort_key			= cnvtupper(n.source_string)
	from	reaction			r,
			nomenclature		n
	plan r
		where expand(nNum, 1, size(rAllergy->allergies, 5), r.allergy_id, rAllergy->allergies[nNum].allergy_id, 
		                                                    r.allergy_instance_id, rAllergy->allergies[nNum].allergy_instance_id)
		and r.active_ind = 1
		and r.end_effective_dt_tm > sysdate
	join n
		where n.nomenclature_id = r.reaction_nom_id
	order allergy_id, allergy_instance_id, sort_key
	head allergy_id
	    x = 0
    head allergy_instance_id	    
		nCount = 0
	detail
	    nPos = locateval(nNum, 1, size(rAllergy->allergies, 5), r.allergy_id, rAllergy->allergies[nNum].allergy_id, 
		                                                    r.allergy_instance_id, rAllergy->allergies[nNum].allergy_instance_id)
		nCount = nCount + 1
		stat = alterlist(rAllergy->allergies[nPos].reaction, nCount)
 
		rAllergy->allergies[nPos].reaction[nCount].reaction = n.source_string
		rAllergy->allergies[nPos].reaction[nCount].reaction_identifier = n.source_identifier
		rAllergy->allergies[nPos].reaction[nCount].reaction_ftdesc = r.reaction_ftdesc
	with expand=2, nocounter
endif
 
; Collect the comments
if (validate(payload->allergy->comments, 0) = 1)
	select into "nl:"
		allergy_id          = ac.allergy_id,
		allergy_instance_id = ac.allergy_instance_id,
		sort_date			= format(ac.comment_dt_tm, "yyyymmddhhmmss;;q")
	from	allergy_comment		ac
	plan ac
		where expand(nNum, 1, size(rAllergy->allergies, 5), ac.allergy_id, rAllergy->allergies[nNum].allergy_id, 
		                                                    ac.allergy_instance_id, rAllergy->allergies[nNum].allergy_instance_id)
		and ac.active_ind = 1
		and ac.end_effective_dt_tm > sysdate
	order allergy_id, allergy_instance_id, sort_date desc
	head allergy_id
	    x = 0
    head allergy_instance_id	    
		nCount = 0
	head sort_date
		x = 0
	detail
	    nPos = locateval(nNum, 1, size(rAllergy->allergies, 5), ac.allergy_id, rAllergy->allergies[nNum].allergy_id, 
		                                                    ac.allergy_instance_id, rAllergy->allergies[nNum].allergy_instance_id)

		nCount = nCount + 1
		stat = alterlist(rAllergy->allergies[nNum].comment, nCount)
 
		rAllergy->allergies[nNum].comment[nCount].comment_dt_tm = ac.comment_dt_tm
		rAllergy->allergies[nNum].comment[nCount].comment_prsnl_id = ac.comment_prsnl_id
		rAllergy->allergies[nNum].comment[nCount].allergy_comment = replace(replace(ac.allergy_comment, char(13), ""), char(10), "\\n")
 
	with expand=2, nocounter
endif
 
; Skip the rest if no allergies loaded
if (size(rAllergy->allergies, 5) = 0)
	go to end_program
endif

#skip_logic

if (validate(payload->allergy->skipJSON, 0) = 0) 
    call add_standard_output(cnvtrectojson(rAllergy, 4, 1))
endif
 
#end_program
 
END GO
/*************************************************************************
 
        Script Name:    1co5_mpage_apo.prg
 
        Description:    Clinical Office - MPage Developer
        				Address/Phone/Organization Data Retrieval
 
        Date Written:   September 1, 2025
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
 Called from 1CO5_MPAGE_ENTRY. Do not attempt to run stand alone.
 
 Possible Payload values:
 
	"patientSource":[
		{"personId": value, "encntrId": value}
	],
	"orgSource": [
		{"organizationId": value}
	],
	"organization": {
		"aliases": true
	},
	"address": true,
	"phone": true,
	"skipJSON": true,
	"typeList": [
		{"codeSet": value, "type": "value", "typeCd": value}
	]
 
 
 *************************************************************************
                            Revision Information
 *************************************************************************
 Rev    Date     By             Comments
 ------ -------- -------------- ------------------------------------------
 001    01/09/25 J. Simpson     Initial Development
 *************************************************************************/
 
drop program 1co5_mpage_apo:group1 go
create program 1co5_mpage_apo:group1
 
; Check to see if running from mPage entry script
if (validate(payload->address) = 0 and validate(payload->phone) = 0 and validate(payload->organization) = 0)
	go to end_program
endif

declare nNum = i4
declare cParser = vc
declare cParser2= vc

free record rAPO
record rAPO (
    1 apo_executed                         = i4
	1 address[*]
        2 parent_entity_id                 = f8
        2 parent_entity_name               = vc
        2 address_id                       = f8	
        2 address_type_cd                  = f8
        2 address_type                     = vc
        2 address_type_meaning             = vc
        2 address_type_seq                 = i4
        2 active_ind                       = i4
        2 beg_effective_dt_tm              = dq8
        2 end_effective_dt_tm              = dq8
        2 street_addr                      = vc
        2 street_addr2                     = vc
        2 street_addr3                     = vc
        2 street_addr4                     = vc
        2 city                             = vc
        2 state_cd                         = f8
        2 state                            = vc
        2 zip_code                         = vc
        2 county_cd                        = f8
        2 county                           = vc
        2 country_cd                       = f8
        2 country                          = vc
	1 phone[*]
        2 parent_entity_id                 = f8
        2 parent_entity_name               = vc
        2 phone_id                         = f8
        2 phone_type_cd                    = f8
        2 phone_type                       = vc
        2 phone_type_meaning               = vc
        2 phone_type_seq                   = i4
        2 active_ind                       = i4
        2 beg_effective_dt_tm              = dq8
        2 end_effective_dt_tm              = dq8
        2 phone_number                     = vc
        2 phone_formatted                  = vc
        2 extension                        = vc
	1 organization[*]
	   2 organization_id                   = f8
	   2 org_name                          = vc
	   2 federal_tax_id_nbr                = vc
	   2 org_status_cd                     = f8
	   2 org_status                        = vc
	   2 org_class_cd                      = f8
	   2 org_class                         = vc
	   2 external_ind                      = i4
	   2 aliases[*]
	       3 alias_pool_cd                 = f8
	       3 alias_pool                    = vc
	       3 org_alias_type_cd             = f8
	       3 org_alias_type                = vc
	       3 org_alias_type_meaning        = vc
	       3 org_alias_sub_type_cd         = f8
	       3 org_alias_sub_type            = vc
	       3 alias                         = vc
	       3 alias_formatted               = vc
) with persistscript

; Alternate reference structure - Populate all structures with empty values
if (run_stats->reference_ind = 1)
    set stat = alterlist(rAPO->address, 1)
    set stat = alterlist(rAPO->phone, 1)
    set stat = alterlist(rAPO->organization, 1)
    set stat = alterlist(rAPO->organization[1]->aliases, 1)
        
    ; Populate the code sets
    select into "nl:"
        object_name         = if (dcd.table_name = "ORGANIZATION")
                                "organization"
                              elseif (dcd.table_name = "ORGANIZATION_ALIAS")
                                "organization"
                              elseif (dcd.table_name = "ADDRESS")
                                "address"
                              elseif (dcd.table_name = "PHONE")
                                "phone"
                              endif
    from    dm_columns_doc      dcd
    plan dcd
        where dcd.table_name in ("ORGANIZATION", "ORGANIZATION_ALIAS", "ADDRESS", "PHONE")
        and dcd.code_set > 0
    detail
        call add_ref_code_set(object_name, camel_field(dcd.column_name), dcd.description, dcd.code_set)        
    with counter        
        
    go to skip_logic
endif 

; Add the person id values to the parent_values structure
;if (validate(payload->address->clearPatientSource, 0) = 0 and
;     validate(payload->phone->clearPatientSource, 0) = 0 and
;     validate(patient_source->patients) = 1)
if (validate(patient_source->patients) = 1)
	set stat = alterlist(parent_values->data, size(patient_source->patients, 5))
 
	for (nloop = 1 to size(patient_source->patients, 5))
		set parent_values->data[nloop].parent_entity_id = patient_source->patients[nloop].person_id
		set parent_values->data[nloop].parent_entity_name = "PERSON"
	endfor
endif

set rAPO->apo_executed = 1
 
; Add the organization id values to the parent values structure
if (validate(payload->orgsource) = 1)
	set nstart = size(parent_values->data, 5)
	set stat = alterlist(parent_values->data, size(payload->orgsource, 5) + nstart)
	for (nloop = 1 to size(payload->orgsource, 5))
		set parent_values->data[nstart + nloop].parent_entity_id = payload->orgsource[nloop].organizationid
		set parent_values->data[nstart + nloop].parent_entity_name = "ORGANIZATION"
	endfor
endif
 
if (validate(payload->organization) = 0 or validate(payload->orgsource) = 0)
	go to skip_orgs
endif
  
; Collect the organization data
; -----------------------------

call echorecord(parent_values)
 
; Basic organization level information
select into "nl:"
from	organization		o
plan o
    ;where expand(nNum, 1, size(payload->orgSource, 5), o.organization_id, payload->orgSource[nNum].organizationId)
    where expand(nNum, 1, size(parent_values->data, 5), o.organization_id, parent_values->data[nNum].parent_entity_id,
                                                        "ORGANIZATION", parent_values->data[nNum].parent_entity_name)
    and o.active_ind = 1
    and o.end_effective_dt_tm > sysdate
head report
    nCount = 0
detail
    nCount = nCount + 1
    stat = alterlist(rAPO->organization, nCount)
    
    rAPO->organization[nCount].organization_id = o.organization_id
    rAPO->organization[nCount].org_name = o.org_name
    rAPO->organization[nCount].federal_tax_id_nbr = o.federal_tax_id_nbr
    rAPO->organization[nCount].org_status_cd = o.org_status_cd
    rAPO->organization[nCount].org_status = uar_get_code_display(o.org_status_cd)
    rAPO->organization[nCount].org_class_cd = o.org_class_cd
    rAPO->organization[nCount].org_class = uar_get_code_display(o.org_class_cd)
    rAPO->organization[nCount].external_ind = o.external_ind
with expand=2
 
; Set the Parser
call type_parser("oa.org_alias_type_cd", 334)
 
; Org Aliases
if (validate(payload->organization->aliases, 0) = 1)
    select into "nl:"
        organization_id     = oa.organization_id
	from	organization_alias			oa
	plan oa
	   where expand(nNum, 1, size(rAPO->organization, 5), oa.organization_id, rAPO->organization[nNum].organization_id)
		and parser(cParser)
		and oa.active_ind = 1
		and oa.end_effective_dt_tm > sysdate
    order organization_id
    head organization_id
        nPos = locateval(nNum, 1, size(rAPO->organization, 5), organization_id, rAPO->organization[nNum].organization_id)
        nCount = 0
    detail
        nCount = nCount + 1
        stat = alterlist(rAPO->organization[nPOS]->aliases, nCount)
        
        rAPO->organization[nPOS]->aliases[nCount].alias_pool_cd = oa.alias_pool_cd
        rAPO->organization[nPOS]->aliases[nCount].alias_pool = uar_get_code_display(oa.alias_pool_cd)
        rAPO->organization[nPOS]->aliases[nCount].org_alias_type_cd = oa.org_alias_type_cd
        rAPO->organization[nPOS]->aliases[nCount].org_alias_type = uar_get_code_display(oa.org_alias_type_cd)
        rAPO->organization[nPOS]->aliases[nCount].org_alias_type_meaning = uar_get_code_meaning(oa.org_alias_type_cd)
        rAPO->organization[nPOS]->aliases[nCount].org_alias_sub_type_cd = oa.org_alias_sub_type_cd
        rAPO->organization[nPOS]->aliases[nCount].org_alias_sub_type = uar_get_code_display(oa.org_alias_sub_type_cd)
        rAPO->organization[nPOS]->aliases[nCount].alias = oa.alias
        rAPO->organization[nPOS]->aliases[nCount].alias_formatted = cnvtalias(oa.alias, oa.alias_pool_cd)
    with expand=2		
endif
  
#skip_orgs
 
; Skip address/phone if no id's loaded
if (size(parent_values->data, 5) = 0)
	go to end_program
endif
 
if (validate(payload->address) = 0)
	go to skip_address
endif
 
; Set the Parser
call type_parser("a.address_type_cd", 212)
 
; Collect the addresses
; ---------------------
select into "nl:"
	parent_entity_id		= a.parent_entity_id,
	parent_entity_name		= a.parent_entity_name,
	address_type			= uar_get_code_display(a.address_type_cd),
	address_type_seq		= a.address_type_seq
from	address				a
plan a
    where expand(nNum, 1, size(parent_values->data, 5), a.parent_entity_id, parent_values->data[nNum].parent_entity_id,
                                                        a.parent_entity_name, parent_values->data[nNum].parent_entity_name)
	and parser(cParser)
	and a.active_ind = 1
	and a.end_effective_dt_tm > sysdate
order parent_entity_id, parent_entity_name, address_type, address_type_seq
head report
    nCount = 0
detail
    nCount = nCount + 1
    stat = alterlist(rAPO->address, nCount)
    
    rAPO->address[nCount].parent_entity_id = a.parent_entity_id    
    rAPO->address[nCount].parent_entity_name = a.parent_entity_name
    rAPO->address[nCount].address_id = a.address_id
    rAPO->address[nCount].address_type_cd = a.address_type_cd
    rAPO->address[nCount].address_type = uar_get_code_display(a.address_type_cd)
    rAPO->address[nCount].address_type_meaning = uar_get_code_meaning(a.address_type_cd)
    rAPO->address[nCount].address_type_seq = a.address_type_seq
    rAPO->address[nCount].active_ind = a.active_ind
    rAPO->address[nCount].beg_effective_dt_tm = a.beg_effective_dt_tm
    rAPO->address[nCount].end_effective_dt_tm = a.end_effective_dt_tm
    rAPO->address[nCount].street_addr = a.street_addr
    rAPO->address[nCount].street_addr2 = a.street_addr2
    rAPO->address[nCount].street_addr3 = a.street_addr3
    rAPO->address[nCount].street_addr4 = a.street_addr4
    rAPO->address[nCount].city = a.city
    rAPO->address[nCount].state_cd = a.state_cd
    rAPO->address[nCount].state = evaluate(a.state_cd, 0.0, a.state, uar_get_code_display(a.state_cd))
    rAPO->address[nCount].zip_code = a.zipcode
    rAPO->address[nCount].county_cd = a.county_cd
    rAPO->address[nCount].county = evaluate(a.county_cd, 0.0, a.county, uar_get_code_display(a.county_cd))
    rAPO->address[nCount].country_cd = a.country_cd
    rAPO->address[nCount].country = evaluate(a.country_cd, 0.0, a.country, uar_get_code_display(a.country_cd))
with expand=2
 
#skip_address
 
if (validate(payload->phone) = 0)
	go to skip_phone
endif
 
; Set the Parser
call type_parser("ph.phone_type_cd", 43)

; Phone format option
declare nPhoneOption = i4 with noconstant(0)
if (validate(payload->phone->phoneOption,0) = 1)
    set nPhoneOption = cnvtint(payload->phone->phoneOption)
endif
 
; Collect the phone information
; -----------------------------
select into "nl:"
	parent_entity_id		= ph.parent_entity_id,
	parent_entity_name		= ph.parent_entity_name,
	phone_type     			= uar_get_code_display(ph.phone_type_cd),
	phone_formatted			= cnvtphone(cnvtalphanum(ph.phone_num), ph.phone_format_cd, nphoneoption),
	phone_type_seq			= ph.phone_type_seq
from	phone				ph
plan ph
    where expand(nNum, 1, size(parent_values->data, 5), ph.parent_entity_id, parent_values->data[nNum].parent_entity_id,
                                                        ph.parent_entity_name, parent_values->data[nNum].parent_entity_name)
	and parser(cParser)
	and ph.active_ind = 1
	and ph.end_effective_dt_tm > sysdate
order parent_entity_id, parent_entity_name, phone_type, phone_type_seq
head report
    nCount = 0
detail
    nCount = nCount + 1
    stat = alterlist(rAPO->phone, nCount)

    rAPO->phone[nCount].parent_entity_id = ph.parent_entity_id
    rAPO->phone[nCount].parent_entity_name = ph.parent_entity_name
    rAPO->phone[nCount].phone_id = ph.phone_id
    rAPO->phone[nCount].phone_type_cd = ph.phone_type_cd
    rAPO->phone[nCount].phone_type = uar_get_code_display(ph.phone_type_cd)
    rAPO->phone[nCount].phone_type_meaning = uar_get_code_meaning(ph.phone_type_cd)
    rAPO->phone[nCount].phone_type_seq = ph.phone_type_seq
    rAPO->phone[nCount].active_ind = ph.active_ind
    rAPO->phone[nCount].beg_effective_dt_tm = ph.beg_effective_dt_tm
    rAPO->phone[nCount].end_effective_dt_tm = ph.end_effective_dt_tm
    rAPO->phone[nCount].phone_number = ph.phone_num
    rAPO->phone[nCount].phone_formatted = cnvtphone(cnvtalphanum(ph.phone_num), ph.phone_format_cd, nPhoneOption)
    rAPO->phone[nCount].extension = ph.extension
with expand = 2
 
#skip_phone

#skip_logic

if (validate(payload->diagnosis->skipJSON, 0) = 0) 
    call add_standard_output(cnvtrectojson(rAPO, 4, 1))
endif

#end_program
 
END GO
/*************************************************************************
 
        Script Name:    1co5_mpage_census_list:group1
 
        Description:    Clinical Office - MPage Developer
        				LoadList Census Script
 
        Date Written:   July 4, 2020
        Written by:     John Simpson
                        Precision Healthcare Solutions
 
 *************************************************************************
		   Copyright (c) 2020 Precision Healthcare Solutions
 
 NO PART OF THIS CODE MAY BE COPIED, MODIFIED OR DISTRIBUTED WITHOUT
 PRIOR WRITTEN CONSENT OF PRECISION HEALTHCARE SOLUTIONS EXECUTIVE
 LEADERSHIP TEAM.
 
 FOR LICENSING TERMS PLEASE VISIT www.clinicaloffice.com/mpage/license
 
 *************************************************************************
                            Special Instructions
 *************************************************************************
 Called from 1CO5_MPAGE_ENTRY. Do not attempt to run stand alone. If you
 wish to test the development of your custom script from the CCL back-end,
 please run with 1CO_MPAGE_TEST.
 
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
	},
	"typeList": [
		{"codeSet": value, "type": "value", "typeCd": value}
	]
 
 
 
 *************************************************************************
                            Revision Information
 *************************************************************************
 Rev    Date     By             Comments
 ------ -------- -------------- ------------------------------------------
 001    07/04/20 J. Simpson     Initial Development
 *************************************************************************/
 
drop program 1co5_mpage_census_list:group1 go
create program 1co5_mpage_census_list:group1
  
; Define the custom record structure you wish to have sent back in the JSON to the mPage. The name
; of the record structure can be anything you want.
free record rCustom
record rCustom (
	1 visits[*]
		2 person_id					= f8
		2 encntr_id					= f8
)
 
; Set the Parser for the various filters
declare cParser = vc
declare cParser2 = vc
 
call Type_Parser("e.encntr_type_class_cd", 69)
set cParser2 = cParser
call Type_Parser("ed.loc_nurse_unit_cd", 220)
 
; Clear the patient source
set stat = initrec(patient_source)
 
; Collect the census
select into "nl:"
	person_id				= e.person_id,
	encntr_id				= e.encntr_id
from	encntr_domain			ed,
		encounter				e
plan ed
	where ed.end_effective_dt_tm > sysdate
	and parser(cParser)
	and ed.active_ind = 1
join e
	where e.encntr_id = ed.encntr_id
	and nullind(e.disch_dt_tm) = 1
	and parser(cParser2)
	and e.active_ind = 1
order person_id, encntr_id
head report
	nPerson = 0
	nEncounter = 0
head person_id
	nPerson = nPerson + 1
 
	stat = alterlist(patient_source->patients, nPerson)
	patient_source->patients[nPerson].person_id = person_id
head encntr_id
	nEncounter = nEncounter + 1
 
	stat = alterlist(patient_source->visits, nEncounter)
	patient_source->visits[nEncounter].person_id = person_id
	patient_source->visits[nEncounter].encntr_id = encntr_id
with nocounter
 
; Update rCustom with the patient list
if (size(patient_source->visits, 5) > 0)
	set stat = moverec(patient_source->visits, rCustom)
 
	call Add_Custom_Output(cnvtrectojson(rCustom, 4, 1))
endif
 
#end_program
 
end go
/*************************************************************************
 
        Script Name:    1co5_mpage_component.prg
 
        Description:    Clinical Office - MPage Developer
        				Retrieves path for MPage Components
 
        Date Written:   September 27, 2025
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
 *************************************************************************
                            Revision Information
 *************************************************************************
 Rev    Date     By             Comments
 ------ -------- -------------- ------------------------------------------
 001    09/27/25 J. Simpson     Initial Development
 *************************************************************************/
 
DROP PROGRAM 1co5_mpage_component:group1 GO
CREATE PROGRAM 1co5_mpage_component:group1
 
prompt 
	"Header Title" = "" 

with headerTitle
  
; Variable declarations
declare _Memory_Reply_String = vc
declare cPath = vc
declare cPiece = vc
 
free record response
record response (
    1 url               = vc
    1 component         = vc
    1 success_ind       = i4
)
 
; Collect component path
select into "nl:"
from    dm_info         d
plan d
    where d.info_domain = "Clinical Office Component"
    and d.info_name = $headerTitle
detail
    response->success_ind = 1
    cPath = d.info_char
    if (substring(1,4,cPath) != "http")
        response->component = d.info_char        
    else    ; Need to parse the component name
        nPiece = 1
        while (cPiece != "x")
            cPiece = (piece(d.info_char, "/", nPiece, "x"))
            if (cPiece != "x")
                response->component = cPiece
            endif
            nPiece = nPiece + 1
        endwhile
        response->url = cPath
    endif
with counter     

; Will skip the dm_info lookup if http: or https: path sent and a component
if (trim(response->url) = "")
    select into "nl:"
        full_path = build(d.info_char,"/custom_mpage_content/", cPath)
    from dm_info d
    plan d
        where d.info_domain = "INS"
        and d.info_name = "CONTENT_SERVICE_URL"
    head report
        response->url = trim(full_path,3)
    with nocounter
endif

set _Memory_Reply_String = cnvtrectojson(response, 4, 1)

call echo(_Memory_Reply_String)
 
#end_program
 
end go
/*************************************************************************
 
        Script Name:    1CO5_MPAGE_CVLOOKUP.PRG
 
        Description:    Clinical Office - mPage Edition
        				Generic Code Value Lookup
 
        Date Written:   February 6, 2025
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
 Called from 1CO_MPAGE_ENTRY. Do not attempt to run stand alone.
 
 Possible Payload values:
 
     "codeSet": [
		{ "cs": cs, "cv": codeValue, "filter": displayKey, "alias": displayKey, "outboundAlias": true }
     ]
 
  *************************************************************************
                            Revision Information
 *************************************************************************
 Rev    Date     By             Comments
 ------ -------- -------------- ------------------------------------------
 001    02/03/18 J. Simpson     Initial Development
 002    02/10/21 J. Simpson     Set default sort to display key
 003    05/16/22 J. Simpson     Switched to use CNVTRECTOJSON
 *************************************************************************/
 
drop program 1co5_mpage_cvlookup:group1 go
create program 1co5_mpage_cvlookup:group1
 
; Check to see if running from mPage entry script
if (validate(payload->codevalue) = 0)
	go to end_program
endif
 
record rCodeValue (
	1 code_values[*]
		2 code_value					= f8
		2 code_set						= i4
		2 cdf_meaning					= vc
		2 display						= vc
		2 display_key					= vc
		2 description					= vc
		2 definition					= vc
		2 alias_ind						= vc
		2 alias							= vc
		2 outbound_ind					= vc
		2 outbound						= vc
)
 
for (nLoop = 1 to size(payload->codevalue, 5))
	; Ensure the parser works
	if (payload->codevalue[nloop].filter = "")
		set payload->codevalue[nloop].filter = "1=1"
	endif
 
	; Collect the code values
	select into "nl:"
	from	code_value	cv
	plan cv
		where (cv.code_set = payload->codevalue[nloop].cs
		or cv.code_value = payload->codevalue[nloop].value)
		and cv.code_value != 0
		and parser(payload->codevalue[nloop].filter)
		and cv.active_ind = 1
		and cv.end_effective_dt_tm > sysdate
		and cv.code_set > 0
    order cv.display_key
	head report
		ncount = size(rCodeValue->code_values, 5)
	detail
		ncount = ncount + 1
		stat = alterlist(rCodeValue->code_values, ncount)
 
		rCodeValue->code_values[ncount].code_value = cv.code_value
		rCodeValue->code_values[ncount].code_set = cv.code_set
		rCodeValue->code_values[ncount].cdf_meaning = cv.cdf_meaning
		rCodeValue->code_values[ncount].display = cv.display
		rCodeValue->code_values[ncount].display_key = cv.display_key
		rCodeValue->code_values[ncount].description = cv.description
		rCodeValue->code_values[ncount].definition = cv.definition
		rCodeValue->code_values[ncount].alias_ind = payload->codevalue[nloop].alias
		rCodeValue->code_values[ncount].outbound_ind = payload->codevalue[nloop].outboundalias
	with nocounter
endfor
 
; Collect the alias
select into "nl:"
	dseq			= d.seq,
	primary_ind		= cva.primary_ind
from	(dummyt				d with seq=value(size(rCodeValue->code_values, 5))),
		code_value_alias	cva,
		code_value			cv
plan d
	where trim(rCodeValue->code_values[d.seq].alias_ind) != ""
join cva
	where cva.code_value = rCodeValue->code_values[d.seq].code_value
join cv
	where cv.code_value = cva.contributor_source_cd
	and cv.display_key = cnvtupper(rCodeValue->code_values[d.seq].alias_ind)
order dseq, primary_ind
detail
	rCodeValue->code_values[d.seq].alias = cva.alias
with nocounter
 
; Collect outbound alias
select into "nl:"
	dseq			= d.seq
from	(dummyt				d with seq=value(size(rCodeValue->code_values, 5))),
		code_value_outbound	cvo,
		code_value			cv
plan d
	where trim(rCodeValue->code_values[d.seq].outbound_ind) != ""
join cvo
	where cvo.code_value = rCodeValue->code_values[d.seq].code_value
join cv
	where cv.code_value = cvo.contributor_source_cd
	and cv.display_key = cnvtupper(rCodeValue->code_values[d.seq].outbound_ind)
order dseq
detail
	rCodeValue->code_values[d.seq].outbound = cvo.alias
with nocounter
 
; Output the JSON
if (size(rCodeValue->code_values, 5) > 0)
	call add_standard_output(cnvtrectojson(rCodeValue, 4, 1))
endif
 
#end_program
 
end go
/*************************************************************************
 
        Script Name:    1CO_MPAGE_DIAGNOSIS.PRG
 
        Description:    Clinical Office - mPage Edition
        				Diagnosis Data Retrieval
 
        Date Written:   February 3, 2025
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
 Called from 1CO5_MPAGE_ENTRY. Do not attempt to run stand alone.
 
 Possible Payload values:
 
	"patientSource":[
		{"personId": value, "encntrId": value}
	],
	"diagnosis": true,
	"typeList": [
		{"codeSet": value, "type": "value", "typeCd": value}
	]
 
 
 
 *************************************************************************
                            Revision Information
 *************************************************************************
 Rev    Date     By             Comments
 ------ -------- -------------- ------------------------------------------
 001    03/02/25 J. Simpson     Initial Development
 *************************************************************************/
 
drop program 1co5_mpage_diagnosis:group1 go
create program 1co5_mpage_diagnosis:group1
 
; check to see if running from mpage entry script
if (validate(payload->diagnosis) = 0 or size(patient_source->visits, 5) = 0)
	go to end_program
endif
 
free record rDiagnosis 
record rDiagnosis (
	1 diagnosis[*]
		2 person_id						= f8
		2 encntr_id						= f8
		2 diagnosis_id					= f8
		2 nomenclature_id				= f8
		2 dx_source_string				= vc
		2 dx_source_identifier			= vc
		2 dx_source_vocab               = vc
		2 diag_dt_tm					= dq8
		2 diag_type                     = vc
		2 diagnostic_category           = vc
		2 diag_priority					= i4
		2 diag_prsnl_id					= f8
		2 diag_prsnl_name				= vc
		2 diag_class                    = vc
		2 confid_level                  = vc
		2 attestation_dt_tm				= dq8
		2 diag_ftdesc					= vc
		2 mod_nomenclature_id			= f8
		2 mod_source_string				= vc
		2 mod_source_identifier			= vc
		2 mod_source_vocab              = vc		
		2 diag_note						= vc
		2 condition_qual                = vc
		2 clinical_service              = vc
		2 confirmation_status           = vc
		2 classification                = vc
		2 severity_class                = vc
		2 certainty                     = vc
		2 probability					= i4
		2 diagnosis_display				= vc
		2 severity_ftdesc				= vc
		2 long_blob_id					= f8
		2 ranking                       = vc
		2 severity                      = vc		
		2 diagnosis_group				= f8
		2 clinical_diag_priority		= i4
		2 present_on_admit              = vc
		2 hac_ind						= i4
		2 laterality                    = vc
		2 originating_nomenclature_id	= f8
		2 orig_dx_source_string			= vc
		2 orig_dx_source_identifier		= vc
		2 orig_dx_source_vocab          = vc		
		2 diag_type_cd					= f8
		2 dx_source_vocab_cd			= f8
		2 diagnostic_category_cd		= f8
		2 diag_class_cd					= f8
		2 confid_level_cd				= f8
		2 mod_source_vocab_cd			= f8
		2 conditional_qual_cd			= f8
		2 clinical_service_cd			= f8
		2 confirmation_status_cd		= f8
		2 classification_cd				= f8
		2 severity_class_cd				= f8
		2 certainty_cd					= f8
		2 ranking_cd					= f8
		2 severity_cd					= f8
		2 present_on_admit_cd			= f8
		2 laterality_cd					= f8
		2 orig_dx_source_vocab_cd		= f8

) with persistscript

; Alternate reference structure - Populate all structures with empty values
if (run_stats->reference_ind = 1)
    set stat = alterlist(rDiagnosis->diagnosis, 1)
        
    ; Populate the code sets
    select into "nl:"
    from    dm_columns_doc      dcd
    plan dcd
        where dcd.table_name in ("DIAGNOSIS", "NOMENCLATURE")
        and dcd.code_set > 0
    detail
        call add_ref_code_set("diagnosis", camel_field(dcd.column_name), dcd.description, dcd.code_set)        
    with counter        
        
    go to skip_logic
endif 

 
declare cParser = vc
declare cParser2 = vc
declare nNum = i4
 
; Set the Parser
call type_parser("n.source_vocabulary_cd", 400)
set cParser2 = cParser
call type_parser("dx.diag_type_cd", 17)
 
; Collect the diagnosis
select into "nl:"
from	diagnosis			dx,
		nomenclature		n,
		nomenclature		n2,
		nomenclature		n3
plan dx
	where expand(nNum, 1, size(patient_source->visits, 5), dx.encntr_id, patient_source->visits[nNum].encntr_id)
	and parser(cParser)
	and dx.active_ind = 1
	and dx.end_effective_dt_tm > sysdate
join n
	where n.nomenclature_id = dx.nomenclature_id
	and parser(cParser2)
join n2
	where n2.nomenclature_id = dx.mod_nomenclature_id
join n3
	where n3.nomenclature_id = dx.originating_nomenclature_id
head report
	nCount = 0
detail
	nCount = nCount + 1
	stat = alterlist(rDiagnosis->diagnosis, nCount)
 
	rDiagnosis->diagnosis[nCount].person_id = dx.person_id
	rDiagnosis->diagnosis[nCount].encntr_id = dx.encntr_id
	rDiagnosis->diagnosis[nCount].diagnosis_id = dx.diagnosis_id
	rDiagnosis->diagnosis[nCount].nomenclature_id = dx.nomenclature_id
	rDiagnosis->diagnosis[nCount].dx_source_string = n.source_string
	rDiagnosis->diagnosis[nCount].dx_source_identifier = n.source_identifier
	rDiagnosis->diagnosis[nCount].dx_source_vocab = uar_get_code_display(n.source_vocabulary_cd)
	rDiagnosis->diagnosis[nCount].dx_source_vocab_cd = n.source_vocabulary_cd
	rDiagnosis->diagnosis[nCount].diag_dt_tm = dx.diag_dt_tm
	rDiagnosis->diagnosis[nCount].diag_type = uar_get_code_display(dx.diag_type_cd)
	rDiagnosis->diagnosis[nCount].diag_type_cd = dx.diag_type_cd
	rDiagnosis->diagnosis[nCount].diagnostic_category = uar_get_code_display(dx.diagnostic_category_cd)
	rDiagnosis->diagnosis[nCount].diagnostic_category_cd = dx.diagnostic_category_cd
	rDiagnosis->diagnosis[nCount].diag_priority = dx.diag_priority
	rDiagnosis->diagnosis[nCount].diag_prsnl_id = dx.diag_prsnl_id
	rDiagnosis->diagnosis[nCount].diag_prsnl_name = dx.diag_prsnl_name
	rDiagnosis->diagnosis[nCount].diag_class = uar_get_code_display(dx.diag_class_cd)
	rDiagnosis->diagnosis[nCount].diag_class_cd = dx.diag_class_cd
	rDiagnosis->diagnosis[nCount].confid_level = uar_get_code_display(dx.confid_level_cd)
	rDiagnosis->diagnosis[nCount].confid_level_cd = dx.confid_level_cd
	rDiagnosis->diagnosis[nCount].attestation_dt_tm = dx.attestation_dt_tm
	rDiagnosis->diagnosis[nCount].diag_ftdesc = dx.diag_ftdesc
	rDiagnosis->diagnosis[nCount].mod_nomenclature_id = dx.mod_nomenclature_id
	rDiagnosis->diagnosis[nCount].mod_source_identifier = n2.source_identifier
	rDiagnosis->diagnosis[nCount].mod_source_string = n2.source_string
	rDiagnosis->diagnosis[nCount].mod_source_vocab_cd = n2.source_vocabulary_cd
	rDiagnosis->diagnosis[nCount].diag_note = dx.diag_note
	rDiagnosis->diagnosis[nCount].condition_qual = uar_get_code_display(dx.conditional_qual_cd)
	rDiagnosis->diagnosis[nCount].conditional_qual_cd = dx.conditional_qual_cd
	rDiagnosis->diagnosis[nCount].clinical_service = uar_get_code_display(dx.clinical_service_cd)
	rDiagnosis->diagnosis[nCount].clinical_service_cd = dx.clinical_service_cd
	rDiagnosis->diagnosis[nCount].confirmation_status = uar_get_code_display(dx.confirmation_status_cd)
	rDiagnosis->diagnosis[nCount].confirmation_status_cd = dx.confirmation_status_cd
	rDiagnosis->diagnosis[nCount].classification = uar_get_code_display(dx.classification_cd)
	rDiagnosis->diagnosis[nCount].classification_cd = dx.classification_cd
	rDiagnosis->diagnosis[nCount].severity_class = uar_get_code_display(dx.classification_cd)
	rDiagnosis->diagnosis[nCount].severity_class_cd = dx.severity_class_cd
	rDiagnosis->diagnosis[nCount].certainty = uar_get_code_display(dx.certainty_cd)
	rDiagnosis->diagnosis[nCount].certainty_cd = dx.certainty_cd
	rDiagnosis->diagnosis[nCount].probability = dx.probability
	rDiagnosis->diagnosis[nCount].diagnosis_display = dx.diagnosis_display
	rDiagnosis->diagnosis[nCount].severity_ftdesc = dx.severity_ftdesc
	rDiagnosis->diagnosis[nCount].long_blob_id = dx.long_blob_id
	rDiagnosis->diagnosis[nCount].ranking = uar_get_code_display(dx.ranking_cd)
	rDiagnosis->diagnosis[nCount].ranking_cd = dx.ranking_cd
	rDiagnosis->diagnosis[nCount].severity = uar_get_code_display(dx.severity_cd)
	rDiagnosis->diagnosis[nCount].severity_cd = dx.severity_cd
	rDiagnosis->diagnosis[nCount].diagnosis_group = dx.diagnosis_group
	rDiagnosis->diagnosis[nCount].clinical_diag_priority = dx.clinical_diag_priority
	rDiagnosis->diagnosis[nCount].present_on_admit = uar_get_code_display(dx.present_on_admit_cd)
	rDiagnosis->diagnosis[nCount].present_on_admit_cd = dx.present_on_admit_cd
	rDiagnosis->diagnosis[nCount].hac_ind = dx.hac_ind
	rDiagnosis->diagnosis[nCount].laterality = uar_get_code_display(dx.laterality_cd)
	rDiagnosis->diagnosis[nCount].laterality_cd = dx.laterality_cd
	rDiagnosis->diagnosis[nCount].originating_nomenclature_id = dx.originating_nomenclature_id
	rDiagnosis->diagnosis[nCount].orig_dx_source_string = n3.source_string
	rDiagnosis->diagnosis[nCount].orig_dx_source_identifier = n3.source_identifier
	rDiagnosis->diagnosis[nCount].orig_dx_source_vocab = uar_get_code_display(n3.source_vocabulary_cd)
	rDiagnosis->diagnosis[nCount].orig_dx_source_vocab_cd = n3.source_vocabulary_cd
 
with expand=2, nocounter
 
; Skip the rest if no diagnosis loaded
if (size(rDiagnosis->diagnosis, 5) = 0)
	go to end_program
endif

#skip_logic

if (validate(payload->diagnosis->skipJSON, 0) = 0) 
    call add_standard_output(cnvtrectojson(rDiagnosis, 4, 1))
endif
 
#end_program
 
end go
/*************************************************************************
 
        Script Name:    1CO5_MPAGE_DM_INFO.PRG
 
        Description:    Clinical Office - MPage Edition
        				DM_INFO Read/Write Script
 
        Date Written:   June 5, 2018
        Written by:     John Simpson
                        Precision Healthcare Solutions
 
 *************************************************************************
		   Copyright (c) 2018 Precision Healthcare Solutions
 
 NO PART OF THIS CODE MAY BE COPIED, MODIFIED OR DISTRIBUTED WITHOUT
 PRIOR WRITTEN CONSENT OF PRECISION HEALTHCARE SOLUTIONS EXECUTIVE
 LEADERSHIP TEAM.
 
 FOR LICENSING TERMS PLEASE VISIT www.clinicaloffice.com/mpage/license
 
 *************************************************************************
                            Special Instructions
 *************************************************************************
 Called from 1CO_MPAGE_ENTRY. Do not attempt to run stand alone. If you
 wish to test the development of your custom script from the CCL back-end,
 please run with 1CO_MPAGE_TEST.
 
 Possible Payload values:
 
	"customScript": {
		"script": [
			"name": "1CO_MPAGE_DM_INFO:GROUP1",
			"id": "identifier for your output, omit if you won't be returning data",
			"run": "pre or post",
			"parameters": {
				"action": "read, write or delete",
				"data": [{
					"infoDomain": "string",
					"infoName": "string",
					"infoDate": "JavaScript Date or null",
					"infoChar": "string",
					"infoNumber": number,
					"infoLongText": "string",
					"infoDomainId": number
				}]
			}
		],
		"clearPatientSource": true
	}
 
 
 *************************************************************************
                            Revision Information
 *************************************************************************
 Rev    Date     By             Comments
 ------ -------- -------------- ------------------------------------------
 001    06/05/18 J. Simpson     Initial Development
 *************************************************************************/
drop program 1co5_mpage_dm_info:group1 go
create program 1co5_mpage_dm_info:group1
 
DECLARE cACTION = vc
  
; Define the custom record structure you wish to have sent back in the JSON to the mPage. The name
; of the record structure can be anything you want.
FREE RECORD rCUSTOM
RECORD rCUSTOM (
	1 DM_INFO[*]
		2 INFO_DOMAIN				= vc
		2 INFO_NAME					= vc
		2 INFO_DATE					= dq8
		2 INFO_CHAR					= vc
		2 INFO_NUMBER				= f8
		2 LONG_TEXT_ID				= f8
		2 LONG_TEXT					= vc
		2 UPDT_DT_TM				= dq8
		2 UPDT_ID					= f8
		2 INFO_DOMAIN_ID			= f8
		2 ACTION_STATUS				= vc
)
 
; Check the action and perform the correct tasks
IF (VALIDATE(PAYLOAD->CUSTOMSCRIPT->SCRIPT[nSCRIPT]->PARAMETERS->ACTION) = 1)
	SET cACTION = SUBSTRING(1,1,CNVTUPPER(PAYLOAD->CUSTOMSCRIPT->SCRIPT[nSCRIPT]->PARAMETERS->ACTION))
 
	; Read the records (Needed for read operation, write if an update and to collect the LONG_TEXT_ID if deleting)
	SELECT INTO "NL:"
		D.SEQ, DI.SEQ
	FROM	(DUMMYT			D WITH SEQ=VALUE(SIZE(PAYLOAD->CUSTOMSCRIPT->SCRIPT[nSCRIPT]->PARAMETERS->DATA, 5))),
			DM_INFO			DI,
			DUMMYT			D_OJ
	PLAN D
	JOIN D_OJ
	JOIN DI
		WHERE PAYLOAD->CUSTOMSCRIPT->SCRIPT[nSCRIPT]->PARAMETERS->DATA[D.SEQ].INFODOMAIN = DI.INFO_DOMAIN
		AND (
				PAYLOAD->CUSTOMSCRIPT->SCRIPT[nSCRIPT]->PARAMETERS->DATA[D.SEQ].INFONAME = DI.INFO_NAME
				OR
				(TRIM(PAYLOAD->CUSTOMSCRIPT->SCRIPT[nSCRIPT]->PARAMETERS->DATA[D.SEQ].INFONAME) = NULL AND cACTION="R")
			)
		AND (
				PAYLOAD->CUSTOMSCRIPT->SCRIPT[nSCRIPT]->PARAMETERS->DATA[D.SEQ].INFODOMAINID = DI.INFO_DOMAIN_ID
				OR
				(PAYLOAD->CUSTOMSCRIPT->SCRIPT[nSCRIPT]->PARAMETERS->DATA[D.SEQ].INFODOMAINID = 0 AND cACTION="R")
			)
	HEAD REPORT
		nCOUNT = 0
 
		SUBROUTINE ADD_DM_INFO(cINFO_DOMAIN, cINFO_NAME, dINFO_DATE, cINFO_CHAR, nINFO_NUMBER, nLONG_TEXT_ID,
						cLONG_TEXT, dUPDT_DT_TM, nUPDT_ID, nINFO_DOMAIN_ID, cACTION_STATUS)
			nCOUNT = nCOUNT + 1
			STAT = ALTERLIST(rCUSTOM->DM_INFO, nCOUNT)
 
			rCUSTOM->DM_INFO[nCOUNT].INFO_DOMAIN = cINFO_DOMAIN
			rCUSTOM->DM_INFO[nCOUNT].INFO_NAME = cINFO_NAME
			rCUSTOM->DM_INFO[nCOUNT].INFO_DATE = dINFO_DATE
			rCUSTOM->DM_INFO[nCOUNT].INFO_CHAR = cINFO_CHAR
			rCUSTOM->DM_INFO[nCOUNT].INFO_NUMBER = nINFO_NUMBER
			rCUSTOM->DM_INFO[nCOUNT].LONG_TEXT_ID = nLONG_TEXT_ID
			rCUSTOM->DM_INFO[nCOUNT].LONG_TEXT = cLONG_TEXT
			rCUSTOM->DM_INFO[nCOUNT].UPDT_DT_TM = dUPDT_DT_TM
			rCUSTOM->DM_INFO[nCOUNT].UPDT_ID = nUPDT_ID
			rCUSTOM->DM_INFO[nCOUNT].INFO_DOMAIN_ID = nINFO_DOMAIN_ID
			rCUSTOM->DM_INFO[nCOUNT].ACTION_STATUS = cACTION_STATUS
		END
	DETAIL
		; Read Action, Data found
		IF (cACTION = "R" AND DI.SEQ > 0)
			CALL ADD_DM_INFO(	DI.INFO_DOMAIN, DI.INFO_NAME, DI.INFO_DATE, DI.INFO_CHAR, DI.INFO_NUMBER, DI.INFO_LONG_ID,
								"", DI.UPDT_DT_TM, DI.UPDT_ID, DI.INFO_DOMAIN_ID, "READ")
		; Insert new record
		ELSEIF (cACTION = "W" AND DI.SEQ = 0)
			CALL ADD_DM_INFO(	PAYLOAD->CUSTOMSCRIPT->SCRIPT[nSCRIPT]->PARAMETERS->DATA[D.SEQ].INFODOMAIN,
								PAYLOAD->CUSTOMSCRIPT->SCRIPT[nSCRIPT]->PARAMETERS->DATA[D.SEQ].INFONAME,
								CNVTDATETIME(PAYLOAD->CUSTOMSCRIPT->SCRIPT[nSCRIPT]->PARAMETERS->DATA[D.SEQ].INFODATE),
								PAYLOAD->CUSTOMSCRIPT->SCRIPT[nSCRIPT]->PARAMETERS->DATA[D.SEQ].INFOCHAR,
								PAYLOAD->CUSTOMSCRIPT->SCRIPT[nSCRIPT]->PARAMETERS->DATA[D.SEQ].INFONUMBER,
								0,
								PAYLOAD->CUSTOMSCRIPT->SCRIPT[nSCRIPT]->PARAMETERS->DATA[D.SEQ].INFOLONGTEXT,
								SYSDATE,
								nPRSNL_ID,
								PAYLOAD->CUSTOMSCRIPT->SCRIPT[nSCRIPT]->PARAMETERS->DATA[D.SEQ].INFODOMAINID,
								"INSERT")
		ELSEIF (cACTION = "W" AND DI.SEQ > 0)
			CALL ADD_DM_INFO(	DI.INFO_DOMAIN,
								DI.INFO_NAME,
								CNVTDATETIME(PAYLOAD->CUSTOMSCRIPT->SCRIPT[nSCRIPT]->PARAMETERS->DATA[D.SEQ].INFODATE),
								PAYLOAD->CUSTOMSCRIPT->SCRIPT[nSCRIPT]->PARAMETERS->DATA[D.SEQ].INFOCHAR,
								PAYLOAD->CUSTOMSCRIPT->SCRIPT[nSCRIPT]->PARAMETERS->DATA[D.SEQ].INFONUMBER,
								DI.INFO_LONG_ID,
								PAYLOAD->CUSTOMSCRIPT->SCRIPT[nSCRIPT]->PARAMETERS->DATA[D.SEQ].INFOLONGTEXT,
								SYSDATE,
								nPRSNL_ID,
								DI.INFO_DOMAIN_ID,
								"UPDATE")
		ELSEIF (cACTION = "D" AND DI.SEQ > 0)
			CALL ADD_DM_INFO(	DI.INFO_DOMAIN, DI.INFO_NAME, DI.INFO_DATE, DI.INFO_CHAR, DI.INFO_NUMBER, DI.INFO_LONG_ID,
								"", DI.UPDT_DT_TM, DI.UPDT_ID, DI.INFO_DOMAIN_ID, "DELETE")
		ENDIF
	WITH OUTERJOIN = D_OJ
 
	IF (SIZE(rCUSTOM->DM_INFO, 5) = 0)
		GO TO END_PROGRAM
	ENDIF
 
	; For new writes, determine the next LONG_TEXT_ID to insert
	SELECT INTO "NL:"
		D.SEQ, LONG_SEQ = SEQ(LONG_DATA_SEQ, NEXTVAL)
	FROM	(DUMMYT				D WITH SEQ=VALUE(SIZE(rCUSTOM->DM_INFO, 5))),
			DUAL				DL
	PLAN D
		WHERE rCUSTOM->DM_INFO[D.SEQ].ACTION_STATUS = "INSERT"
		AND SIZE(TRIM(rCUSTOM->DM_INFO[D.SEQ].LONG_TEXT)) > 0
	JOIN DL
	DETAIL
		rCUSTOM->DM_INFO[D.SEQ].LONG_TEXT_ID = LONG_SEQ
	WITH COUNTER
 
	; Read the existing long text data
	SELECT INTO "NL:"
		D.SEQ, LT.LONG_TEXT
	FROM 	(DUMMYT				D WITH SEQ=VALUE(SIZE(rCUSTOM->DM_INFO, 5))),
			LONG_TEXT			LT
	PLAN D
		WHERE rCUSTOM->DM_INFO[D.SEQ].ACTION_STATUS = "READ"
		AND rCUSTOM->DM_INFO[D.SEQ].LONG_TEXT_ID > 0
	JOIN LT
		WHERE LT.LONG_TEXT_ID = rCUSTOM->DM_INFO[D.SEQ].LONG_TEXT_ID
		AND LT.PARENT_ENTITY_NAME = "DM_INFO"
		AND LT.ACTIVE_IND = 1
	DETAIL
		rCUSTOM->DM_INFO[D.SEQ].LONG_TEXT = LT.LONG_TEXT
	WITH COUNTER
 
	; Insert/Update/Delete the records
	IF (cACTION != "R")
		FOR (nLOOP = 1 TO SIZE(rCUSTOM->DM_INFO, 5))
 
			; Perform Insert
			; --------------
			IF (rCUSTOM->DM_INFO[nLOOP].ACTION_STATUS = "INSERT")
				INSERT INTO DM_INFO DI
				SET DI.INFO_DOMAIN = rCUSTOM->DM_INFO[nLOOP].INFO_DOMAIN,
					DI.INFO_NAME = rCUSTOM->DM_INFO[nLOOP].INFO_NAME,
					DI.INFO_DATE = CNVTDATETIME(rCUSTOM->DM_INFO[nLOOP].INFO_DATE),
					DI.INFO_CHAR = rCUSTOM->DM_INFO[nLOOP].INFO_CHAR,
					DI.INFO_NUMBER = rCUSTOM->DM_INFO[nLOOP].INFO_NUMBER,
					DI.INFO_LONG_ID = rCUSTOM->DM_INFO[nLOOP].LONG_TEXT_ID,
					DI.UPDT_DT_TM = CNVTDATETIME(rCUSTOM->DM_INFO[nLOOP].UPDT_DT_TM),
					DI.UPDT_ID = rCUSTOM->DM_INFO[nLOOP].UPDT_ID,
					DI.INFO_DOMAIN_ID = rCUSTOM->DM_INFO[nLOOP].INFO_DOMAIN_ID
 
				IF (rCUSTOM->DM_INFO[nLOOP].LONG_TEXT_ID > 0)
					INSERT INTO LONG_TEXT LT
					SET LT.LONG_TEXT_ID = rCUSTOM->DM_INFO[nLOOP].LONG_TEXT_ID,
						LT.UPDT_DT_TM = SYSDATE,
						LT.UPDT_ID = rCUSTOM->DM_INFO[nLOOP].UPDT_ID,
						LT.ACTIVE_IND = 1,
						LT.ACTIVE_STATUS_DT_TM = SYSDATE,
						LT.ACTIVE_STATUS_PRSNL_ID = rCUSTOM->DM_INFO[nLOOP].UPDT_ID,
						LT.PARENT_ENTITY_NAME = "DM_INFO",
						LT.LONG_TEXT = rCUSTOM->DM_INFO[nLOOP].LONG_TEXT
				ENDIF
 
			; Perform Update
			; --------------
			ELSEIF (rCUSTOM->DM_INFO[nLOOP].ACTION_STATUS = "UPDATE")
				UPDATE INTO DM_INFO DI
				SET DI.INFO_DATE = CNVTDATETIME(rCUSTOM->DM_INFO[nLOOP].INFO_DATE),
					DI.INFO_CHAR = rCUSTOM->DM_INFO[nLOOP].INFO_CHAR,
					DI.INFO_NUMBER = rCUSTOM->DM_INFO[nLOOP].INFO_NUMBER,
					DI.INFO_LONG_ID = rCUSTOM->DM_INFO[nLOOP].LONG_TEXT_ID,
					DI.UPDT_DT_TM = CNVTDATETIME(rCUSTOM->DM_INFO[nLOOP].UPDT_DT_TM),
					DI.UPDT_ID = rCUSTOM->DM_INFO[nLOOP].UPDT_ID
				WHERE DI.INFO_DOMAIN = rCUSTOM->DM_INFO[nLOOP].INFO_DOMAIN
				AND DI.INFO_NAME = rCUSTOM->DM_INFO[nLOOP].INFO_NAME
				AND DI.INFO_DOMAIN_ID = rCUSTOM->DM_INFO[nLOOP].INFO_DOMAIN_ID
 
				IF (rCUSTOM->DM_INFO[nLOOP].LONG_TEXT_ID > 0)
					UPDATE INTO LONG_TEXT LT
					SET LT.UPDT_DT_TM = SYSDATE,
						LT.UPDT_ID = rCUSTOM->DM_INFO[nLOOP].UPDT_ID,
						LT.LONG_TEXT = rCUSTOM->DM_INFO[nLOOP].LONG_TEXT
					WHERE LT.LONG_TEXT_ID = rCUSTOM->DM_INFO[nLOOP].LONG_TEXT_ID
					AND LT.PARENT_ENTITY_NAME = "DM_INFO"
				ENDIF
 
			; Perform Delete
			; --------------
			ELSEIF (rCUSTOM->DM_INFO[nLOOP].ACTION_STATUS = "DELETE")
				DELETE FROM DM_INFO DI
				WHERE DI.INFO_DOMAIN = rCUSTOM->DM_INFO[nLOOP].INFO_DOMAIN
				AND DI.INFO_NAME = rCUSTOM->DM_INFO[nLOOP].INFO_NAME
				AND DI.INFO_DOMAIN_ID = rCUSTOM->DM_INFO[nLOOP].INFO_DOMAIN_ID
 
				IF (rCUSTOM->DM_INFO[nLOOP].LONG_TEXT_ID > 0)
					UPDATE INTO LONG_TEXT LT
					SET LT.UPDT_DT_TM = SYSDATE,
						LT.UPDT_ID = rCUSTOM->DM_INFO[nLOOP].UPDT_ID,
						LT.ACTIVE_IND = 0
					WHERE LT.LONG_TEXT_ID = rCUSTOM->DM_INFO[nLOOP].LONG_TEXT_ID
					AND LT.PARENT_ENTITY_NAME = "DM_INFO"
				ENDIF
 
 
			ENDIF
		ENDFOR
 
		COMMIT
 
	ENDIF
 
	; Write the output back to the MPage
	CALL ADD_CUSTOM_OUTPUT(CNVTRECTOJSON(rCUSTOM, 4, 1))
 
ENDIF
 
#END_PROGRAM
 
END GO
 
/*************************************************************************
 
        Script Name:    1co5_mpage_encounter.prg
 
        Description:    Clinical Office - MPage Developer
        				Encounter Data Retrieval
 
        Date Written:   August 31, 2025
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
 Called from 1CO5_MPAGE_ENTRY. Do not attempt to run stand alone.
 
 Possible Payload values:
 
	"patientSource":[
		{"personId": value, "encntrId": value}
	],
	"encounter": {
		"aliases": true,
		"encounterInfo": true,
		"encounterPlanReltn": true,
		"personReltn": true,
		"prsnlReltn": true,
		"locHist": true,
		"loadExtendedPersons": true,
		"skipJSON": true
	},
	"typeList": [
		{"codeSet": value, "type": "value", "typeCd": value}
	]
 
 
 
 *************************************************************************
                            Revision Information
 *************************************************************************
 Rev    Date     By             Comments
 ------ -------- -------------- ------------------------------------------
 001    08/31/25 J. Simpson     Initial Development
 *************************************************************************/

drop program 1co5_mpage_encounter:group1 go
create program 1co5_mpage_encounter:group1

; Check to see if running from mPage entry script
if (validate(payload->encounter) = 0 or size(patient_source->visits, 5) = 0)
	go to end_program
endif

; Required variables
declare cParser = vc
declare nNum = i4
declare cString = vc

free record rEncounter
record rEncounter (
    1 encounters[*]
        2 encntr_id                     = f8
        2 person_id                     = f8
        2 encntr_class                  = vc
        2 encntr_type                   = vc
        2 encntr_type_class             = vc
        2 encntr_status                 = vc
        2 pre_reg_dt_tm                 = dq8
        2 pre_reg_prsnl_id              = f8
        2 reg_dt_tm                     = dq8
        2 reg_prsnl_id                  = f8
        2 est_arrive_dt_tm              = dq8
        2 est_depart_dt_tm              = dq8
        2 arrive_dt_tm                  = dq8
        2 depart_dt_tm                  = dq8
        2 admit_type                    = vc
        2 admit_src                     = vc
        2 admit_mode                    = vc
        2 disch_disposition             = vc
        2 disch_to_loctn                = vc
        2 readmit                       = vc
        2 accommodation                 = vc
        2 accommodation_request         = vc
        2 accommodation_reason          = vc
        2 ambulatory_cond               = vc
        2 courtesy                      = vc
        2 isolation                     = vc
        2 med_service                   = vc
        2 confid_level                  = vc
        2 vip                           = vc
        2 location                      = vc
        2 loc_facility                  = vc
        2 loc_building                  = vc
        2 loc_nurse_unit                = vc
        2 loc_room                      = vc
        2 loc_bed                       = vc
        2 disch_dt_tm                   = dq8
        2 organization_id               = f8
        2 reason_for_visit              = vc
        2 encntr_financial_id           = f8
        2 financial_class               = vc
        2 trauma                        = vc
        2 triage                        = vc
        2 triage_dt_tm                  = dq8
        2 visitor_status                = vc
        2 inpatient_admit_dt_tm         = dq8
        2 encntr_class_cd               = f8
        2 encntr_type_cd                = f8
        2 encntr_type_class_cd          = f8
        2 encntr_status_cd              = f8
        2 admit_type_cd                 = f8
        2 admit_src_cd                  = f8
        2 admit_mode_cd                 = f8
        2 disch_disposition_cd          = f8
        2 disch_to_loctn_cd             = f8
        2 readmit_cd                    = f8
        2 accommodation_cd              = f8
        2 accommodation_request_cd      = f8
        2 accommodation_reason_cd       = f8
        2 ambulatory_cond_cd            = f8
        2 courtesy_cd                   = f8
        2 isolation_cd                  = f8
        2 med_service_cd                = f8
        2 confid_level_cd               = f8
        2 vip_cd                        = f8
        2 location_cd                   = f8
        2 location_org_id               = f8
        2 loc_facility_cd               = f8
        2 loc_building_cd               = f8
        2 loc_nurse_unit_cd             = f8
        2 loc_room_cd                   = f8
        2 loc_bed_cd                    = f8
        2 financial_class_cd            = f8
        2 trauma_cd                     = f8
        2 triage_cd                     = f8
        2 visitor_status_cd             = f8
        2 aliases[*]
            3 alias_pool                = vc
            3 alias_type                = vc
            3 alias_type_meaning        = vc
            3 alias                     = vc
            3 alias_formatted           = vc
            3 alias_sub_type            = vc
            3 alias_pool_cd             = f8
            3 encntr_alias_type_cd      = f8
            3 encntr_alias_sub_type_cd  = f8
        2 person_reltn[*]
            3 related_person_id         = f8
            3 name_full_formatted       = vc
            3 person_reltn_type         = vc
            3 person_reltn_type_meaning = vc
            3 person_reltn_type_cd      = f8
            3 person_reltn              = vc
            3 person_reltn_cd           = f8
            3 related_person_reltn      = vc
            3 related_person_reltn_cd   = f8
            3 contact_role              = vc
            3 contact_role_cd           = f8
            3 genetic_relationship_ind  = i4
            3 living_with_ind           = i4
            3 visitation_allowed        = vc
            3 visitation_allowed_cd     = f8
            3 family_reltn_sub_type     = vc
            3 family_reltn_sub_type_cd  = f8
            3 default_reltn_ind         = i4
            3 copy_correspondence       = vc
            3 copy_correspondence_cd    = f8
            3 priority_seq              = i4
            3 internal_seq              = i4
            3 relation_seq              = i4
        2 prsnl_reltn[*]
            3 reltn_type                = vc
            3 reltn_type_meaning        = vc
            3 person_id                 = f8
            3 priority_seq              = i4
            3 internal_seq              = i4
            3 prsnl_type                = vc
            3 name_full_formatted       = vc
            3 physician_ind             = i4
            3 position                  = vc
            3 name_last                 = vc
            3 name_first                = vc
            3 user_name                 = vc
            3 encntr_prsnl_r_cd         = f8
            3 prsnl_type_cd             = f8
            3 position_cd               = f8
        2 encntr_info[*]
            3 info_type                 = vc
            3 info_type_meaning         = vc
            3 info_sub_type             = vc
            3 info_sub_type_meaning     = vc
            3 value_numeric_ind         = i4
            3 value_numeric             = i4
            3 value_dt_tm               = dq8
            3 chartable_ind             = i4
            3 priority_seq              = i4
            3 internal_seq              = i4
            3 value                     = vc
            3 long_text                 = vc
            3 info_type_cd              = f8
            3 info_sub_type_cd          = f8
            3 value_cd                  = f8
        2 encounter_plan_reltn[*]
            3 encntr_plan_reltn_id      = f8
            3 person_plan_reltn_id      = f8
            3 health_plan_id            = f8
            3 organization_id           = f8
            3 priority_seq              = i4
            3 member_nbr                = vc
            3 signature_on_file         = vc
            3 signature_on_file_cd      = f8
            3 balance_type              = vc
            3 balance_type_cd           = f8
            3 deduct_amt                = f8
            3 deduct_met_amt            = f8
            3 deduct_met_dt_tm          = dq8
            3 verify_status             = vc
            3 verify_status_cd          = f8
            3 verify_dt_tm              = dq8
            3 verify_prsnl_id           = f8
            3 insured_card_name         = vc
            3 group_name                = vc
            3 group_nbr                 = vc
            3 policy_nbr                = vc
            3 member_person_code        = vc
            3 life_rsv_days             = i4
            3 life_rsv_remain_days      = i4
            3 life_rsv_daily_ded_amt    = f8
            3 life_rsv_daily_ded_qual   = vc
            3 life_rsv_daily_ded_qual_cd= f8
            3 card_issue_nbr            = i4
            3 card_category             = vc
            3 card_category_cd          = f8
            3 program_status            = vc
            3 program_status_cd         = f8
            3 denial_reason             = vc
            3 denial_reason_cd          = f8
            3 coverage_comments         = vc
            3 verify_source             = vc
            3 verify_source_cd          = f8
            3 ext_payer_name            = vc
            3 ext_payer_ident           = vc
            3 alt_member_nbr            = vc
            3 generic_health_plan_name  = vc
            3 plan_type                 = vc
            3 plan_type_cd              = f8
            3 plan_class                = vc
            3 plan_class_cd             = f8
            3 plan_name                 = vc
            3 plan_desc                 = vc
            3 financial_class           = vc
            3 financial_class_cd        = f8
            3 baby_coverage             = vc
            3 baby_coverage_cd          = f8
            3 comb_baby_bill            = vc
            3 comb_baby_bill_cd         = f8
            3 service_type              = vc
            3 service_type_cd           = f8
            3 plan_category             = vc
            3 plan_category_cd          = f8
            3 priority_ranking_nbr      = i4            
        2 loc_hist[*]
            3 beg_effective_dt_tm       = dq8
            3 end_effective_dt_tm       = dq8
            3 arrive_dt_tm              = dq8
            3 arrive_prsnl_id           = f8
            3 depart_dt_tm              = dq8
            3 depart_prsnl_id           = f8
            3 location                  = vc
            3 loc_facility              = vc
            3 loc_building              = vc
            3 loc_nurse_unit            = vc
            3 loc_room                  = vc
            3 loc_bed                   = vc
            3 encntr_type               = vc
            3 med_service               = vc
            3 transaction_dt_tm         = dq8
            3 activity_dt_tm            = dq8
            3 accommodation             = vc
            3 accommodation_request     = vc
            3 accommodation_reason      = vc
            3 admit_type                = vc
            3 isolation                 = vc
            3 organization_id           = f8
            3 encntr_type_class         = vc
            3 location_cd               = f8
            3 location_org_id           = f8
            3 loc_facility_cd           = f8
            3 loc_building_cd           = f8
            3 loc_nurse_unit_cd         = f8
            3 loc_room_cd               = f8
            3 loc_bed_cd                = f8
            3 encntr_type_cd            = f8
            3 med_service_cd            = f8
            3 accommodation_cd          = f8
            3 accommodation_request_cd  = f8
            3 accommodation_reason_cd   = f8
            3 admit_type_cd             = f8
            3 isolation_cd              = f8
            3 encntr_type_class_cd      = f8            
) with persistscript

; Alternate reference structure - Populate all structures with empty values
if (run_stats->reference_ind = 1)
    set stat = alterlist(rEncounter->encounters, 1)
    set stat = alterlist(rEncounter->encounters[1]->aliases, 1)
    set stat = alterlist(rEncounter->encounters[1]->prsnl_reltn, 1)
    set stat = alterlist(rEncounter->encounters[1]->encntr_info, 1)
    set stat = alterlist(rEncounter->encounters[1]->encounter_plan_reltn, 1)
    set stat = alterlist(rEncounter->encounters[1]->loc_hist, 1)
    set stat = alterlist(rEncounter->encounters[1]->person_reltn, 1)    
        
    ; Populate the code sets
    select into "nl:"
    from    dm_columns_doc      dcd
    plan dcd
        where dcd.table_name in ("ENCOUNTER", "ENCNTR_ALIAS", "ENCNTR_PRSNL_RELTN", "PRSNL", "ENCNTR_INFO", 
                                "ENCNTR_LOC_HIST", "ENCNTR_PERSON_RELTN")
        and dcd.code_set > 0
    detail
        call add_ref_code_set("encounters", camel_field(dcd.column_name), dcd.description, dcd.code_set)        
    with counter        
        
    go to skip_logic
endif 


; Initialize the population size
set stat = alterlist(rEncounter->encounters, size(patient_source->visits, 5))

; Loop through all the patients
for (nLoop = 1 to size(patient_source->visits, 5))
    set rEncounter->encounters[nLoop].encntr_id = patient_source->visits[nLoop].encntr_id
endfor

; Collect the core encounter
select into "nl:"
from 	encounter			e,
        location            l
plan e
    where expand(nNum, 1, size(rEncounter->encounters, 5), e.encntr_id, rEncounter->encounters[nNum].encntr_id)
join l
    where l.location_cd = e.location_cd    
detail
    nPos = locateval(nNum, 1, size(rEncounter->encounters, 5), e.encntr_id, rEncounter->encounters[nNum].encntr_id)
    rEncounter->encounters[nPos].person_id = e.person_id
    rEncounter->encounters[nPos].encntr_class = uar_get_code_display(e.encntr_class_cd)
    rEncounter->encounters[nPos].encntr_type = uar_get_code_display(e.encntr_type_cd)
    rEncounter->encounters[nPos].encntr_type_class = uar_get_code_display(e.encntr_type_class_cd)
    rEncounter->encounters[nPos].encntr_status = uar_get_code_display(e.encntr_status_cd)
    rEncounter->encounters[nPos].pre_reg_dt_tm = e.pre_reg_dt_tm
    rEncounter->encounters[nPos].pre_reg_prsnl_id = e.pre_reg_prsnl_id
    rEncounter->encounters[nPos].reg_dt_tm = e.reg_dt_tm
    rEncounter->encounters[nPos].reg_prsnl_id = e.reg_prsnl_id
    rEncounter->encounters[nPos].est_arrive_dt_tm = e.est_arrive_dt_tm
    rEncounter->encounters[nPos].est_depart_dt_tm = e.est_depart_dt_tm
    rEncounter->encounters[nPos].arrive_dt_tm = e.arrive_dt_tm
    rEncounter->encounters[nPos].depart_dt_tm = e.depart_dt_tm
    rEncounter->encounters[nPos].admit_type = uar_get_code_display(e.admit_type_cd)
    rEncounter->encounters[nPos].admit_src = uar_get_code_display(e.admit_src_cd)
    rEncounter->encounters[nPos].admit_mode = uar_get_code_display(e.admit_mode_cd)
    rEncounter->encounters[nPos].disch_disposition = uar_get_code_display(e.disch_disposition_cd)
    rEncounter->encounters[nPos].disch_to_loctn = uar_get_code_display(e.disch_to_loctn_cd)
    rEncounter->encounters[nPos].readmit = uar_get_code_display(e.readmit_cd)
    rEncounter->encounters[nPos].accommodation = uar_get_code_display(e.accommodation_cd)
    rEncounter->encounters[nPos].accommodation_request = uar_get_code_display(e.accommodation_request_cd)
    rEncounter->encounters[nPos].accommodation_reason = uar_get_code_display(e.accommodation_reason_cd)
    rEncounter->encounters[nPos].ambulatory_cond = uar_get_code_display(e.ambulatory_cond_cd)
    rEncounter->encounters[nPos].courtesy = uar_get_code_display(e.courtesy_cd)
    rEncounter->encounters[nPos].isolation = uar_get_code_display(e.isolation_cd)
    rEncounter->encounters[nPos].med_service = uar_get_code_display(e.med_service_cd)
    rEncounter->encounters[nPos].confid_level = uar_get_code_display(e.confid_level_cd)
    rEncounter->encounters[nPos].vip = uar_get_code_display(e.vip_cd)
    rEncounter->encounters[nPos].location = uar_get_code_display(e.location_cd)
    rEncounter->encounters[nPos].loc_facility = uar_get_code_display(e.loc_facility_cd)
    rEncounter->encounters[nPos].loc_building = uar_get_code_display(e.loc_building_cd)
    rEncounter->encounters[nPos].loc_nurse_unit = uar_get_code_display(e.loc_nurse_unit_cd)
    rEncounter->encounters[nPos].loc_room = uar_get_code_display(e.loc_room_cd)
    rEncounter->encounters[nPos].loc_bed = uar_get_code_display(e.loc_bed_cd)
    rEncounter->encounters[nPos].disch_dt_tm = e.disch_dt_tm
    rEncounter->encounters[nPos].organization_id = e.organization_id
    rEncounter->encounters[nPos].reason_for_visit = e.reason_for_visit
    rEncounter->encounters[nPos].encntr_financial_id = e.encntr_financial_id
    rEncounter->encounters[nPos].financial_class = uar_get_code_display(e.financial_class_cd)
    rEncounter->encounters[nPos].trauma = uar_get_code_display(e.trauma_cd)
    rEncounter->encounters[nPos].triage = uar_get_code_display(e.triage_cd)
    rEncounter->encounters[nPos].triage_dt_tm = e.triage_dt_tm
    rEncounter->encounters[nPos].visitor_status = uar_get_code_display(e.visitor_status_cd)
    rEncounter->encounters[nPos].inpatient_admit_dt_tm = e.inpatient_admit_dt_tm
    rEncounter->encounters[nPos].encntr_class_cd = e.encntr_class_cd
    rEncounter->encounters[nPos].encntr_type_cd = e.encntr_type_cd
    rEncounter->encounters[nPos].encntr_type_class_cd = e.encntr_type_class_cd
    rEncounter->encounters[nPos].encntr_status_cd = e.encntr_status_cd
    rEncounter->encounters[nPos].admit_type_cd = e.admit_type_cd
    rEncounter->encounters[nPos].admit_src_cd = e.admit_src_cd
    rEncounter->encounters[nPos].admit_mode_cd = e.admit_mode_cd
    rEncounter->encounters[nPos].disch_disposition_cd = e.disch_disposition_cd
    rEncounter->encounters[nPos].disch_to_loctn_cd = e.disch_to_loctn_cd
    rEncounter->encounters[nPos].readmit_cd = e.readmit_cd
    rEncounter->encounters[nPos].accommodation_cd = e.accommodation_cd
    rEncounter->encounters[nPos].accommodation_request_cd = e.accommodation_request_cd
    rEncounter->encounters[nPos].accommodation_reason_cd = e.accommodation_reason_cd
    rEncounter->encounters[nPos].ambulatory_cond_cd = e.ambulatory_cond_cd
    rEncounter->encounters[nPos].courtesy_cd = e.courtesy_cd
    rEncounter->encounters[nPos].isolation_cd = e.isolation_cd
    rEncounter->encounters[nPos].med_service_cd = e.med_service_cd
    rEncounter->encounters[nPos].confid_level_cd = e.confid_level_cd
    rEncounter->encounters[nPos].vip_cd = e.vip_cd
    rEncounter->encounters[nPos].location_cd = e.location_cd
    rEncounter->encounters[nPos].location_org_id = l.organization_id
    rEncounter->encounters[nPos].loc_facility_cd = e.loc_facility_cd
    rEncounter->encounters[nPos].loc_building_cd = e.loc_building_cd
    rEncounter->encounters[nPos].loc_room_cd = e.loc_room_cd
    rEncounter->encounters[nPos].loc_bed_cd = e.loc_bed_cd
    rEncounter->encounters[nPos].financial_class_cd = e.financial_class_cd
    rEncounter->encounters[nPos].trauma_cd = e.trauma_cd
    rEncounter->encounters[nPos].triage_cd = e.triage_cd
    rEncounter->encounters[nPos].visitor_status_cd = e.visitor_status_cd
    
    
    if (validate(payload->encounter->loadExtendedPersons, 0) = 1)
        call add_organization(e.organization_id)
        call add_prsnl(e.pre_reg_prsnl_id)
        call add_prsnl(e.reg_prsnl_id)
    endif    
        
with counter, expand=2

; Collect the Encounter Level Aliases
; -----------------------------------
if (validate(payload->encounter->aliases, 0) = 1)
 
    ; Set the Parser
    call type_parser("ea.encntr_alias_type_cd", 319)
 
    ; Collect the alias
    select into "nl:"
    from	encntr_alias		ea
    plan ea
        where expand(nNum, 1, size(rEncounter->encounters, 5), ea.encntr_id, rEncounter->encounters[nNum].encntr_id)
        and parser(cParser)
        and ea.active_ind = 1
        and ea.end_effective_dt_tm > sysdate
    order ea.encntr_id
	head ea.encntr_id
        nPos = locateval(nNum, 1, size(rEncounter->encounters, 5), ea.encntr_id, rEncounter->encounters[nNum].encntr_id)
        nCount = 0
    detail
        nCount = nCount + 1
        stat = alterlist(rEncounter->encounters[nPos].aliases, nCount)
            
        rEncounter->encounters[nPos].aliases[nCount].alias_pool = uar_get_code_display(ea.alias_pool_cd)
        rEncounter->encounters[nPos].aliases[nCount].alias_type = uar_get_code_display(ea.encntr_alias_type_cd)
        rEncounter->encounters[nPos].aliases[nCount].alias_type_meaning = uar_get_code_meaning(ea.encntr_alias_type_cd)
        rEncounter->encounters[nPos].aliases[nCount].alias = ea.alias
        rEncounter->encounters[nPos].aliases[nCount].alias_formatted = cnvtalias(ea.alias, ea.alias_pool_cd)
        rEncounter->encounters[nPos].aliases[nCount].alias_sub_type = uar_get_code_display(ea.encntr_alias_sub_type_cd)
        rEncounter->encounters[nPos].aliases[nCount].alias_pool_cd = ea.alias_pool_cd
        rEncounter->encounters[nPos].aliases[nCount].encntr_alias_type_cd = ea.encntr_alias_type_cd
        rEncounter->encounters[nPos].aliases[nCount].encntr_alias_sub_type_cd = ea.encntr_alias_sub_type_cd       
    with counter, expand=2
endif    

; Collect the Person relationships at the encounter level
; -------------------------------------------------------
if (validate(payload->encounter->personReltn, 0) = 1)

    ; Set the Parser
	call type_parser("epr.person_reltn_type_cd", 351)

    select into "nl:"
        encntr_id       = epr.encntr_id
    from    encntr_person_reltn     epr,
            person                  p
    plan epr
        where expand(nNum, 1, size(rEncounter->encounters, 5), epr.encntr_id, rEncounter->encounters[nNum].encntr_id)
        and parser(cParser)
    	and epr.active_ind = 1
		and epr.end_effective_dt_tm > sysdate
	join p
		where p.person_id = epr.related_person_id
    order encntr_id
	head encntr_id
        nPos = locateval(nNum, 1, size(rEncounter->encounters, 5), epr.encntr_id, rEncounter->encounters[nNum].encntr_id)
        nCount = 0
    detail
        nCount = nCount + 1
        stat = alterlist(rEncounter->encounters[nPos].person_reltn, nCount)
            
        rEncounter->encounters[nPos].person_reltn[nCount].related_person_id = epr.related_person_id
        rEncounter->encounters[nPos].person_reltn[nCount].name_full_formatted = p.name_full_formatted
        rEncounter->encounters[nPos].person_reltn[nCount].person_reltn_type = uar_get_code_display(epr.person_reltn_type_cd)
        rEncounter->encounters[nPos].person_reltn[nCount].person_reltn_type_meaning = uar_get_code_meaning(epr.person_reltn_type_cd)
        rEncounter->encounters[nPos].person_reltn[nCount].person_reltn_type_cd = epr.person_reltn_type_cd
        rEncounter->encounters[nPos].person_reltn[nCount].person_reltn = uar_get_code_display(epr.person_reltn_cd)
        rEncounter->encounters[nPos].person_reltn[nCount].person_reltn_cd = epr.person_reltn_cd
        rEncounter->encounters[nPos].person_reltn[nCount].related_person_reltn = uar_get_code_display(epr.related_person_reltn_cd)
        rEncounter->encounters[nPos].person_reltn[nCount].related_person_reltn_cd = epr.related_person_reltn_cd
        rEncounter->encounters[nPos].person_reltn[nCount].contact_role = uar_get_code_display(epr.contact_role_cd)
        rEncounter->encounters[nPos].person_reltn[nCount].contact_role_cd = epr.contact_role_cd
        rEncounter->encounters[nPos].person_reltn[nCount].genetic_relationship_ind = epr.genetic_relationship_ind
        rEncounter->encounters[nPos].person_reltn[nCount].living_with_ind = epr.living_with_ind
        rEncounter->encounters[nPos].person_reltn[nCount].visitation_allowed = uar_get_code_display(epr.visitation_allowed_cd)
        rEncounter->encounters[nPos].person_reltn[nCount].visitation_allowed_cd = epr.visitation_allowed_cd
        rEncounter->encounters[nPos].person_reltn[nCount].family_reltn_sub_type = 
                                                    uar_get_code_display(epr.family_reltn_sub_type_cd)
        rEncounter->encounters[nPos].person_reltn[nCount].family_reltn_sub_type_cd = epr.family_reltn_sub_type_cd
        rEncounter->encounters[nPos].person_reltn[nCount].default_reltn_ind = epr.default_reltn_ind
        rEncounter->encounters[nPos].person_reltn[nCount].copy_correspondence = uar_get_code_display(epr.copy_correspondence_cd)
        rEncounter->encounters[nPos].person_reltn[nCount].copy_correspondence_cd = epr.copy_correspondence_cd
        rEncounter->encounters[nPos].person_reltn[nCount].priority_seq = epr.priority_seq
        rEncounter->encounters[nPos].person_reltn[nCount].internal_seq = epr.internal_seq
        rEncounter->encounters[nPos].person_reltn[nCount].relation_seq = epr.relation_seq

        if (validate(payload->encounter->loadExtendedPersons, 0) = 1)
            call add_person_to_patient_source(epr.related_person_id)
        endif    
		
    with counter, expand=2
endif

; Collect the Prsnl relationships at the encounter level
; ------------------------------------------------------
if (validate(payload->encounter->prsnlreltn, 0) = 1)
		
    ; Set the Parser
	call type_parser("epr.encntr_prsnl_r_cd", 333)
		
    select into "nl:"
    from 	encntr_prsnl_reltn		epr,
			prsnl					p
    plan epr
        where expand(nNum, 1, size(rEncounter->encounters, 5), epr.encntr_id, rEncounter->encounters[nNum].encntr_id)
        and parser(cParser)
    	and epr.active_ind = 1
		and epr.end_effective_dt_tm > sysdate
	join p
		where p.person_id = epr.prsnl_person_id
    order epr.encntr_id
	head epr.encntr_id
        nPos = locateval(nNum, 1, size(rEncounter->encounters, 5), epr.encntr_id, rEncounter->encounters[nNum].encntr_id)
        nCount = 0
    detail
        nCount = nCount + 1
        stat = alterlist(rEncounter->encounters[nPos].prsnl_reltn, nCount)
        
        rEncounter->encounters[nPos].prsnl_reltn[nCount].reltn_type = uar_get_code_display(epr.encntr_prsnl_r_cd)
        rEncounter->encounters[nPos].prsnl_reltn[nCount].reltn_type_meaning = uar_get_code_meaning(epr.encntr_prsnl_r_cd)
        rEncounter->encounters[nPos].prsnl_reltn[nCount].person_id = p.person_id
        rEncounter->encounters[nPos].prsnl_reltn[nCount].priority_seq = epr.priority_seq
        rEncounter->encounters[nPos].prsnl_reltn[nCount].internal_seq = epr.internal_seq
        rEncounter->encounters[nPos].prsnl_reltn[nCount].prsnl_type = uar_get_code_display(p.prsnl_type_cd)
        rEncounter->encounters[nPos].prsnl_reltn[nCount].name_full_formatted = p.name_full_formatted
        rEncounter->encounters[nPos].prsnl_reltn[nCount].physician_ind = p.physician_ind
        rEncounter->encounters[nPos].prsnl_reltn[nCount].position = uar_get_code_display(p.position_cd)
        rEncounter->encounters[nPos].prsnl_reltn[nCount].name_last = p.name_last
        rEncounter->encounters[nPos].prsnl_reltn[nCount].name_first = p.name_first
        rEncounter->encounters[nPos].prsnl_reltn[nCount].user_name = p.username
        rEncounter->encounters[nPos].prsnl_reltn[nCount].encntr_prsnl_r_cd = epr.encntr_prsnl_r_cd
        rEncounter->encounters[nPos].prsnl_reltn[nCount].prsnl_type_cd = p.prsnl_type_cd
        rEncounter->encounters[nPos].prsnl_reltn[nCount].position_cd = p.position_cd
        
        if (validate(payload->encounter->loadExtendedPersons, 0) = 1)
            call add_prsnl(p.person_id)
        endif    
        
    with counter, expand=2
endif

; Collect the ENCOUNTER_INFO records
; ----------------------------------
if (validate(payload->encounter->encounterInfo, 0) = 1)
    ; set the parser
    call type_parser("ei.info_sub_type_cd", 356)
 
    select into "nl:"
    from 	encntr_info			ei,
	       	long_text			lt
    plan ei
        where expand(nNum, 1, size(rEncounter->encounters, 5), ei.encntr_id, rEncounter->encounters[nNum].encntr_id)
        and parser(cParser)
        and ei.active_ind = 1
        and ei.end_effective_dt_tm > sysdate
    join lt
        where lt.long_text_id = outerjoin(ei.long_text_id)
    order ei.encntr_id
	head ei.encntr_id
        nPos = locateval(nNum, 1, size(rEncounter->encounters, 5), ei.encntr_id, rEncounter->encounters[nNum].encntr_id)
        nCount = 0
    detail
        nCount = nCount + 1
        stat = alterlist(rEncounter->encounters[nPos].encntr_info, nCount)
        
        rEncounter->encounters[nPos].encntr_info[nCount].info_type = uar_get_code_display(ei.info_type_cd)
        rEncounter->encounters[nPos].encntr_info[nCount].info_type_meaning = uar_get_code_meaning(ei.info_type_cd)
        rEncounter->encounters[nPos].encntr_info[nCount].info_sub_type = uar_get_code_display(ei.info_sub_type_cd)
        rEncounter->encounters[nPos].encntr_info[nCount].info_sub_type_meaning = uar_get_code_meaning(ei.info_sub_type_cd)
        rEncounter->encounters[nPos].encntr_info[nCount].value_numeric_ind = ei.value_numeric_ind
        rEncounter->encounters[nPos].encntr_info[nCount].value_numeric = ei.value_numeric
        rEncounter->encounters[nPos].encntr_info[nCount].value_dt_tm = ei.value_dt_tm
        rEncounter->encounters[nPos].encntr_info[nCount].chartable_ind = ei.chartable_ind
        rEncounter->encounters[nPos].encntr_info[nCount].priority_seq = ei.priority_seq
        rEncounter->encounters[nPos].encntr_info[nCount].internal_seq = ei.internal_seq
        rEncounter->encounters[nPos].encntr_info[nCount].value = uar_get_code_display(ei.value_cd)
        rEncounter->encounters[nPos].encntr_info[nCount].long_text = lt.long_text
        rEncounter->encounters[nPos].encntr_info[nCount].info_type_cd = ei.info_type_cd
        rEncounter->encounters[nPos].encntr_info[nCount].info_sub_type_cd = ei.info_sub_type_cd
        rEncounter->encounters[nPos].encntr_info[nCount].value_cd = ei.value_cd
    with counter, expand=2
endif

; Collect the encounter plan relationships with health plan fields
; ----------------------------------------------------------------
if (validate(payload->encounter->encounterPlanReltn, 0) = 1)

    select into "nl:"
        person_id               = epr.person_id,
        priority_seq            = epr.priority_seq
    from    encntr_plan_reltn        epr,
            health_plan              hp,
            long_text                lt
    plan epr
        where expand(nNum, 1, size(rEncounter->encounters, 5), epr.encntr_id, rEncounter->encounters[nNum].encntr_id)
        and epr.active_ind = 1
        and epr.active_status_cd = cv48_Active
        and epr.end_effective_dt_tm > sysdate
    join hp
        where hp.health_plan_id = epr.health_plan_id
    join lt
        where lt.long_text_id = outerjoin(epr.coverage_comments_long_text_id)
    order person_id, priority_seq
    detail
        nPos = locateval(nNum, 1, size(rEncounter->encounters, 5), epr.person_id, rEncounter->encounters[nNum].person_id)
    
        nPlanCount = size(rEncounter->encounters[nNum].encounter_plan_reltn, 5)+1
        stat = alterlist(rEncounter->encounters[nNum].encounter_plan_reltn, nPlanCount)
        
        rEncounter->encounters[nNum].encounter_plan_reltn[nPlanCount].encntr_plan_reltn_id = epr.encntr_plan_reltn_id
        rEncounter->encounters[nNum].encounter_plan_reltn[nPlanCount].person_plan_reltn_id = epr.person_plan_reltn_id
        rEncounter->encounters[nNum].encounter_plan_reltn[nPlanCount].health_plan_id = epr.health_plan_id
        rEncounter->encounters[nNum].encounter_plan_reltn[nPlanCount].organization_id = epr.organization_id
        rEncounter->encounters[nNum].encounter_plan_reltn[nPlanCount].priority_seq = epr.priority_seq
        rEncounter->encounters[nNum].encounter_plan_reltn[nPlanCount].member_nbr = epr.member_nbr
        rEncounter->encounters[nNum].encounter_plan_reltn[nPlanCount].signature_on_file = 
                uar_get_code_display(epr.signature_on_file_cd)
        rEncounter->encounters[nNum].encounter_plan_reltn[nPlanCount].signature_on_file_cd = epr.signature_on_file_cd
        rEncounter->encounters[nNum].encounter_plan_reltn[nPlanCount].balance_type = uar_get_code_display(epr.balance_type_cd)
        rEncounter->encounters[nNum].encounter_plan_reltn[nPlanCount].balance_type_cd = epr.balance_type_cd
        rEncounter->encounters[nNum].encounter_plan_reltn[nPlanCount].deduct_amt = epr.deduct_amt
        rEncounter->encounters[nNum].encounter_plan_reltn[nPlanCount].deduct_met_amt = epr.deduct_met_amt
        rEncounter->encounters[nNum].encounter_plan_reltn[nPlanCount].deduct_met_dt_tm = epr.deduct_met_dt_tm
        rEncounter->encounters[nNum].encounter_plan_reltn[nPlanCount].verify_status = uar_get_code_display(epr.verify_status_cd)
        rEncounter->encounters[nNum].encounter_plan_reltn[nPlanCount].verify_status_cd = epr.verify_status_cd
        rEncounter->encounters[nNum].encounter_plan_reltn[nPlanCount].verify_dt_tm = epr.verify_dt_tm
        rEncounter->encounters[nNum].encounter_plan_reltn[nPlanCount].verify_prsnl_id = epr.verify_prsnl_id
        rEncounter->encounters[nNum].encounter_plan_reltn[nPlanCount].insured_card_name = epr.insured_card_name
        rEncounter->encounters[nNum].encounter_plan_reltn[nPlanCount].group_name = hp.group_name
        rEncounter->encounters[nNum].encounter_plan_reltn[nPlanCount].group_nbr = hp.group_nbr
        rEncounter->encounters[nNum].encounter_plan_reltn[nPlanCount].policy_nbr = hp.policy_nbr
        rEncounter->encounters[nNum].encounter_plan_reltn[nPlanCount].member_person_code = epr.member_person_code
        rEncounter->encounters[nNum].encounter_plan_reltn[nPlanCount].life_rsv_days = epr.life_rsv_days
        rEncounter->encounters[nNum].encounter_plan_reltn[nPlanCount].life_rsv_remain_days = epr.life_rsv_remain_days
        rEncounter->encounters[nNum].encounter_plan_reltn[nPlanCount].life_rsv_daily_ded_amt = epr.life_rsv_daily_ded_amt
        rEncounter->encounters[nNum].encounter_plan_reltn[nPlanCount].life_rsv_daily_ded_qual = 
                                                                           uar_get_code_display(epr.life_rsv_daily_ded_qual_cd)
        rEncounter->encounters[nNum].encounter_plan_reltn[nPlanCount].life_rsv_daily_ded_qual_cd = epr.life_rsv_daily_ded_qual_cd
        rEncounter->encounters[nNum].encounter_plan_reltn[nPlanCount].card_issue_nbr = epr.card_issue_nbr
        rEncounter->encounters[nNum].encounter_plan_reltn[nPlanCount].card_category = uar_get_code_display(epr.card_category_cd)
        rEncounter->encounters[nNum].encounter_plan_reltn[nPlanCount].card_category_cd = epr.card_category_cd
        rEncounter->encounters[nNum].encounter_plan_reltn[nPlanCount].program_status = uar_get_code_display(epr.program_status_cd)
        rEncounter->encounters[nNum].encounter_plan_reltn[nPlanCount].program_status_cd = epr.program_status_cd
        rEncounter->encounters[nNum].encounter_plan_reltn[nPlanCount].denial_reason = uar_get_code_display(epr.denial_reason_cd)
        rEncounter->encounters[nNum].encounter_plan_reltn[nPlanCount].denial_reason_cd = epr.denial_reason_cd
        rEncounter->encounters[nNum].encounter_plan_reltn[nPlanCount].coverage_comments = lt.long_text
        rEncounter->encounters[nNum].encounter_plan_reltn[nPlanCount].verify_source = uar_get_code_display(epr.verify_source_cd)
        rEncounter->encounters[nNum].encounter_plan_reltn[nPlanCount].verify_source_cd = epr.verify_source_cd
        rEncounter->encounters[nNum].encounter_plan_reltn[nPlanCount].ext_payer_name = epr.ext_payer_name
        rEncounter->encounters[nNum].encounter_plan_reltn[nPlanCount].ext_payer_ident = epr.ext_payer_ident
        rEncounter->encounters[nNum].encounter_plan_reltn[nPlanCount].alt_member_nbr = epr.alt_member_nbr
        rEncounter->encounters[nNum].encounter_plan_reltn[nPlanCount].generic_health_plan_name = epr.generic_health_plan_name
        rEncounter->encounters[nNum].encounter_plan_reltn[nPlanCount].plan_type = uar_get_code_display(hp.plan_type_cd)
        rEncounter->encounters[nNum].encounter_plan_reltn[nPlanCount].plan_type_cd = hp.plan_type_cd
        rEncounter->encounters[nNum].encounter_plan_reltn[nPlanCount].plan_class = uar_get_code_display(hp.plan_class_cd)
        rEncounter->encounters[nNum].encounter_plan_reltn[nPlanCount].plan_class_cd = hp.plan_class_cd
        rEncounter->encounters[nNum].encounter_plan_reltn[nPlanCount].plan_name = hp.plan_name
        rEncounter->encounters[nNum].encounter_plan_reltn[nPlanCount].plan_desc = hp.plan_desc
        rEncounter->encounters[nNum].encounter_plan_reltn[nPlanCount].financial_class = uar_get_code_display(hp.financial_class_cd)
        rEncounter->encounters[nNum].encounter_plan_reltn[nPlanCount].financial_class_cd = hp.financial_class_cd
        rEncounter->encounters[nNum].encounter_plan_reltn[nPlanCount].baby_coverage = uar_get_code_display(hp.baby_coverage_cd)
        rEncounter->encounters[nNum].encounter_plan_reltn[nPlanCount].baby_coverage_cd = hp.baby_coverage_cd
        rEncounter->encounters[nNum].encounter_plan_reltn[nPlanCount].comb_baby_bill = uar_get_code_display(hp.comb_baby_bill_cd)
        rEncounter->encounters[nNum].encounter_plan_reltn[nPlanCount].comb_baby_bill_cd = hp.comb_baby_bill_cd
        rEncounter->encounters[nNum].encounter_plan_reltn[nPlanCount].service_type = uar_get_code_display(hp.service_type_cd)
        rEncounter->encounters[nNum].encounter_plan_reltn[nPlanCount].service_type_cd = hp.service_type_cd
        rEncounter->encounters[nNum].encounter_plan_reltn[nPlanCount].plan_category = uar_get_code_display(hp.plan_category_cd)
        rEncounter->encounters[nNum].encounter_plan_reltn[nPlanCount].plan_category_cd = hp.plan_category_cd
        rEncounter->encounters[nNum].encounter_plan_reltn[nPlanCount].priority_ranking_nbr = hp.priority_ranking_nbr
        
        if (validate(payload->encounter->loadExtendedPersons, 0) = 1)
            call add_organization(epr.organization_id)
            call add_prsnl(epr.verify_prsnl_id)
        endif
            
    with expand=2, uar_code(d)

endif


; Collect the ENCOUNTER_LOC_HIST records
; --------------------------------------
if (validate(payload->encounter->lochist, 0) = 1)
    select into "nl:"
    from 	encntr_loc_hist		elh,
            location            l
    plan elh
        where expand(nNum, 1, size(rEncounter->encounters, 5), elh.encntr_id, rEncounter->encounters[nNum].encntr_id)
        and elh.active_ind = 1
    join l
        where l.location_cd = elh.location_cd        
    order elh.encntr_id
	head elh.encntr_id
        nPos = locateval(nNum, 1, size(rEncounter->encounters, 5), elh.encntr_id, rEncounter->encounters[nNum].encntr_id)
        nCount = 0
    detail
        nCount = nCount + 1
        stat = alterlist(rEncounter->encounters[nPos].loc_hist, nCount)
                        
        rEncounter->encounters[nPos].loc_hist[nCount].beg_effective_dt_tm = elh.beg_effective_dt_tm
        rEncounter->encounters[nPos].loc_hist[nCount].end_effective_dt_tm = elh.end_effective_dt_tm
        rEncounter->encounters[nPos].loc_hist[nCount].arrive_dt_tm = elh.arrive_dt_tm
        rEncounter->encounters[nPos].loc_hist[nCount].arrive_prsnl_id = elh.arrive_prsnl_id
        rEncounter->encounters[nPos].loc_hist[nCount].depart_dt_tm = elh.depart_dt_tm
        rEncounter->encounters[nPos].loc_hist[nCount].depart_prsnl_id = elh.depart_prsnl_id
        rEncounter->encounters[nPos].loc_hist[nCount].location = uar_get_code_display(elh.location_cd)
        rEncounter->encounters[nPos].loc_hist[nCount].loc_facility = uar_get_code_display(elh.loc_facility_cd)
        rEncounter->encounters[nPos].loc_hist[nCount].loc_building = uar_get_code_display(elh.loc_building_cd)
        rEncounter->encounters[nPos].loc_hist[nCount].loc_nurse_unit = uar_get_code_display(elh.loc_nurse_unit_cd)
        rEncounter->encounters[nPos].loc_hist[nCount].loc_room = uar_get_code_display(elh.loc_room_cd)
        rEncounter->encounters[nPos].loc_hist[nCount].loc_bed = uar_get_code_display(elh.loc_bed_cd)
        rEncounter->encounters[nPos].loc_hist[nCount].encntr_type = uar_get_code_display(elh.encntr_type_cd)
        rEncounter->encounters[nPos].loc_hist[nCount].med_service = uar_get_code_display(elh.med_service_cd)
        rEncounter->encounters[nPos].loc_hist[nCount].transaction_dt_tm = elh.transaction_dt_tm
        rEncounter->encounters[nPos].loc_hist[nCount].activity_dt_tm = elh.activity_dt_tm
        rEncounter->encounters[nPos].loc_hist[nCount].accommodation = uar_get_code_display(elh.accommodation_cd)
        rEncounter->encounters[nPos].loc_hist[nCount].accommodation_request = uar_get_code_display(elh.accommodation_request_cd)
        rEncounter->encounters[nPos].loc_hist[nCount].accommodation_reason = uar_get_code_display(elh.accommodation_reason_cd)
        rEncounter->encounters[nPos].loc_hist[nCount].admit_type = uar_get_code_display(elh.admit_type_cd)
        rEncounter->encounters[nPos].loc_hist[nCount].isolation = uar_get_code_display(elh.isolation_cd)
        rEncounter->encounters[nPos].loc_hist[nCount].organization_id = elh.organization_id
        rEncounter->encounters[nPos].loc_hist[nCount].encntr_type_class = uar_get_code_display(elh.encntr_type_class_cd)
        rEncounter->encounters[nPos].loc_hist[nCount].location_cd = elh.location_cd
        rEncounter->encounters[nPos].loc_hist[nCount].location_org_id = l.organization_id
        rEncounter->encounters[nPos].loc_hist[nCount].loc_facility_cd = elh.loc_facility_cd
        rEncounter->encounters[nPos].loc_hist[nCount].loc_building_cd = elh.loc_building_cd
        rEncounter->encounters[nPos].loc_hist[nCount].loc_nurse_unit_cd = elh.loc_nurse_unit_cd
        rEncounter->encounters[nPos].loc_hist[nCount].loc_room_cd = elh.loc_room_cd
        rEncounter->encounters[nPos].loc_hist[nCount].loc_bed_cd = elh.loc_bed_cd
        rEncounter->encounters[nPos].loc_hist[nCount].encntr_type_cd = elh.encntr_type_cd
        rEncounter->encounters[nPos].loc_hist[nCount].med_service_cd = elh.med_service_cd
        rEncounter->encounters[nPos].loc_hist[nCount].accommodation_cd = elh.accommodation_cd
        rEncounter->encounters[nPos].loc_hist[nCount].accommodation_request_cd = elh.accommodation_request_cd
        rEncounter->encounters[nPos].loc_hist[nCount].accommodation_reason_cd = elh.accommodation_reason_cd
        rEncounter->encounters[nPos].loc_hist[nCount].admit_type_cd = elh.admit_type_cd
        rEncounter->encounters[nPos].loc_hist[nCount].isolation_cd = elh.isolation_cd
        rEncounter->encounters[nPos].loc_hist[nCount].encntr_type_class_cd = elh.encntr_type_class_cd       

        if (validate(payload->encounter->loadExtendedPersons, 0) = 1)
            call add_organization(elh.organization_id)
            call add_organization(l.organization_id)
            call add_prsnl(elh.arrive_prsnl_id)
            call add_prsnl(elh.depart_prsnl_id)
        endif    

    with counter, expand=2
endif    

#skip_logic

if (validate(payload->encounter->skipJSON, 0) = 0)
    call add_standard_output(cnvtrectojson(rEncounter, 4, 1))
endif

#end_program

end go 
/*************************************************************************
 
        Script Name:    1co5_mpage_enc_list.prg
 
        Description:    Clinical Office - MPage Developer
        				Encounter Load List Script
 
        Date Written:   May 15, 2018
        Written by:     John Simpson
                        Precision Healthcare Solutions
 
 *************************************************************************
		   Copyright (c) 2018 Precision Healthcare Solutions
 
 NO PART OF THIS CODE MAY BE COPIED, MODIFIED OR DISTRIBUTED WITHOUT
 PRIOR WRITTEN CONSENT OF PRECISION HEALTHCARE SOLUTIONS EXECUTIVE
 LEADERSHIP TEAM.
 
 FOR LICENSING TERMS PLEASE VISIT www.clinicaloffice.com/mpage/license
 
 *************************************************************************
                            Special Instructions
 *************************************************************************
 Called from 1CO_MPAGE_ENTRY. Do not attempt to run stand alone. If you
 wish to test the development of your custom script from the CCL back-end,
 please run with 1CO_MPAGE_TEST.
 
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
	},
	"typeList": [
		{"codeSet": value, "type": "value", "typeCd": value}
	]
 
 
 
 *************************************************************************
                            Revision Information
 *************************************************************************
 Rev    Date     By             Comments
 ------ -------- -------------- ------------------------------------------
 001    05/07/18 J. Simpson     Initial Development
 002	08/15/18 J. Simpson		Added ability to filter by org name
 003	06/25/19 J. Simpson		Changed org name filter to support multiple orgs
 004	01/18/20 J. Simpson		Added ability to filter by encntr_type_class_cd
 *************************************************************************/
DROP PROGRAM 1co5_mpage_enc_list:GROUP1 GO
CREATE PROGRAM 1co5_mpage_enc_list:GROUP1
 
SET cDATE_FIELD = PAYLOAD->CUSTOMSCRIPT->SCRIPT[nSCRIPT]->PARAMETERS.DATEFIELD
SET dFROM_DATE = PAYLOAD->CUSTOMSCRIPT->SCRIPT[nSCRIPT]->PARAMETERS.FROMDATE
SET dTO_DATE = PAYLOAD->CUSTOMSCRIPT->SCRIPT[nSCRIPT]->PARAMETERS.TODATE
  
; Define the custom record structure you wish to have sent back in the JSON to the mPage. The name
; of the record structure can be anything you want.
FREE RECORD rCUSTOM
RECORD rCUSTOM (
	1 VISITS[*]
		2 PERSON_ID					= f8
		2 ENCNTR_ID					= f8
)
 
; Set the Parser for the various filters
DECLARE cPARSER  = vc
DECLARE cPARSER2 = vc
DECLARE cPARSER3 = vc
DECLARE cPARSER4 = vc
DECLARE cPARSER5 = vc
DECLARE cPARSER6 = vc
DECLARE cPARSER7 = vc
DECLARE nNum = i4
 
CALL TYPE_PARSER("E.DISCH_DISPOSITION_CD", 19)
SET cPARSER2 = cPARSER
CALL TYPE_PARSER("E.ENCNTR_TYPE_CD", 71)
SET cPARSER3 = cPARSER
CALL TYPE_PARSER("E.MED_SERVICE_CD", 34)
SET cPARSER4 = cPARSER
CALL TYPE_PARSER("E.LOC_FACILITY_CD", 220)
SET cPARSER5 = cPARSER
CALL TYPE_PARSER("E.LOC_NURSE_UNIT_CD", 220)
SET cPARSER7 = cPARSER
CALL TYPE_PARSER("E.ENCNTR_TYPE_CLASS_CD", 69)
 
; Parse the Organization Name
SET cPARSER6 = "1=1"
 
IF (VALIDATE(PAYLOAD->CUSTOMSCRIPT->SCRIPT[nSCRIPT]->PARAMETERS.ORGANIZATIONS) > 0)
	SET cPARSER6 = "1=0"	; Set false in case org name does not exist
 
	SELECT INTO "NL:"
		ORGANIZATION_ID			= O.ORGANIZATION_ID
	FROM	ORGANIZATION		O
	PLAN O
		WHERE EXPAND(nNum, 1, SIZE(PAYLOAD->CUSTOMSCRIPT->SCRIPT[nSCRIPT]->PARAMETERS->ORGANIZATIONS, 5), O.ORG_NAME_KEY,
						PAYLOAD->CUSTOMSCRIPT->SCRIPT[nSCRIPT]->PARAMETERS->ORGANIZATIONS[nNum].ORGNAME)
	HEAD REPORT
		cPARSER6 = ""
	DETAIL
		IF (TRIM(cPARSER6) = "")
			cPARSER6 = CONCAT(cPARSER6, BUILD(ORGANIZATION_ID))
		ELSE
			cPARSER6 = CONCAT(cPARSER6, "," , BUILD(ORGANIZATION_ID))
		ENDIF
	FOOT REPORT
		cPARSER6 = CONCAT("E.ORGANIZATION_ID IN (", TRIM(cPARSER6, 3), ")")
	WITH COUNTER
ENDIF
  
; If the patient list was cleared, we will look for encounters for anybody
IF (SIZE(PATIENT_SOURCE->PATIENTS, 5) = 0)
call echo(cparser6)
 
	SELECT INTO "NL:"
		PERSON_ID			= E.PERSON_ID,
		ENCNTR_ID			= E.ENCNTR_ID
	FROM	ENCOUNTER				E
	PLAN E
		WHERE PARSER(CONCAT("E.", cDATE_FIELD, " BETWEEN CNVTDATETIME(", BUILD(dFROM_DATE),
					") AND CNVTDATETIME(", BUILD(dTO_DATE), ")"))
		AND PARSER(cPARSER)
		AND PARSER(cPARSER2)
		AND PARSER(cPARSER3)
		AND PARSER(cPARSER4)
		AND PARSER(cPARSER5)
		AND PARSER(cPARSER6)
		AND E.ACTIVE_IND = 1
	ORDER PERSON_ID, ENCNTR_ID
	HEAD REPORT
		nPERSON = 0
		nENCOUNTER = 0
	HEAD PERSON_ID
		nPERSON = nPERSON + 1
 
		STAT = ALTERLIST(PATIENT_SOURCE->PATIENTS, nPERSON)
		PATIENT_SOURCE->PATIENTS[nPERSON].PERSON_ID = PERSON_ID
	HEAD ENCNTR_ID
		nENCOUNTER = nENCOUNTER + 1
 
		STAT = ALTERLIST(PATIENT_SOURCE->VISITS, nENCOUNTER)
		PATIENT_SOURCE->VISITS[nENCOUNTER].PERSON_ID = PERSON_ID
		PATIENT_SOURCE->VISITS[nENCOUNTER].ENCNTR_ID = ENCNTR_ID
	WITH NOCOUNTER
 
; If the patient list was not cleared, get the encounters for the listed patients
ELSE
	; First make a copy of the patient source
	SET STAT = COPYREC(PATIENT_SOURCE, TEMP_PAT_SOURCE, 1)
 
	; Clear the patient source as we need to populate it with our results
	SET STAT = INITREC(PATIENT_SOURCE)
 
	SELECT INTO "NL:"
		PERSON_ID			= E.PERSON_ID,
		ENCNTR_ID			= E.ENCNTR_ID
	FROM	(DUMMYT					D WITH SEQ=VALUE(SIZE(TEMP_PAT_SOURCE->PATIENTS, 5))),
			ENCOUNTER				E
	PLAN D
	JOIN E
		WHERE E.PERSON_ID = TEMP_PAT_SOURCE->PATIENTS[D.SEQ].PERSON_ID
		AND PARSER(CONCAT("E.", cDATE_FIELD, " BETWEEN CNVTDATETIME(", BUILD(dFROM_DATE),
					") AND CNVTDATETIME(", BUILD(dTO_DATE), ")"))
		AND PARSER(cPARSER)
		AND PARSER(cPARSER2)
		AND PARSER(cPARSER3)
		AND PARSER(cPARSER4)
		AND PARSER(cPARSER5)
		AND PARSER(cPARSER6)
		AND E.ACTIVE_IND = 1
	ORDER PERSON_ID, ENCNTR_ID
	HEAD REPORT
		nPERSON = 0
		nENCOUNTER = 0
	HEAD PERSON_ID
		nPERSON = nPERSON + 1
 
		STAT = ALTERLIST(PATIENT_SOURCE->PATIENTS, nPERSON)
		PATIENT_SOURCE->PATIENTS[nPERSON].PERSON_ID = PERSON_ID
	HEAD ENCNTR_ID
		nENCOUNTER = nENCOUNTER + 1
 
		STAT = ALTERLIST(PATIENT_SOURCE->VISITS, nENCOUNTER)
		PATIENT_SOURCE->VISITS[nENCOUNTER].PERSON_ID = PERSON_ID
		PATIENT_SOURCE->VISITS[nENCOUNTER].ENCNTR_ID = ENCNTR_ID
	WITH NOCOUNTER
ENDIF
 
IF (SIZE(PATIENT_SOURCE->VISITS, 5) > 0)
 
	SET STAT = MOVEREC(PATIENT_SOURCE->VISITS, rCUSTOM)
    CALL ADD_CUSTOM_OUTPUT(CNVTRECTOJSON(rCUSTOM, 4, 1))
 
ENDIF
 
 
 
#END_PROGRAM
 
END GO
 
/*************************************************************************
 
        Script Name:    1co5_mpage_entry.prg
 
        Description:    Clinical Office - MPage Developer
        				Chart level initial entry point for mPages
 
        Date Written:   December 12, 2024
        Written by:     John Simpson
                        Precision Healthcare Solutions
 
 *************************************************************************
		   Copyright (c) 2025 Precision Healthcare Solutions
 
 NO PART OF THIS CODE MAY BE COPIED, MODIFIED OR DISTRIBUTED WITHOUT
 PRIOR WRITTEN CONSENT OF PRECISION HEALTHCARE SOLUTIONS EXECUTIVE
 LEADERSHIP TEAM.
 
 FOR LICENSING TERMS PLEASE VISIT www.clinicaloffice.com
 
 *************************************************************************
                            Special Instructions
 *************************************************************************
 Called from Angular mPage Application through Chart Level mPages
 *************************************************************************
                            Revision Information
 *************************************************************************
 Rev    Date     By             Comments
 ------ -------- -------------- ------------------------------------------
 001    12/12/24 J. Simpson     Initial Development
 *************************************************************************/
 
DROP PROGRAM 1co5_mpage_entry:group1 GO
CREATE PROGRAM 1co5_mpage_entry:group1
 
prompt
	"Output to File/Printer/MINE" = "MINE"     ;* Enter or select the printer or file name to send this report to.
	, "Person ID" = 0
	, "Encounter ID" = 0
	, "Debugger Indicator" = 0
	, "Process ID" = 0
	, "Extra Configuration Information" = ""
 
with outdev, person_id, encntr_id, debug_ind, id, config
 
; Set the absolute maximum possible variable length
set modify maxvarlen 268435456
 
; Variable declarations
declare _Memory_Reply_String = vc
declare cParser = vc
declare nPrsnl_Id = f8 with noconstant(reqinfo->updt_id)
declare cv48_Active = f8 with noconstant(uar_get_code_by("MEANING", 48, "ACTIVE"))
declare nCopyChartIdToPayload = i4 with noconstant(0)
declare nPos = i4
declare nNum = i4

; Subroutine declarations
declare add_ref_code_set(cObjectName=vc, cColumnName=vc, cDescription=vc, nCodeSet=i4)=null
declare combine_strings(cString1=vc, cString2=vc, cDelimiter=vc)=vc
declare add_person_to_patient_source(nPersonId=f8)=null
declare add_prsnl(nPersonId=f8)=null
declare add_organization(nOrganizationId=f8)=null
declare camel_field(cText=vc)=vc
declare writeDebugSession(null)=null

; Record structure definitions
; Define run stats record structure
free record run_stats
record run_stats (
	1 id			       = i4
	1 start_time	       = dq8
	1 end_time		       = dq8
	1 status		       = vc
	1 hex_mode             = i4
	1 debug_file           = vc
	1 reference_ind        = i4
	1 domain               = vc
	1 node                 = vc	
	1 prsnl_id		       = f8
	1 prsnl_name           = vc
	1 physician_ind        = i4
	1 position_cd          = f8
	1 position             = vc
	1 username            = vc
)

; Chart Record Structure
free record chart_id
record chart_id (
	1 person_id		       = f8
	1 encntr_id		       = f8
	1 name_full_formatted  = vc
)

; Reference Codeset structure
free record ref_code_set
record ref_code_set (
    1 ref_code_set[*]
        2 object_name       = vc
        2 column_name       = vc
        2 description       = vc
        2 code_set          = i4
)

; Primary Patient Source Structure
free record patient_source
record patient_source (
	1 visits[*]
		2 person_id		= f8
		2 encntr_id		= f8
	1 patients[*]
		2 person_id		= f8
)
 
; Define the PARENT_VALUES structure used to store ID's we want addresses/phones for.
free record parent_values
record parent_values (
	1 data[*]
		2 parent_entity_id			= f8
		2 parent_entity_name		= vc
)

; Define Prsnl lookupt structure
free record prsnl_source
record prsnl_source (
    1 data[*]
        2 person_id                 = f8
)
 
; Test incoming config parameter and assign to CONFIG record structure
if (trim($config) != "")
    free record config
    set stat = cnvtjsontorec(concat(^{"config":^, $config, ^}^), 0, 0, 1)    
endif
 
; Test incoming blob data and assign to PAYLOAD record structure
if (validate(request->blob_in))
	if (request->blob_in > " ")
	
        ; 011: Convert JSON from Hex first if needed
	    if (validate(config->hexMode, 0) = 1)
	      set run_stats->hex_mode = 1
	      set stat = cnvtjsontorec(cnvthexraw(request->blob_in), 0, 0, 1)	      
	    else	
		  set stat = cnvtjsontorec(request->blob_in, 0, 0, 1)
		endif	
	
		if ($debug_ind = 1)
		  call writeDebugSession(null)
		endif
	endif
endif

set run_stats->id = $id
set run_stats->start_time = sysdate
set run_stats->domain = curdomain
set run_stats->node = curnode
set run_stats->prsnl_id = nPrsnl_Id

; Collect the name of the user
select into "nl:"
from    prsnl           p
plan p
    where p.person_id = run_stats->prsnl_id
detail
    run_stats->prsnl_name = p.name_full_formatted
    run_stats->position_cd = p.position_cd
    run_stats->position = uar_get_code_display(p.position_cd)
    run_stats->physician_ind = p.physician_ind
    run_stats->username = p.username
with counter


; Abort if invalid payload    
if (validate(payload) = 0)
	set run_stats->status = "ERROR: Invalid Payload"
 
	go to finalize_payload	
endif


; Populate the callback structure which contains the PERSON_ID, ENCNTR_ID and PRSNL_ID values
; passed from PowerChart to CCL. These values are passed back to the mPage for use in some of
; the data services. 
set chart_id->person_id = $person_id
set chart_id->encntr_id = $encntr_id
 
; If chart mode and encntr_id is 0, try loading the user prefs
if (validate(config->mode) = 1)
 
    if (config->mode = "CHART" and chart_id->encntr_id = 0)
        select into "nl:"
        from    dm_info         d,
                encounter       e,
                person          p
        plan d
            where d.info_domain = "CLINICAL OFFICE"
            and d.info_name = "DEVELOPER TEST VISIT"
            and d.info_domain_id = run_stats->prsnl_id
        join e
            where e.encntr_id = d.info_number
        join p
            where p.person_id = e.person_id
        detail
            nCopyChartIdToPayload = 1
            chart_id->person_id = e.person_id
            chart_id->encntr_id = e.encntr_id
 
            run_stats->status = concat("Chart Level MPage with no encounter. Using testing value from dm_info: ",
                                    trim(p.name_full_formatted), " (ENCNTR_ID: ", trim(cnvtstring(e.encntr_id)), ")")
        with counter
    endif

    ; Collect the patient name
    if (config->mode = "CHART")
        select into "nl:"
        from    person          p
        plan p
            where p.person_id = chart_id->person_id
        detail
            chart_id->name_full_formatted = p.name_full_formatted
        with counter
    endif
 
endif

; Mark if reference run	
if (validate(payload->reference) = 1)
    set run_stats->reference_ind = payload->reference
endif
 
; Populate the encounter/person structure. While we could use the values from payload without
; copying to the PATIENT_SOURCE structure, having it available in a seperate record structure
; allows for dynamically loading from other CCL scripts.
; (e.g. Current Census followed by Patient Demographics) 
IF (VALIDATE(PAYLOAD->PATIENTSOURCE) = 1)
	; Populate the PATIENT_SOURCE structure and assign any missing PERSON_ID values
	SELECT INTO "NL:"
		PERSON_ID			= IF (PAYLOAD->PATIENTSOURCE[D.SEQ].PERSONID > 0)
								PAYLOAD->PATIENTSOURCE[D.SEQ].PERSONID
							  ELSE
							  	E.PERSON_ID
							  ENDIF,
		ENCNTR_ID			= PAYLOAD->PATIENTSOURCE[D.SEQ].ENCNTRID
	FROM	(DUMMYT				D WITH SEQ=VALUE(SIZE(PAYLOAD->PATIENTSOURCE, 5))),
			ENCOUNTER			E
	PLAN D
		WHERE PAYLOAD->PATIENTSOURCE[D.SEQ].PERSONID > 0
		OR PAYLOAD->PATIENTSOURCE[D.SEQ].ENCNTRID > 0
	JOIN E
		WHERE E.ENCNTR_ID = PAYLOAD->PATIENTSOURCE[D.SEQ].ENCNTRID
	ORDER PERSON_ID, ENCNTR_ID
	HEAD REPORT
		STAT = ALTERLIST(PATIENT_SOURCE->VISITS, SIZE(PAYLOAD->PATIENTSOURCE, 5))
		nPATCOUNT = 0
		nVISCOUNT = 0
	HEAD PERSON_ID
		nPATCOUNT = nPATCOUNT + 1
		STAT = ALTERLIST(PATIENT_SOURCE->PATIENTS, nPATCOUNT)
 
		PATIENT_SOURCE->PATIENTS[nPATCOUNT].PERSON_ID = PERSON_ID
	DETAIL
		nVISCOUNT = nVISCOUNT + 1
		PATIENT_SOURCE->VISITS[nVISCOUNT].ENCNTR_ID = ENCNTR_ID
		PATIENT_SOURCE->VISITS[nVISCOUNT].PERSON_ID = PERSON_ID
	WITH COUNTER
ENDIF
 
; Check to see if PATIENT_SOURCE is empty and if it is populate with the id's passed from the mPage
if (size(patient_source->visits, 5) = 0)
	set stat = alterlist(patient_source->visits, 1)
	set stat = alterlist(patient_source->patients, 1)
 
	if (nCopyChartIdToPayload = 1)
        set patient_source->visits[1].person_id = chart_id->person_id
        set patient_source->visits[1].encntr_id = chart_id->encntr_id
 
        set patient_source->patients[1].person_id = chart_id->person_id
	else
        set patient_source->visits[1].person_id = $person_id
        set patient_source->visits[1].encntr_id = $encntr_id
 
        set patient_source->patients[1].person_id = $person_id
    endif
endif
 
; ******************************************************************************************
; Execute Pre-CCL scripts (These are scripts that need to run first as they may populate the
; PATIENT_SOURCE record structure
; ******************************************************************************************
if ($debug_ind = 0)
    ; Clear all entries from the PATIENT_SOURCE structure if parameter passed
    if (validate(payload->clearPatientSource, 0) = 1)
        set stat = initrec(patient_source)
	endif

    if(validate(payload->customScript) = 1)
	   call run_custom("PRE")
    endif

    ; ******************************************************************************************
    ; Execute the main data collection CCL scripts based on the payload
    ; ******************************************************************************************
 
    if (validate(payload->codevalue) = 1) execute 1co5_mpage_cvlookup:group1 endif
    if (validate(payload->encounter) = 1) execute 1co5_mpage_encounter:group1 endif
    if (validate(payload->person) = 1) execute 1co5_mpage_person:group1 endif
    if (validate(payload->prsnl) = 1) execute 1co5_mpage_prsnl:group1 endif
    if (validate(payload->organization) = 1 or
    	validate(payload->address) = 1 or
	   validate(payload->phone) = 1) execute 1co5_mpage_apo:group1
    endif   
    if (validate(payload->allergy) = 1) execute 1co5_mpage_allergy:group1 endif
    if (validate(payload->diagnosis) = 1) execute 1co5_mpage_diagnosis:group1 endif
    if (validate(payload->problem) = 1) execute 1co5_mpage_problem:group1 endif
 
    ; ******************************************************************************************
    ; Execute any custom Post-CCL scripts
    ; ******************************************************************************************
    call run_custom("POST")
    
    if (size(ref_code_set->ref_code_set, 5) > 0)
        call add_standard_output(cnvtrectojson(ref_code_set, 4, 1))
    endif        
       
endif
 
#FINALIZE_PAYLOAD
 
; Add any CCL Errors to the reply string
declare cErrorMessage = c132
declare cErrors = vc
declare nErrorCode = i4 with noconstant(1)
while (nErrorCode != 0)
    set nErrorCode = error(cErrorMessage, 0)
    if (nErrorCode != 0)
        if (trim(cErrors) != "")
            set cErrors = concat(trim(cErrors), ",")
        endif
        set cErrors = concat(trim(cErrors), ^{"code":^, build(nErrorCode), ^,"message":"^, trim(cErrorMessage), ^"}^)
    endif
endwhile
set cErrors = concat(^"errors":[^, trim(cErrors), ^]^)
 
; Generate the final _Memory_Reply_String
SET RUN_STATS->END_TIME = SYSDATE
SET _Memory_Reply_String = BUILD("{",
								FIXJSON(CNVTRECTOJSON(RUN_STATS, 4, 1)), ",",
								FIXJSON(CNVTRECTOJSON(CHART_ID, 4, 1)), ",",
								cErrors,
								 _Memory_Reply_String,
								 "}")
 
; 011: Convert JSON to Hex first if needed
if (validate(config->hexMode, 0) = 1) 
    set _Memory_Reply_String = cnvtrawhex(_Memory_Reply_String)
endif

; ******************
; Custom subroutines
; ******************
 
; FIXJSON - Removes the first and last character off a JSON string. This is necessary
; to combine multiple JSON strings into a single payload for return to the mPage
SUBROUTINE (FIXJSON(cJSON = vc) = vc WITH COPY)
	SET cJSON = SUBSTRING(2, SIZE(cJSON) - 2, cJSON)
	RETURN(cJSON)
END
 
; Runs the custom script pre or post block
subroutine run_custom(cType)
	if (validate(payload->customscript->script) = 1)
		set _Memory_Reply_String = concat(_Memory_Reply_String, ^,"custom^, cnvtcap(cType), ^":[^)
		for (nScript = 1 to size(payload->customscript->script, 5))		
			if (cnvtupper(payload->customscript->script[nScript].run) = cnvtupper(cType))
				call parser(concat("execute ", trim(payload->customscript->script[nScript].name), " go"))
			endif
		endfor
		set _Memory_Reply_String = concat(_Memory_Reply_String, ^]^)
	endif
end
 
; Adds standard output objects to the output JSON stream
subroutine add_standard_output(cJSON)
    set nPos = findstring(":", cJSON)
	if (substring(size(trim(_Memory_Reply_String)),1,_Memory_Reply_String) != "[")
        set _Memory_Reply_String = concat(_Memory_Reply_String, ^,^)
	endif
	set _Memory_Reply_String = concat(_Memory_Reply_String, substring(nPos+2, size(trim(cJSON))-(nPos+3), cJSON))
end
 
; Adds the output of the custom record structure to the output JSON stream
SUBROUTINE ADD_CUSTOM_OUTPUT(cJSON)
	IF (TRIM(PAYLOAD->CUSTOMSCRIPT->SCRIPT[nSCRIPT].ID) != "")
		SET nPOS = FINDSTRING(":", cJSON)
		IF (SUBSTRING(SIZE(TRIM(_Memory_Reply_String)),1,_Memory_Reply_String) != "[")
			SET _Memory_Reply_String = CONCAT(_Memory_Reply_String, ^,^)
		ENDIF
		SET _Memory_Reply_String = CONCAT(_Memory_Reply_String,
											^{"id":"^, PAYLOAD->CUSTOMSCRIPT->SCRIPT[nSCRIPT].ID, ^",^,
											^"data"^, SUBSTRING(nPOS, SIZE(cJSON)-nPOS, cJSON), ^}^)
	ENDIF
END
 
; Returns a parser string listing code values
; ---------------------------------------------------
SUBROUTINE TYPE_PARSER(cPREFIX, nCODE_SET)
	SET cPARSER = "1=1"		; Default all
 
	IF (VALIDATE(PAYLOAD->TYPELIST) = 1)
 
		SELECT DISTINCT INTO "NL:"
		    code_value = cv.code_value
		    ;CODE_VALUE			= CNVTINT(CV.CODE_VALUE)
		FROM 	(DUMMYT			D WITH SEQ=VALUE(SIZE(PAYLOAD->TYPELIST,5))),
				CODE_VALUE		CV
		PLAN D
		JOIN CV
			WHERE (CV.CODE_VALUE = PAYLOAD->TYPELIST[D.SEQ].TYPECD AND nCODE_SET = PAYLOAD->TYPELIST[D.SEQ].CODESET)
			OR
				(CV.CODE_SET = PAYLOAD->TYPELIST[D.SEQ].CODESET
				AND (
						(CV.CDF_MEANING = PAYLOAD->TYPELIST[D.SEQ].TYPE AND TRIM(CV.CDF_MEANING) != "")
						OR CV.DISPLAY_KEY = PAYLOAD->TYPELIST[D.SEQ].TYPE
					)
				AND CV.CODE_SET = nCODE_SET
				AND CV.CODE_VALUE > 0
				AND CV.ACTIVE_IND = 1
				AND CV.END_EFFECTIVE_DT_TM > SYSDATE
			)
		ORDER CODE_VALUE
		HEAD REPORT
			cPARSER = " "
		DETAIL
			cPARSER = BUILD(cPARSER, ",", CODE_VALUE)
		FOOT REPORT
			cPARSER = CONCAT(cPREFIX, " IN (", TRIM(SUBSTRING(2,SIZE(cPARSER),cPARSER)), ")")
		WITH COUNTER
	ENDIF
END

; Add a codeset reference for use by the referenceService
; -------------------------------------------------------
subroutine add_ref_code_set(cObjectName, cColumnName, cDescription, nCodeSet)
    set nNum = size(ref_code_set->ref_code_set, 5) + 1
    set stat = alterlist(ref_code_set->ref_code_set, nNum)
    set ref_code_set->ref_code_set[nNum].object_name = cObjectName
    set ref_code_set->ref_code_set[nNum].column_name = cColumnName
    set ref_code_set->ref_code_set[nNum].description = cDescription
    set ref_code_set->ref_code_set[nNum].code_set = nCodeSet
end

; Converts text field names to javascript camel case e.g. PERSON_ID => personId
subroutine camel_field(cText)
    declare nPiece = i4 with noconstant(1)
    declare cCamelString = vc with noconstant("")
    declare cPiece = vc
    
    while(cPiece != "<COMPLETE>")
        set cPiece = piece(cnvtlower(cText), "_", nPiece, "<COMPLETE>")
        if (cPiece != "<COMPLETE>")
            set cCamelString = evaluate(nPiece, 1, cPiece, concat(trim(cCamelString), trim(cnvtcap(cPiece))))
        endif            
        set nPiece = nPiece + 1
    endwhile
    
    return(cCamelString)
end

; Write out the parameter blob for running later on the back-end
subroutine writeDebugSession(null)
    declare cBlobIn = vc

    if (validate(config->hexMode, 0) = 1)	
        set cBlobIn = cnvthexraw(request->blob_in)
    else	
        set cBlobIn = request->blob_in
    endif	

    free record fRec
    record fRec (
        1 file_desc     = w8
        1 file_offset   = i4
        1 file_dir      = i4
        1 file_name     = vc
        1 file_buf      = vc
    )
    
    set run_stats->debug_file = concat("1co_debug_", trim(cnvtstring(reqinfo->updt_id)),".json") 
    set fRec->file_name = run_stats->debug_file
    set fRec->file_buf = "w"
    set stat = cclio("OPEN", fRec)
    set fRec->file_buf = cBlobIn
    set stat = cclio("PUTS", fRec)
    set stat = cclio("CLOSE", fRec)
end

; Returns the combination of two strings with delimeter between or value of populated field if only one field populated.
subroutine combine_strings(cString1, cString2, cDelimiter)
    if (trim(cString1) = "")
        return (cString2)
    elseif (trim(cString2) = "")
        return (cString1)
    else
        return(concat(trim(cString1), cDelimiter, trim(cString2)))
    endif
end

; Add a person_id if it does not exist to the patient source
subroutine add_person_to_patient_source(nPersonId)

    if (locateval(nNum, 1, size(patient_source->patients, 5), nPersonId, patient_source->patients[nNum].person_id) = 0)
    
        set stat = alterlist(patient_source->patients, size(patient_source->patients, 5)+1)
        set patient_source->patients[size(patient_source->patients, 5)].person_id = nPersonId
    
    endif

end

; Add a person_id to the PRSNL search structure
subroutine add_prsnl(nPersonId)

    if (locateval(nNum, 1, size(prsnl_source->data, 5), nPersonId, prsnl_source->data[nNum].person_id) = 0 and nPersonId > 0)
        
        set stat = alterlist(prsnl_source->data, size(prsnl_source->data, 5)+1)
        set prsnl_source->data[size(prsnl_source->data, 5)].person_id = nPersonId
        
    endif

end

; Add an organization to be loaded
subroutine add_organization(nOrganizationId)
    
    if (locateval(nNum, 1, size(parent_values->data, 5), nOrganizationId, parent_values->data[nNum].parent_entity_id,
                                                         "ORGANIZATION", parent_values->data[nNum].parent_entity_name) = 0)
        set stat = alterlist(parent_values->data, size(parent_values->data, 5)+1)
        set parent_values->data[size(parent_values->data, 5)].parent_entity_id = nOrganizationId
        set parent_values->data[size(parent_values->data, 5)].parent_entity_name = "ORGANIZATION"
    endif
    
end
 
end go
/*************************************************************************
 
        Script Name:    1co5_mpage_person.prg
 
        Description:    Clinical Office - MPage Edition V5
        				Person Data Retrieval
 
        Date Written:   February 6, 2025
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
 Called from 1co5_mpage_entry. Do not attempt to run stand alone.
 
 Possible Payload values:
 
	"patientSource":[
		{"personId": value, "encntrId": value}
	],
	"person": {
		"aliases": true,
		"names": true,
		"personInfo": true,
		"prsnlReltn": true,
		"personReltn": true,
		"personPlanReltn": true,
		"personCodeReltn": true,
		"orgReltn": true,
		"loadExtendedPersons": true,
		"skipJSON": true
	},
	"typeList": [
		{"codeSet": value, "type": "value", "typeCd": value}
	]
 
 
 
 *************************************************************************
                            Revision Information
 *************************************************************************
 Rev    Date     By             Comments
 ------ -------- -------------- ------------------------------------------
 001    06/02/25 J. Simpson     Initial Development
 002    09/13/25 J. Simpson     Added loadExtendedPersons to trigger loading related person records
 *************************************************************************/
 
drop program 1co5_mpage_person:group1 go
create program 1co5_mpage_person:group1
 
; Check to see if running from mPage entry script
if (validate(payload->person) = 0 or size(patient_source->patients, 5) = 0)
	go to end_program
endif
 
; Required variables
declare cParser = vc
declare nNum = i4
declare cString = vc
 
free record rPerson
record rPerson (
    1 persons[*]
        2 person_id                     = f8
        2 logical_domain_id             = f8
        2 name_full_formatted           = vc
        2 name_last                     = vc
        2 name_first                    = vc
		2 name_middle                   = vc
		2 birth_dt_tm                   = dq8
		2 age                           = vc
        2 deceased_dt_tm                = dq8
        2 last_encntr_dt_tm             = dq8
        2 autopsy                       = vc
        2 deceased                      = vc
        2 ethnic_grp                    = vc
        2 language                      = vc
        2 marital_type                  = vc
        2 race                          = vc
        2 religion                      = vc
        2 sex                           = vc
        2 species                       = vc
        2 confid_level                  = vc
        2 vip                           = vc
        2 interp_required               = vc
        2 living_will                   = vc
        2 gest_age_at_birth             = i4
        2 gest_age_method               = vc
        2 health_info_access_offered    = vc
        2 autopsy_cd                    = f8
        2 deceased_cd                   = f8
        2 ethnic_grp_cd                 = f8
        2 language_cd                   = f8
        2 marital_type_cd               = f8
        2 race_cd                       = f8
        2 religion_cd                   = f8
        2 sex_cd                        = f8
        2 species_cd                    = f8
        2 confid_level_cd               = f8
        2 vip_cd                        = f8
        2 interp_required_cd            = f8
        2 living_will_cd                = f8
        2 gest_age_method_cd            = f8
        2 health_info_access_offered_cd = f8
        2 aliases[*]
            3 alias_pool                = vc
            3 alias_type                = vc
            3 alias_type_meaning        = vc
            3 alias                     = vc
            3 alias_formatted           = vc
            3 alias_sub_type            = vc
            3 visit_seq_nbr             = i4
            3 health_card_province      = vc
            3 health_card_ver_code      = vc
            3 health_card_issue_dt_tm   = dq8
            3 health_card_expiry_dt_tm  = dq8
            3 health_card_type          = vc
            3 alias_pool_cd             = f8
            3 person_alias_type_cd      = f8
            3 person_alias_sub_type_cd  = f8
        2 names[*]
            3 name_type                 = vc
            3 name_type_meaning         = vc
            3 beg_effective_dt_tm       = dq8
            3 end_effective_dt_tm       = dq8
            3 name_full_formatted       = vc
            3 name_first                = vc
            3 name_middle               = vc
            3 name_last                 = vc
            3 name_degree               = vc
            3 name_title                = vc
            3 name_prefix               = vc
            3 name_suffix               = vc
            3 name_initials             = vc
            3 name_type_seq             = i4
            3 name_type_cd              = f8
        2 prsnl_reltn[*]
            3 reltn_type                = vc
            3 reltn_type_meaning        = vc
            3 person_id                 = f8
            3 priority_seq              = i4
            3 prsnl_type                = vc
            3 name_full_formatted       = vc
            3 physician_ind             = i4
            3 position                  = vc
            3 name_last                 = vc
            3 name_first                = vc
            3 user_name                 = vc
            3 person_prsnl_r_cd         = f8
            3 prsnl_type_cd             = f8
            3 position_cd               = f8
        2 person_reltn[*]
            3 person_reltn_type         = vc
            3 person_reltn_type_meaning = vc
            3 person_reltn              = vc
            3 related_person_reltn      = vc
            3 person_id                 = f8
            3 priority_seq              = i4
            3 internal_seq              = i4
            3 name_full_formatted       = vc
            3 name_last                 = vc
            3 name_first                = vc
            3 name_middle               = vc
            3 person_reltn_type_cd      = f8
            3 person_reltn_cd           = f8
            3 related_person_reltn_cd   = f8
        2 person_plan_reltn[*]
            3 health_plan_id            = f8
            3 person_plan_r             = vc
            3 person_plan_r_cd          = f8
            3 organization_id           = f8
            3 priority_seq              = i4
            3 member_nbr                = vc
            3 signature_on_file         = vc
            3 signature_on_file_cd      = f8
            3 balance_type              = vc
            3 balance_type_cd           = f8
            3 deduct_amt                = f8
            3 deduct_met_amt            = f8
            3 deduct_met_dt_tm          = dq8
            3 coverage_type             = vc
            3 coverage_type_cd          = f8
            3 max_out_pckt_amt          = f8
            3 max_out_pckt_dt_tm        = dq8
            3 fam_deduct_met_amt        = f8
            3 fam_deduct_met_dt_tm      = dq8
            3 verify_status             = vc
            3 verify_status_cd          = f8
            3 verify_dt_tm              = dq8
            3 verify_prsnl_id           = f8
            3 insured_card_name         = vc
            3 group_name                = vc
            3 group_nbr                 = vc
            3 policy_nbr                = vc
            3 subscriber_person_name    = vc
            3 subscriber_person_id      = f8
            3 member_person_code        = vc
            3 life_rsv_days             = i4
            3 life_rsv_remain_days      = i4
            3 life_rsv_daily_ded_amt    = f8
            3 life_rsv_daily_ded_qual   = vc
            3 life_rsv_daily_ded_qual_cd= f8
            3 card_issue_nbr            = i4
            3 card_category             = vc
            3 card_category_cd          = f8
            3 program_status            = vc
            3 program_status_cd         = f8
            3 denial_reason             = vc
            3 denial_reason_cd          = f8
            3 coverage_comments         = vc
            3 contract_code             = vc
            3 verify_source             = vc
            3 verify_source_cd          = f8
            3 ext_payer_name            = vc
            3 ext_payer_ident           = vc
            3 alt_member_nbr            = vc
            3 generic_health_plan_name  = vc
            3 plan_type                 = vc
            3 plan_type_cd              = f8
            3 plan_class                = vc
            3 plan_class_cd             = f8
            3 plan_name                 = vc
            3 plan_desc                 = vc
            3 financial_class           = vc
            3 financial_class_cd        = f8
            3 baby_coverage             = vc
            3 baby_coverage_cd          = f8
            3 comb_baby_bill            = vc
            3 comb_baby_bill_cd         = f8
            3 service_type              = vc
            3 service_type_cd           = f8
            3 plan_category             = vc
            3 plan_category_cd          = f8
            3 priority_ranking_nbr      = i4
        2 person_org_reltn[*]
            3 person_org_reltn_id       = f8
            3 person_org_reltn          = vc
            3 person_org_reltn_meaning  = vc
            3 organization_id           = f8
            3 empl_type                 = vc
            3 empl_status               = vc
            3 org_name                  = vc
            3 priority_seq              = i4
            3 person_org_reltn_cd       = f8
            3 empl_type_cd              = f8
            3 empl_status_cd            = f8
        2 person_info[*]
            3 info_type                 = vc
            3 info_type_meaning         = vc
            3 info_sub_type             = vc
            3 info_sub_type_meaning     = vc
            3 value_numeric_ind         = i4
            3 value_numeric             = f8
            3 value_dt_tm               = dq8
            3 chartable_ind             = i4
            3 priority_seq              = i4
            3 internal_seq              = i4
            3 value                     = vc
            3 long_text                 = vc
            3 info_type_cd              = f8
            3 info_sub_type_cd          = f8
            3 value_cd                  = f8
        2 person_code_reltn[*]
            3 person_code_value_r_id    = f8
            3 code_set                  = f8
            3 code_value                = f8
            3 display                   = vc
) with persistscript

; Alternate reference structure - Populate all structures with empty values
if (run_stats->reference_ind = 1)
    set stat = alterlist(rPerson->persons, 1)
    set stat = alterlist(rPerson->persons[1]->aliases, 1)
    set stat = alterlist(rPerson->persons[1]->names, 1)
    set stat = alterlist(rPerson->persons[1]->prsnl_reltn, 1)
    set stat = alterlist(rPerson->persons[1]->person_reltn, 1)
    set stat = alterlist(rPerson->persons[1]->person_plan_reltn, 1)
    set stat = alterlist(rPerson->persons[1]->person_org_reltn, 1)
    set stat = alterlist(rPerson->persons[1]->person_info, 1)
    set stat = alterlist(rPerson->persons[1]->person_code_reltn, 1)
        
    ; Populate the code sets
    select into "nl:"
    from    dm_columns_doc      dcd
    plan dcd
        where dcd.table_name in ("PERSON", "PERSON_PATIENT", "PERSON_ALIAS", "PERSON_NAME", "PERSON_PRSNL_RELTN",
                                "PRSNL", "PERSON_PERSON_RELTN", "PERSON_ORG_RELTN", "ORGANIZATION", "PERSON_INFO",
                                "PERSON_PLAN_RELTN","HEALTH_PLAN")
        and dcd.code_set > 0
    detail
        call add_ref_code_set("persons", camel_field(dcd.column_name), dcd.description, dcd.code_set)        
    with counter        
        
    go to skip_logic
endif 

call loadPersons(null)

; Setup multiple loops if necessary when loading extended person data
declare nPersonRun = i4 with noconstant(1)
if (validate(payload->person->loadExtendedPersons, 0) = 1)
   set nPersonRun = 2
endif

#person_run

; Collect the person relationships at the person level
; ----------------------------------------------------
if (validate(payload->person->personreltn, 0) = 1)
    ; set the Parser
	call type_parser("ppr.person_reltn_type_cd", 351)
	 
    select into "nl:"
        person_id               = ppr.person_id,
        person_reltn_type_cd	= ppr.person_reltn_type_cd,
        priority_seq			= ppr.priority_seq,
        internal_seq            = ppr.internal_seq,
        sort_date				= format(ppr.beg_effective_dt_tm,"yyyymmddhhmmss;;q"),
        person_person_reltn_id  = ppr.person_person_reltn_id
    from    person_person_reltn     ppr,
            person                  p
    plan ppr
        where expand(nNum, 1, size(rPerson->persons, 5), ppr.person_id, rPerson->persons[nNum].person_id)
        and parser(cParser)
        and ppr.active_ind = 1
        and ppr.end_effective_dt_tm > sysdate
    join p
        where p.person_id = ppr.related_person_id
    order person_id, person_reltn_type_cd, priority_seq, internal_seq, sort_date desc
    head person_id
        nPos = locateval(nNum, 1, size(rPerson->persons, 5), ppr.person_id, rPerson->persons[nNum].person_id)
        nCount = 0
    head person_reltn_type_cd
        x = 0
    head priority_seq
        x = 0
    head internal_seq
        if (nPersonRun = 1)
            nCount = nCount + 1
            stat = alterlist(rPerson->persons[nPos].person_reltn, nCount)
 
            rPerson->persons[nPos].person_reltn[nCount].person_reltn_type = uar_get_code_display(ppr.person_reltn_type_cd)
            rPerson->persons[nPos].person_reltn[nCount].person_reltn_type_meaning = uar_get_code_meaning(ppr.person_reltn_type_cd)
            rPerson->persons[nPos].person_reltn[nCount].person_reltn = uar_get_code_display(ppr.person_reltn_cd)
            rPerson->persons[nPos].person_reltn[nCount].related_person_reltn = uar_get_code_display(ppr.related_person_reltn_cd)
            rPerson->persons[nPos].person_reltn[nCount].person_id = ppr.related_person_id
            rPerson->persons[nPos].person_reltn[nCount].priority_seq = ppr.priority_seq
            rPerson->persons[nPos].person_reltn[nCount].internal_seq = ppr.internal_seq
            rPerson->persons[nPos].person_reltn[nCount].name_full_formatted = p.name_full_formatted
            rPerson->persons[nPos].person_reltn[nCount].name_last = p.name_last
            rPerson->persons[nPos].person_reltn[nCount].name_first = p.name_first
            rPerson->persons[nPos].person_reltn[nCount].name_middle = p.name_middle
            rPerson->persons[nPos].person_reltn[nCount].person_reltn_type_cd = ppr.person_reltn_type_cd
            rPerson->persons[nPos].person_reltn[nCount].person_reltn_cd = ppr.person_reltn_cd
            rPerson->persons[nPos].person_reltn[nCount].related_person_reltn_cd = ppr.related_person_reltn_cd
        else    
            call add_person_to_patient_source(ppr.related_person_id)
;            call loadPersons(null)
        endif
    with expand=2        
endif

; Collect the person plan relationships with health plan fields
; -------------------------------------------------------------
if (validate(payload->person->personPlanReltn, 0) = 1)

    select into "nl:"
        person_id               = ppr.person_id,
        priority_seq            = ppr.priority_seq
    from    person_plan_reltn        ppr,
            health_plan              hp,
            person                   p,
            long_text                lt
    plan ppr
        where expand(nNum, 1, size(rPerson->persons, 5), ppr.person_id, rPerson->persons[nNum].person_id)
        and ppr.active_ind = 1
        and ppr.active_status_cd = cv48_Active
        and ppr.end_effective_dt_tm > sysdate
    join hp
        where hp.health_plan_id = ppr.health_plan_id
    join p
        where p.person_id = ppr.subscriber_person_id
    join lt
        where lt.long_text_id = outerjoin(ppr.coverage_comments_long_text_id)
    order person_id, priority_seq
    detail
        if (nPersonRun = 1)

            nPos = locateval(nNum, 1, size(rPerson->persons, 5), ppr.person_id, rPerson->persons[nNum].person_id)
    
            nPlanCount = size(rPerson->persons[nNum].person_plan_reltn, 5)+1
            stat = alterlist(rPerson->persons[nNum].person_plan_reltn, nPlanCount)
        
            rPerson->persons[nNum].person_plan_reltn[nPlanCount].health_plan_id = ppr.health_plan_id
            rPerson->persons[nNum].person_plan_reltn[nPlanCount].person_plan_r = uar_get_code_display(ppr.person_plan_r_cd)
            rPerson->persons[nNum].person_plan_reltn[nPlanCount].person_plan_r_cd = ppr.person_plan_r_cd
            rPerson->persons[nNum].person_plan_reltn[nPlanCount].organization_id = ppr.organization_id
            rPerson->persons[nNum].person_plan_reltn[nPlanCount].priority_seq = ppr.priority_seq
            rPerson->persons[nNum].person_plan_reltn[nPlanCount].member_nbr = ppr.member_nbr
            rPerson->persons[nNum].person_plan_reltn[nPlanCount].signature_on_file = uar_get_code_display(ppr.signature_on_file_cd)
            rPerson->persons[nNum].person_plan_reltn[nPlanCount].signature_on_file_cd = ppr.signature_on_file_cd
            rPerson->persons[nNum].person_plan_reltn[nPlanCount].balance_type = uar_get_code_display(ppr.balance_type_cd)
            rPerson->persons[nNum].person_plan_reltn[nPlanCount].balance_type_cd = ppr.balance_type_cd
            rPerson->persons[nNum].person_plan_reltn[nPlanCount].deduct_amt = ppr.deduct_amt
            rPerson->persons[nNum].person_plan_reltn[nPlanCount].deduct_met_amt = ppr.deduct_met_amt
            rPerson->persons[nNum].person_plan_reltn[nPlanCount].deduct_met_dt_tm = ppr.deduct_met_dt_tm
            rPerson->persons[nNum].person_plan_reltn[nPlanCount].coverage_type = uar_get_code_display(ppr.coverage_type_cd)
            rPerson->persons[nNum].person_plan_reltn[nPlanCount].coverage_type_cd = ppr.coverage_type_cd
            rPerson->persons[nNum].person_plan_reltn[nPlanCount].max_out_pckt_amt = ppr.max_out_pckt_amt
            rPerson->persons[nNum].person_plan_reltn[nPlanCount].max_out_pckt_dt_tm = ppr.max_out_pckt_dt_tm
            rPerson->persons[nNum].person_plan_reltn[nPlanCount].fam_deduct_met_amt = ppr.fam_deduct_met_amt
            rPerson->persons[nNum].person_plan_reltn[nPlanCount].fam_deduct_met_dt_tm = ppr.fam_deduct_met_dt_tm
            rPerson->persons[nNum].person_plan_reltn[nPlanCount].verify_status = uar_get_code_display(ppr.verify_status_cd)
            rPerson->persons[nNum].person_plan_reltn[nPlanCount].verify_status_cd = ppr.verify_status_cd
            rPerson->persons[nNum].person_plan_reltn[nPlanCount].verify_dt_tm = ppr.verify_dt_tm
            rPerson->persons[nNum].person_plan_reltn[nPlanCount].verify_prsnl_id = ppr.verify_prsnl_id
            rPerson->persons[nNum].person_plan_reltn[nPlanCount].insured_card_name = ppr.insured_card_name
            rPerson->persons[nNum].person_plan_reltn[nPlanCount].group_name = hp.group_name
            rPerson->persons[nNum].person_plan_reltn[nPlanCount].group_nbr = hp.group_nbr
            rPerson->persons[nNum].person_plan_reltn[nPlanCount].policy_nbr = hp.policy_nbr
            rPerson->persons[nNum].person_plan_reltn[nPlanCount].subscriber_person_id = ppr.subscriber_person_id
            rPerson->persons[nNum].person_plan_reltn[nPlanCount].subscriber_person_name = p.name_full_formatted
            rPerson->persons[nNum].person_plan_reltn[nPlanCount].member_person_code = ppr.member_person_code
            rPerson->persons[nNum].person_plan_reltn[nPlanCount].life_rsv_days = ppr.life_rsv_days
            rPerson->persons[nNum].person_plan_reltn[nPlanCount].life_rsv_remain_days = ppr.life_rsv_remain_days
            rPerson->persons[nNum].person_plan_reltn[nPlanCount].life_rsv_daily_ded_amt = ppr.life_rsv_daily_ded_amt
            rPerson->persons[nNum].person_plan_reltn[nPlanCount].life_rsv_daily_ded_qual = 
                                                                            uar_get_code_display(ppr.life_rsv_daily_ded_qual_cd)
            rPerson->persons[nNum].person_plan_reltn[nPlanCount].life_rsv_daily_ded_qual_cd = ppr.life_rsv_daily_ded_qual_cd
            rPerson->persons[nNum].person_plan_reltn[nPlanCount].card_issue_nbr = ppr.card_issue_nbr
            rPerson->persons[nNum].person_plan_reltn[nPlanCount].card_category = uar_get_code_display(ppr.card_category_cd)
            rPerson->persons[nNum].person_plan_reltn[nPlanCount].card_category_cd = ppr.card_category_cd
            rPerson->persons[nNum].person_plan_reltn[nPlanCount].program_status = uar_get_code_display(ppr.program_status_cd)
            rPerson->persons[nNum].person_plan_reltn[nPlanCount].program_status_cd = ppr.program_status_cd
            rPerson->persons[nNum].person_plan_reltn[nPlanCount].denial_reason = uar_get_code_display(ppr.denial_reason_cd)
            rPerson->persons[nNum].person_plan_reltn[nPlanCount].denial_reason_cd = ppr.denial_reason_cd
            rPerson->persons[nNum].person_plan_reltn[nPlanCount].coverage_comments = lt.long_text
            rPerson->persons[nNum].person_plan_reltn[nPlanCount].contract_code = ppr.contract_code
            rPerson->persons[nNum].person_plan_reltn[nPlanCount].verify_source = uar_get_code_display(ppr.verify_source_cd)
            rPerson->persons[nNum].person_plan_reltn[nPlanCount].verify_source_cd = ppr.verify_source_cd
            rPerson->persons[nNum].person_plan_reltn[nPlanCount].ext_payer_name = ppr.ext_payer_name
            rPerson->persons[nNum].person_plan_reltn[nPlanCount].ext_payer_ident = ppr.ext_payer_ident
            rPerson->persons[nNum].person_plan_reltn[nPlanCount].alt_member_nbr = ppr.alt_member_nbr
            rPerson->persons[nNum].person_plan_reltn[nPlanCount].generic_health_plan_name = ppr.generic_health_plan_name
            rPerson->persons[nNum].person_plan_reltn[nPlanCount].plan_type = uar_get_code_display(hp.plan_type_cd)
            rPerson->persons[nNum].person_plan_reltn[nPlanCount].plan_type_cd = hp.plan_type_cd
            rPerson->persons[nNum].person_plan_reltn[nPlanCount].plan_class = uar_get_code_display(hp.plan_class_cd)
            rPerson->persons[nNum].person_plan_reltn[nPlanCount].plan_class_cd = hp.plan_class_cd
            rPerson->persons[nNum].person_plan_reltn[nPlanCount].plan_name = hp.plan_name
            rPerson->persons[nNum].person_plan_reltn[nPlanCount].plan_desc = hp.plan_desc
            rPerson->persons[nNum].person_plan_reltn[nPlanCount].financial_class = uar_get_code_display(hp.financial_class_cd)
            rPerson->persons[nNum].person_plan_reltn[nPlanCount].financial_class_cd = hp.financial_class_cd
            rPerson->persons[nNum].person_plan_reltn[nPlanCount].baby_coverage = uar_get_code_display(hp.baby_coverage_cd)
            rPerson->persons[nNum].person_plan_reltn[nPlanCount].baby_coverage_cd = hp.baby_coverage_cd
            rPerson->persons[nNum].person_plan_reltn[nPlanCount].comb_baby_bill = uar_get_code_display(hp.comb_baby_bill_cd)
            rPerson->persons[nNum].person_plan_reltn[nPlanCount].comb_baby_bill_cd = hp.comb_baby_bill_cd
            rPerson->persons[nNum].person_plan_reltn[nPlanCount].service_type = uar_get_code_display(hp.service_type_cd)
            rPerson->persons[nNum].person_plan_reltn[nPlanCount].service_type_cd = hp.service_type_cd
            rPerson->persons[nNum].person_plan_reltn[nPlanCount].plan_category = uar_get_code_display(hp.plan_category_cd)
            rPerson->persons[nNum].person_plan_reltn[nPlanCount].plan_category_cd = hp.plan_category_cd
            rPerson->persons[nNum].person_plan_reltn[nPlanCount].priority_ranking_nbr = hp.priority_ranking_nbr
        else
            call add_organization(ppr.organization_id)
            call add_prsnl(ppr.verify_prsnl_id)
            call add_person_to_patient_source(ppr.subscriber_person_id)
;            call loadPersons(null)
        endif
    
    
    with expand=2, uar_code(d)

endif

; If running an extended person search, we need to re-run the last statement with the new id's added
if (nPersonRun = 2)
    call loadPersons(null)
    set nPersonRun = 1
    go to person_run
endif


; Person and Patient Information
select into "nl:"
from    person              p,
        person_patient      pp
plan p
    where expand(nNum, 1, size(rPerson->persons, 5), p.person_id, rPerson->persons[nNum].person_id)
    and p.active_ind = 1
    and p.end_effective_dt_tm > sysdate
    and p.active_status_cd = cv48_Active
join pp
    where pp.person_id = outerjoin(p.person_id)
    and pp.active_ind = outerjoin(1)
    and pp.end_effective_dt_tm > outerjoin(sysdate)
    and pp.active_status_cd = outerjoin(cv48_Active)
detail
    nPos = locateval(nNum, 1, size(rPerson->persons, 5), p.person_id, rPerson->persons[nNum].person_id)
 
    rPerson->persons[nPos].logical_domain_id = p.logical_domain_id
    rPerson->persons[nPos].name_full_formatted = p.name_full_formatted
    rPerson->persons[nPos].name_last = p.name_last
    rPerson->persons[nPos].name_first = p.name_first
    rPerson->persons[nPos].name_middle = p.name_middle
    rPerson->persons[nPos].birth_dt_tm = p.birth_dt_tm
    rPerson->persons[nPos].age = trim(cnvtage(p.birth_dt_tm),3)
    rPerson->persons[nPos].deceased_dt_tm = p.deceased_dt_tm
    rPerson->persons[nPos].last_encntr_dt_tm = p.last_encntr_dt_tm
    rPerson->persons[nPos].autopsy = uar_get_code_display(p.autopsy_cd)
    rPerson->persons[nPos].deceased = uar_get_code_display(p.deceased_cd)
    rPerson->persons[nPos].ethnic_grp = uar_get_code_display(p.ethnic_grp_cd)
    rPerson->persons[nPos].language = uar_get_code_display(p.language_cd)
    rPerson->persons[nPos].marital_type = uar_get_code_display(p.marital_type_cd)
    rPerson->persons[nPos].race = uar_get_code_display(p.race_cd)
    rPerson->persons[nPos].religion = uar_get_code_display(p.religion_cd)
    rPerson->persons[nPos].sex = uar_get_code_display(p.sex_cd)
    rPerson->persons[nPos].species = uar_get_code_display(p.species_cd)
    rPerson->persons[nPos].confid_level = uar_get_code_display(p.confid_level_cd)
    rPerson->persons[nPos].vip = uar_get_code_display(p.vip_cd)
    rPerson->persons[nPos].autopsy_cd = p.autopsy_cd
    rPerson->persons[nPos].deceased_cd = p.deceased_cd
    rPerson->persons[nPos].ethnic_grp_cd = p.ethnic_grp_cd
    rPerson->persons[nPos].language_cd = p.language_cd
    rPerson->persons[nPos].marital_type_cd = p.marital_type_cd
    rPerson->persons[nPos].race_cd = p.race_cd
    rPerson->persons[nPos].religion_cd = p.religion_cd
    rPerson->persons[nPos].sex_cd = p.sex_cd
    rPerson->persons[nPos].species_cd = p.species_cd
    rPerson->persons[nPos].confid_level_cd = p.confid_level_cd
    rPerson->persons[nPos].vip_cd = p.vip_cd
 
    if (validate(payload->person->patient, 0) = 1)
        rPerson->persons[nPos].interp_required = uar_get_code_display(pp.interp_required_cd)
        rPerson->persons[nPos].living_will = uar_get_code_display(pp.living_will_cd)
        rPerson->persons[nPos].gest_age_at_birth = pp.gest_age_at_birth
        rPerson->persons[nPos].gest_age_method = uar_get_code_display(pp.gest_age_method_cd)
        rPerson->persons[nPos].health_info_access_offered = uar_get_code_display(pp.health_info_access_offered_cd)
        rPerson->persons[nPos].interp_required_cd = pp.interp_required_cd
        rPerson->persons[nPos].living_will_cd = pp.living_will_cd
        rPerson->persons[nPos].gest_age_method_cd = pp.gest_age_method_cd
        rPerson->persons[nPos].health_info_access_offered_cd = pp.health_info_access_offered_cd
    endif
with counter, expand=2
 
; Collect the Person Level Aliases
; --------------------------------
if (validate(payload->person->aliases, 0) = 1)
 
    ; set the Parser
	call type_parser("pa.person_alias_type_cd", 4)
 
	; Collect the alias values
	select into "nl:"
        person_id           = pa.person_id
	from  person_alias            pa
	plan pa
        where expand(nNum, 1, size(rPerson->persons, 5), pa.person_id, rPerson->persons[nNum].person_id)
        and parser(cParser)
		and pa.active_ind = 1
		and pa.end_effective_dt_tm > sysdate
    order person_id
	head person_id
        nPos = locateval(nNum, 1, size(rPerson->persons, 5), pa.person_id, rPerson->persons[nNum].person_id)
        nCount = 0
    detail
        nCount = nCount + 1
        stat = alterlist(rPerson->persons[nPos].aliases, nCount)
 
        rPerson->persons[nPos].aliases[nCount].alias_pool = uar_get_code_display(pa.alias_pool_cd)
        rPerson->persons[nPos].aliases[nCount].alias_type = uar_get_code_display(pa.person_alias_type_cd)
        rPerson->persons[nPos].aliases[nCount].alias_type_meaning = uar_get_code_meaning(pa.person_alias_type_cd)
        rPerson->persons[nPos].aliases[nCount].alias = pa.alias
        rPerson->persons[nPos].aliases[nCount].alias_formatted = cnvtalias(pa.alias, pa.alias_pool_cd)
        rPerson->persons[nPos].aliases[nCount].alias_sub_type = uar_get_code_display(pa.person_alias_sub_type_cd)
        rPerson->persons[nPos].aliases[nCount].visit_seq_nbr = pa.visit_seq_nbr
        rPerson->persons[nPos].aliases[nCount].health_card_province = pa.health_card_province
        rPerson->persons[nPos].aliases[nCount].health_card_ver_code = pa.health_card_ver_code
        rPerson->persons[nPos].aliases[nCount].health_card_issue_dt_tm = pa.health_card_issue_dt_tm
        rPerson->persons[nPos].aliases[nCount].health_card_expiry_dt_tm = pa.health_card_expiry_dt_tm
        rPerson->persons[nPos].aliases[nCount].health_card_type = pa.health_card_type
        rPerson->persons[nPos].aliases[nCount].alias_pool_cd = pa.alias_pool_cd
        rPerson->persons[nPos].aliases[nCount].person_alias_type_cd = pa.person_alias_type_cd
        rPerson->persons[nPos].aliases[nCount].person_alias_sub_type_cd = pa.person_alias_sub_type_cd
 
    with expand=2
endif
 
; Collect the person name information
; -----------------------------------
if (validate(payload->person->names, 0) = 1)
 
    ; set the Parser
    call type_parser("pn.name_type_cd", 213)
 
    select into "nl:"
        person_id           = pn.person_id
    from  person_name             pn
    plan pn
        where expand(nNum, 1, size(rPerson->persons, 5), pn.person_id, rPerson->persons[nNum].person_id)
		and parser(cParser)
		and pn.active_ind = 1
		and pn.end_effective_dt_tm > sysdate
    order person_id
    head person_id
        nPos = locateval(nNum, 1, size(rPerson->persons, 5), pn.person_id, rPerson->persons[nNum].person_id)
        nCount = 0
    detail
        nCount = nCount + 1
        stat = alterlist(rPerson->persons[nPos].names, nCount)
 
        rPerson->persons[nPos].names[nCount].name_type = uar_get_code_display(pn.name_type_cd)
        rPerson->persons[nPos].names[nCount].name_type_meaning = uar_get_code_meaning(pn.name_type_cd)
        rPerson->persons[nPos].names[nCount].beg_effective_dt_tm = pn.beg_effective_dt_tm
        rPerson->persons[nPos].names[nCount].end_effective_dt_tm = pn.end_effective_dt_tm
        rPerson->persons[nPos].names[nCount].name_full_formatted = pn.name_full
        rPerson->persons[nPos].names[nCount].name_first = pn.name_first
        rPerson->persons[nPos].names[nCount].name_middle = pn.name_middle
        rPerson->persons[nPos].names[nCount].name_last = pn.name_last
        rPerson->persons[nPos].names[nCount].name_degree = pn.name_degree
        rPerson->persons[nPos].names[nCount].name_title = pn.name_title
        rPerson->persons[nPos].names[nCount].name_prefix = pn.name_prefix
        rPerson->persons[nPos].names[nCount].name_suffix = pn.name_suffix
        rPerson->persons[nPos].names[nCount].name_initials = pn.name_initials
        rPerson->persons[nPos].names[nCount].name_type_seq = pn.name_type_seq
        rPerson->persons[nPos].names[nCount].name_type_cd = pn.name_type_cd
    with expand=2
endif
 
; Collect the Prsnl relationships at the person level
; ---------------------------------------------------
if (validate(payload->person->prsnlreltn, 0) = 1)
 
    ; set the Parser
    call type_parser("ppr.person_prsnl_r_cd", 331)
 
    select into "nl:"
        person_id       = ppr.person_id
    from    person_prsnl_reltn      ppr,
            prsnl                   p
    plan ppr
        where expand(nNum, 1, size(rPerson->persons, 5), ppr.person_id, rPerson->persons[nNum].person_id)
        and parser(cParser)
        and ppr.active_ind = 1
        and ppr.end_effective_dt_tm > sysdate
    join p
        where p.person_id = ppr.prsnl_person_id
    order person_id
    head person_id
        nPos = locateval(nNum, 1, size(rPerson->persons, 5), ppr.person_id, rPerson->persons[nNum].person_id)
        nCount = 0
    detail
        nCount = nCount + 1
        stat = alterlist(rPerson->persons[nPos].prsnl_reltn, nCount)
 
        rPerson->persons[nPos].prsnl_reltn[nCount].reltn_type = uar_get_code_display(ppr.person_prsnl_r_cd)
        rPerson->persons[nPos].prsnl_reltn[nCount].reltn_type_meaning = uar_get_code_meaning(ppr.person_prsnl_r_cd)
        rPerson->persons[nPos].prsnl_reltn[nCount].person_id = p.person_id
        rPerson->persons[nPos].prsnl_reltn[nCount].priority_seq = ppr.priority_seq
        rPerson->persons[nPos].prsnl_reltn[nCount].prsnl_type = uar_get_code_display(p.prsnl_type_cd)
        rPerson->persons[nPos].prsnl_reltn[nCount].name_full_formatted = p.name_full_formatted
        rPerson->persons[nPos].prsnl_reltn[nCount].physician_ind = p.physician_ind
        rPerson->persons[nPos].prsnl_reltn[nCount].position = uar_get_code_display(p.position_cd)
        rPerson->persons[nPos].prsnl_reltn[nCount].name_last = p.name_last
        rPerson->persons[nPos].prsnl_reltn[nCount].name_first = p.name_first
        rPerson->persons[nPos].prsnl_reltn[nCount].user_name = p.username
        rPerson->persons[nPos].prsnl_reltn[nCount].person_prsnl_r_cd = ppr.person_prsnl_r_cd
        rPerson->persons[nPos].prsnl_reltn[nCount].prsnl_type_cd = p.prsnl_type_cd
        rPerson->persons[nPos].prsnl_reltn[nCount].position_cd = p.position_cd
        
        call add_prsnl(p.person_id)
 
    with expand=2
endif  
 
; Collect the Person Organization Relationships (e.g. Employer)
; -------------------------------------------------------------
if (validate(payload->person->orgreltn, 0) = 1)
 
    ; set the Parser
	call type_parser("por.person_org_reltn_cd", 338)
 
    select into "nl:"
        person_id               = por.person_id
    from    person_org_reltn        por,
            organization            o
    plan por
        where expand(nNum, 1, size(rPerson->persons, 5), por.person_id, rPerson->persons[nNum].person_id)
        and parser(cParser)
        and por.active_ind = 1
        and por.end_effective_dt_tm > sysdate
    join o
        where o.organization_id = por.organization_id
    order person_id
    head person_id
        nPos = locateval(nNum, 1, size(rPerson->persons, 5), por.person_id, rPerson->persons[nNum].person_id)
        nCount = 0
    detail
        nCount = nCount + 1
        stat = alterlist(rPerson->persons[nPos].person_org_reltn, nCount)
 
        rPerson->persons[nPos].person_org_reltn[nCount].person_org_reltn_id = por.person_org_reltn_id
        rPerson->persons[nPos].person_org_reltn[nCount].person_org_reltn = uar_get_code_display(por.person_org_reltn_cd)
        rPerson->persons[nPos].person_org_reltn[nCount].person_org_reltn_meaning = uar_get_code_meaning(por.person_org_reltn_cd)
        rPerson->persons[nPos].person_org_reltn[nCount].organization_id = por.organization_id
        rPerson->persons[nPos].person_org_reltn[nCount].empl_type = uar_get_code_display(por.empl_type_cd)
        rPerson->persons[nPos].person_org_reltn[nCount].empl_status = uar_get_code_display(por.empl_status_cd)
        if (por.organization_id > 0)
            rPerson->persons[nPos].person_org_reltn[nCount].org_name = o.org_name
        else
            rPerson->persons[nPos].person_org_reltn[nCount].org_name = por.ft_org_name
        endif
        rPerson->persons[nPos].person_org_reltn[nCount].priority_seq = por.priority_seq
        rPerson->persons[nPos].person_org_reltn[nCount].person_org_reltn_cd = por.person_org_reltn_cd
        rPerson->persons[nPos].person_org_reltn[nCount].empl_type_cd = por.empl_type_cd
        rPerson->persons[nPos].person_org_reltn[nCount].empl_status_cd = por.empl_status_cd
        
        call add_organization(por.organization_id)
    with expand=2
 
endif
 
; Collect the PERSON Info records
; -------------------------------
if (validate(payload->person->personinfo, 0) = 1)
 
    ; set the Parser
    call type_parser("pi.info_sub_type_cd", 356)
 
    select into "nl:"
        person_id                   = pi.person_id,
        person_info_id              = pi.person_info_id,
        person_code_value_r_id      = pcv.person_code_value_r_id
    from    person_info             pi,
            person_code_value_r     pcv,
            long_text               lt
    plan pi
        where expand(nNum, 1, size(rPerson->persons, 5), pi.person_id, rPerson->persons[nNum].person_id)
        and parser(cParser)
        and pi.active_ind = 1
        and pi.end_effective_dt_tm > sysdate
        and pi.active_status_cd = cv48_Active
    join pcv
        where pcv.person_id = outerjoin(pi.person_id)
        and pcv.code_value = outerjoin(pi.value_cd)
        and pcv.code_value > outerjoin(0)
        and pcv.active_ind = outerjoin(1)
        and pcv.active_status_cd = outerjoin(cv48_Active)
        and pcv.end_effective_dt_tm > outerjoin(sysdate)
    join lt
        where lt.long_text_id = outerjoin(pi.long_text_id)
        and lt.active_ind = outerjoin(1)
    order person_id, person_info_id, person_code_value_r_id
    head person_id
        nPos = locateval(nNum, 1, size(rPerson->persons, 5), pi.person_id, rPerson->persons[nNum].person_id)
        nCount = 0
    head person_info_id
        cString = ""
    head person_code_value_r_id
        x = 0
    detail
        if (pcv.code_value > 0)
            if (trim(cString) != "")
                cString = concat(trim(cString), ",")
            endif
            cString = trim(concat(trim(cString), " ", uar_get_code_display(pcv.code_value)),3)
        endif
    foot person_code_value_r_id
        x = 0
    foot person_info_id
        nCount = nCount + 1
        stat = alterlist(rPerson->persons[nPos].person_info, nCount)
 
        rPerson->persons[nPos].person_info[nCount].info_type = uar_get_code_display(pi.info_type_cd)
        rPerson->persons[nPos].person_info[nCount].info_type_meaning = uar_get_code_meaning(pi.info_type_cd)
        rPerson->persons[nPos].person_info[nCount].info_sub_type = uar_get_code_display(pi.info_sub_type_cd)
        rPerson->persons[nPos].person_info[nCount].info_sub_type_meaning = uar_get_code_meaning(pi.info_sub_type_cd)
        rPerson->persons[nPos].person_info[nCount].value_numeric_ind = pi.value_numeric_ind
        rPerson->persons[nPos].person_info[nCount].value_numeric = pi.value_numeric
        rPerson->persons[nPos].person_info[nCount].value_dt_tm = pi.value_dt_tm
        rPerson->persons[nPos].person_info[nCount].chartable_ind = pi.chartable_ind
        rPerson->persons[nPos].person_info[nCount].priority_seq = pi.priority_seq
        rPerson->persons[nPos].person_info[nCount].internal_seq = pi.internal_seq
        if (trim(cString) != "")
            rPerson->persons[nPos].person_info[nCount].value = cString
        else
            rPerson->persons[nPos].person_info[nCount].value = uar_get_code_display(pi.value_cd)
        endif
        rPerson->persons[nPos].person_info[nCount].long_text = lt.long_text
        rPerson->persons[nPos].person_info[nCount].info_type_cd = pi.info_type_cd
        rPerson->persons[nPos].person_info[nCount].info_sub_type_cd = pi.info_sub_type_cd
        rPerson->persons[nPos].person_info[nCount].value_cd = pi.value_cd
 
    with expand=2
endif
 
; Collect the code value relationships
if (validate(payload->person->personCodeReltn, 0) = 1)
    select into "nl:"
        person_id                   = pcv.person_id,
        code_set                    = pcv.code_set,
        code_value                  = pcv.code_value,
        display                     = uar_get_code_display(pcv.code_value)
    from  person_code_value_r     pcv
    plan pcv
        where expand(nNum, 1, size(rPerson->persons, 5), pcv.person_id, rPerson->persons[nNum].person_id)
        and pcv.active_ind = 1
        and pcv.active_status_cd = outerjoin(cv48_Active)
        and pcv.end_effective_dt_tm > outerjoin(sysdate)
    order person_id, code_set, display
    head person_id
        nPos = locateval(nNum, 1, size(rPerson->persons, 5), pcv.person_id, rPerson->persons[nNum].person_id)
        nCount = 0
    detail
        nCount = nCount + 1
        stat = alterlist(rPerson->persons[nPos].person_code_reltn, nCount)
 
        rPerson->persons[nPos].person_code_reltn[nCount].person_code_value_r_id = pcv.person_code_value_r_id
        rPerson->persons[nPos].person_code_reltn[nCount].code_set = code_set
        rPerson->persons[nPos].person_code_reltn[nCount].code_value = code_value
        rPerson->persons[nPos].person_code_reltn[nCount].display = display
    with counter, expand=2
 
endif 

; Loads the record structure with person records from the patient_source
subroutine loadPersons(null) 
    ; Initialize the population size
    set stat = alterlist(rPerson->persons, size(patient_source->patients, 5))
 
    ; Loop through all the patients
    for (nLoop = 1 to size(patient_source->patients, 5))
        set rPerson->persons[nLoop].person_id = patient_source->patients[nloop].person_id
    endfor
end    


#skip_logic
 
if (validate(payload->person->skipJSON, 0) = 0) 
    call add_standard_output(cnvtrectojson(rPerson, 4, 1))
endif
 
#end_program
 
end go
/*************************************************************************
 
        Script Name:    1co5_mpage_problem.prg
 
        Description:    Clinical Office - MPage Developer
        				Problem Data Retrieval
 
        Date Written:   August 31, 2025
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
 Called from 1CO5_MPAGE_ENTRY. Do not attempt to run stand alone.
 
 Possible Payload values:
 
	"patientSource":[
		{"personId": value, "encntrId": value}
	],
	"problem": {
    	"comments": true
	},
	"typeList": [
		{"codeSet": value, "type": "value", "typeCd": value}
	]
 
 
 
 *************************************************************************
                            Revision Information
 *************************************************************************
 Rev    Date     By             Comments
 ------ -------- -------------- ------------------------------------------
 001    31/08/25 J. Simpson     Initial Development
 *************************************************************************/
 
drop program 1co5_mpage_problem:group1 go
create program 1co5_mpage_problem:group1
 
; Check to see if running from mPage entry script
if (validate(payload->problem) = 0 or size(patient_source->patients, 5) = 0)
	go to end_program
endif
 
record rProblem (
	1 problem[*]
		2 person_id						= f8
		2 problem_instance_id			= f8
		2 problem_id					= f8
		2 nomenclature_id				= f8
		2 prob_source_string			= vc
		2 prob_source_identifier		= vc
		2 prob_source_vocab_cd			= f8
		2 problem_ftdesc				= vc
		2 estimated_resolution_dt_tm	= dq8
		2 actual_resolution_dt_tm		= dq8
		2 classification_cd				= f8
		2 persistence_cd				= f8
		2 confirmation_status_cd		= f8
		2 life_cycle_status_cd			= f8
		2 life_cycle_dt_tm				= dq8
		2 onset_dt_cd					= f8
		2 onset_dt_tm					= dq8
		2 ranking_cd					= f8
		2 certainty_cd					= f8
		2 probability					= f8
		2 person_aware_cd				= f8
		2 prognosis_cd					= f8
		2 person_aware_prognosis_cd		= f8
		2 family_aware_cd				= f8
		2 sensitivity					= i4
		2 course_cd						= f8
		2 cancel_reason_cd				= f8
		2 onset_dt_flag					= i4
		2 status_updt_precision_cd		= f8
		2 status_updt_flag				= i4
		2 status_updt_dt_tm				= dq8
		2 qualifier_cd					= f8
		2 annotated_display				= vc
		2 severity_class_cd				= f8
		2 severity_cd					= f8
		2 severity_ftdesc				= vc
		2 life_cycle_dt_cd				= f8
		2 life_cycle_dt_flag			= i4
		2 problem_type_flag				= i4
		2 laterality_cd					= f8
		2 originating_nomenclature_id	= f8
		2 orig_prob_source_string		= vc
		2 orig_prob_source_identifier	= vc
		2 orig_prob_source_vocab_cd		= f8
		2 comment[*]
			3 comment_dt_tm				= dq8
			3 comment_prsnl_id			= f8
			3 problem_comment			= vc
) with persistscript

; Alternate reference structure - Populate all structures with empty values
if (run_stats->reference_ind = 1)
    set stat = alterlist(rProblem->problem, 1)
    set stat = alterlist(rProblem->problem[1]->comment, 1)
        
    ; Populate the code sets
    select into "nl:"
    from    dm_columns_doc      dcd
    plan dcd
        where dcd.table_name in ("PROBLEM", "NOMENCLATURE")
        and dcd.code_set > 0
    detail
        call add_ref_code_set("problem", camel_field(dcd.column_name), dcd.description, dcd.code_set)        
    with counter        
        
    go to skip_logic
endif 

declare nNum = i4
declare cParser = vc
declare cParser2= vc
 
; Set the Parser
call type_parser("n.source_vocabulary_cd", 400)
set cParser2 = cParser
call type_parser("p.life_cycle_status_cd", 12030)
 
; Collect the problems
select into "nl:"
    person_id           = p.person_id,
	sort_key			= cnvtupper(p.annotated_display)
from	problem				p,
		nomenclature		n,
		nomenclature		n2
plan p
	where expand(nNum, 1, size(patient_source->patients, 5), p.person_id, patient_source->patients[nNum].person_id)
	and parser(cParser)
	and p.active_ind = 1
	and p.end_effective_dt_tm > sysdate
join n
	where n.nomenclature_id = p.nomenclature_id
	and parser(cParser2)
join n2
	where n2.nomenclature_id = p.originating_nomenclature_id
order by person_id, sort_key
head report
	nCount = 0
detail
	nCount = nCount + 1
	stat = alterlist(rProblem->problem, nCount)
 
	rProblem->problem[nCount].person_id = p.person_id
	rProblem->problem[nCount].problem_instance_id = p.problem_instance_id
	rProblem->problem[nCount].problem_id = p.problem_id
	rProblem->problem[nCount].nomenclature_id = p.nomenclature_id
	rProblem->problem[nCount].prob_source_string = n.source_string
	rProblem->problem[nCount].prob_source_identifier = n.source_identifier
	rProblem->problem[nCount].prob_source_vocab_cd = n.source_vocabulary_cd
	rProblem->problem[nCount].problem_ftdesc = p.problem_ftdesc
	rProblem->problem[nCount].estimated_resolution_dt_tm = p.estimated_resolution_dt_tm
	rProblem->problem[nCount].actual_resolution_dt_tm = p.actual_resolution_dt_tm
	rProblem->problem[ncount].classification_cd = p.classification_cd
	rProblem->problem[ncount].persistence_cd = p.persistence_cd
	rProblem->problem[ncount].confirmation_status_cd = p.confirmation_status_cd
	rProblem->problem[ncount].life_cycle_status_cd = p.life_cycle_status_cd
	rProblem->problem[ncount].life_cycle_dt_tm = p.life_cycle_dt_tm
	rProblem->problem[ncount].onset_dt_cd = p.onset_dt_cd
	rProblem->problem[ncount].onset_dt_tm = p.onset_dt_tm
	rProblem->problem[ncount].ranking_cd = p.ranking_cd
	rProblem->problem[ncount].certainty_cd = p.certainty_cd
	rProblem->problem[ncount].probability = p.probability
	rProblem->problem[ncount].person_aware_cd = p.person_aware_cd
	rProblem->problem[ncount].prognosis_cd = p.prognosis_cd
	rProblem->problem[ncount].person_aware_prognosis_cd = p.person_aware_prognosis_cd
	rProblem->problem[ncount].family_aware_cd = p.family_aware_cd
	rProblem->problem[ncount].sensitivity = p.sensitivity
	rProblem->problem[ncount].course_cd = p.course_cd
	rProblem->problem[ncount].cancel_reason_cd = p.cancel_reason_cd
	rProblem->problem[ncount].onset_dt_flag = p.onset_dt_flag
	rProblem->problem[ncount].status_updt_precision_cd = p.status_updt_precision_cd
	rProblem->problem[ncount].status_updt_flag = p.status_updt_flag
	rProblem->problem[ncount].status_updt_dt_tm = p.status_updt_dt_tm
	rProblem->problem[ncount].qualifier_cd = p.qualifier_cd
	rProblem->problem[ncount].annotated_display = p.annotated_display
	rProblem->problem[ncount].severity_class_cd = p.severity_class_cd
	rProblem->problem[ncount].severity_cd = p.severity_cd
	rProblem->problem[ncount].severity_ftdesc = p.severity_ftdesc
	rProblem->problem[ncount].life_cycle_dt_cd = p.life_cycle_dt_cd
	rProblem->problem[ncount].life_cycle_dt_flag = p.life_cycle_dt_flag
	rProblem->problem[ncount].problem_type_flag = p.problem_type_flag
	rProblem->problem[ncount].laterality_cd = p.laterality_cd
	rProblem->problem[ncount].originating_nomenclature_id = p.originating_nomenclature_id
	rProblem->problem[ncount].orig_prob_source_string = n2.source_string
	rProblem->problem[ncount].orig_prob_source_identifier = n2.source_identifier
	rProblem->problem[ncount].orig_prob_source_vocab_cd = n2.source_vocabulary_cd
with nocounter
 
; Collect the comments
if (validate(payload->problem->comments, 0) = 1)
	select into "nl:"
	    problem_id          = pc.problem_id,
        sort_date           = format(pc.comment_dt_tm, "yyyymmddhhmmss;;q")
	from	problem_comment		pc
	plan pc
		where expand(nNum, 1, size(rProblem->problem, 5), pc.problem_id, rProblem->problem[nNum].problem_id)
		and pc.active_ind = 1
		and pc.end_effective_dt_tm > sysdate
	order problem_id, sort_date desc
	head problem_id
		ncount = 0
		
		nPos = locateval(nNum, 1, size(rProblem->problem, 5), pc.problem_id, rProblem->problem[nNum].problem_id)
	head sort_date
		x = 0
	detail
		ncount = ncount + 1
		stat = alterlist(rProblem->problem[nNum].comment, ncount)
 
		rProblem->problem[nPos].comment[ncount].comment_dt_tm = pc.comment_dt_tm
		rProblem->problem[nPos].comment[ncount].comment_prsnl_id = pc.comment_prsnl_id
		rProblem->problem[nPos].comment[ncount].problem_comment = replace(replace(pc.problem_comment, char(13), ""), char(10), "\\n")
 
	with expand=2, nocounter
endif
  
; Skip the rest if no diagnosis loaded
if (size(rProblem->problem, 5) = 0)
	go to end_program
endif

#skip_logic

if (validate(payload->diagnosis->skipJSON, 0) = 0) 
    call add_standard_output(cnvtrectojson(rProblem, 4, 1))
endif
 
#end_program
 
end go
/*************************************************************************
 
        Script Name:    1co5_mpage_prsnl.prg
 
        Description:    Clinical Office - MPage Developer V5
        				Prsnl Data Retrieval
 
        Date Written:   October 25, 2025
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
 Called from 1co5_mpage_entry. Do not attempt to run stand alone.
 
 Possible Payload values:
 
	"prsnlSource": [
		{"personId": value}
	],	
	"prsnl": {
        "aliases": true,
        "credential": true,
        "prsnlGroup": true,
        "orgReltn": true,
        "prsnlPrsnlReltn": true,
        "loadExtendedPersons": true,
        "skipJSON": true
    },
	"typeList": [
		{"codeSet": value, "type": "value", "typeCd": value}
	]
 
 
 
 *************************************************************************
                            Revision Information
 *************************************************************************
 Rev    Date     By             Comments
 ------ -------- -------------- ------------------------------------------
 001    10/25/25 J. Simpson     Initial Development
 *************************************************************************/
 
drop program 1co5_mpage_prsnl:group1 go
create program 1co5_mpage_prsnl:group1 

; Check to see if running from mPage entry script
if (validate(payload->prsnl) = 0)
	go to end_program
endif
 
; Required variables
declare cParser = vc
declare nNum = i4
declare cString = vc

; Add the PRSNL rows from the incoming prsnlSource
if (validate(payload->prsnlSource) = 1)
    for (nLoop = 1 to size(payload->prsnlSource, 5))
        call add_prsnl(payload->prsnlSource[nLoop].personId)
    endfor
endif
  
free record rPrsnl
record rPrsnl (
    1 prsnl[*]
        2 person_id                     = f8
        2 beg_effective_dt_tm           = dq8
        2 end_effective_dt_tm           = dq8
        2 prsnl_type                    = vc
        2 prsnl_type_cd                 = f8
        2 name_full_formatted           = vc
        2 email                         = vc
        2 physician_ind                 = i4
        2 position                      = vc
        2 position_cd                   = f8
        2 department                    = vc
        2 department_cd                 = f8
        2 section                       = vc
        2 section_cd                    = f8
        2 name_last                     = vc
        2 name_first                    = vc
        2 username                      = vc
        2 prim_assign_loc               = vc
        2 prim_assign_loc_cd            = f8
        2 physician_status              = vc
        2 physician_status_cd           = f8
        2 logical_domain_grp_id         = f8
        2 logical_domain_id             = f8
        2 external_ind                  = i4
        2 aliases[*]
            3 alias_pool                = vc
            3 alias_pool_cd             = f8
            3 prsnl_alias_type          = vc
            3 prsnl_alias_type_cd       = f8
            3 alias                     = vc
            3 alias_formatted           = vc
            3 prsnl_alias_sub_type      = vc
            3 prsnl_alias_sub_type_cd   = f8
        2 credential[*]            
            3 credential                = vc
            3 credential_cd             = f8
            3 credential_type           = vc
            3 credential_type_cd        = f8
            3 display_seq               = i4
            3 id_number                 = vc
            3 renewal_dt_tm             = dq8
            3 state                     = vc
            3 state_cd                  = f8
            3 valid_for                 = vc
            3 valid_for_cd              = f8
        2 prsnl_group[*]
            3 prsnl_group_id            = f8
            3 prsnl_group_r             = vc
            3 prsnl_group_r_cd          = f8
            3 prsnl_group_type          = vc
            3 prsnl_group_type_cd       = f8
            3 prsnl_group_name          = vc
            3 prsnl_group_desc          = vc
            3 service_resource          = vc
            3 service_resource_cd       = f8
            3 prsnl_group_class         = vc
            3 prsnl_group_class_cd      = f8
        2 prsnl_prsnl_reltn[*]
            3 prsnl_prsnl_reltn         = vc
            3 prsnl_prsnl_reltn_cd      = f8
            3 related_person_id         = f8
            3 related_person_name       = vc
            3 related_person_position_cd= f8
            3 related_person_position   = vc
        2 prsnl_org_reltn[*]
            3 organization_id           = f8
            3 confid_level              = vc
            3 confid_level_cd           = f8
            3 org_name                  = vc
            
) with persistscript

; Alternate reference structure - Populate all structures with empty values
if (run_stats->reference_ind = 1)
    set stat = alterlist(rPrsnl->prsnl, 1)
    set stat = alterlist(rPrsnl->prsnl[1].aliases, 1)
    set stat = alterlist(rPrsnl->prsnl[1].credential, 1)
    set stat = alterlist(rPrsnl->prsnl[1].prsnl_group, 1)
    set stat = alterlist(rPrsnl->prsnl[1].prsnl_prsnl_reltn, 1)
    set stat = alterlist(rPrsnl->prsnl[1].prsnl_org_reltn, 1)

    ; Populate the code sets
    select into "nl:"
    from    dm_columns_doc      dcd
    plan dcd
        where dcd.table_name in ("CREDENTIAL", "PRSNL", "PRSNL_ALIAS", "PRSNL_PRSNL_RELTN",
                                    "PRSNL_GROUP_RELTN","PRSNL_GROUP", "PRSNL_ORG_RELTN")
        and dcd.code_set > 0
    detail
        call add_ref_code_set("prsnl", camel_field(dcd.column_name), dcd.description, dcd.code_set)        
    with counter        
        
    go to skip_logic
endif 

call loadPrsnl(null)

; Setup multiple loops if necessary when loading extended person data
declare nPrsnlRun = i4 with noconstant(1)
if (validate(payload->prsnl->loadExtendedPersons, 0) = 1)
   set nPrsnlRun = 2
endif

#prsnl_run

; Collect the Prsnl Prsnl Relationships
if (validate(payload->prsnl->prsnlPrsnlReltn, 0) = 1)

    select into "nl:"
        person_id   = ppr.person_id
    from    prsnl_prsnl_reltn       ppr,
            prsnl                   p
    plan ppr
        where expand(nNum, 1, size(rPrsnl->prsnl, 5), ppr.person_id, rPrsnl->prsnl[nNum].person_id)
        and ppr.active_ind = 1
        and ppr.active_status_cd = cv48_Active
        and ppr.end_effective_dt_tm > sysdate
    join p
        where p.person_id = ppr.related_person_id
    order person_id        
    head person_id
        nCount = 0
        nPos = locateval(nNum, 1, size(rPrsnl->prsnl, 5), ppr.person_id, rPrsnl->prsnl[nNum].person_id)
    detail
        if (nPrsnlRun = 1)
            nCount = nCount + 1
            stat = alterlist(rPrsnl->prsnl[nPos].prsnl_prsnl_reltn, nCount)
            rPrsnl->prsnl[nPos]->prsnl_prsnl_reltn[nCount].prsnl_prsnl_reltn = uar_get_code_display(ppr.prsnl_prsnl_reltn_cd)
            rPrsnl->prsnl[nPos]->prsnl_prsnl_reltn[nCount].prsnl_prsnl_reltn_cd = ppr.prsnl_prsnl_reltn_cd
            rPrsnl->prsnl[nPos]->prsnl_prsnl_reltn[nCount].related_person_id = ppr.related_person_id
            rPrsnl->prsnl[nPos]->prsnl_prsnl_reltn[nCount].related_person_name = p.name_full_formatted
            rPrsnl->prsnl[nPos]->prsnl_prsnl_reltn[nCount].related_person_position_cd = p.position_cd
            rPrsnl->prsnl[nPos]->prsnl_prsnl_reltn[nCount].related_person_position = uar_get_code_display(p.position_cd)
        else
            call add_prsnl(ppr.related_person_id)            
        endif
    with expand=2

endif

; If running an extended person search, we need to re-run the last statement with the new id's added
if (nPrsnlRun = 2)
    call loadPrsnl(null)
    set nPrsnlRun = 1
    go to prsnl_run
endif

; Continue normal run
select into "nl:"
    person_id       = p.person_id
from    prsnl               p
plan p
    where expand(nNum, 1, size(rPrsnl->prsnl, 5), p.person_id, rPrsnl->prsnl[nNum].person_id)
order person_id
head person_id
    nPos = locateval(nNum, 1, size(rPrsnl->prsnl, 5), p.person_id, rPrsnl->prsnl[nNum].person_id)
    
    rPrsnl->prsnl[nPos].beg_effective_dt_tm = p.beg_effective_dt_tm
    rPrsnl->prsnl[nPos].end_effective_dt_tm = p.end_effective_dt_tm
    rPrsnl->prsnl[nPos].prsnl_type = uar_get_code_display(p.prsnl_type_cd)
    rPrsnl->prsnl[nPos].prsnl_type_cd = p.prsnl_type_cd
    rPrsnl->prsnl[nPos].name_full_formatted = p.name_full_formatted
    rPrsnl->prsnl[nPos].email = p.email
    rPrsnl->prsnl[nPos].physician_ind = p.physician_ind
    rPrsnl->prsnl[nPos].position = uar_get_code_display(p.position_cd)
    rPrsnl->prsnl[nPos].position_cd = p.position_cd
    rPrsnl->prsnl[nPos].department = uar_get_code_display(p.department_cd)
    rPrsnl->prsnl[nPos].department_cd = p.department_cd
    rPrsnl->prsnl[nPos].section = uar_get_code_display(p.section_cd)
    rPrsnl->prsnl[nPos].section_cd = p.section_cd
    rPrsnl->prsnl[nPos].name_last = p.name_last
    rPrsnl->prsnl[nPos].name_first = p.name_first
    rPrsnl->prsnl[nPos].username = p.username
    rPrsnl->prsnl[nPos].prim_assign_loc = uar_get_code_display(p.prim_assign_loc_cd)
    rPrsnl->prsnl[nPos].prim_assign_loc_cd = p.prim_assign_loc_cd
    rPrsnl->prsnl[nPos].physician_status = uar_get_code_display(p.physician_status_cd)
    rPrsnl->prsnl[nPos].physician_status_cd = p.physician_status_cd
    rPrsnl->prsnl[nPos].logical_domain_grp_id = p.logical_domain_grp_id
    rPrsnl->prsnl[nPos].logical_domain_id = p.logical_domain_id
    rPrsnl->prsnl[nPos].external_ind = p.external_ind
with expand=2

; Collect the aliases
if (validate(payload->prsnl->aliases, 0) = 1)
    select into "nl:"
        person_id       = pa.person_id
    from    prsnl_alias         pa
    plan pa
        where expand(nNum, 1, size(rPrsnl->prsnl, 5), pa.person_id, rPrsnl->prsnl[nNum].person_id)
        and pa.active_ind = 1
        and pa.active_status_cd = cv48_Active
        and pa.end_effective_dt_tm > sysdate
    order person_id
    head person_id
        nPos = locateval(nNum, 1, size(rPrsnl->prsnl, 5), pa.person_id, rPrsnl->prsnl[nNum].person_id)
        nCount = 0
    detail
        nCount = nCount + 1
        stat = alterlist(rPrsnl->prsnl[nPos].aliases, nCount)
        rPrsnl->prsnl[nPos]->aliases[nCount].alias_pool = uar_get_code_display(pa.alias_pool_cd)
        rPrsnl->prsnl[nPos]->aliases[nCount].alias_pool_cd = pa.alias_pool_cd
        rPrsnl->prsnl[nPos]->aliases[nCount].prsnl_alias_type = uar_get_code_display(pa.prsnl_alias_type_cd)
        rPrsnl->prsnl[nPos]->aliases[nCount].prsnl_alias_type_cd = pa.prsnl_alias_type_cd
        rPrsnl->prsnl[nPos]->aliases[nCount].alias = pa.alias
        rPrsnl->prsnl[nPos]->aliases[nCount].alias_formatted = cnvtalias(pa.alias, pa.alias_pool_cd)
        rPrsnl->prsnl[nPos]->aliases[nCount].prsnl_alias_sub_type = uar_get_code_display(pa.prsnl_alias_sub_type_cd)
        rPrsnl->prsnl[nPos]->aliases[nCount].prsnl_alias_sub_type_cd = pa.prsnl_alias_sub_type_cd
    with expand=2
endif

; Collect prsnl groups
if (validate(payload->prsnl->prsnlGroup, 0) = 1)
    select into "nl:"
        person_id       = pgr.person_id
    from    prsnl_group_reltn           pgr,
            prsnl_group                 pg
    plan pgr
        where expand(nNum, 1, size(rPrsnl->prsnl, 5), pgr.person_id, rPrsnl->prsnl[nNum].person_id)
        and pgr.active_ind = 1
        and pgr.active_status_cd = cv48_Active
        and pgr.end_effective_dt_tm > sysdate
    join pg
        where pg.prsnl_group_id = pgr.prsnl_group_id
        and pg.active_ind = 1
        and pg.active_status_cd = cv48_Active
        and pg.end_effective_dt_tm > sysdate
    order person_id
    head person_id
        nPos = locateval(nNum, 1, size(rPrsnl->prsnl, 5), pgr.person_id, rPrsnl->prsnl[nNum].person_id)
        nCount = 0
    detail
        nCount = nCount + 1
        stat = alterlist(rPrsnl->prsnl[nPos].prsnl_group, nCount)
        rPrsnl->prsnl[nPos]->prsnl_group[nCount].prsnl_group_id = pgr.prsnl_group_id
        rPrsnl->prsnl[nPos]->prsnl_group[nCount].prsnl_group_r = uar_get_code_display(pgr.prsnl_group_r_cd)
        rPrsnl->prsnl[nPos]->prsnl_group[nCount].prsnl_group_r_cd = pgr.prsnl_group_r_cd
        rPrsnl->prsnl[nPos]->prsnl_group[nCount].prsnl_group_type = uar_get_code_display(pg.prsnl_group_type_cd)
        rPrsnl->prsnl[nPos]->prsnl_group[nCount].prsnl_group_type_cd = pg.prsnl_group_type_cd
        rPrsnl->prsnl[nPos]->prsnl_group[nCount].prsnl_group_name = pg.prsnl_group_name
        rPrsnl->prsnl[nPos]->prsnl_group[nCount].prsnl_group_desc = pg.prsnl_group_desc
        rPrsnl->prsnl[nPos]->prsnl_group[nCount].service_resource = uar_get_code_display(pg.service_resource_cd)
        rPrsnl->prsnl[nPos]->prsnl_group[nCount].service_resource_cd = pg.service_resource_cd
        rPrsnl->prsnl[nPos]->prsnl_group[nCount].prsnl_group_class = uar_get_code_display(pg.prsnl_group_class_cd)
        rPrsnl->prsnl[nPos]->prsnl_group[nCount].prsnl_group_class_cd = pg.prsnl_group_class_cd
    with expand=2
        
endif

; Collect credentials
if (validate(payload->prsnl->credential, 0) = 1)
    select into "nl:"
        person_id               = c.prsnl_id,
        credential_type_cd      = c.credential_type_cd,
        display_seq             = c.display_seq
    from    credential              c
    plan c
        where expand(nNum, 1, size(rPrsnl->prsnl, 5), c.prsnl_id, rPrsnl->prsnl[nNum].person_id)
        and c.active_status_cd = cv48_Active
        and c.active_ind = 1
        and c.end_effective_dt_tm > sysdate
    order person_id, credential_type_cd, display_seq
    head person_id
        nPos = locateval(nNum, 1, size(rPrsnl->prsnl, 5), c.prsnl_id, rPrsnl->prsnl[nNum].person_id)
        nCount = 0
    detail
        nCount = nCount + 1
        stat = alterlist(rPrsnl->prsnl[nPos].credential, nCount)
        rPrsnl->prsnl[nPos].credential[nCount].credential = uar_get_code_display(c.credential_cd)
        rPrsnl->prsnl[nPos].credential[nCount].credential_cd = c.credential_cd
        rPrsnl->prsnl[nPos].credential[nCount].credential_type = uar_get_code_display(c.credential_type_cd)
        rPrsnl->prsnl[nPos].credential[nCount].credential_type_cd = c.credential_type_cd
        rPrsnl->prsnl[nPos].credential[nCount].display_seq = c.display_seq
        rPrsnl->prsnl[nPos].credential[nCount].id_number = c.id_number
        rPrsnl->prsnl[nPos].credential[nCount].renewal_dt_tm = c.renewal_dt_tm
        rPrsnl->prsnl[nPos].credential[nCount].state = uar_get_code_display(c.state_cd)
        rPrsnl->prsnl[nPos].credential[nCount].state_cd = c.state_cd
        rPrsnl->prsnl[nPos].credential[nCount].valid_for = uar_get_code_display(c.valid_for_cd)
        rPrsnl->prsnl[nPos].credential[nCount].valid_for_cd = c.valid_for_cd    
    with expand=2
endif

; Collect Org Relationships
if (validate(payload->prsnl->orgReltn, 0) = 1)
    select into "nl:"
        person_id               = por.person_id
    from    prsnl_org_reltn     por,
            organization        o
    plan por
        where expand(nNum, 1, size(rPrsnl->prsnl, 5), por.person_id, rPrsnl->prsnl[nNum].person_id)
        and por.active_status_cd = cv48_Active
        and por.active_ind = 1
        and por.end_effective_dt_tm > sysdate
    join o
        where o.organization_id = por.organization_id
    order person_id
    head person_id
        nPos = locateval(nNum, 1, size(rPrsnl->prsnl, 5), por.person_id, rPrsnl->prsnl[nNum].person_id)
        nCount = 0
    detail
        nCount = nCount + 1
        stat = alterlist(rPrsnl->prsnl[nPos].prsnl_org_reltn, nCount)
        rPrsnl->prsnl[nPos]->prsnl_org_reltn[nCount].organization_id = por.organization_id
        rPrsnl->prsnl[nPos]->prsnl_org_reltn[nCount].confid_level = uar_get_code_display(por.confid_level_cd)
        rPrsnl->prsnl[nPos]->prsnl_org_reltn[nCount].confid_level_cd = por.confid_level_cd
        rPrsnl->prsnl[nPos]->prsnl_org_reltn[nCount].org_name = o.org_name
        
        call add_organization(por.organization_id)
    with expand=2
endif

#skip_logic

if (validate(payload->prsnl->skipJSON, 0) = 0)
    call add_standard_output(cnvtrectojson(rPrsnl, 4, 1))
endif

#end_program

; Loads the record structure with person records from the patient_source
subroutine loadPrsnl(null) 
    ; Initialize the population size
    set stat = alterlist(rPrsnl->prsnl, size(prsnl_source->data, 5))
 
    ; Loop through all the patients
    for (nLoop = 1 to size(prsnl_source->data, 5))
        set rPrsnl->prsnl[nLoop].person_id = prsnl_source->data[nloop].person_id
    endfor
end    

end go
/*************************************************************************
 
        Script Name:    1co_mpage_redirect.prg
 
        Description:    Clinical Office - mPage Edition
        				Perform a redirect to WebSphere
 
        Date Written:   March 17, 2021
        Written by:     John Simpson
                        Precision Healthcare Solutions
 
 *************************************************************************
		   Copyright (c) 2018 Precision Healthcare Solutions
 
 NO PART OF THIS CODE MAY BE COPIED, MODIFIED OR DISTRIBUTED WITHOUT
 PRIOR WRITTEN CONSENT OF PRECISION HEALTHCARE SOLUTIONS EXECUTIVE
 LEADERSHIP TEAM.
 
 FOR LICENSING TERMS PLEASE VISIT www.clinicaloffice.com/mpage/license
 
 *************************************************************************
                            Special Instructions
 *************************************************************************
 Used for back-end testing of Clinical Office mPage scripts. You can
 test your own payload JSON by modifying the payload code below.
 *************************************************************************
                            Revision Information
 *************************************************************************
 Rev    Date     By             Comments
 ------ -------- -------------- ------------------------------------------
 001    02/03/18 J. Simpson     Initial Development
 002    05/19/21 J. Simpson     Added support for external links on components
 003    04/27/22 J. Simpson     Switched to JavaScript location instead of meta refresh
 004    05/18/23 J. Simpson     Added support for new component lookup
 *************************************************************************/
 
DROP PROGRAM 1co5_mpage_redirect:group1 GO
CREATE PROGRAM 1co5_mpage_redirect:group1
 
prompt
	"Output to File/Printer/MINE" = "MINE"   ;* Enter or select the printer or file name to send this report to.
	, "Path" = ""
 
with outdev, path
  
; Variable declarations
declare _Memory_Reply_String = vc
declare cPath = vc with noconstant($path)
declare cPiece = vc
 
if (findstring("/index.html", cPath) = 0)
    set cPath = concat(trim(cPath), "/index.html#/")
endif
 
free record response
record response (
    1 url               = vc
    1 component         = vc
)
 
; Check for new MS Edge Component
if (cnvtupper($outdev) = "EDGE-COMPONENT")

    select into "nl:"
    from    dm_info         d
    plan d
        where d.info_domain = "Clinical Office Component"
        and d.info_name = $path
    detail
        cPath = d.info_char
        if (substring(1,4,d.info_char) != "http")
            response->component = d.info_char
        else    ; Need to parse the component name
            nPiece = 1
            while (cPiece != "x")
                cPiece = (piece(d.info_char, "/", nPiece, "x"))
                if (cPiece != "x")
                    response->component = cPiece
                endif
                nPiece = nPiece + 1
            endwhile
            
            call echo(response->component)
        endif
    with counter     
endif

; Will skip the dm_info lookup if http: or https: path sent and a component
if (substring(1,4,cnvtlower(cPath)) = "http")
    set response->url = cPath
 
    if (cnvtupper($outdev) in ("COMPONENT", "EDGE-COMPONENT"))
        go to skip_lookup
    endif
endif
 
select into "nl:"
    full_path = if(trim(response->url) != "")
                    response->url
                else
                    build(d.info_char,"/custom_mpage_content/", cPath)
                endif
from dm_info d
plan d
    where d.info_domain = "INS"
    and d.info_name = "CONTENT_SERVICE_URL"
head report
    if (cnvtupper($outdev) in ("COMPONENT","EDGE-COMPONENT"))
        response->url = trim(full_path,3)
    else
        _Memory_Reply_String = concat(
            ^<!DOCTYPE html>^,
            ^<html><head>^,
            ^<script>window.location.href="^, trim(full_path,3), ^"</script>^,
            ^</head><body><p>Preparing Report Output</p></body></html>^) 
    endif
with maxrow=1, maxcol=500, noformfeed, format=variable, counter
 
#skip_lookup
 
if (cnvtupper($outdev) in ("COMPONENT", "EDGE-COMPONENT"))
    set _Memory_Reply_String = cnvtrectojson(response, 4, 1)
endif

call echo(_Memory_Reply_String)
 
#end_program
 
end go
/*************************************************************************
 
        Script Name:    1co5_mpage_setup.prg
 
        Description:    Clinical Office - MPage Developer
        				View MPage Setup information and configure components
 
        Date Written:   September 27, 2025
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
 *************************************************************************
                            Revision Information
 *************************************************************************
 Rev    Date     By             Comments
 ------ -------- -------------- ------------------------------------------
 001    09/27/25 J. Simpson     Initial Development
 *************************************************************************/
drop program 1co5_mpage_setup:group1 go
create program 1co5_mpage_setup:group1
 
prompt
	"Output to File/Printer/MINE" = "MINE"   ;* Enter or select the printer or file name to send this report to.
 
with OUTDEV
 
 
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
 
; Check to see where running from.
if (validate(patient_source) = 0)
    execute 1co5_mpage_redirect:group1 ^MINE^,^https://www.clinicaloffice.com/mpages/clinical-office-mpage-setup^
endif
 
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
 
if (validate(payload->customscript->script[nscript]->parameters.action) != 1)
    go to end_program
endif
  
; Define subroutines
declare init(null)=null
 
; Define rCustom structure
free record rCustom
record rCustom (
    1 status_code               = i4
)
 
; Initialize variables
declare nNum = i4
declare nPos = i4
 
; Process each action
case (payload->customscript->script[nscript]->parameters.action)
 
     of "init": call init(null) 

endcase

; Initialize MPage values
subroutine init(null)

    free record rCustom
    record rCustom (
        1 components[*]
            2 in_bedrock_id                 = i4
            2 mapped_id                     = i4
            2 label                         = vc
            2 path                          = vc
            2 mapping_status                = vc
            2 last_update_dt_tm             = dq8
            2 updt_id                       = f8
            2 mapping_last_updated_by       = vc
        1 manager_url                       = vc
        1 host
            2 domain                        = vc
            2 zone                          = vc
            2 full_canonical_domain_name    = vc
            2 service_directory_url         = vc        
    ) with persist
    
    ; Collect the list of components from Bedrock
    select distinct into "nl:"
        label = v2.mpage_param_value
    from    br_datamart_filter      f,
            br_datamart_value       v,
            br_datamart_value       v2,
            br_datamart_filter      f2
    plan f
        where f.filter_mean = "CUSTOM_COMP_PRG*"
    join v
        where v.br_datamart_filter_id = f.br_datamart_filter_id
        and v.freetext_desc = "clinical_office.mpage_component"
        and v.end_effective_dt_tm > sysdate            
    join v2
        where v2.br_datamart_category_id = v.br_datamart_category_id
        and v2.mpage_param_mean = "mp_label"
        and v2.end_effective_dt_tm > sysdate
    join f2
        where f2.br_datamart_filter_id = v2.br_datamart_filter_id
        and substring(17,3,f.filter_mean) = substring(13, 3, f2.filter_mean)
    order label
    head report
        nCount = 0
    detail
        nCount = nCount + 1
        stat = alterlist(rCustom->components, nCount)
        rCustom->components[nCount].label = label
        rCustom->components[nCount].in_bedrock_id = 1
        rCustom->components[nCount].mapping_status = "Not Mapped."
    with counter
    
    ; Collect component mappings from DM_INFO
    select into "nl:"
    from    dm_info             di,
            prsnl               p
    plan di
        where di.info_domain = "Clinical Office Component" ;"clinical_office.mpage_developer"
    join p
        where p.person_id = di.updt_id
    detail
        nPos = locateval(nNum, 1, size(rCustom->components, 5), di.info_name, rCustom->components[nNum].label)
        if (nPos = 0)   ; Insert orphaned records no longer in Bedrock into memory.
            stat = alterlist(rCustom->components, size(rCustom->components, 5)+1)
            nPos = size(rCustom->components, 5)
            rCustom->components[nPos].label = di.info_name
            rCustom->components[nPos].mapping_status = "No longer exists in Bedrock, please delete."
        else
            rCustom->components[nPos].mapping_status = "Mapping Completed."
        endif
        
        rCustom->components[nPos].mapped_id = 1
        rCustom->components[nPos].path = di.info_char
        rCustom->components[nPos].last_update_dt_tm = di.updt_dt_tm
        rCustom->components[nPos].updt_id = di.updt_id
        rCustom->components[nPos].mapping_last_updated_by = p.name_full_formatted
    with expand=1        

    ; Collect the manager path
    select into "nl:"                    
    from dm_info d
    plan d
        where d.info_domain = "INS"
        and d.info_name = "CONTENT_SERVICE_URL"
    head report
        rCustom->manager_url = build(d.info_char,"/manager")
    with counter
    
    ; Collect Service Directory information
    set rCustom->host.domain = trim(cnvtlower(logical("environment")),3)

    declare cUri = vc
    declare cTmp = vc with noconstant(concat("1co5_mp_domain",format(sysdate,"yyyymmddhhmmss;;d"),".txt"))
    declare cCmd = vc with noconstant("hostnew -t SRV")
    if (cursys != cursys2)
        set cCmd = "host -N 2 -t SRV"
    endif
    declare cDcl = vc with noconstant(concat(cCmd, " _cerner_", rCustom->host.domain, "_mqclient._tcp >> ", cTmp))
 
    call dcl(cDcl, size(cDcl), 0)
    free define rtl
    define rtl is value(cTmp)
 
    select into "nl:"
    from rtlt   r
    detail
        zoneStart = findstring("._tcp.", r.line)+6
        zoneEnd = findstring(char(32), r.line, zoneStart)
        rCustom->host.zone = substring(zoneStart, (zoneEnd-zoneStart), r.line)
        rCustom->host.full_canonical_domain_name = concat(trim(rCustom->host.domain), ".", trim(rCustom->host.zone))
    with counter        
 
    set stat = remove(cTmp)
 
    ; Determine DNS name (requires multiple passes)
    call getServiceDirectoryUrl(build2("_cerner_svcdirssl_", rCustom->host.domain, "._tcp.", rCustom->host.zone),"https://")
    call getServiceDirectoryUrl(build2("_cerner_svcdir_", rCustom->host.domain, "._tcp.", rCustom->host.zone),"http://")
    call getServiceDirectoryUrl(build2("_cerner_svcdirssl._tcp.", rCustom->host.zone),"https://")
    call getServiceDirectoryUrl(build2("_cerner_svcdir._tcp.", rCustom->host.zone),"http://")    

end 
 
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

; Used to determine the correct service directory
subroutine getServiceDirectoryUrl(cPath, cPrefix)
 
    if (trim(rCustom->host.service_directory_url) = "")
 
        set cDcl = concat(cCmd, " ", cPath, " >> ", cTmp)
        call dcl(cDcl, size(cDcl), 0)
 
        free define rtl
        define rtl is value(cTmp)
 
        select into "nl:"
            text = trim(substring(findstring(char(32), trim(r.line), 1, 1),255,r.line),3)
        from rtlt   r
        plan r
        detail
            url = substring(1,size(trim(text))-1, text)
            if (cnvtint(substring(1,1,url)) = 0)
                rCustom->host.service_directory_url = concat(cPrefix, url)
            endif
        with maxcol=500
 
        set stat = remove(cTmp)
    endif
 
end
 
#end_program
 
end go
/*************************************************************************
 
        Script Name:    1co5_mpage_test.prg
 
        Description:    Clinical Office - MPage Developer
        				Back-end payload test utility
 
        Date Written:   December 19, 2024
        Written by:     John Simpson
                        Precision Healthcare Solutions
 
 *************************************************************************
		   Copyright (c) 2024 Precision Healthcare Solutions
 
 NO PART OF THIS CODE MAY BE COPIED, MODIFIED OR DISTRIBUTED WITHOUT
 PRIOR WRITTEN CONSENT OF PRECISION HEALTHCARE SOLUTIONS EXECUTIVE
 LEADERSHIP TEAM.
 
 FOR LICENSING TERMS PLEASE VISIT www.clinicaloffice.com/mpage/license
 
 *************************************************************************
                            Special Instructions
 *************************************************************************
 Used for back-end testing of Clinical Office mPage scripts.
 *************************************************************************
                            Revision Information
 *************************************************************************
 Rev    Date     By             Comments
 ------ -------- -------------- ------------------------------------------
 001    12/19/24 J. Simpson     Initial Development
 *************************************************************************/
 
drop program 1co5_mpage_test:group1 go
create program 1co5_mpage_test:group1
 
prompt 
	"Output to File/Printer/MINE" = "MINE"           ;* Enter or select the printer or file name to send this report to.
	, "Filename (Leave blank to use default)" = "" 

with outdev, filename
 
; set the absolute maximum possible variable length
set modify maxvarlen 268435456
 
record request (
	1 blob_in = vc
)
 
; Variable declarations
declare _Memory_Reply_String = vc
declare nPERSON_ID = f8 with noconstant(0.0)
declare nENCNTR_ID = f8 with noconstant(0.0)
 
free record fRec
record fRec (
    1 file_desc     = w8
    1 file_offset   = i4
    1 file_dir      = i4
    1 file_name     = vc
    1 file_buf      = vc
)
    
declare cDebugFile = vc with noconstant(concat("1co_debug_", trim(cnvtstring(reqinfo->updt_id)),".json"))
if (trim($filename) != "")
    set cDebugFile = $filename
endif

if (findfile(cDebugFile) = 0)
    set _Memory_Reply_String = concat("File ", cDebugFile, " not found on ", curnode)
    go to end_program
endif
    
set fRec->file_name = cDebugFile
set fRec->file_buf = "r"
set stat = cclio("OPEN", fRec)

; Determine number of characters in file
set fRec->file_dir = 2
set stat = cclio("SEEK", fRec)
set nFileSize = cclio("TELL", fRec)

; Read the file content
set fRec->file_dir = 0
set stat = cclio("SEEK", fRec)
set fRec->file_buf = notrim(fillstring(value(nFileSize), " "))
set stat = cclio("READ", fRec)

set stat = cclio("CLOSE", fRec)

set request->BLOB_IN = fRec->file_buf
 
execute 1co5_mpage_entry:group1 "MINE", value(nPERSON_ID), value(nENCNTR_ID), 0, 0, ^{"mode":"CHART"}^

#end_program
 
call echo(_Memory_Reply_String)
 
END GO
/*************************************************************************
 
        Script Name:    1CO5_PING.PRG
 
        Description:    Clinical Office - MPage Developer
        				Basic Ping Back
 
        Date Written:   July 8, 2025
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
 Called from 1co5_mpage_entry. Do not attempt to run stand alone.
  
 *************************************************************************
                            Revision Information
 *************************************************************************
 Rev    Date     By             Comments
 ------ -------- -------------- ------------------------------------------
 001    07/08/25 J. Simpson     Initial Development
 *************************************************************************/
 
drop program 1co5_ping:group1 go
create program 1co5_ping:group1
 
free record rCustom
record rCustom (
	1 ping				= i4
)

call add_custom_output(cnvtrectojson(rCustom, 4, 1))

#end_program
 
end go
/*************************************************************************
 
        Script Name:    1co5_prsnl_search.prg
 
        Description:    Clinical Office - MPage Developer
                        Prsnl Search Component CCL Support Script
 
        Date Written:   March 3, 2022
        Written by:     John Simpson
                        Precision Healthcare Solutions
 
 *************************************************************************
            Copyright (c) 2022 Precision Healthcare Solutions
 
 NO PART OF THIS CODE MAY BE COPIED, MODIFIED OR DISTRIBUTED WITHOUT
 PRIOR WRITTEN CONSENT OF PRECISION HEALTHCARE SOLUTIONS EXECUTIVE
 LEADERSHIP TEAM.
 
 FOR LICENSING TERMS PLEASE VISIT www.clinicaloffice.com/mpage/license
 
 *************************************************************************
                            Special Instructions
 *************************************************************************
 Called from select component
 *************************************************************************
                            Revision Information
 *************************************************************************
 Rev    Date     By             Comments
 ------ -------- -------------- ------------------------------------------
 001    03/03/22 J. Simpson     Initial Development
 002    12/23/23 J. Simpson     Allow support for mpage-personnel-search component
 *************************************************************************/
drop program 1co5_prsnl_search:group1 go
create program 1co5_prsnl_search:group1
 
; Declare variables and subroutines
declare nNum = i4
declare nDefault = i4
declare cMode = vc with noconstant("mpage-select")
declare cFirstName = vc
declare cLastName = vc

; Custom parser declarations
declare cPrsnlParser = vc with noconstant("1=1")
declare cPrsnlPhysParser = vc with noconstant("1=1")
declare cDefaultParser = vc with noconstant("1=1")

; Collect code values
declare cv48_Active = f8 with noconstant(uar_get_code_by("MEANING", 48, "ACTIVE"))

; Determine the run mode
if (validate(payload->customscript->script[nscript]->parameters->mode) = 1)
    set cMode = payload->customscript->script[nscript]->parameters->mode
endif

free record rCustom

if (cMode = "provider-search")

    record rCustom (
        1 status
            2 error_ind             = i4
            2 message               = vc
        1 data[*]
            2 person_id             = f8
            2 name_full_formatted   = vc
            2 position_cd           = f8
            2 position              = vc
            2 physician             = vc
    )
       
    ; Setup name values
    if (validate(payload->customscript->script[nscript]->parameters->fullname) > 0)
        set cLastName = trim(cnvtupper(piece(payload->customscript->script[nscript]->parameters->fullname, "," , 1,
                            payload->customscript->script[nscript]->parameters->fullname)),3)
        set cFirstName = trim(cnvtupper(piece(payload->customscript->script[nscript]->parameters->fullname, "," , 2, "")),3)
    endif

    if (validate(payload->customscript->script[nscript]->parameters->lastname) > 0)
        set cLastName = cnvtupper(payload->customscript->script[nscript]->parameters->lastname)
    endif

    if (validate(payload->customscript->script[nscript]->parameters->firstname) > 0)
        set cFirstName = cnvtupper(payload->customscript->script[nscript]->parameters->firstname)
    endif

    set cPrsnlParser = "p.name_last_key = patstring(cLastName)"

    if (size(cFirstName) > 0)
        set cPrsnlParser = concat(cPrsnlParser, " and p.name_first_key = patstring(cFirstName)")
    endif
    
    call echorecord(payload->customscript->script[nscript]->parameters)
    
    ; Physician Indicator
    if (payload->customscript->script[nscript]->parameters->physicianInd = 1)
        set cPrsnlPhysParser = "p.physician_ind = 1"
    endif
    
    ; Position Codes
    if (size(payload->customscript->script[nscript]->parameters->positionCd, 5) > 0)
        if (payload->customscript->script[nscript]->parameters->positionCd > 0.0)
        
            set cDefaultParser = concat(^expand(nNum,1,size(payload->customscript->script[nscript]->parameters->positionCd, 5),^,
                                ^p.position_cd, payload->customscript->script[nscript]->parameters->positionCd[nNum])^)
        endif
    endif
        
elseif (cMode = "mpage-select")
    
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
    set rParam->code_set = payload->customscript->script[nscript]->parameters->codeset
    set rParam->value_type = payload->customscript->script[nscript]->parameters->valuetype
    set rParam->search_value = cnvtupper(payload->customscript->script[nscript]->parameters->searchvalue)

    if (validate(payload->customscript->script[nScript]->parameters->default) = 1)
        if (size(payload->customscript->script[nScript]->parameters->default, 5) > 0)
            set stat = alterlist(rParam->default, size(payload->customscript->script[nScript]->parameters->default, 5))
            for (nLoop = 1 to size(rParam->default, 5))   
                set rParam->default[nLoop] = cnvtreal(payload->customscript->script[nScript]->parameters->default[nLoop])
            endfor        
        endif
    endif
  
    ; Build the user search parser
    if (rParam->search_value != "")
    
        set cPrsnlParser = concat(^p.name_last_key = patstring(|^,
                                    piece(rParam->search_value,",",1,rParam->search_value), ^*|)^)
 
        if (piece(rParam->search_value,",",2,"onlylastname") != "onlylastname")
            set cPrsnlParser = concat(trim(cPrsnlParser),
                                ^ and p.name_first_key = patstring(|^,
                                trim(piece(rParam->search_value,",",2,rParam->search_value),3), ^*|)^)
        endif
    ; Build a parser for default values                                    
    elseif (rParam->search_limit > 0 and size(rParam->default, 5) > 0)

        set cDefaultParser = ^expand(nNum, 1, size(rParam->default, 5), p.person_id, rParam->default[nNum])^
        set nDefault = 1

    endif

    if (rParam->physician_ind = 1)
        set cPrsnlPhysParser = "p.physician_ind = 1"
    endif

    ; Perform a limit check to determine if too many values exist to upload
    ; ---------------------------------------------------------------------
    if (rParam->search_limit > 0)
    
        ; Perform your select to count the results you are after
        select into "nl:"
            row_count   = count(p.person_id)
        from    prsnl           p
        plan p
            where parser(cPrsnlParser)
            and parser(cPrsnlPhysParser)
            and p.active_ind = 1
            and p.active_status_cd = cv48_Active
            and p.end_effective_dt_tm > sysdate
        
        ; WARNING: Avoid modifying the detail section below or your code may fail
        detail
            if (row_count > rParam->search_limit and size(rParam->default, 5) = 0)
                rCustom->status->error_ind = 1
                rCustom->status->message = concat(build(cnvtint(row_count)), " records retrieved. Limit is ", 
                                        build(rParam->search_limit), ".")
            endif
            rCustom->status->count = row_count            
        with nocounter        
    
    endif
endif    

    ; Perform the load if search limit does not fail
if (rCustom->status->error_ind = 0 or nDefault = 1)

    set rCustom->status.message = "No records qualified."

    select into "nl:"
    from    prsnl           p
    plan p
        where parser(cPrsnlParser)
        and parser(cPrsnlPhysParser)
        and parser(cDefaultParser)
        and p.active_ind = 1
        and p.active_status_cd = cv48_Active
        and p.end_effective_dt_tm > sysdate
    order p.name_full_formatted        
    head report
        rCustom->status.message = "Ok."
        nCount = 0
        
    ; WARNING: Detail section must write to rCustom->data[].key and rCustom->data[].value        
    detail
        nCount = nCount + 1
        stat = alterlist(rCustom->data, nCount)
        
        if (cMode = "mpage-select")
            statKey = assign(validate(rCustom->data[nCount].key), p.person_id)
            statValue = assign(validate(rCustom->data[nCount].value), p.name_full_formatted)

        elseif (cMode = "provider-search")            
            statPersonId = assign(validate(rCustom->data[nCount].person_id), p.person_id)
            statNameFull = assign(validate(rCustom->data[nCount].name_full_formatted), p.name_full_formatted)
            statPosCd = assign(validate(rCustom->data[nCount].position_cd), p.position_cd)
            statPosition = assign(validate(rCustom->data[nCount].position), uar_get_code_display(p.position_cd))
            statPhys = assign(validate(rCustom->data[nCount].physician), evaluate(p.physician_ind, 1, "Yes", "No"))

        endif
        
    with counter        

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

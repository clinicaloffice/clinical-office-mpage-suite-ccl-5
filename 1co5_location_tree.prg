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

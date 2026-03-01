/*************************************************************************
 
        Script Name:    1co5_custom_tables.prg
 
        Description:    Clinical Office MPage Suite Custom Tables Creation
 
        Date Written:   February 8, 2026
        Written by:     John Simpson
                        Precision Healthcare Solutions
 
 *************************************************************************
            Copyright (c) 2025/2026 Precision Healthcare Solutions
 
 NO PART OF THIS CODE MAY BE COPIED, MODIFIED OR DISTRIBUTED WITHOUT
 PRIOR WRITTEN CONSENT OF PRECISION HEALTHCARE SOLUTIONS EXECUTIVE
 LEADERSHIP TEAM.
 
 FOR LICENSING TERMS PLEASE VISIT https://www.clinicaloffice.com

 *************************************************************************
                            Special Instructions
 *************************************************************************
 Need to cycle servers 58, 79, 178 and 179 after changes
 
 *************************************************************************
                            Revision Information
 *************************************************************************
 Rev    Date     By             Comments
 ------ -------- -------------- ------------------------------------------
 001    02/08/26 J. Simpson     Initial Development
 *************************************************************************/

drop program 1co5_custom_tables:group1 go
create program 1co5_custom_tables:group1

prompt 
	"Mode: (C)reate or (O)ragen" = "" 

with mode

declare table_exists(cTable=vc)=i4 with persist
declare seq_exists(cSequence=vc)=i4 with persist

if (cnvtupper($mode) = "C")

    call echo("Creating Clinical Office MPage Suite tables")

    ; Create the custom sequences
    if (seq_exists("cust_co_ref_seq") = 0)
        rdb create sequence cust_co_ref_seq end
    endif
        
    ; Custom Reference Table    
    if (table_exists("cust_co_reference") = 0)
        select into table cust_co_reference
            ref_id              = type("f8"),
            ref_name            = type("vc40"),
            ref_task            = type("vc40"),
            description         = type("vc100"),
            parent_entity_id    = type("f8"),
            parent_entity_name  = type("vc32"),
            sequence            = type("i4"),
            ref_text            = type("zvc32000"),
            active_ind          = type("i4"),
            create_prsnl_id     = type("f8"),
            create_dt_tm        = type("dq8"),
            updt_id             = type("f8"),
            updt_dt_tm          = type("dq8"),
            beg_effective_dt_tm = type("dq8"),
            end_effective_dt_tm = type("dq8")
        from dummyt         d
        with    constraint(ref_id, "primary key", "unique"),
                index(ref_name, ref_task, parent_entity_id, sequence),
                index(updt_dt_tm, updt_id),
                synonym = "CUST_CO_REFERENCE",
                organization = "P"        
    endif
    
elseif (cnvtupper($mode) = "O")

    drop table cust_co_reference
    execute oragen3 "CUST_CO_REFERENCE"

endif

call echo("***********************************************************************************")
call echo("                               OPERATION COMPLETE")
call echo("***********************************************************************************")
call echo("Please cycle servers 58, 79, 178 and 179 to reflect changes in Cerner applications.")
call echo("***********************************************************************************")

; Determines if a table exists in the dictionary
subroutine table_exists(cTable)

    declare nExists = i4 with noconstant(0)
    
    select into "nl:"
    from    dba_tables      d
    plan d
        where d.table_name = cnvtupper(cTable)
    detail
        nExists = 1

        call echo(concat("Table ", cTable, " exists and will not be created."))

    with counter    
    
    return (nExists)

end

; Determines if a sequence exists in the dictionary
subroutine seq_exists(cSequence)

    declare nExists = i4 with noconstant(0)
    
    select into "nl:"
    from    dba_sequences   d
    plan d
        where d.sequence_name = cnvtupper(cSequence)
    detail
        nExists = 1
        
        call echo(concat("Sequence ", cSequence, " exists and will not be created."))
    with counter
    
    return (nExists)

end
        
end go

000100050118     H DECEDIT('0,') DATEDIT(*YMD.)
000200021214      *
000300110202     Fmontalbanoif   E             DISK
000301110202     F                                     RENAME(montalbano:montrec)
000400110202     FWfval00f  o    E             DISK
000401110202      * Ds esterna per TISE70R
000402110202     D tise70ds      E DS
001000051125     c                   do        *hival
001100110202     c                   read      montalbano
001200110202     c                   if        %eof(montalbano)
001300051125     c                   leave
001400051125     c                   endif
001401110202     c                   clear                   tise70ds
001402110202     c                   eval      I70TLA='S'
001404110202     c                   z-add     tamksc        I70KSC
001405110202     c                   move      tamctr        I70CTR
001406110202     c                   z-add     201001        I70DAI
001407110202     c                   z-add     201012        I70DAF
001500110202     c                   call      'TISE70R'
001600110202     c                   parm                    tise70ds
001601110202     c                   z-add     ORGCAR        WCAR
001602110202     c                   movel     DESCR         WCARD
001603110202     c                   move      TAMATB        WATB
001604110202     c                   z-add     TAMKSC        WKSC
001605110202     c                   movel     ACORAG        WRAG
001606110202     c                   z-add     TAMCTR        WCTR
001607110202     c                   z-add     TAMPRG        WPRG
001609110202     c                   z-add     O70NSP        W70NSP
001610110202     c                   z-add     O70NCO        W70NCO
001611110202     c                   z-add     O70PKG        W70PKG
001612110202     c                   z-add     O70VOL        W70VOL
001613110202     c                   z-add     O70RIC        W70RIC
001614110202     c                   z-add     O70RIR        W70RIR
001615110202     c                   z-add     O70RIP        W70RIP
001616110202     c                   write     wfval000
002400051125     c                   enddo
002500051125     c                   seton                                        lr

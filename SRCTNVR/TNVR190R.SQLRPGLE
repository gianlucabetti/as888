000001090406      *PARMS DFTACTGRP(*NO) ACTGRP(QILE) OPTION(*NOXREF)
000002090406      *PARMS CLOSQLCSR(*ENDMOD)
000003090406      *PARMS DYNUSRPRF(*OWNER)
000004090406     h decedit('0,') datedit(*ymd/) option(*nodebugio)
000200990901     F*------------------------------------------------------------------------*
000300990901     F* DATA BASE
000400990901     F*------------------------------------------------------------------------*
000500090406     FTITAS30C  UF   E           K DISK    ignore(titasp00)
000501090406     f                                     ignore(titas000)
001000001005     F*------------------------------------------------------------------------*
001001090406      * - Dati di WFCFT00F estratti via SQL
001002090406     d WFCFTds       e ds                  extname(WFCFT00F)  inz
002700001005      *------------------------------------------------------------------------*
003300001005      *
003301090406     c/EXEC SQL
003302090406     c+ declare A1 cursor for
003303090406     c+ select  *
003304090406     c+ from    edpconfs03/wfcft00f
003305090406     c+ where   cftksc =  1900005
003306090406     c+ order   by cftaas, cftlnp, cftnrs, cftnsp
003307090406     c+ for     FETCH  only
003308090406     c/END-EXEC
003309090406      *
003310090406     c/EXEC SQL
003311090406     c+ open A1
003312090406     c/END-EXEC
003700001005      *
004500001005      *
004600001005     C/EXEC SQL
004601090406     c+ fetch next from A1 into :WFCFTds
004800001005     C/END-EXEC
004900001005      *
005000001005     C                   dow       sqlcod=*zeros
005100001005      *
005101090406     c     ktas          chain     titas30c
005102090406     c                   If        %found(titas30c)
005103090406     c                   eval      tasimv = cftimvp
005104090406     c                   eval      taspor = cftporp
005105090406     c                   update    titas010
005106090406     c                   endif
005400001005      *
005500001005     C/EXEC SQL
005501090406     c+ fetch next from A1 into :WFCFTds
005700001005     C/END-EXEC
005800001005     C                   ENDDO
005900001005      *
006000001005     C/EXEC SQL
006100090406     C+ close A1
006200001005     C/END-EXEC
006300000204     C*
006400000204     C                   SETON                                        LR
017100990901     C*------------------------------------------------------------------------*
017200990901     C* *INZSR - OPERAZIONI INIZIALI
017300990901     C*------------------------------------------------------------------------*
017400990901     C     *INZSR        BEGSR
017500000204     C*
017600090406     C     kTAS          klist
017700090406     C                   kfld                    cftAAS
017800090406     C                   kfld                    cftLNP
017900090406     C                   kfld                    cftNRS
018000090406     C                   kfld                    cftNSP
018100001005     C*
019800990901     C                   ENDSR
019900000203     C*------------------------------------------------------------------------*

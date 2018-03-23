000100040917     H DECEDIT('0,') DATEDIT(*yMd.)
000101070406     D*
000102070406     D FIGRV00F      E DS
002003070330     D*
002004070416     D DS_S_GRV        DS
002007070330     D  s_grvKEY                           like(grvKEY)
002008070416     D  s_grvDES                           like(grvDES)
002009070406     D*
002010070406     D In_Fil          s              3s 0
002011070406     D In_Dat          s              8s 0
002012070406     D Out_KeyVer      s              8
002013070416     D Out_VerDes      s             30
002014070406     D Out_Esito       s              1
002100050103     C**********************************************************************
002200040917     C     *ENTRY        PLIST
002300070406     C                   PARM                    In_Fil
002301070406     C                   PARM                    In_Dat
002302070406     C                   PARM                    Out_KeyVer
002303070416     C                   PARM                    Out_VerDes
002304070406     C                   PARM                    Out_Esito
002305070406     C*
002306070406     C                   EXSR      REPVER
014200951025     C*
014300070406     C                   SETON                                        LR
014301070330     C*------------------------------------------------------------------------*
014302070406     C* REPVER - Routine di reperimento versioe giri x fil/data
014303070330     C*------------------------------------------------------------------------*
014304070406     C     REPVER        BEGSR
014313070330     C*
014314070330     C/EXEC SQL
014315070330     C+ declare C1 cursor for
014316070416     C+ select grvKEY, grvDES
014317070406     C+ from figrv00f
014319070406     C+ where grvFGS = :In_Fil and
014320070406     C+ :In_Dat between grvDDE and grvDSCAD
014324070330     C+ for read only
014325070330     C/END-EXEC
014326070330     C
014327070330     C/EXEC SQL
014328070330     C+ open C1
014329070330     C/END-EXEC
014330070330     C
014331070330     C/EXEC SQL
014332070416     C+ Fetch next from C1 into :ds_s_grv
014333070330     C/END-EXEC
014334070330     C*
014335070330     C* Se reperito qualcosa => analizzo
014337070330     C                   if        sqlcod = *zeros
014338070406     C                   z-add     *zeros        wConta            6 0
014339070330     C                   dow       sqlcod = *zeros
014340070406     C                   add       1             wConta
014341070406     C                   eval      Out_KeyVer = s_grvKEY
014342070416     C                   eval      Out_VerDes = s_grvDES
014344070330     C*
014345070406     C* Leggo la prossima versione per la fil/data richiesti
014347070330     C/EXEC SQL
014348070416     C+ Fetch next from C1 into :ds_s_grv
014349070330     C/END-EXEC
014350070330     C                   enddo
014351070330     C                   endif
014359070330     C*
014360070330     C/EXEC SQL
014361070330     C+ close C1
014362070330     C/END-EXEC
014363070330     C*
014364070406     C* Se presenti + versioni valide alla stessa data x la stessa filiale => errore
014365070406     C                   if        wConta <> 1
014366070406     C                   eval      Out_Esito = '1'
014370070406     C                   endif
014371070406     C*
014372070330     C                   ENDSR
014414070330     C*--------------------------------------------------------------------------------------------*
014415070330     C* *inzsr - operazioni iniziali
014416070330     C*--------------------------------------------------------------------------------------------*
014417070330     C     *inzsr        BEGSR
014425070330     C*
014426070330     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
014427070330     C
014428070330     C/EXEC SQL
014429070330     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
014430070330     C/END-EXEC
014431070330     C*
014432070330     C                   ENDSR

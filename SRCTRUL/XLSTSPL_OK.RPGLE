000100020909      *---------------------------------------------------------------*
000200020909     D CMD             S             80    DIM(1) CTDATA PERRCD(1)
000300020909      *---------------------------------------------------------------*
000400020910     D                SDS
000500051017     D  USNAM                254    263                                         * utente corrente
000600051019     D*
000700051017     D                 DS
000800051017     D  USRSPACENAM            1     10
000900051017     D  USRSPACELIB           11     20
001000051017     D  USRSPACE               1     20
001100051019     D*
001200020909     D                 DS
001300051017     D  USRSPACEFMT            1     10
001400051017     D  USRSPACELEN           11     14B 0
001500051017     D  USRSPACEINZ           15     15
001600051017     D  USRSPACEAUT           16     25
001700051017     D  USRSPACETXT           26     75
001800051019     D*
001900020909     D                 DS
002000020909     D  LENDTA                 1      4B 0
002100020909     D  STRPOS                 5      8B 0
002200020909     D  SPALEN                 9     12B 0
002300020909     D  SPASNR                13     16B 0
002400051019     D*
002500020909     D RCVPRI          DS
002600020909     D  PRISET                 1      4B 0
002700020909     D  PRINUM                 9     12B 0
002800020909     D  PRISIZ                13     16B 0
002900051019     D*
003000051019     D**************
003100051019     D* DS ESTERNE DI PROCEDURE
003200051019     D**************
003300051019     D XLSTSPLDS     E DS
003400051019     D XLSTSPLDS1    E DS
003500051019     D XLSTSPLDS2    E DS
003600051019     D XSPLATRDS1    E DS
003700051019     D*
003800051019     D**************
003900051019     D* DS ESTERNE DI SISTEMA X GESTIONE RITORNO ERRORI API
004000051019     D**************
004100051019     D/COPY QSYSINC/QRPGLESRC,QUSEC
004200051019     D*
004300041006     D********
004400051019     D* SCHIERE SPOOL RITORNATI IN OUT
004500041006     D********
004600051019     D SKSPOOL         S                   like(XLSTSPLDS2) dim(9999)
004700051019     D                                     inz(*blanks)
004800051019     D*
004900051017     D********
005000051019     D* VARIABILI D WRK
005100051017     D********
005200051017     D wQUSLSPL_KEYF   S             10i 0 dim(9999) inz(*zeros)
005300051017     D wQUSLSPL_NUMF   S             10i 0 inz(*zeros)
005400051017     D wQUSLSPL_AUXS   S             10i 0 inz(*zeros)
005500051019     D jSKSPOOL        S              4  0 inz(*zeros)
005600051017     C*---------------------------------------------------------------*
005700970929     C     *ENTRY        PLIST
005800051019     C                   PARM                    XLSTSPLDS
005900051018     C*
006000051018     C* Reperisco livello d release del S.O. del sistema corrente
006100051018     C
006200051018     C     *DTAARA       DEFINE    QSS1MRI       Rel_Sys_OS        4
006300051018     C                   IN(e)     Rel_Sys_OS
006400051018     C                   IF        %error OR
006500051018     C                             %subst(Rel_Sys_OS:1:2) = 'V4'
006600051018     C                   SETON                                        50
006700051018     C                   ELSE
006800051018     C                   SETOFF                                       50
006900051018     C                   ENDIF
007000051017     C*
007100051017     C* Elimino subito lo user-space d wrk
007200020909     C                   EXSR      DLTOUT
007300051017     C*
007400051017     C* Verifico i parametri d input
007500051019     C                   IF        §LST_USER = *BLANKS
007600051019     C***                MOVEL     USNAM         §LST_USER                      * utente corrente
007700051019     C                   EVAL      §LST_USER = '*ALL'                           * tutti gli utenti
007800040202     C                   ENDIF
007900051018     C*
008000051019     C                   IF        §LST_OQNAM = *BLANKS
008100051019     C                   EVAL      §LST_OQNAM = '*ALL'
008200051019     C                   EVAL      §LST_OQLIB = *BLANKS
008300040202     C                   ELSE
008400051019     C                   IF        §LST_OQNAM <> '*ALL' AND
008500051019     C                             §LST_OQLIB  = *BLANKS
008600051019     C                   EVAL      §LST_OQLIB  = '*LIBL'
008700040202     C                   ENDIF
008800040202     C                   ENDIF
008900051018     C*
009000051019     C                   IF        §LST_FTYP = *BLANKS
009100051019     C                   EVAL      §LST_FTYP = '*ALL'
009200051017     C                   ENDIF
009300051018     C*
009400051019     C                   IF        §LST_UDTA = *BLANKS
009500051019     C                   EVAL      §LST_UDTA  = '*ALL'
009600051017     C                   ENDIF
009700051018     C*
009800051019     C                   IF        §LST_DATDA = *BLANKS
009900051019     C                   EVAL      §LST_DATDA = '*ALL'
010000051019     C                   EVAL      §LST_ORADA = *BLANKS
010100051017     C                   ENDIF
010200051018     C*
010300051019     C                   IF        §LST_DATA = *BLANKS
010400051019     C                   EVAL      §LST_DATA = '*ALL'
010500051019     C                   EVAL      §LST_ORAA = *BLANKS
010600051019     C                   ENDIF
010700051018     C*
010800051019     C                   IF        §LST_TIPSP = *BLANKS
010900051019     C                   EVAL      §LST_TIPSP = '*ALL'
011000051018     C                   ENDIF
011100051019     C*
011200051019     C                   IF        §LST_JOBID <> *BLANKS
011300051019     C                   EVAL      §LST_USER  = *BLANKS
011400051019     C                   EVAL      §LST_OQLIB = *BLANKS
011500051019     C                   EVAL      §LST_OQNAM = *BLANKS
011600051019     C                   EVAL      §LST_FTYP  = *BLANKS
011700051019     C                   EVAL      §LST_UDTA  = *BLANKS
011800051019     C                   ENDIF
011900051017     C*
012000051019     C* Creo lo user-space d wrk in QYEMP con il nome 'XLSTSPL'
012100051019     C                   MOVEL(P)  'XLSTSPL'     USRSPACENAM
012200051017     C                   MOVEL(P)  'QTEMP'       USRSPACELIB
012300051017     C                   Z-ADD     00100         USRSPACELEN
012400051017     C                   MOVEL     '*ALL'        USRSPACEAUT
012500051019     C                   MOVEL     'XLSTSPL'     USRSPACETXT
012600020909     C                   CALL      'QUSCRTUS'
012700051017     C                   PARM                    USRSPACE
012800051017     C                   PARM                    USRSPACEFMT
012900051017     C                   PARM                    USRSPACELEN
013000051017     C                   PARM                    USRSPACEINZ
013100051017     C                   PARM                    USRSPACEAUT
013200051017     C                   PARM                    USRSPACETXT
013300051017     C*
013400051017     C* Reperisce la lista spool secondo selezioni richieste e la rivcersa nello user-space d wrk appena creato
013500051017     C                   CLEAR                   QUSEC
013600051017     C                   EVAL      QUSBPRV = %size(QUSEC)
013700051019     C                   EVAL      in_OUTQNAMLIB = §LST_OQNAM + §LST_OQLIB
013800051018     C*
013900051018     C* Se la versione d O.S. del sistema corrente è la V4* effettuo la chiamata all'API con 1 minor numero d parametri
014000051018     C                   IF        *IN50 = *ON
014100040202     C                   CALL(e)   'QUSLSPL'
014200051017     C                   PARM                    USRSPACE
014300051017     C                   PARM      'SPLF0100'    wQUSLSPL_FMTN     8
014400051019     C                   PARM                    §LST_USER
014500051017     C                   PARM                    in_OUTQNAMLIB    20
014600051019     C                   PARM                    §LST_FTYP
014700051019     C                   PARM                    §LST_UDTA
014800051017     C                   PARM                    QUSEC
014900051019     C                   PARM                    §LST_JOBID
015000051017     C                   PARM                    wQUSLSPL_KEYF
015100051017     C                   PARM                    wQUSLSPL_NUMF
015200051017     C                   PARM                    wQUSLSPL_AUXS
015300051018     C                   ELSE
015400051018     C                   CALL(e)   'QUSLSPL'
015500051019     C                   PARM                    USRSPACE
015600051019     C                   PARM      'SPLF0100'    wQUSLSPL_FMTN     8
015700051019     C                   PARM                    §LST_USER
015800051019     C                   PARM                    in_OUTQNAMLIB    20
015900051019     C                   PARM                    §LST_FTYP
016000051019     C                   PARM                    §LST_UDTA
016100051019     C                   PARM                    QUSEC
016200051019     C                   PARM                    §LST_JOBID
016300051019     C                   PARM                    wQUSLSPL_KEYF
016400051019     C                   PARM                    wQUSLSPL_NUMF
016500051019     C                   PARM                    wQUSLSPL_AUXS
016600051019     C                   PARM      '*CURRENT'    wQUSLSPL_SYSN     8
016700051019     C                   PARM                    §LST_DATDA
016800051019     C                   PARM                    §LST_ORADA
016900051019     C                   PARM                    §LST_DATA
017000051019     C                   PARM                    §LST_ORAA
017100051018     C                   ENDIF
017200051017     C*
017300051018     C* Gestisco l'errore e quindi esco ritornando esito negativo della richiesta
017400051017     C                   IF        %error OR QUSEI <> *blanks
017500051019     C                   EVAL      §LST_ESITO = 'E'
017600051018     C                   ELSE
017700051018     C*
017800051018     C* Lettura user-space con lista spool reperiti
017900051018     C                   Z-ADD     16            LENDTA
018000051018     C                   Z-ADD     125           STRPOS
018100020909     C                   CALL      'QUSRTVUS'
018200051017     C                   PARM                    USRSPACE
018300020909     C                   PARM                    STRPOS
018400020909     C                   PARM                    LENDTA
018500020909     C                   PARM                    RCVPRI
018600020909     C*
018700020909     C                   Z-ADD     PRISET        STRPOS
018800020909     C                   ADD       1             STRPOS
018900020909     C                   Z-ADD     PRISIZ        LENDTA
019000020909     C                   Z-ADD     1             COUNT            15 0
019100020910     C*
019200020910     C                   EXSR      CICLO
019300051019     C                   ENDIF
019400051017     C*
019500051019     C* Al termine elimino lo user-space d wrk
019600020909     C                   EXSR      DLTOUT
019700051017     C*
019800051017     C* Ed esco
019900910702     C                   SETON                                          LR
020000051017     C*---------------------------------------------------------------*
020100051017     C*  LETTURA USER-SPACE
020200051017     C*---------------------------------------------------------------*
020300051017     C     CICLO         BEGSR
020400051017     C*
020500020910     C     COUNT         DOWLE     PRINUM
020600051019     C*
020700051019     C* Lettura user-space con lista spool reperiti
020800020910     C                   CALL      'QUSRTVUS'
020900051017     C                   PARM                    USRSPACE
021000020910     C                   PARM                    STRPOS
021100020910     C                   PARM                    LENDTA
021200051019     C                   PARM                    XLSTSPLDS1
021300051019     C*
021400051019     C* Inizializzo indicatore x considerazione si/no spool corrente e ds d procedura
021500051019     C                   SETOFF                                       51
021600051019     C                   CLEAR                   XSPLATRDS1
021700051019     C*
021800051019     C* Se richiesti determinati in ingresso parametri d selezione occorre reperire gli attributi dello spool
021900051019     C                   IF        §LST_TIPSP <> '*ALL' OR
022000051019     C                             §LST_PAGDA  > *ZEROS OR
022100051019     C                             §LST_PAGA   > *ZEROS
022200051019     C                   EVAL      SPALEN = %size(XSPLATRDS1)
022300020910     C                   MOVEL     'SPLA0100'    SPAIDR            8
022400020910     C                   MOVEL     '*INT'        SPAQJN           26
022500051019     C                   MOVEL     §LST1_JID     SPAIJN           16
022600051019     C                   MOVEL     §LST1_SID     SPASPL           16
022700020910     C                   MOVEL     '*INT'        SPASNM           10
022800020910     C                   Z-ADD     0             SPASNR
022900020910     C                   CALL      'QUSRSPLA'
023000051019     C                   PARM                    XSPLATRDS1
023100020910     C                   PARM                    SPALEN
023200020910     C                   PARM                    SPAIDR
023300020910     C                   PARM                    SPAQJN
023400020910     C                   PARM                    SPAIJN
023500020910     C                   PARM                    SPASPL
023600020910     C                   PARM                    SPASNM
023700020910     C                   PARM                    SPASNR
023800051017     C*
023900020910     C                   ADD       1             COUNT
024000020910     C                   ADD       PRISIZ        STRPOS
024100051017     C*
024200051019     C* Verifico determinati parametri d selezione ricevuti in ingresso
024300051019     C                   IF        §LST_PAGDA > *ZEROS AND
024400051019     C                             §SPLA1_TPG < §LST_PAGDA
024500051019     C                   SETON                                        51
024600051019     C                   ENDIF
024700051019     C                   IF        §LST_PAGA > *ZEROS AND
024800051019     C                             §SPLA1_TPG > §LST_PAGA
024900051019     C                   SETON                                        51
025000051019     C                   ENDIF
025100051019     C                   IF        §LST_TIPSP        <> '*ALL'     AND
025200051019     C                             %trim(§SPLA1_PRT) <> §LST_TIPSP
025300051019     C                   SETON                                        51
025400051019     C                   ENDIF
025500051019     C                   ELSE
025600051019     C                   EVAL      §SPLA1_JOB = §LST1_JID
025700051019     C                   EVAL      §SPLA1_SPL = §LST1_SID
025800051019     C                   ENDIF
025900051019     C*
026000051019     C* Se spool corrente è da considerare lo passo nei parametri d out
026100051019     C                   IF        *IN51 = *OFF
026200051019     C                   ADD       1             jSKSPOOL
026300051019     C                   EVAL      §LST2_JOB = §SPLA1_JOB
026400051019     C                   EVAL      §LST2_SPL = §SPLA1_SPL
026500051019     C                   EVAL      SKSPOOL(jSKSPOOL) = XLSTSPLDS2
026600051019     C                   ENDIF
026700051019     C*
026800020910     C                   ENDDO
026900051019     C*
027000051019     C* Quindi restituisco in out i primi 1000 spool repriti corrispondenti alle selezioni richieste
027100051019     C                   MOVEA     SKSPOOL       §LST_LISTA
027200051017     C*
027300020910     C                   ENDSR
027400051017     C*---------------------------------------------------------------*
027500051017     C*  ELIMINA LO SPAZIO UTENTE DALLA QTEMP
027600051017     C*---------------------------------------------------------------*
027700051017     C     DLTOUT        BEGSR
027800051017     C*
027900051019     C                   CALL(e)   'QCMDEXC'
028000051017     C                   PARM                    CMD
028100051017     C                   PARM      80            LEN              15 5
028200051017     C*
028300051017     C                   ENDSR
028400051017     C*---------------------------------------------------------------*
028500020910**
028600051019DLTUSRSPC  USRSPC(QTEMP/XLSTSPL)

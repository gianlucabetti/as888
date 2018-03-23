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
001800051024     D*
001900051024     D                 DS
002000051024     D  LENDTA                 1      4B 0
002100051024     D  STRPOS                 5      8B 0
002200051024     D  SPALEN                 9     12B 0
002300051024     D  SPASNR                13     16B 0
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
003600051021     D XSPLATRDS     E DS
003700051019     D XSPLATRDS1    E DS
003800051019     D*
003900051019     D**************
004000051019     D* DS ESTERNE DI SISTEMA X GESTIONE RITORNO ERRORI API
004100051019     D**************
004200051019     D/COPY QSYSINC/QRPGLESRC,QUSEC
004300051019     D*
004400041006     D********
004500051019     D* SCHIERE SPOOL RITORNATI IN OUT
004600041006     D********
004700051019     D SKSPOOL         S                   like(XLSTSPLDS2) dim(9999)
004800051019     D                                     inz(*blanks)
004900051019     D*
005000051017     D********
005100051019     D* VARIABILI D WRK
005200051017     D********
005300051017     D wQUSLSPL_KEYF   S             10i 0 dim(9999) inz(*zeros)
005400051017     D wQUSLSPL_NUMF   S             10i 0 inz(*zeros)
005500051017     D wQUSLSPL_AUXS   S             10i 0 inz(*zeros)
005600051025     D wLST_OQLIB      S                   like(§LST_OQLIB) inz(*blanks)
005700051025     D wLST_OQNAM      S                   like(§LST_OQNAM) inz(*blanks)
005800051025     D wLST_FTYP       S                   like(§LST_FTYP)  inz(*blanks)
005900051025     D wLST_UDTA       S                   like(§LST_UDTA)  inz(*blanks)
006000051019     D jSKSPOOL        S              4  0 inz(*zeros)
006100051017     C*---------------------------------------------------------------*
006200970929     C     *ENTRY        PLIST
006300051019     C                   PARM                    XLSTSPLDS
006400051018     C*
006500051018     C* Reperisco livello d release del S.O. del sistema corrente
006600051018     C
006700051024     C     *DTAARA       DEFINE    QSS1MRI       Rel_Sys_OS      750
006800051024     C                   IN        Rel_Sys_OS
006900051018     C                   IF        %error OR
007000051018     C                             %subst(Rel_Sys_OS:1:2) = 'V4'
007100051018     C                   SETON                                        50
007200051018     C                   ELSE
007300051018     C                   SETOFF                                       50
007400051018     C                   ENDIF
007500051017     C*
007600051017     C* Elimino subito lo user-space d wrk
007700020909     C                   EXSR      DLTOUT
007800051017     C*
007900051017     C* Verifico i parametri d input
008000051019     C                   IF        §LST_USER = *BLANKS
008100051019     C***                MOVEL     USNAM         §LST_USER                      * utente corrente
008200051019     C                   EVAL      §LST_USER = '*ALL'                           * tutti gli utenti
008300040202     C                   ENDIF
008400051018     C*
008500051019     C                   IF        §LST_OQNAM = *BLANKS
008600051019     C                   EVAL      §LST_OQNAM = '*ALL'
008700051019     C                   EVAL      §LST_OQLIB = *BLANKS
008800040202     C                   ELSE
008900051019     C                   IF        §LST_OQNAM <> '*ALL' AND
009000051019     C                             §LST_OQLIB  = *BLANKS
009100051019     C                   EVAL      §LST_OQLIB  = '*LIBL'
009200040202     C                   ENDIF
009300040202     C                   ENDIF
009400051018     C*
009500051019     C                   IF        §LST_FTYP = *BLANKS
009600051019     C                   EVAL      §LST_FTYP = '*ALL'
009700051017     C                   ENDIF
009800051018     C*
009900051019     C                   IF        §LST_UDTA = *BLANKS
010000051019     C                   EVAL      §LST_UDTA  = '*ALL'
010100051017     C                   ENDIF
010200051018     C*
010300051019     C                   IF        §LST_TIPSP = *BLANKS
010400051019     C                   EVAL      §LST_TIPSP = '*ALL'
010500051018     C                   ENDIF
010600051024     C*
010700051024     C                   IF        §LST_DATDA = *BLANKS
010800051024     C                   EVAL      §LST_DATDA = '*ALL'
010900051024     C                   ENDIF
011000051024     C*
011100051024     C                   IF        §LST_DATDA = '*ALL'
011200051024     C                   EVAL      §LST_ORADA =  *BLANKS
011300051024     C                   EVAL      §LST_DATA  =  *BLANKS
011400051024     C                   EVAL      §LST_ORAA  =  *BLANKS
011500051024     C                   ENDIF
011600051024     C*
011700051024     C                   IF        §LST_DATDA <> *BLANKS AND
011800051024     C                             §LST_DATA  <> '*ALL'  AND
011900051024     C                             §LST_DATA   =  *BLANKS
012000051024     C                   EVAL      §LST_DATA   =  '*LAST'
012100051024     C                   EVAL      §LST_ORAA   =  *BLANKS
012200051024     C                   ENDIF
012300051019     C*
012400051024     C                   IF        §LST_QJOBN <> *BLANKS
012500051019     C                   EVAL      §LST_USER  = *BLANKS
012600051025     C                   EVAL      wLST_OQLIB = §LST_OQLIB
012700051025     C                   EVAL      wLST_OQNAM = §LST_OQNAM
012800051025     C                   EVAL      wLST_FTYP  = §LST_FTYP
012900051025     C                   EVAL      wLST_UDTA  = §LST_UDTA
013000051025     C                   EVAL      §LST_OQLIB = *BLANKS
013100051025     C                   EVAL      §LST_OQNAM = *BLANKS
013200051025     C                   EVAL      §LST_FTYP  = *BLANKS
013300051025     C                   EVAL      §LST_UDTA  = *BLANKS
013400051024     C                   EVAL      wQUSLSPL_AUXS = -1
013500051019     C                   ENDIF
013600051017     C*
013700051019     C* Creo lo user-space d wrk in QYEMP con il nome 'XLSTSPL'
013800051019     C                   MOVEL(P)  'XLSTSPL'     USRSPACENAM
013900051017     C                   MOVEL(P)  'QTEMP'       USRSPACELIB
014000051017     C                   Z-ADD     00100         USRSPACELEN
014100051017     C                   MOVEL     '*ALL'        USRSPACEAUT
014200051019     C                   MOVEL     'XLSTSPL'     USRSPACETXT
014300020909     C                   CALL      'QUSCRTUS'
014400051017     C                   PARM                    USRSPACE
014500051017     C                   PARM                    USRSPACEFMT
014600051017     C                   PARM                    USRSPACELEN
014700051017     C                   PARM                    USRSPACEINZ
014800051017     C                   PARM                    USRSPACEAUT
014900051017     C                   PARM                    USRSPACETXT
015000051017     C*
015100051017     C* Reperisce la lista spool secondo selezioni richieste e la rivcersa nello user-space d wrk appena creato
015200051017     C                   CLEAR                   QUSEC
015300051017     C                   EVAL      QUSBPRV = %size(QUSEC)
015400051019     C                   EVAL      in_OUTQNAMLIB = §LST_OQNAM + §LST_OQLIB
015500051024     C                   MOVEL     'SPLF0100'    wQUSLSPL_FMTN
015600051024     C                   MOVEL     '*CURRENT'    wQUSLSPL_SYSN
015700051018     C*
015800051018     C* Se la versione d O.S. del sistema corrente è la V4* effettuo la chiamata all'API con 1 minor numero d parametri
015900051018     C                   IF        *IN50 = *ON
016000040202     C                   CALL(e)   'QUSLSPL'
016100051017     C                   PARM                    USRSPACE
016200051024     C                   PARM                    wQUSLSPL_FMTN     8
016300051019     C                   PARM                    §LST_USER
016400051017     C                   PARM                    in_OUTQNAMLIB    20
016500051019     C                   PARM                    §LST_FTYP
016600051019     C                   PARM                    §LST_UDTA
016700051024     C                   PARM                    QUSEC
016800051024     C                   PARM                    §LST_QJOBN
016900051024     C                   PARM                    wQUSLSPL_KEYF
017000051024     C                   PARM                    wQUSLSPL_NUMF
017100051018     C                   ELSE
017200051018     C                   CALL(e)   'QUSLSPL'
017300051019     C                   PARM                    USRSPACE
017400051024     C                   PARM                    wQUSLSPL_FMTN     8
017500051019     C                   PARM                    §LST_USER
017600051019     C                   PARM                    in_OUTQNAMLIB    20
017700051019     C                   PARM                    §LST_FTYP
017800051019     C                   PARM                    §LST_UDTA
017900051019     C                   PARM                    QUSEC
018000051024     C                   PARM                    §LST_QJOBN
018100051019     C                   PARM                    wQUSLSPL_KEYF
018200051019     C                   PARM                    wQUSLSPL_NUMF
018300051024     C                   PARM                    wQUSLSPL_AUXS
018400051024     C                   PARM                    wQUSLSPL_SYSN     8
018500051019     C                   PARM                    §LST_DATDA
018600051019     C                   PARM                    §LST_ORADA
018700051019     C                   PARM                    §LST_DATA
018800051019     C                   PARM                    §LST_ORAA
018900051018     C                   ENDIF
019000051017     C*
019100051018     C* Gestisco l'errore e quindi esco ritornando esito negativo della richiesta
019200051017     C                   IF        %error OR QUSEI <> *blanks
019300051019     C                   EVAL      §LST_ESITO = 'E'
019400051018     C                   ELSE
019500051018     C*
019600051018     C* Lettura user-space con lista spool reperiti
019700051024     C                   CLEAR                   QUSEC
019800051024     C                   EVAL      QUSBPRV = %size(QUSEC)
019900051018     C                   Z-ADD     16            LENDTA
020000051018     C                   Z-ADD     125           STRPOS
020100051024     C                   CALL(e)   'QUSRTVUS'
020200051017     C                   PARM                    USRSPACE
020300020909     C                   PARM                    STRPOS
020400020909     C                   PARM                    LENDTA
020500020909     C                   PARM                    RCVPRI
020600051024     C                   PARM                    QUSEC
020700051024     C*
020800051024     C* Gestisco l'errore e quindi esco ritornando esito negativo della richiesta
020900051024     C                   IF        %error OR QUSEI <> *blanks
021000051024     C                   EVAL      §LST_ESITO = 'E'
021100051024     C                   ELSE
021200051024     C*
021300020909     C                   Z-ADD     PRISET        STRPOS
021400020909     C                   ADD       1             STRPOS
021500020909     C                   Z-ADD     PRISIZ        LENDTA
021600020909     C                   Z-ADD     1             COUNT            15 0
021700020910     C*
021800020910     C                   EXSR      CICLO
021900051019     C                   ENDIF
022000051024     C                   ENDIF
022100051017     C*
022200051019     C* Al termine elimino lo user-space d wrk
022300020909     C                   EXSR      DLTOUT
022400051017     C*
022500051017     C* Ed esco
022600910702     C                   SETON                                          LR
022700051017     C*---------------------------------------------------------------*
022800051017     C*  LETTURA USER-SPACE
022900051017     C*---------------------------------------------------------------*
023000051017     C     CICLO         BEGSR
023100051017     C*
023200020910     C     COUNT         DOWLE     PRINUM
023300051019     C*
023400051019     C* Lettura user-space con lista spool reperiti
023500020910     C                   CALL      'QUSRTVUS'
023600051017     C                   PARM                    USRSPACE
023700020910     C                   PARM                    STRPOS
023800020910     C                   PARM                    LENDTA
023900051019     C                   PARM                    XLSTSPLDS1
024000051019     C*
024100051019     C* Inizializzo indicatore x considerazione si/no spool corrente e ds d procedura
024200051019     C                   SETOFF                                       51
024300051021     C                   CLEAR                   XSPLATRDS
024400051019     C                   CLEAR                   XSPLATRDS1
024500051019     C*
024600051019     C* Se richiesti determinati in ingresso parametri d selezione occorre reperire gli attributi dello spool
024700051019     C                   IF        §LST_TIPSP <> '*ALL' OR
024800051019     C                             §LST_PAGDA  > *ZEROS OR
024900051024     C                             §LST_PAGA   > *ZEROS OR
025000051024     C                             (%subst(Rel_Sys_OS:1:2) = 'V4' AND
025100051024     C                             §LST_DATDA <> *BLANKS          AND
025200051025     C                             §LST_DATDA <> '*ALL')  OR
025300051025     C                             (wLST_FTYP <> *BLANKS AND
025400051025     C                              wLST_FTYP <> '*ALL') OR
025500051025     C                             (wLST_UDTA <> *BLANKS AND
025600051025     C                              wLST_UDTA <> '*ALL')
025700051024     C*
025800051024     C* Reperisco gli attributi dello spool corrente
025900051021     C                   EVAL      §SPLA_JID  = §LST1_JID
026000051021     C                   EVAL      §SPLA_SID  = §LST1_SID
026100051021     C                   CALL(e)   'XSPLATR'
026200051021     C                   PARM                    XSPLATRDS
026300051021     C*
026400051021     C* Se tutto ok
026500051021     C                   IF        %error OR §SPLA_ERR = 'E'
026600051025     C                   SETON                                        51
026700051021     C                   ELSE
026800051021     C                   EVAL      XSPLATRDS1 = §SPLA_ATTR
026900051017     C*
027000051019     C* Verifico determinati parametri d selezione ricevuti in ingresso
027100051019     C                   IF        §LST_PAGDA > *ZEROS AND
027200051019     C                             §SPLA1_TPG < §LST_PAGDA
027300051019     C                   SETON                                        51
027400051019     C                   ENDIF
027500051021     C*
027600051019     C                   IF        §LST_PAGA > *ZEROS AND
027700051019     C                             §SPLA1_TPG > §LST_PAGA
027800051019     C                   SETON                                        51
027900051019     C                   ENDIF
028000051021     C*
028100051024     C                   IF        §LST_TIPSP <> '*ALL' AND
028200051019     C                             %trim(§SPLA1_PRT) <> §LST_TIPSP
028300051019     C                   SETON                                        51
028400051019     C                   ENDIF
028500051024     C*
028600051024     C                   IF        §LST_DATDA <> *BLANKS AND
028700051024     C                             §LST_DATDA <> '*ALL'
028800051024     C                   IF        §SPLA1_FOP >= §LST_DATDA AND
028900051024     C                             §SPLA1_TIM >= §LST_ORADA
029000051024     C                   ELSE
029100051024     C                   SETON                                        51
029200051024     C                   ENDIF
029300051024     C                   ENDIF
029400051024     C*
029500051024     C                   IF        §LST_DATA  <> *BLANKS AND
029600051024     C                             §LST_DATA  <> '*LAST'
029700051024     C                   IF        §SPLA1_FOP <= §LST_DATA AND
029800051024     C                             §SPLA1_TIM <= §LST_ORAA
029900051024     C                   ELSE
030000051024     C                   SETON                                        51
030100051024     C                   ENDIF
030200051024     C                   ENDIF
030300051025     C*
030400051025     C                   IF        wLST_OQLIB <> *BLANKS AND
030500051025     C                             wLST_OQLIB <> '*ALL'
030600051025     C                   IF        wLST_OQLIB <>  §SPLA1_QLB
030700051025     C                   SETON                                        51
030800051025     C                   ENDIF
030900051025     C                   ENDIF
031000051025     C*
031100051025     C                   IF        wLST_OQNAM <> *BLANKS AND
031200051025     C                             wLST_OQNAM <> '*ALL'
031300051025     C                   IF        wLST_OQNAM <>  §SPLA1_QNM
031400051025     C                   SETON                                        51
031500051025     C                   ENDIF
031600051025     C                   ENDIF
031700051025     C*
031800051025     C                   IF        wLST_FTYP <> *BLANKS AND
031900051025     C                             wLST_FTYP <> '*ALL'
032000051025     C                   IF        wLST_FTYP <>  §SPLA1_TYP
032100051025     C                   SETON                                        51
032200051025     C                   ENDIF
032300051025     C                   ENDIF
032400051025     C*
032500051025     C                   IF        wLST_UDTA <> *BLANKS AND
032600051025     C                             wLST_UDTA <> '*ALL'
032700051025     C                   IF        wLST_UDTA <>  §SPLA1_DTA
032800051025     C                   SETON                                        51
032900051025     C                   ENDIF
033000051025     C                   ENDIF
033100051024     C*
033200051021     C                   ENDIF
033300051021     C*
033400051019     C                   ELSE
033500051019     C                   EVAL      §SPLA1_JOB = §LST1_JID
033600051019     C                   EVAL      §SPLA1_SPL = §LST1_SID
033700051019     C                   ENDIF
033800051019     C*
033900051019     C* Se spool corrente è da considerare lo passo nei parametri d out
034000051019     C                   IF        *IN51 = *OFF
034100051019     C                   ADD       1             jSKSPOOL
034200051019     C                   EVAL      §LST2_JOB = §SPLA1_JOB
034300051019     C                   EVAL      §LST2_SPL = §SPLA1_SPL
034400051019     C                   EVAL      SKSPOOL(jSKSPOOL) = XLSTSPLDS2
034500051019     C                   ENDIF
034600051021     C*
034700051021     C* Proseguo con la lettura dello user-space
034800051024     C                   Z-ADD     724           SPALEN
034900051024     C                   Z-ADD     0             SPASNR
035000051021     C                   ADD       1             COUNT
035100051021     C                   ADD       PRISIZ        STRPOS
035200051019     C*
035300020910     C                   ENDDO
035400051019     C*
035500051019     C* Quindi restituisco in out i primi 1000 spool repriti corrispondenti alle selezioni richieste
035600051019     C                   MOVEA     SKSPOOL       §LST_LISTA
035700051017     C*
035800020910     C                   ENDSR
035900051017     C*---------------------------------------------------------------*
036000051017     C*  ELIMINA LO SPAZIO UTENTE DALLA QTEMP
036100051017     C*---------------------------------------------------------------*
036200051017     C     DLTOUT        BEGSR
036300051017     C*
036400051019     C                   CALL(e)   'QCMDEXC'
036500051017     C                   PARM                    CMD
036600051017     C                   PARM      80            LEN              15 5
036700051017     C*
036800051017     C                   ENDSR
036900051017     C*---------------------------------------------------------------*
037000020910**
037100051019DLTUSRSPC  USRSPC(QTEMP/XLSTSPL)

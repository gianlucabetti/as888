000100110304     H DECEDIT('0,') DATEDIT(*DMY.)
000200170314     H DFTACTGRP(*NO) BNDDIR('UBBNDDIR') ACTGRP(*CALLER)
000300110304     H*--------------------
000400001218     FTABEL00F  IF   E           K DISK
000500100219     FWADLY05L  IF   E           K DISK
000600110304     FWFCLN11L  UF A E           K DISK    PREFIX(F11_)
000700110307     F                                     RENAME(WFCLN100:WFCLN110)
000800110304     FWFCLN12L  UF A E           K DISK    PREFIX(F12_)
000900110307     F                                     RENAME(WFCLN100:WFCLN120)
001000150210     FWFCLN13L  UF A E           K DISK    PREFIX(F13_)
001100150210     F                                     RENAME(WFCLN100:WFCLN130)
001200170314     FTIVGD00F  O    E             DISK    COMMIT
001300001218     D*--------------------
001400001218     D* DS ESTERNE
001500001218     D*--------------------
001600110307     D WFCLN_10      E DS                  EXTNAME(WFCLN10F)
001700110307     D WFCLN_11      E DS                  EXTNAME(WFCLN10F) PREFIX(F11_)
001800110307     D WFCLN_12      E DS                  EXTNAME(WFCLN10F) PREFIX(F12_)
001900150210     D WFCLN_13      E DS                  EXTNAME(WFCLN10F) PREFIX(F13_)
002000161205     D psds           sds
002100161205     D  procname         *PROC
002200170314     D TIS7VASDS     E DS
002300110307     D TITAS00F      E DS
002400900517     D KPJBA         E DS
002500100219     D DTASFLO       E DS
002600100219     D DSPR          E DS
002700100219     D DS14          E DS
002800120214     D DS3K          E DS
002900110304     D*--------------------
003000110304     D* SCHIERA PER REPERIMENTO CLIENTI "FIGLI" DI UN UNIFICANTE
003100110304     D*--------------------
003200110304     D KUNI            S              7  0 DIM(1000) DESCEND
003300130403     D*-------------------
003400130403     D* DS REPERIMENTO PADRE/FIGLI
003500130403     D*-------------------
003600130403     D TIBS10DS      E DS
003700130403     D skc                           11  0 DIM(500) overlay(D10SKC)
003800110304     D*--------------------
003900110304     D* DS DI RIDEFINIZIOINE PARAMETRI IN INPUT
004000110304     D*--------------------
004100110304     D DSINPUT         DS
004200110304     D  DATADA                        8  0
004300110304     D  DATAA                         8  0
004400110304     D  CODCLI                        7  0
004500110304     D  FLGUNI                        1
004600110304     D  FLGCONS                       1
004700110304     D  CODCLIVAS                     7  0
004800110304     D  TIPFILE                       2
004900110304     D  FLGEXE                        1
005000110304     D  TIPCLI                        1
005100110304     D  DACMDA                        8  0
005200110304     D  DACMA                         8  0
005300110304     D  TIPBOL                        2
005400110307     D  NUMSER                        2
005500110304     D  TIPDET                        1
005600130411     D  TIPAFFID                      1
005700110304     D*--------------------
005800110304
005900010605     D*--------------------
006000010605     D* VARIABILI DI WRK
006100010605     D*--------------------
006200100219     D wANT            s              7  0 inz
006300100222     D wM12            s              7  0 inz
006400100219     D wOKS            s              7  0 inz
006500150210     D wR12            s              7  0 inz
006600150210     D wR24            s              7  0 inz
006700100219     D wRIT            s              7  0 inz
006800110304
006900110304
007000110304     D*------------------
007100110304     D* VARIABILI D WRK
007200110304     D*------------------
007300110304     D  wAAS           S                   like(tasAAS) inz
007400110304     D  wLNP           S                   like(tasLNP) inz
007500110304     D  wNRS           S                   like(tasNRS) inz
007600110304     D  wNSP           S                   like(tasNSP) inz
007700110304
007800110304
007900110304     D*------------------
008000110304     D* LINKING A DEFINIZIONI ESTERNE
008100110304     D*------------------
008200110304     D/COPY GAITRASRC/SRCPROTOPR,UBLBLSPE
008300110304     D/COPY GAITRASRC/SRCPROTOPI,UBLBLSPE
008400010605
008500010605
008600920812     C*---------------------------------------------------------------*
008700001218     C* MAIN
008800001218     C*---------------------------------------------------------------*
008900110304     C*
009000110304     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
009100110304     C
009200110304     C/EXEC SQL
009300161207     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
009400110304     C/END-EXEC
009500110304     C
009600161206     C*
009700161206     C* Avvio il monitoring del processo
009800161206     C                   monitor
009900110304     C*
010000120214     C                   exsr      cartbl
010100110304     C                   exsr      chkpar
010200110304     C*
010300110304     C* Ciclo sulla schiera dei clienti figli dell'unificante preso in input
010400110304     C                   z-add     1             I                 4 0
010500110304     C                   sorta     KUNI
010600110304     C                   dow       KUNI(I) > *zeros
010700001218     C                   exsr      procedi
010800110307     C                   add       1             I
010900110304     C                   enddo
011000161206     C*
011100161206     C* Scarico i dati in DOWNLOAD
011200110307     C                   exsr      WRITIVGD
011300170314     C*
011400170314     C* Sancisco il commit
011500170314     C                   commit(e)
011600170314     C*
011700170314     C* Su errore
011800170314     C                   on-error
011900170314     C                   exsr      exeerr
012000170314     C*
012100170314     C* Fine monitoring
012200170314     C                   endmon
012300170314     C*
012400170314     C                   seton                                        LR
012500110304
012600110304
012700110304
012800110304     C*------------------------------------------------------------------------*
012900110304     C* CHKPAR - Routine di controllo parametri ricevuti in input
013000110304     C*------------------------------------------------------------------------*
013100110304     C     CHKPAR        BEGSR
013200110304     C*
013300110304     C* Verifico le date
013400110304     C                   if        DATADA > *zeros AND
013500110304     C                             DATAA  = *zeros
013600110304     C                   movel     *all'9'       DATAA
013700110304     C                   endif
013800110304     C                   if        DACMDA > *zeros AND
013900110304     C                             DACMA  = *zeros
014000110304     C                   movel     *all'9'       DACMA
014100110304     C                   endif
014200110307     C*
014300110307     C* Verifico il livello d dettaglio richiesto
014400150210     C                   setoff                                       262728
014500110307     C                   select
014600110307     C                   when      TIPDET = 'C'                                 * x CAP
014700110307     C                   seton                                        26
014800110307     C                   when      TIPDET = 'S'                                 * x SPED
014900110307     C                   seton                                        27
015000150210     C                   when      TIPDET = 'P'                                 * x PROV
015100150210     C                   seton                                        28
015200110307     C                   endsl
015300110304     C*
015400110304     C                   ENDSR
015500110304     C*------------------------------------------------------------------------*
015600110304
015700110304
015800110304
015900110304     C*------------------------------------------------------------------------*
016000110304     C* PROCEDI - Routine principale
016100110304     C*------------------------------------------------------------------------*
016200110304     C     PROCEDI       BEGSR
016300110304     C*
016400110304     C* Se richiesta elaborazione x codice cliente mittente...
016500110304     C                   select
016600110304     C                   when      TIPCLI = *blanks
016700110304     C                   exsr      EXECCM
016800110304     C*
016900110304     C* Se richiesta elaborazione x codice cliente fatturazione...
017000110304     C                   when      TIPCLI = 'K'
017100111130     C                   exsr      EXEKSC
017200110304     C*
017300110304     C* Se richiesta elaborazione x entrambi: codice cliente mittente + fatturazione...
017400110304     C                   when      TIPCLI = 'E'
017500110304     C                   exsr      EXECCM
017600111130     C                   exsr      EXEKSC
017700110304     C*
017800110304     C                   endsl
017900110304     C*
018000110304     C                   ENDSR
018100110304     C*------------------------------------------------------------------------*
018200110304
018300110304
018400110304
018500110304     C*------------------------------------------------------------------------*
018600110304     C* EXECCM   - Elaborazione x codice cliente mittente
018700110304     C*------------------------------------------------------------------------*
018800110304     C     EXECCM        BEGSR
018900110304     C*
019000110304     C                   movel     'C'           wChkCli           1
019100110304     C*
019200110304     C                   select
019300110304     C* ...se richiesta elaborazione x data spedizione
019400110304     C                   when      DATADA > *zeros
019500110304     C                   exsr      SQ_CCMDSP
019600110304     C* ...se richiesta elaborazione x data consegna merce
019700110304     C                   when      DACMDA > *zeros
019800110304     C                   exsr      SQ_CCMDCM
019900110304     C                   endsl
020000110304     C*
020100110304     C                   ENDSR
020200110304     C*------------------------------------------------------------------------*
020300111130
020400111130
020500111130
020600111130     C*------------------------------------------------------------------------*
020700111130     C* EXEKSC   - Elaborazione x codice cliente fatturazione
020800111130     C*------------------------------------------------------------------------*
020900111130     C     EXEKSC        BEGSR
021000111130     C*
021100111130     C                   movel     'K'           wChkCli           1
021200111130     C*
021300111130     C                   select
021400111130     C* ...se richiesta elaborazione x data spedizione
021500111130     C                   when      DATADA > *zeros
021600111130     C                   exsr      SQ_KSCDSP
021700111130     C* ...se richiesta elaborazione x data consegna merce
021800111130     C                   when      DACMDA > *zeros
021900111130     C                   exsr      SQ_KSCDCM
022000111130     C                   endsl
022100111130     C*
022200111130     C                   ENDSR
022300111130     C*------------------------------------------------------------------------*
022400110304
022500110304
022600110304
022700110304     C*------------------------------------------------------------------------*
022800110304     C* SQ_CCMDSP - Elaborazione x codice cliente mittente + data spedizione
022900110304     C*------------------------------------------------------------------------*
023000110304     C     SQ_CCMDSP     BEGSR
023100110304     C*
023200110304     C                   z-add     KUNI(I)       wParCCM           7 0
023300110304     C*
023400110304     C/EXEC SQL
023500110304     C+ declare C_CCMDSP cursor for
023600110304     C+ select * from titas00f
023700110304     C+ where tasccm = :wParCCM and
023800110304     C+ tasaas*10000+tasmgs between :DATADA and :DATAA
023900110304     C+ union
024000110304     C+ select * from titas10f
024100110304     C+ where tasccm = :wParCCM and
024200110304     C+ tasaas*10000+tasmgs between :DATADA and :DATAA
024300110304     C+ union
024400110304     C+ select * from titasP0f
024500110304     C+ where tasccm = :wParCCM and
024600110304     C+ tasaas*10000+tasmgs between :DATADA and :DATAA
024700110304     C+ for read only
024800110304     C/END-EXEC
024900110304     C
025000110304     C/EXEC SQL
025100110304     C+ open C_CCMDSP
025200110304     C/END-EXEC
025300110304     C
025400110304     C/EXEC SQL
025500110304     C+ Fetch C_CCMDSP into :TITAS00F
025600110304     C/END-EXEC
025700110304     C*
025800110304     C                   dow       sqlcod = *zeros
025900110304     C                   exsr      verifica
026000110304     C                   if        CHKREC = 'S'
026100110304     C                   exsr      scriviWF
026200110304     C                   endif
026300110304     C
026400110304     C/EXEC SQL
026500110304     C+ Fetch C_CCMDSP into :TITAS00F
026600110304     C/END-EXEC
026700110304     C*
026800110304     C                   enddo
026900110304     C*
027000110304     C/EXEC SQL
027100110304     C+ close C_CCMDSP
027200110304     C/END-EXEC
027300110304     C*
027400110304     C*
027500110304     C                   ENDSR
027600110304     C*------------------------------------------------------------------------*
027700111130
027800111130
027900111130
028000111130     C*------------------------------------------------------------------------*
028100111130     C* SQ_KSCDSP - Elaborazione x codice cliente fatturazione + data spedizione
028200111130     C*------------------------------------------------------------------------*
028300111130     C     SQ_KSCDSP     BEGSR
028400111130     C*
028500111130     C                   z-add     KUNI(I)       wParKSC           7 0
028600111130     C*
028700111130     C/EXEC SQL
028800111130     C+ declare C_KSCDSP cursor for
028900111130     C+ select * from titas00f
029000111130     C+ where tasksc = :wParKSC and
029100111130     C+ tasaas*10000+tasmgs between :DATADA and :DATAA
029200111130     C+ union
029300111130     C+ select * from titas10f
029400111130     C+ where tasksc = :wParKSC and
029500111130     C+ tasaas*10000+tasmgs between :DATADA and :DATAA
029600111130     C+ union
029700111130     C+ select * from titasP0f
029800111130     C+ where tasksc = :wParKSC and
029900111130     C+ tasaas*10000+tasmgs between :DATADA and :DATAA
030000111130     C+ for read only
030100111130     C/END-EXEC
030200111130     C
030300111130     C/EXEC SQL
030400111130     C+ open C_KSCDSP
030500111130     C/END-EXEC
030600111130     C
030700111130     C/EXEC SQL
030800111130     C+ Fetch C_KSCDSP into :TITAS00F
030900111130     C/END-EXEC
031000111130     C*
031100111130     C                   dow       sqlcod = *zeros
031200111130     C                   exsr      verifica
031300111130     C                   if        CHKREC = 'S'
031400111130     C                   exsr      scriviWF
031500111130     C                   endif
031600111130     C
031700111130     C/EXEC SQL
031800111130     C+ Fetch C_KSCDSP into :TITAS00F
031900111130     C/END-EXEC
032000111130     C*
032100111130     C                   enddo
032200111130     C*
032300111130     C/EXEC SQL
032400111130     C+ close C_KSCDSP
032500111130     C/END-EXEC
032600111130     C*
032700111130     C*
032800111130     C                   ENDSR
032900111130     C*------------------------------------------------------------------------*
033000110304
033100110304
033200110304
033300110304     C*------------------------------------------------------------------------*
033400110304     C* SQ_CCMDCM - Elaborazione x codice cliente mittente + data consegna merce
033500110304     C*------------------------------------------------------------------------*
033600110304     C     SQ_CCMDCM     BEGSR
033700110304     C*
033800110304     C                   z-add     KUNI(I)       wParCCM           7 0
033900110304     C*
034000110304     C/EXEC SQL
034100110304     C+ declare C_CCMDCM cursor for
034200110304     C+ select * from titas00f
034300110304     C+ where tasccm = :wParCCM and
034400110304     C+ tasdcm between :DACMDA and :DACMA
034500110304     C+ union
034600110304     C+ select * from titas10f
034700110304     C+ where tasccm = :wParCCM and
034800110304     C+ tasdcm between :DACMDA and :DACMA
034900110304     C+ union
035000110304     C+ select * from titasP0f
035100110304     C+ where tasccm = :wParCCM and
035200110304     C+ tasdcm between :DACMDA and :DACMA
035300110304     C+ for read only
035400110304     C/END-EXEC
035500110304     C
035600110304     C/EXEC SQL
035700110304     C+ open C_CCMDCM
035800110304     C/END-EXEC
035900110304     C
036000110304     C/EXEC SQL
036100110304     C+ Fetch C_CCMDCM into :TITAS00F
036200110304     C/END-EXEC
036300110304     C*
036400110304     C                   dow       sqlcod = *zeros
036500110304     C                   exsr      verifica
036600110304     C                   if        CHKREC = 'S'
036700110304     C                   exsr      scriviWF
036800110304     C                   endif
036900110304     C
037000110304     C/EXEC SQL
037100110304     C+ Fetch C_CCMDCM into :TITAS00F
037200110304     C/END-EXEC
037300110304     C*
037400110304     C                   enddo
037500110304     C*
037600110304     C/EXEC SQL
037700110304     C+ close C_CCMDCM
037800110304     C/END-EXEC
037900110304     C*
038000110304     C*
038100110304     C                   ENDSR
038200110304     C*------------------------------------------------------------------------*
038300111130
038400111130
038500111130
038600111130     C*------------------------------------------------------------------------*
038700111130     C* SQ_KSCDCM - Elaborazione x codice cliente fatturazione + data consegna merce
038800111130     C*------------------------------------------------------------------------*
038900111130     C     SQ_KSCDCM     BEGSR
039000111130     C*
039100111130     C                   z-add     KUNI(I)       wParKSC           7 0
039200111130     C*
039300111130     C/EXEC SQL
039400111130     C+ declare C_KSCDCM cursor for
039500111130     C+ select * from titas00f
039600111130     C+ where tasksc = :wParKSC and
039700111130     C+ tasdcm between :DACMDA and :DACMA
039800111130     C+ union
039900111130     C+ select * from titas10f
040000111130     C+ where tasksc = :wParKSC and
040100111130     C+ tasdcm between :DACMDA and :DACMA
040200111130     C+ union
040300111130     C+ select * from titasP0f
040400111130     C+ where tasksc = :wParKSC and
040500111130     C+ tasdcm between :DACMDA and :DACMA
040600111130     C+ for read only
040700111130     C/END-EXEC
040800111130     C
040900111130     C/EXEC SQL
041000111130     C+ open C_KSCDCM
041100111130     C/END-EXEC
041200111130     C
041300111130     C/EXEC SQL
041400111130     C+ Fetch C_KSCDCM into :TITAS00F
041500111130     C/END-EXEC
041600111130     C*
041700111130     C                   dow       sqlcod = *zeros
041800111130     C                   exsr      verifica
041900111130     C                   if        CHKREC = 'S'
042000111130     C                   exsr      scriviWF
042100111130     C                   endif
042200111130     C
042300111130     C/EXEC SQL
042400111130     C+ Fetch C_KSCDCM into :TITAS00F
042500111130     C/END-EXEC
042600111130     C*
042700111130     C                   enddo
042800111130     C*
042900111130     C/EXEC SQL
043000111130     C+ close C_KSCDCM
043100111130     C/END-EXEC
043200111130     C*
043300111130     C*
043400111130     C                   ENDSR
043500111130     C*------------------------------------------------------------------------*
043600001218
043700110304
043800110304
043900110304     C*------------------------------------------------------------------------*
044000110304     C* CHKCONS  - Verifica se bolla effettivamente consegnata
044100110304     C*------------------------------------------------------------------------*
044200110304     C     CHKCONS       BEGSR
044300110304     C*
044400110304     C                   setoff                                       4041
044500110304     C                   movel     *blanks       wEsito1           1
044600110304     C                   movel     *blanks       wEsito2           1
044700110304     C*
044800110304     C                   eval      wAAS = tasAAS
044900110304     C                   eval      wLNP = tasLNP
045000110304     C                   eval      wNRS = tasNRS
045100110304     C                   eval      wNSP = tasNSP
045200110304     C*
045300110304     C* Chiamata metodo GetLblTyp
045400110304     C                   eval      wEsito1 = %char(UBLBLSPE_GetLblTyp(
045500110304     C                                                wAAS
045600110304     C                                               :wLNP
045700110304     C                                               :wNRS
045800110304     C                                               :wNSP
045900110304     C                                               :pOutLblTyp
046000110304     C                                               :pOutAnnoBO
046100110304     C                                               :pOutLineaParBO
046200110304     C                                               :pOutSerieBO
046300110304     C                                               :pOutNumSpedBO
046400110304     C                                               :pOutDcmBO
046500110304     C                                               :pOutCcaBO
046600110304     C                                               :pOutRblBO))
046700110304     C* Se trattasi d bolla nn originale reperisco la relativa bolla originale
046800110304     C                   if        pOutLblTyp <> 'O'
046900110304     C                   eval      wAAS = pOutAnnoBO
047000110304     C                   eval      wLNP = pOutLineaParBO
047100110304     C                   eval      wNRS = pOutSerieBO
047200110304     C                   eval      wNSP = pOutNumSpedBO
047300110304     C                   endif
047400110304     C*
047500110304     C* Chiamata metodo GetLastChild
047600110304     C                   eval      wEsito2 = %char(UBLBLSPE_GetLastChild(
047700110304     C                                                wAAS
047800110304     C                                               :wLNP
047900110304     C                                               :wNRS
048000110304     C                                               :wNSP
048100110304     C                                               :pOutAnnoFI
048200110304     C                                               :pOutLineaParFI
048300110304     C                                               :pOutSerieFI
048400110304     C                                               :pOutNumSpedFI
048500110304     C                                               :pOutDcmFI
048600110304     C                                               :pOutCcaFI))
048700110304     C*
048800110304     C* Considerazioni sullo stato corrente della bolla
048900110304     C                   if        (wEsito2 = '9' and pOutDcmBO > *zeros)  or
049000110304     C                             (wEsito2 = '0' and pOutDcmFI > *zeros)
049100161205     C                   seton                                        40        * bolla chiusa
049200110304     C                   endif
049300110304     C*
049400110304     C                   if        (wEsito2 = '9' and pOutDcmBO > *zeros
049500110304     C                                            and pOutCcaBO = *blanks) or
049600110304     C                             (wEsito2 = '0' and pOutDcmFI > *zeros
049700110304     C                                            and pOutCcaFI = *blanks)
049800110304     C                   seton                                        41        * bolla cons. OK
049900110304     C                   endif
050000110304     C*
050100110304     C                   ENDSR
050200110304     C*------------------------------------------------------------------------*
050300110304
050400110304
050500110304
050600110304     C*------------------------------------------------------------------------*
050700110304     C* VERIFICA - Routine di verifica validità record corrente
050800110304     C*------------------------------------------------------------------------*
050900110304     C     VERIFICA      BEGSR
051000110304     C*
051100110304     C                   movel     'S'           CHKREC            1
051200110307     C*
051300110307     C                   movel     tasaas        wrkdata8          8 0
051400110307     C                   move      tasmgs        wrkdata8
051500110307     C                   movel     wrkdata8      wrkAAAAMM         6 0
051600110304     C*
051700110304     C* Verifico le date spedizione
051800110304     C                   if        CHKREC = 'S'
051900110304     C                   if        DATADA > *zeros
052000110304     C                   movel     tasaas        wrkdata           8 0
052100110304     C                   move      tasmgs        wrkdata
052200110304     C                   if        wrkdata < DATADA or
052300110304     C                             wrkdata > DATAA
052400110304     C                   movel     'N'           CHKREC
052500110304     C                   endif
052600110304     C                   endif
052700110304     C                   endif
052800110304     C*
052900110304     C* Verifico le date spedizione
053000110304     C                   if        CHKREC = 'S'
053100110304     C                   if        DACMDA > *zeros
053200110304     C                   if        tasdcm  < DACMDA or
053300110304     C                             tasdcm  > DACMA
053400110304     C                   movel     'N'           CHKREC
053500110304     C                   endif
053600110304     C                   endif
053700110304     C                   endif
053800110304     C*
053900110304     C* Verifico il codice cliente mittente
054000110304     C                   if        CHKREC = 'S'
054100110304     C                   if        wChkCli  = 'C'      AND
054200110304     C                             tasccm  <> KUNI(I)
054300110304     C                   movel     'N'           CHKREC
054400110304     C                   endif
054500110304     C                   endif
054600110304     C*
054700110304     C* Verifico il codice cliente fatturazione (in caso d 'K')
054800110304     C                   if        CHKREC = 'S'
054900110304     C                   if        wChkCli  = 'K'      AND
055000110304     C                             tasksc  <> KUNI(I)
055100110304     C                   movel     'N'           CHKREC
055200110304     C                   endif
055300110304     C                   endif
055400110304     C*
055500110304     C* Verifico il codice cliente fatturazione (in caso d 'E')
055600110304     C                   if        CHKREC = 'S'
055700110304     C                   if        TIPCLI  = 'E'       AND
055800110304     C                             wChkCli = 'K'       AND
055900110304     C                             tasksc  = tasccm
056000110304     C                   movel     'N'           CHKREC
056100110304     C                   endif
056200110304     C                   endif
056300110304     C*
056400110304     C* Verifico la serie
056500110304     C                   if        CHKREC = 'S'
056600110307     C                   if        NUMSER <> *blanks
056700110307     C                   if        tasnrs <> %dec(NUMSER : 2 : 0)
056800110304     C                   movel     'N'           CHKREC
056900110304     C                   endif
057000110304     C                   endif
057100110304     C                   endif
057200110304     C*
057300110304     C* Verifico il tipo bolla
057400110304     C                   if        CHKREC = 'S'
057500140326     C                   select
057600140326     C                   when      TIPBOL = '99'
057700140326     C                   when      TIPBOL = '98' AND
057800140326     C                             tastbl <> 'F1' AND tastbl <> 'A2'
057900140326     C                   movel     'N'           CHKREC
058000140326     C                   when      TIPBOL <> '99' AND
058100140326     C                             TIPBOL <> '98' AND
058200140326     C                             tastbl <> TIPBOL
058300140326     C                   movel     'N'           CHKREC
058400140326     C                   endsl
058500110304     C                   endif
058600110304     C*
058700110304     C* Verifico se richiesto nel lancio che la bolla sia consegnata
058800110304     C                   if        CHKREC = 'S'
058900110304     C                   if        FLGCONS = 'S'
059000110304     C                   exsr      CHKCONS
059100110304     C                   if        *in40 = *on
059200110304     C                   else
059300110304     C                   movel     'N'           CHKREC
059400110304     C                   endif
059500110304     C                   endif
059600110304     C                   endif
059700110304     C*
059800110304     C* Verifico se richiesto nel lancio che la bolla sia consegnata OK
059900110304     C                   if        CHKREC = 'S'
060000110304     C                   if        FLGCONS = 'O'
060100110304     C                   exsr      CHKCONS
060200110304     C                   if        *in41 = *on
060300110304     C                   else
060400110304     C                   movel     'N'           CHKREC
060500110304     C                   endif
060600110304     C                   endif
060700110304     C                   endif
060800110304     C*
060900110304     C* Verifico se richiesto nel lancio che la bolla NON sia consegnata OK
061000110304     C                   if        CHKREC = 'S'
061100110304     C                   if        FLGCONS = 'K'
061200110304     C                   exsr      CHKCONS
061300110304     C                   if        *in41 = *on
061400110304     C                   movel     'N'           CHKREC
061500110304     C                   endif
061600110304     C                   endif
061700110304     C                   endif
061800110304     C*
061900110304     C* Verifico se richiesto nel lancio che la bolla NON sia consegnata
062000110304     C                   if        CHKREC = 'S'
062100110304     C                   if        FLGCONS = 'P'
062200110304     C                   exsr      CHKCONS
062300110304     C                   if        *in40 = *on
062400110304     C                   movel     'N'           CHKREC
062500110304     C                   endif
062600110304     C                   endif
062700110304     C                   endif
062800110304     C*
062900110304     C                   ENDSR
063000110304     C*------------------------------------------------------------------------*
063100001218
063200001218
063300001218
063400001218     C*------------------------------------------------------------------------*
063500110307     C* SCRIVIWF - Routine di scrittura WFCLN10F
063600001218     C*------------------------------------------------------------------------*
063700100219     C     SCRIVIWF      BEGSR
063800100219     C*
063900110307     C                   clear                   WFCLN110
064000110307     C                   clear                   WFCLN120
064100150210     C                   clear                   WFCLN130
064200110307     C                   clear                   WFCLN_10
064300110307     C                   clear                   WFCLN_11
064400110307     C                   clear                   WFCLN_12
064500150210     C                   clear                   WFCLN_13
064600100219     C*
064700100219     C                   eval      dlyLNP = tasLNP
064800100219     C                   eval      dlyTSP = tasTSP
064900120309     C*
065000120309     C                   if        dlyTSP = 'D'
065100120309     C                   eval      dlyTSP = 'C'
065200120309     C                   endif
065300120309     C*
065400100219     C                   eval      dlyNAR = tasNZD
065500100219     C                   eval      dlyCAP = tasCAD
065600100219     C     KEYdly05_P    chain     wadly05l
065700100219     C                   if        %found(wadly05l)
065800100219     C                   eval      WCLNDLYTRC = DLYTRC
065900100219     C                   endif
066000100219     C*
066100100219     C                   eval      tblcod = 'PR'
066200100219     C                   eval      tblkey = tasprd
066300100219     C     KEYtab_C      chain     tabel00f
066400100219     C                   if        %found(tabel00f)
066500100219     C                   eval      DSPR = tbluni
066600100219     C                   eval      tblcod = '14'
066700100219     C                   eval      tblkey = §PRCRE
066800100219     C     KEYtab_C      chain     tabel00f
066900100219     C                   if        %found(tabel00f)
067000100219     C                   eval      DS14 = tbluni
067100110307     C                   eval      wclnREG = §14DES
067200100219     C                   endif
067300100219     C                   endif
067400100219     C*
067500100219     C                   z-add     *zeros        wANT
067600100222     C                   z-add     *zeros        wM12
067700100219     C                   z-add     *zeros        wOKS
067800150210     C                   z-add     *zeros        wR12
067900150210     C                   z-add     *zeros        wR24
068000100219     C                   z-add     *zeros        wRIT
068100130411     C*
068200130411     C*
068300130411     C* Verifico tipo affidabilità richiesta
068400130411     C                   select
068500130411     C*
068600130411     C* - affidabilità consegna
068700130411     C                   when      TIPAFFID = 'C'
068800001218     C*
068900100219     C* ...in anticipo
069000100219     C                   select
069100130411     C                   when      tasnci >= -999 AND
069200130411     C                             tasnci <= -24
069300100219     C                   add       1             wANT
069400100222     C*
069500100222     C* ...border-line
069600130411     C                   when      tasnci >  -24  AND
069700130411     C                             tasnci <= -12
069800100222     C                   add       1             wM12
069900100219     C*
070000100219     C* ...in orario
070100130411     C                   when      tasnci >  -12  AND
070200130411     C                             tasnci <=   0
070300100219     C                   add       1             wOKS
070400150210     C*
070500150210     C* ...in ritardo entro 12 ore
070600150210     C                   when      tasnci >    0  AND
070700150210     C                             tasnci <=  12
070800150210     C                   add       1             wR12
070900150210     C*
071000150210     C* ...in ritardo entro 24 ore
071100150210     C                   when      tasnci >   12  AND
071200150210     C                             tasnci <=  24
071300150210     C                   add       1             wR24
071400150210     C*
071500150210     C* ...in ritardo "molto"
071600150210     C                   when      tasnci >   24
071700150210     C                   add       1             wRIT
071800150210     C                   endsl
071900130411     C*
072000130411     C* - affidabilità cliente (*DEFAULT)
072100130411     C                   other
072200130411     C*
072300130411     C* ...in anticipo
072400130411     C                   select
072500150210     C                   when      tasnrc >= -999 AND
072600150210     C                             tasnrc <= -24
072700130411     C                   add       1             wANT
072800130411     C*
072900130411     C* ...border-line
073000150210     C                   when      tasnrc >  -24  AND
073100150210     C                             tasnrc <= -12
073200130411     C                   add       1             wM12
073300130411     C*
073400130411     C* ...in orario
073500150210     C                   when      tasnrc >  -12  AND
073600150210     C                             tasnrc <=   0
073700130411     C                   add       1             wOKS
073800150210     C*
073900150210     C* ...in ritardo entro 12 ore
074000150210     C                   when      tasnrc >    0  AND
074100150210     C                             tasnrc <=  12
074200150210     C                   add       1             wR12
074300150210     C*
074400150210     C* ...in ritardo entro 24 ore
074500150210     C                   when      tasnrc >   12  AND
074600150210     C                             tasnrc <=  24
074700150210     C                   add       1             wR24
074800150210     C*
074900150210     C* ...in ritardo "molto"
075000150210     C                   when      tasnrc >   24
075100150210     C                   add       1             wRIT
075200150210     C                   endsl
075300130411     C*
075400130411     C                   endsl
075500100219     C*
075600100219     C* Conteggio spedizioni DIR/GIAC/CCA
075700100219     C                   eval      dtasflo = tasflo
075800100219     C                   if        tasCCA   <> *blanks OR
075900100219     C                             tasFGC   <> *blanks OR
076000100219     C                             §FLONAV  <> *blanks
076100100219     C                   eval      wclnANOM = 'S'
076200100219     C                   endif
076300100219     C*
076400111130     C                   eval      wclnCCM    = tasccm
076500111130     C                   if        wChkCli = 'K'
076600111130     C                   eval      wclnCCM    = tasksc
076700111130     C                   endif
076800130403     C*
076900130403     C* Se richiesta estrazione per "unificante" (di qualunque tipo come CCM considero sempre
077000130403     C* l'unificante)
077100130403     C
077200130403     C                   if        FLGUNI <> *blanks
077300130403     C                   eval      wclnCCM    = CODCLI
077400130403     C                   endif
077500111130     C*
077600100219     C                   eval      wclnAAAAMM = wrkAAAAMM
077700100219     C                   eval      wclnPRD    = tasprd
077800100219     C                   eval      wclnCAD    = tascad
077900100219     C                   eval      wclnISO    = tasfin
078000110307     C                   eval      wclnAAS    = tasaas
078100110307     C                   eval      wclnLNP    = taslnp
078200110307     C                   eval      wclnNRS    = tasnrs
078300110307     C                   eval      wclnNSP    = tasnsp
078400110307     C*
078500110307     C                   select
078600110307     C                   when      *in26
078700110307     C     KEYcln_12C    chain     wfcln12l
078800110307     C                   if        %found(wfcln12l)
078900110307     C                   eval      WFCLN_10 = WFCLN_12
079000100219     C                   add       wANT          wclnANT
079100100222     C                   add       wM12          wclnM12
079200100219     C                   add       wOKS          wclnOKS
079300150210     C                   add       wR12          wclnR12
079400150210     C                   add       wR24          wclnR24
079500100219     C                   add       wRIT          wclnRIT
079600100219     C                   add       1             wclnTOT
079700110307     C                   eval      WFCLN_12 = WFCLN_10
079800110307     C                   update    WFCLN120
079900100219     C                   else
080000100219     C                   add       wANT          wclnANT
080100100222     C                   add       wM12          wclnM12
080200100219     C                   add       wOKS          wclnOKS
080300150210     C                   add       wR12          wclnR12
080400150210     C                   add       wR24          wclnR24
080500100219     C                   add       wRIT          wclnRIT
080600100219     C                   add       1             wclnTOT
080700110307     C                   eval      WFCLN_12 = WFCLN_10
080800110307     C                   write     WFCLN120
080900100219     C                   endif
081000110307     C*
081100110307     C                   when      *in27
081200150211     C     KEYcln_11C    chain     wfcln11l
081300150211     C                   if        %found(wfcln11l)
081400150211     C                   eval      WFCLN_10 = WFCLN_11
081500150211     C                   add       wANT          wclnANT
081600150211     C                   add       wM12          wclnM12
081700150211     C                   add       wOKS          wclnOKS
081800150211     C                   add       wR12          wclnR12
081900150211     C                   add       wR24          wclnR24
082000150211     C                   add       wRIT          wclnRIT
082100150211     C                   add       1             wclnTOT
082200150211     C                   eval      WFCLN_11 = WFCLN_10
082300150211     C                   update    WFCLN110
082400150211     C                   else
082500150211     C                   add       wANT          wclnANT
082600150211     C                   add       wM12          wclnM12
082700150211     C                   add       wOKS          wclnOKS
082800150211     C                   add       wR12          wclnR12
082900150211     C                   add       wR24          wclnR24
083000150211     C                   add       wRIT          wclnRIT
083100150211     C                   add       1             wclnTOT
083200150211     C                   eval      WFCLN_11 = WFCLN_10
083300150211     C                   write     WFCLN110
083400150211     C                   endif
083500150210     C*
083600150210     C                   when      *in28
083700150211     C     KEYcln_13C    chain     wfcln13l
083800150211     C                   if        %found(wfcln13l)
083900150211     C                   eval      WFCLN_10 = WFCLN_13
084000150211     C                   add       wANT          wclnANT
084100150211     C                   add       wM12          wclnM12
084200150211     C                   add       wOKS          wclnOKS
084300150211     C                   add       wR12          wclnR12
084400150211     C                   add       wR24          wclnR24
084500150211     C                   add       wRIT          wclnRIT
084600150211     C                   add       1             wclnTOT
084700150211     C                   eval      WFCLN_13 = WFCLN_10
084800150211     C                   update    WFCLN130
084900150211     C                   else
085000150211     C                   add       wANT          wclnANT
085100150211     C                   add       wM12          wclnM12
085200150211     C                   add       wOKS          wclnOKS
085300150211     C                   add       wR12          wclnR12
085400150211     C                   add       wR24          wclnR24
085500150211     C                   add       wRIT          wclnRIT
085600150211     C                   add       1             wclnTOT
085700150211     C                   eval      WFCLN_13 = WFCLN_10
085800150211     C                   write     WFCLN130
085900150211     C                   endif
086000110307     C*
086100110307     C                   endsl
086200001218     C*
086300001218     C                   ENDSR
086400001221     C*------------------------------------------------------------------------*
086500110304
086600110304
086700110304
086800110304     C*------------------------------------------------------------------------*
086900110304     C* WRITIVGD - Routine di scrittura file TIVGD (file VAS generico download)
087000110304     C*------------------------------------------------------------------------*
087100110304     C     WRITIVGD      BEGSR
087200170314     C*
087300170314     C* Inizializzo variabili d wrk
087400170314     C                   movel     'N'           wProcedi          1
087500170314     C*
087600170314     C* Stacco progressivo univoco download
087700170314     C                   CLEAR                   TIS7VASDS
087800170314     C                   EVAL      i§7VASOPZ = 'PRG'
087900170314     C                   CALL(e)   'TIS7VASR1'
088000170314     C                   PARM                    TIS7VASDS
088100170314     C*
088200170314     C* Se OK => proseguo
088300170314     C                   if        not %error AND
088400170314     C                             o§7VASOK = *on AND o§7VASPRG <> *blanks
088500170314     C                   movel     'S'           wProcedi
088600170314     C                   endif
088700170314     C*
088800170314     C* Se ok a procedere => elaboro
088900170314     C                   if        wProcedi = 'S'
089000110304     C*
089100110307     C* Leggo tutto il file WFCLN11L in QTEMP del job corrente
089200110307     C     *loval        setll     WFCLN11L
089300110307     C                   read      WFCLN11L
089400110307     C                   dow       not %eof(WFCLN11L)
089500110307     C*
089600161206     C                   clear                   tivgd000
089700110307     C                   eval      vgdDTA = %subst(WFCLN_11:1:%size(WFCLN_11))
089800110304     C                   eval      vgdTIP = TIPFILE
089900170314     C                   eval      vgdKSU = '0'+%editc(CODCLIVAS:'X')
090000110304     C                   eval      vgdTSC = 'WW'
090100170314     C                   eval      vgdDAT = datcor
090200170314     C                   eval      vgdPRG = o§7VASPRG
090300170314     C                   eval      vgdPGM = procname
090400170314     C                   eval      vgdSTO = '?'
090500161206     C*
090600161206     C                   write     tivgd000
090700110304     C*
090800110307     C                   read      WFCLN11L
090900110307     C                   enddo
091000170314     C*
091100170314     C* Finalizzo la transazione
091200170314     C                   EVAL      i§7VASOPZ  = 'RLS'
091300170314     C                   EVAL      i§7VASTIP  = TIPFILE
091400170314     C                   EVAL      i§7VASKSU  = '0'+%editc(CODCLIVAS:'X')
091500170314     C                   EVAL      i§7VASTSC  = 'WW'
091600170314     C                   EVAL      i§7VASSTO  = '?'
091700170314     C                   EVAL      i§7VASSTTO = 'L'
091800170314     C                   EVAL      i§7VASPRG  = o§7VASPRG
091900170314     C                   CALL(e)   'TIS7VASR1'
092000170314     C                   PARM                    TIS7VASDS
092100170314     C*
092200170314     C* Verifico esito chiamata
092300170314     C                   if        %error OR o§7VASOK = *off
092400170314     C                   exsr      EXEERR
092500170314     C                   endif
092600170314     C*
092700170314     C                   endif
092800110307     C*
092900110304     C                   ENDSR
093000110304     C*------------------------------------------------------------------------*
093100120214
093200120214
093300120214
093400120214     C*------------------------------------------------------------------------*
093500120214     C* CARTBL - Routine di caricamento dati tabellati
093600120214     C*------------------------------------------------------------------------*
093700120214     C     CARTBL        BEGSR
093800120214     C*
093900120214     C                   Z-ADD     1             I                 4 0
094000120214     C                   MOVEL     CODCLI        KUNI(I)
094100130403     C*
094200130403     C                   SELECT
094300130403     C                   WHEN      FLGUNI = 'U'
094400120214     C                   MOVEL     '3K'          tblCOD
094500120214     C     KEYtab_P      CHAIN     TABEL                              31
094600120214     C     *IN31         DOWEQ     '0'
094700120214     C     TBLFLG        IFEQ      ' '
094800120214     C                   MOVEL     TBLUNI        DS3K
094900120214     C     §3KCKS        IFEQ      CODCLI
095000120214     C                   ADD       1             I
095100120214     C                   MOVEL     TBLKEY        wKUNI             7 0
095200120214     C     wKUNI         LOOKUP    KUNI                                   55
095300120214     C                   IF        %equal
095400120214     C                   ELSE
095500120214     C                   Z-ADD     wKUNI         KUNI(I)
095600120214     C                   ENDIF
095700120214     C                   ENDIF
095800120214     C                   ENDIF
095900120214     C     KEYtab_P      READE     TABEL                                  31
096000120214     C                   ENDDO
096100130403     C*
096200130403     C                   WHEN      FLGUNI = 'T'
096300130403     C                   CLEAR                   TIBS10DS
096400130403     C                   EVAL      D10DRF = datcor                              *data riferimento
096500130403     C                   EVAL      D10TLE = 'ST'                                *tipo legame
096600130403     C                   EVAL      D10PAF = 'F'                                 *cerca se padre
096700130403     C                   EVAL      D10COD = CODCLI
096800130403     C                   CALL      'TIBS10R'
096900130403     C                   PARM                    TIBS10DS
097000130403     C                   IF        D10ERR = *BLANKS                             *Padre
097100130403     C                   Z-ADD     1             I
097200130403     C                   DOW       skc(I) > *zeros
097300130403     C                   Z-ADD     skc(I)        KUNI(I)
097400130403     C                   ADD       1             I
097500130403     C                   ENDDO
097600130403     C                   ENDIF
097700130403     C*
097800130403     C                   ENDSL
097900120214     C*
098000120214     C                   ENDSR
098100120214     C*---------------------------------------------------------------*
098200170314
098300170314
098400170314
098500170314     C*------------------------------------------------------------------------*
098600170314     C* EXEERR - Routine di esecuzione azioni su Errore
098700170314     C*------------------------------------------------------------------------*
098800170314     C     EXEERR        BEGSR
098900170314     C*
099000170314     C                   dump(A)
099100170322     C                   rolbk(e)
099200170314     C                   seton                                        lr
099300170314     C                   return
099400170314     C*
099500170314     C                   ENDSR
099600170314     C*------------------------------------------------------------------------*
099700120214
099800001218
099900001218
100000001218     C*------------------------------------------------------------------------*
100100001218     C* *INZSR - ROUTINE INIZIALE
100200001218     C*------------------------------------------------------------------------*
100300001218     C     *INZSR        BEGSR
100400001218     C*
100500001218     C     *ENTRY        PLIST
100600100219     C                   PARM                    KPJBA
100700001218     C*
100800100219     C                   MOVEL     KPJBU         DSINPUT
100900100219     C                   EVAL      tblKUT = 1
101000110307     C*
101100110307     C* Determino la data corrente
101200110307     C                   Z-ADD     *zeros        datcor            8 0
101300110307     C                   EVAL      datcor = %dec(%date() : *ISO)
101400001218     C*
101500001218     C* Definizioni chiavi
101600100219     C*
101700100219     C     KEYdly05_P    KLIST
101800100219     C                   KFLD                    dlyLNP
101900100219     C                   KFLD                    dlyTSP
102000100219     C                   KFLD                    dlyNAR
102100100219     C                   KFLD                    dlyCAP
102200120214     C*
102300120214     C     KEYtab_P      KLIST
102400120214     C                   KFLD                    tblKUT
102500120214     C                   KFLD                    tblCOD
102600001218     C*
102700100219     C     KEYtab_C      KLIST
102800100219     C                   KFLD                    tblKUT
102900100219     C                   KFLD                    tblCOD
103000100219     C                   KFLD                    tblKEY
103100110307     C*
103200110307     C     KEYcln_11C    KLIST
103300110307     C                   KFLD                    wclnCCM
103400110307     C                   KFLD                    wclnAAAAMM
103500110307     C                   KFLD                    wclnAAS
103600110307     C                   KFLD                    wclnLNP
103700110307     C                   KFLD                    wclnNRS
103800110307     C                   KFLD                    wclnNSP
103900030704     C*
104000110307     C     KEYcln_12C    KLIST
104100100219     C                   KFLD                    wclnCCM
104200100219     C                   KFLD                    wclnAAAAMM
104300110307     C                   KFLD                    wclnREG
104400100219     C                   KFLD                    wclnPRD
104500100219     C                   KFLD                    wclnCAD
104600100219     C                   KFLD                    wclnISO
104700100219     C                   KFLD                    wclnANOM
104800150210     C*
104900150210     C     KEYcln_13C    KLIST
105000150210     C                   KFLD                    wclnCCM
105100150210     C                   KFLD                    wclnAAAAMM
105200150210     C                   KFLD                    wclnREG
105300150210     C                   KFLD                    wclnPRD
105400100219     C*
105500001218     C                   ENDSR

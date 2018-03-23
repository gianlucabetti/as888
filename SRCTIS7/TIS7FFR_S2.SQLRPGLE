000100110304     H DECEDIT('0,') DATEDIT(*DMY.)
000200110304     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP('BARTVAS')
000300110304     H*--------------------
000400001218     FTABEL00F  IF   E           K DISK
000500100219     FWADLY05L  IF   E           K DISK
000600110304     FWFCLN11L  UF A E           K DISK    PREFIX(F11_)
000700110307     F                                     RENAME(WFCLN100:WFCLN110)
000800110304     FWFCLN12L  UF A E           K DISK    PREFIX(F12_)
000900110307     F                                     RENAME(WFCLN100:WFCLN120)
001000150210     FWFCLN13L  UF A E           K DISK    PREFIX(F13_)
001100150210     F                                     RENAME(WFCLN100:WFCLN130)
001200161206     FTIVGD00F  O    E             DISK    COMMIT
001300001218     D*--------------------
001400001218     D* DS ESTERNE
001500001218     D*--------------------
001600110307     D WFCLN_10      E DS                  EXTNAME(WFCLN10F)
001700110307     D WFCLN_11      E DS                  EXTNAME(WFCLN10F) PREFIX(F11_)
001800110307     D WFCLN_12      E DS                  EXTNAME(WFCLN10F) PREFIX(F12_)
001900150210     D WFCLN_13      E DS                  EXTNAME(WFCLN10F) PREFIX(F13_)
002000161205     D psds           sds
002100161205     D  procname         *PROC
002200110307     D TITAS00F      E DS
002300900517     D KPJBA         E DS
002400100219     D DTASFLO       E DS
002500100219     D DSPR          E DS
002600100219     D DS14          E DS
002700120214     D DS3K          E DS
002800110304     D*--------------------
002900110304     D* SCHIERA PER REPERIMENTO CLIENTI "FIGLI" DI UN UNIFICANTE
003000110304     D*--------------------
003100110304     D KUNI            S              7  0 DIM(1000) DESCEND
003200130403     D*-------------------
003300130403     D* DS REPERIMENTO PADRE/FIGLI
003400130403     D*-------------------
003500130403     D TIBS10DS      E DS
003600130403     D skc                           11  0 DIM(500) overlay(D10SKC)
003700110304     D*--------------------
003800110304     D* DS DI RIDEFINIZIOINE PARAMETRI IN INPUT
003900110304     D*--------------------
004000110304     D DSINPUT         DS
004100110304     D  DATADA                        8  0
004200110304     D  DATAA                         8  0
004300110304     D  CODCLI                        7  0
004400110304     D  FLGUNI                        1
004500110304     D  FLGCONS                       1
004600110304     D  CODCLIVAS                     7  0
004700110304     D  TIPFILE                       2
004800110304     D  FLGEXE                        1
004900110304     D  TIPCLI                        1
005000110304     D  DACMDA                        8  0
005100110304     D  DACMA                         8  0
005200110304     D  TIPBOL                        2
005300110307     D  NUMSER                        2
005400110304     D  TIPDET                        1
005500130411     D  TIPAFFID                      1
005600110304     D*--------------------
005700110304
005800010605     D*--------------------
005900010605     D* VARIABILI DI WRK
006000010605     D*--------------------
006100100219     D wANT            s              7  0 inz
006200100222     D wM12            s              7  0 inz
006300100219     D wOKS            s              7  0 inz
006400150210     D wR12            s              7  0 inz
006500150210     D wR24            s              7  0 inz
006600100219     D wRIT            s              7  0 inz
006700110304
006800110304
006900110304     D*------------------
007000110304     D* VARIABILI D WRK
007100110304     D*------------------
007200110304     D  wAAS           S                   like(tasAAS) inz
007300110304     D  wLNP           S                   like(tasLNP) inz
007400110304     D  wNRS           S                   like(tasNRS) inz
007500110304     D  wNSP           S                   like(tasNSP) inz
007600110304
007700110304
007800110304     D*------------------
007900110304     D* LINKING A DEFINIZIONI ESTERNE
008000110304     D*------------------
008100110304     D/COPY GAITRASRC/SRCPROTOPR,UBLBLSPE
008200110304     D/COPY GAITRASRC/SRCPROTOPI,UBLBLSPE
008300010605
008400010605
008500920812     C*---------------------------------------------------------------*
008600001218     C* MAIN
008700001218     C*---------------------------------------------------------------*
008800110304     C*
008900110304     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
009000110304     C
009100110304     C/EXEC SQL
009200161206     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD, COMMIT = *CHG
009300110304     C/END-EXEC
009400110304     C
009500161206     C*
009600161206     C* Avvio il monitoring del processo
009700161206     C                   monitor
009800110304     C*
009900120214     C                   exsr      cartbl
010000110304     C                   exsr      chkpar
010100110304     C*
010200120214     C**** Fisso KUNI(1)
010300120214     C***                   eval      KUNI(1) = CODCLI
010400110304     C*
010500110304     C* Ciclo sulla schiera dei clienti figli dell'unificante preso in input
010600110304     C                   z-add     1             I                 4 0
010700110304     C                   sorta     KUNI
010800110304     C                   dow       KUNI(I) > *zeros
010900001218     C                   exsr      procedi
011000110307     C                   add       1             I
011100110304     C                   enddo
011200161206     C*
011300161206     C* Scarico i dati in DOWNLOAD
011400110307     C                   exsr      WRITIVGD
011500161206     C*
011600161206     C* Sabcisco il commit
011700161206     C                   commit
011800161206     C*
011900161206     C* Su errore
012000161206     C                   on-error
012100161206     C                   exsr      exeerr
012200161206     C*
012300161206     C* Fine monitoring
012400161206     C                   endmon
012500001218     C*
012600001218     C                   seton                                        LR
012700110304
012800110304
012900110304
013000110304     C*------------------------------------------------------------------------*
013100110304     C* CHKPAR - Routine di controllo parametri ricevuti in input
013200110304     C*------------------------------------------------------------------------*
013300110304     C     CHKPAR        BEGSR
013400110304     C*
013500110304     C* Verifico le date
013600110304     C                   if        DATADA > *zeros AND
013700110304     C                             DATAA  = *zeros
013800110304     C                   movel     *all'9'       DATAA
013900110304     C                   endif
014000110304     C                   if        DACMDA > *zeros AND
014100110304     C                             DACMA  = *zeros
014200110304     C                   movel     *all'9'       DACMA
014300110304     C                   endif
014400110307     C*
014500110307     C* Verifico il livello d dettaglio richiesto
014600150210     C                   setoff                                       262728
014700110307     C                   select
014800110307     C                   when      TIPDET = 'C'                                 * x CAP
014900110307     C                   seton                                        26
015000110307     C                   when      TIPDET = 'S'                                 * x SPED
015100110307     C                   seton                                        27
015200150210     C                   when      TIPDET = 'P'                                 * x PROV
015300150210     C                   seton                                        28
015400110307     C                   endsl
015500110304     C*
015600110304     C                   ENDSR
015700110304     C*------------------------------------------------------------------------*
015800110304
015900110304
016000110304
016100110304     C*------------------------------------------------------------------------*
016200110304     C* PROCEDI - Routine principale
016300110304     C*------------------------------------------------------------------------*
016400110304     C     PROCEDI       BEGSR
016500110304     C*
016600110304     C* Se richiesta elaborazione x codice cliente mittente...
016700110304     C                   select
016800110304     C                   when      TIPCLI = *blanks
016900110304     C                   exsr      EXECCM
017000110304     C*
017100110304     C* Se richiesta elaborazione x codice cliente fatturazione...
017200110304     C                   when      TIPCLI = 'K'
017300111130     C                   exsr      EXEKSC
017400110304     C*
017500110304     C* Se richiesta elaborazione x entrambi: codice cliente mittente + fatturazione...
017600110304     C                   when      TIPCLI = 'E'
017700110304     C                   exsr      EXECCM
017800111130     C                   exsr      EXEKSC
017900110304     C*
018000110304     C                   endsl
018100110304     C*
018200110304     C                   ENDSR
018300110304     C*------------------------------------------------------------------------*
018400110304
018500110304
018600110304
018700110304     C*------------------------------------------------------------------------*
018800110304     C* EXECCM   - Elaborazione x codice cliente mittente
018900110304     C*------------------------------------------------------------------------*
019000110304     C     EXECCM        BEGSR
019100110304     C*
019200110304     C                   movel     'C'           wChkCli           1
019300110304     C*
019400110304     C                   select
019500110304     C* ...se richiesta elaborazione x data spedizione
019600110304     C                   when      DATADA > *zeros
019700110304     C                   exsr      SQ_CCMDSP
019800110304     C* ...se richiesta elaborazione x data consegna merce
019900110304     C                   when      DACMDA > *zeros
020000110304     C                   exsr      SQ_CCMDCM
020100110304     C                   endsl
020200110304     C*
020300110304     C                   ENDSR
020400110304     C*------------------------------------------------------------------------*
020500111130
020600111130
020700111130
020800111130     C*------------------------------------------------------------------------*
020900111130     C* EXEKSC   - Elaborazione x codice cliente fatturazione
021000111130     C*------------------------------------------------------------------------*
021100111130     C     EXEKSC        BEGSR
021200111130     C*
021300111130     C                   movel     'K'           wChkCli           1
021400111130     C*
021500111130     C                   select
021600111130     C* ...se richiesta elaborazione x data spedizione
021700111130     C                   when      DATADA > *zeros
021800111130     C                   exsr      SQ_KSCDSP
021900111130     C* ...se richiesta elaborazione x data consegna merce
022000111130     C                   when      DACMDA > *zeros
022100111130     C                   exsr      SQ_KSCDCM
022200111130     C                   endsl
022300111130     C*
022400111130     C                   ENDSR
022500111130     C*------------------------------------------------------------------------*
022600110304
022700110304
022800110304
022900110304     C*------------------------------------------------------------------------*
023000110304     C* SQ_CCMDSP - Elaborazione x codice cliente mittente + data spedizione
023100110304     C*------------------------------------------------------------------------*
023200110304     C     SQ_CCMDSP     BEGSR
023300110304     C*
023400110304     C                   z-add     KUNI(I)       wParCCM           7 0
023500110304     C*
023600110304     C/EXEC SQL
023700110304     C+ declare C_CCMDSP cursor for
023800110304     C+ select * from titas00f
023900110304     C+ where tasccm = :wParCCM and
024000110304     C+ tasaas*10000+tasmgs between :DATADA and :DATAA
024100110304     C+ union
024200110304     C+ select * from titas10f
024300110304     C+ where tasccm = :wParCCM and
024400110304     C+ tasaas*10000+tasmgs between :DATADA and :DATAA
024500110304     C+ union
024600110304     C+ select * from titasP0f
024700110304     C+ where tasccm = :wParCCM and
024800110304     C+ tasaas*10000+tasmgs between :DATADA and :DATAA
024900110304     C+ for read only
025000110304     C/END-EXEC
025100110304     C
025200110304     C/EXEC SQL
025300110304     C+ open C_CCMDSP
025400110304     C/END-EXEC
025500110304     C
025600110304     C/EXEC SQL
025700110304     C+ Fetch C_CCMDSP into :TITAS00F
025800110304     C/END-EXEC
025900110304     C*
026000110304     C                   dow       sqlcod = *zeros
026100110304     C                   exsr      verifica
026200110304     C                   if        CHKREC = 'S'
026300110304     C                   exsr      scriviWF
026400110304     C                   endif
026500110304     C
026600110304     C/EXEC SQL
026700110304     C+ Fetch C_CCMDSP into :TITAS00F
026800110304     C/END-EXEC
026900110304     C*
027000110304     C                   enddo
027100110304     C*
027200110304     C/EXEC SQL
027300110304     C+ close C_CCMDSP
027400110304     C/END-EXEC
027500110304     C*
027600110304     C*
027700110304     C                   ENDSR
027800110304     C*------------------------------------------------------------------------*
027900111130
028000111130
028100111130
028200111130     C*------------------------------------------------------------------------*
028300111130     C* SQ_KSCDSP - Elaborazione x codice cliente fatturazione + data spedizione
028400111130     C*------------------------------------------------------------------------*
028500111130     C     SQ_KSCDSP     BEGSR
028600111130     C*
028700111130     C                   z-add     KUNI(I)       wParKSC           7 0
028800111130     C*
028900111130     C/EXEC SQL
029000111130     C+ declare C_KSCDSP cursor for
029100111130     C+ select * from titas00f
029200111130     C+ where tasksc = :wParKSC and
029300111130     C+ tasaas*10000+tasmgs between :DATADA and :DATAA
029400111130     C+ union
029500111130     C+ select * from titas10f
029600111130     C+ where tasksc = :wParKSC and
029700111130     C+ tasaas*10000+tasmgs between :DATADA and :DATAA
029800111130     C+ union
029900111130     C+ select * from titasP0f
030000111130     C+ where tasksc = :wParKSC and
030100111130     C+ tasaas*10000+tasmgs between :DATADA and :DATAA
030200111130     C+ for read only
030300111130     C/END-EXEC
030400111130     C
030500111130     C/EXEC SQL
030600111130     C+ open C_KSCDSP
030700111130     C/END-EXEC
030800111130     C
030900111130     C/EXEC SQL
031000111130     C+ Fetch C_KSCDSP into :TITAS00F
031100111130     C/END-EXEC
031200111130     C*
031300111130     C                   dow       sqlcod = *zeros
031400111130     C                   exsr      verifica
031500111130     C                   if        CHKREC = 'S'
031600111130     C                   exsr      scriviWF
031700111130     C                   endif
031800111130     C
031900111130     C/EXEC SQL
032000111130     C+ Fetch C_KSCDSP into :TITAS00F
032100111130     C/END-EXEC
032200111130     C*
032300111130     C                   enddo
032400111130     C*
032500111130     C/EXEC SQL
032600111130     C+ close C_KSCDSP
032700111130     C/END-EXEC
032800111130     C*
032900111130     C*
033000111130     C                   ENDSR
033100111130     C*------------------------------------------------------------------------*
033200110304
033300110304
033400110304
033500110304     C*------------------------------------------------------------------------*
033600110304     C* SQ_CCMDCM - Elaborazione x codice cliente mittente + data consegna merce
033700110304     C*------------------------------------------------------------------------*
033800110304     C     SQ_CCMDCM     BEGSR
033900110304     C*
034000110304     C                   z-add     KUNI(I)       wParCCM           7 0
034100110304     C*
034200110304     C/EXEC SQL
034300110304     C+ declare C_CCMDCM cursor for
034400110304     C+ select * from titas00f
034500110304     C+ where tasccm = :wParCCM and
034600110304     C+ tasdcm between :DACMDA and :DACMA
034700110304     C+ union
034800110304     C+ select * from titas10f
034900110304     C+ where tasccm = :wParCCM and
035000110304     C+ tasdcm between :DACMDA and :DACMA
035100110304     C+ union
035200110304     C+ select * from titasP0f
035300110304     C+ where tasccm = :wParCCM and
035400110304     C+ tasdcm between :DACMDA and :DACMA
035500110304     C+ for read only
035600110304     C/END-EXEC
035700110304     C
035800110304     C/EXEC SQL
035900110304     C+ open C_CCMDCM
036000110304     C/END-EXEC
036100110304     C
036200110304     C/EXEC SQL
036300110304     C+ Fetch C_CCMDCM into :TITAS00F
036400110304     C/END-EXEC
036500110304     C*
036600110304     C                   dow       sqlcod = *zeros
036700110304     C                   exsr      verifica
036800110304     C                   if        CHKREC = 'S'
036900110304     C                   exsr      scriviWF
037000110304     C                   endif
037100110304     C
037200110304     C/EXEC SQL
037300110304     C+ Fetch C_CCMDCM into :TITAS00F
037400110304     C/END-EXEC
037500110304     C*
037600110304     C                   enddo
037700110304     C*
037800110304     C/EXEC SQL
037900110304     C+ close C_CCMDCM
038000110304     C/END-EXEC
038100110304     C*
038200110304     C*
038300110304     C                   ENDSR
038400110304     C*------------------------------------------------------------------------*
038500111130
038600111130
038700111130
038800111130     C*------------------------------------------------------------------------*
038900111130     C* SQ_KSCDCM - Elaborazione x codice cliente fatturazione + data consegna merce
039000111130     C*------------------------------------------------------------------------*
039100111130     C     SQ_KSCDCM     BEGSR
039200111130     C*
039300111130     C                   z-add     KUNI(I)       wParKSC           7 0
039400111130     C*
039500111130     C/EXEC SQL
039600111130     C+ declare C_KSCDCM cursor for
039700111130     C+ select * from titas00f
039800111130     C+ where tasksc = :wParKSC and
039900111130     C+ tasdcm between :DACMDA and :DACMA
040000111130     C+ union
040100111130     C+ select * from titas10f
040200111130     C+ where tasksc = :wParKSC and
040300111130     C+ tasdcm between :DACMDA and :DACMA
040400111130     C+ union
040500111130     C+ select * from titasP0f
040600111130     C+ where tasksc = :wParKSC and
040700111130     C+ tasdcm between :DACMDA and :DACMA
040800111130     C+ for read only
040900111130     C/END-EXEC
041000111130     C
041100111130     C/EXEC SQL
041200111130     C+ open C_KSCDCM
041300111130     C/END-EXEC
041400111130     C
041500111130     C/EXEC SQL
041600111130     C+ Fetch C_KSCDCM into :TITAS00F
041700111130     C/END-EXEC
041800111130     C*
041900111130     C                   dow       sqlcod = *zeros
042000111130     C                   exsr      verifica
042100111130     C                   if        CHKREC = 'S'
042200111130     C                   exsr      scriviWF
042300111130     C                   endif
042400111130     C
042500111130     C/EXEC SQL
042600111130     C+ Fetch C_KSCDCM into :TITAS00F
042700111130     C/END-EXEC
042800111130     C*
042900111130     C                   enddo
043000111130     C*
043100111130     C/EXEC SQL
043200111130     C+ close C_KSCDCM
043300111130     C/END-EXEC
043400111130     C*
043500111130     C*
043600111130     C                   ENDSR
043700111130     C*------------------------------------------------------------------------*
043800001218
043900110304
044000110304
044100110304     C*------------------------------------------------------------------------*
044200110304     C* CHKCONS  - Verifica se bolla effettivamente consegnata
044300110304     C*------------------------------------------------------------------------*
044400110304     C     CHKCONS       BEGSR
044500110304     C*
044600110304     C                   setoff                                       4041
044700110304     C                   movel     *blanks       wEsito1           1
044800110304     C                   movel     *blanks       wEsito2           1
044900110304     C*
045000110304     C                   eval      wAAS = tasAAS
045100110304     C                   eval      wLNP = tasLNP
045200110304     C                   eval      wNRS = tasNRS
045300110304     C                   eval      wNSP = tasNSP
045400110304     C*
045500110304     C* Chiamata metodo GetLblTyp
045600110304     C                   eval      wEsito1 = %char(UBLBLSPE_GetLblTyp(
045700110304     C                                                wAAS
045800110304     C                                               :wLNP
045900110304     C                                               :wNRS
046000110304     C                                               :wNSP
046100110304     C                                               :pOutLblTyp
046200110304     C                                               :pOutAnnoBO
046300110304     C                                               :pOutLineaParBO
046400110304     C                                               :pOutSerieBO
046500110304     C                                               :pOutNumSpedBO
046600110304     C                                               :pOutDcmBO
046700110304     C                                               :pOutCcaBO
046800110304     C                                               :pOutRblBO))
046900110304     C* Se trattasi d bolla nn originale reperisco la relativa bolla originale
047000110304     C                   if        pOutLblTyp <> 'O'
047100110304     C                   eval      wAAS = pOutAnnoBO
047200110304     C                   eval      wLNP = pOutLineaParBO
047300110304     C                   eval      wNRS = pOutSerieBO
047400110304     C                   eval      wNSP = pOutNumSpedBO
047500110304     C                   endif
047600110304     C*
047700110304     C* Chiamata metodo GetLastChild
047800110304     C                   eval      wEsito2 = %char(UBLBLSPE_GetLastChild(
047900110304     C                                                wAAS
048000110304     C                                               :wLNP
048100110304     C                                               :wNRS
048200110304     C                                               :wNSP
048300110304     C                                               :pOutAnnoFI
048400110304     C                                               :pOutLineaParFI
048500110304     C                                               :pOutSerieFI
048600110304     C                                               :pOutNumSpedFI
048700110304     C                                               :pOutDcmFI
048800110304     C                                               :pOutCcaFI))
048900110304     C*
049000110304     C* Considerazioni sullo stato corrente della bolla
049100110304     C                   if        (wEsito2 = '9' and pOutDcmBO > *zeros)  or
049200110304     C                             (wEsito2 = '0' and pOutDcmFI > *zeros)
049300161205     C                   seton                                        40        * bolla chiusa
049400110304     C                   endif
049500110304     C*
049600110304     C                   if        (wEsito2 = '9' and pOutDcmBO > *zeros
049700110304     C                                            and pOutCcaBO = *blanks) or
049800110304     C                             (wEsito2 = '0' and pOutDcmFI > *zeros
049900110304     C                                            and pOutCcaFI = *blanks)
050000110304     C                   seton                                        41        * bolla cons. OK
050100110304     C                   endif
050200110304     C*
050300110304     C                   ENDSR
050400110304     C*------------------------------------------------------------------------*
050500110304
050600110304
050700110304
050800110304     C*------------------------------------------------------------------------*
050900110304     C* VERIFICA - Routine di verifica validità record corrente
051000110304     C*------------------------------------------------------------------------*
051100110304     C     VERIFICA      BEGSR
051200110304     C*
051300110304     C                   movel     'S'           CHKREC            1
051400110307     C*
051500110307     C                   movel     tasaas        wrkdata8          8 0
051600110307     C                   move      tasmgs        wrkdata8
051700110307     C                   movel     wrkdata8      wrkAAAAMM         6 0
051800110304     C*
051900110304     C* Verifico le date spedizione
052000110304     C                   if        CHKREC = 'S'
052100110304     C                   if        DATADA > *zeros
052200110304     C                   movel     tasaas        wrkdata           8 0
052300110304     C                   move      tasmgs        wrkdata
052400110304     C                   if        wrkdata < DATADA or
052500110304     C                             wrkdata > DATAA
052600110304     C                   movel     'N'           CHKREC
052700110304     C                   endif
052800110304     C                   endif
052900110304     C                   endif
053000110304     C*
053100110304     C* Verifico le date spedizione
053200110304     C                   if        CHKREC = 'S'
053300110304     C                   if        DACMDA > *zeros
053400110304     C                   if        tasdcm  < DACMDA or
053500110304     C                             tasdcm  > DACMA
053600110304     C                   movel     'N'           CHKREC
053700110304     C                   endif
053800110304     C                   endif
053900110304     C                   endif
054000110304     C*
054100110304     C* Verifico il codice cliente mittente
054200110304     C                   if        CHKREC = 'S'
054300110304     C                   if        wChkCli  = 'C'      AND
054400110304     C                             tasccm  <> KUNI(I)
054500110304     C                   movel     'N'           CHKREC
054600110304     C                   endif
054700110304     C                   endif
054800110304     C*
054900110304     C* Verifico il codice cliente fatturazione (in caso d 'K')
055000110304     C                   if        CHKREC = 'S'
055100110304     C                   if        wChkCli  = 'K'      AND
055200110304     C                             tasksc  <> KUNI(I)
055300110304     C                   movel     'N'           CHKREC
055400110304     C                   endif
055500110304     C                   endif
055600110304     C*
055700110304     C* Verifico il codice cliente fatturazione (in caso d 'E')
055800110304     C                   if        CHKREC = 'S'
055900110304     C                   if        TIPCLI  = 'E'       AND
056000110304     C                             wChkCli = 'K'       AND
056100110304     C                             tasksc  = tasccm
056200110304     C                   movel     'N'           CHKREC
056300110304     C                   endif
056400110304     C                   endif
056500110304     C*
056600110304     C* Verifico la serie
056700110304     C                   if        CHKREC = 'S'
056800110307     C                   if        NUMSER <> *blanks
056900110307     C                   if        tasnrs <> %dec(NUMSER : 2 : 0)
057000110304     C                   movel     'N'           CHKREC
057100110304     C                   endif
057200110304     C                   endif
057300110304     C                   endif
057400110304     C*
057500110304     C* Verifico il tipo bolla
057600110304     C                   if        CHKREC = 'S'
057700140326     C                   select
057800140326     C                   when      TIPBOL = '99'
057900140326     C                   when      TIPBOL = '98' AND
058000140326     C                             tastbl <> 'F1' AND tastbl <> 'A2'
058100140326     C                   movel     'N'           CHKREC
058200140326     C                   when      TIPBOL <> '99' AND
058300140326     C                             TIPBOL <> '98' AND
058400140326     C                             tastbl <> TIPBOL
058500140326     C                   movel     'N'           CHKREC
058600140326     C                   endsl
058700110304     C                   endif
058800110304     C*
058900110304     C* Verifico se richiesto nel lancio che la bolla sia consegnata
059000110304     C                   if        CHKREC = 'S'
059100110304     C                   if        FLGCONS = 'S'
059200110304     C                   exsr      CHKCONS
059300110304     C                   if        *in40 = *on
059400110304     C                   else
059500110304     C                   movel     'N'           CHKREC
059600110304     C                   endif
059700110304     C                   endif
059800110304     C                   endif
059900110304     C*
060000110304     C* Verifico se richiesto nel lancio che la bolla sia consegnata OK
060100110304     C                   if        CHKREC = 'S'
060200110304     C                   if        FLGCONS = 'O'
060300110304     C                   exsr      CHKCONS
060400110304     C                   if        *in41 = *on
060500110304     C                   else
060600110304     C                   movel     'N'           CHKREC
060700110304     C                   endif
060800110304     C                   endif
060900110304     C                   endif
061000110304     C*
061100110304     C* Verifico se richiesto nel lancio che la bolla NON sia consegnata OK
061200110304     C                   if        CHKREC = 'S'
061300110304     C                   if        FLGCONS = 'K'
061400110304     C                   exsr      CHKCONS
061500110304     C                   if        *in41 = *on
061600110304     C                   movel     'N'           CHKREC
061700110304     C                   endif
061800110304     C                   endif
061900110304     C                   endif
062000110304     C*
062100110304     C* Verifico se richiesto nel lancio che la bolla NON sia consegnata
062200110304     C                   if        CHKREC = 'S'
062300110304     C                   if        FLGCONS = 'P'
062400110304     C                   exsr      CHKCONS
062500110304     C                   if        *in40 = *on
062600110304     C                   movel     'N'           CHKREC
062700110304     C                   endif
062800110304     C                   endif
062900110304     C                   endif
063000110304     C*
063100110304     C                   ENDSR
063200110304     C*------------------------------------------------------------------------*
063300001218
063400001218
063500001218
063600001218     C*------------------------------------------------------------------------*
063700110307     C* SCRIVIWF - Routine di scrittura WFCLN10F
063800001218     C*------------------------------------------------------------------------*
063900100219     C     SCRIVIWF      BEGSR
064000100219     C*
064100110307     C                   clear                   WFCLN110
064200110307     C                   clear                   WFCLN120
064300150210     C                   clear                   WFCLN130
064400110307     C                   clear                   WFCLN_10
064500110307     C                   clear                   WFCLN_11
064600110307     C                   clear                   WFCLN_12
064700150210     C                   clear                   WFCLN_13
064800100219     C*
064900100219     C                   eval      dlyLNP = tasLNP
065000100219     C                   eval      dlyTSP = tasTSP
065100120309     C*
065200120309     C                   if        dlyTSP = 'D'
065300120309     C                   eval      dlyTSP = 'C'
065400120309     C                   endif
065500120309     C*
065600100219     C                   eval      dlyNAR = tasNZD
065700100219     C                   eval      dlyCAP = tasCAD
065800100219     C     KEYdly05_P    chain     wadly05l
065900100219     C                   if        %found(wadly05l)
066000100219     C                   eval      WCLNDLYTRC = DLYTRC
066100100219     C                   endif
066200100219     C*
066300100219     C                   eval      tblcod = 'PR'
066400100219     C                   eval      tblkey = tasprd
066500100219     C     KEYtab_C      chain     tabel00f
066600100219     C                   if        %found(tabel00f)
066700100219     C                   eval      DSPR = tbluni
066800100219     C                   eval      tblcod = '14'
066900100219     C                   eval      tblkey = §PRCRE
067000100219     C     KEYtab_C      chain     tabel00f
067100100219     C                   if        %found(tabel00f)
067200100219     C                   eval      DS14 = tbluni
067300110307     C                   eval      wclnREG = §14DES
067400100219     C                   endif
067500100219     C                   endif
067600100219     C*
067700100219     C                   z-add     *zeros        wANT
067800100222     C                   z-add     *zeros        wM12
067900100219     C                   z-add     *zeros        wOKS
068000150210     C                   z-add     *zeros        wR12
068100150210     C                   z-add     *zeros        wR24
068200100219     C                   z-add     *zeros        wRIT
068300130411     C*
068400130411     C*
068500130411     C* Verifico tipo affidabilità richiesta
068600130411     C                   select
068700130411     C*
068800130411     C* - affidabilità consegna
068900130411     C                   when      TIPAFFID = 'C'
069000001218     C*
069100100219     C* ...in anticipo
069200100219     C                   select
069300130411     C                   when      tasnci >= -999 AND
069400130411     C                             tasnci <= -24
069500100219     C                   add       1             wANT
069600100222     C*
069700100222     C* ...border-line
069800130411     C                   when      tasnci >  -24  AND
069900130411     C                             tasnci <= -12
070000100222     C                   add       1             wM12
070100100219     C*
070200100219     C* ...in orario
070300130411     C                   when      tasnci >  -12  AND
070400130411     C                             tasnci <=   0
070500100219     C                   add       1             wOKS
070600150210     C*
070700150210     C* ...in ritardo entro 12 ore
070800150210     C                   when      tasnci >    0  AND
070900150210     C                             tasnci <=  12
071000150210     C                   add       1             wR12
071100150210     C*
071200150210     C* ...in ritardo entro 24 ore
071300150210     C                   when      tasnci >   12  AND
071400150210     C                             tasnci <=  24
071500150210     C                   add       1             wR24
071600150210     C*
071700150210     C* ...in ritardo "molto"
071800150210     C                   when      tasnci >   24
071900150210     C                   add       1             wRIT
072000150210     C                   endsl
072100130411     C*
072200130411     C* - affidabilità cliente (*DEFAULT)
072300130411     C                   other
072400130411     C*
072500130411     C* ...in anticipo
072600130411     C                   select
072700150210     C                   when      tasnrc >= -999 AND
072800150210     C                             tasnrc <= -24
072900130411     C                   add       1             wANT
073000130411     C*
073100130411     C* ...border-line
073200150210     C                   when      tasnrc >  -24  AND
073300150210     C                             tasnrc <= -12
073400130411     C                   add       1             wM12
073500130411     C*
073600130411     C* ...in orario
073700150210     C                   when      tasnrc >  -12  AND
073800150210     C                             tasnrc <=   0
073900130411     C                   add       1             wOKS
074000150210     C*
074100150210     C* ...in ritardo entro 12 ore
074200150210     C                   when      tasnrc >    0  AND
074300150210     C                             tasnrc <=  12
074400150210     C                   add       1             wR12
074500150210     C*
074600150210     C* ...in ritardo entro 24 ore
074700150210     C                   when      tasnrc >   12  AND
074800150210     C                             tasnrc <=  24
074900150210     C                   add       1             wR24
075000150210     C*
075100150210     C* ...in ritardo "molto"
075200150210     C                   when      tasnrc >   24
075300150210     C                   add       1             wRIT
075400150210     C                   endsl
075500130411     C*
075600130411     C                   endsl
075700100219     C*
075800100219     C* Conteggio spedizioni DIR/GIAC/CCA
075900100219     C                   eval      dtasflo = tasflo
076000100219     C                   if        tasCCA   <> *blanks OR
076100100219     C                             tasFGC   <> *blanks OR
076200100219     C                             §FLONAV  <> *blanks
076300100219     C                   eval      wclnANOM = 'S'
076400100219     C                   endif
076500100219     C*
076600111130     C                   eval      wclnCCM    = tasccm
076700111130     C                   if        wChkCli = 'K'
076800111130     C                   eval      wclnCCM    = tasksc
076900111130     C                   endif
077000130403     C*
077100130403     C* Se richiesta estrazione per "unificante" (di qualunque tipo come CCM considero sempre
077200130403     C* l'unificante)
077300130403     C
077400130403     C                   if        FLGUNI <> *blanks
077500130403     C                   eval      wclnCCM    = CODCLI
077600130403     C                   endif
077700111130     C*
077800100219     C                   eval      wclnAAAAMM = wrkAAAAMM
077900100219     C                   eval      wclnPRD    = tasprd
078000100219     C                   eval      wclnCAD    = tascad
078100100219     C                   eval      wclnISO    = tasfin
078200110307     C                   eval      wclnAAS    = tasaas
078300110307     C                   eval      wclnLNP    = taslnp
078400110307     C                   eval      wclnNRS    = tasnrs
078500110307     C                   eval      wclnNSP    = tasnsp
078600110307     C*
078700110307     C                   select
078800110307     C                   when      *in26
078900110307     C     KEYcln_12C    chain     wfcln12l
079000110307     C                   if        %found(wfcln12l)
079100110307     C                   eval      WFCLN_10 = WFCLN_12
079200100219     C                   add       wANT          wclnANT
079300100222     C                   add       wM12          wclnM12
079400100219     C                   add       wOKS          wclnOKS
079500150210     C                   add       wR12          wclnR12
079600150210     C                   add       wR24          wclnR24
079700100219     C                   add       wRIT          wclnRIT
079800100219     C                   add       1             wclnTOT
079900110307     C                   eval      WFCLN_12 = WFCLN_10
080000110307     C                   update    WFCLN120
080100100219     C                   else
080200100219     C                   add       wANT          wclnANT
080300100222     C                   add       wM12          wclnM12
080400100219     C                   add       wOKS          wclnOKS
080500150210     C                   add       wR12          wclnR12
080600150210     C                   add       wR24          wclnR24
080700100219     C                   add       wRIT          wclnRIT
080800100219     C                   add       1             wclnTOT
080900110307     C                   eval      WFCLN_12 = WFCLN_10
081000110307     C                   write     WFCLN120
081100100219     C                   endif
081200110307     C*
081300110307     C                   when      *in27
081400150211     C     KEYcln_11C    chain     wfcln11l
081500150211     C                   if        %found(wfcln11l)
081600150211     C                   eval      WFCLN_10 = WFCLN_11
081700150211     C                   add       wANT          wclnANT
081800150211     C                   add       wM12          wclnM12
081900150211     C                   add       wOKS          wclnOKS
082000150211     C                   add       wR12          wclnR12
082100150211     C                   add       wR24          wclnR24
082200150211     C                   add       wRIT          wclnRIT
082300150211     C                   add       1             wclnTOT
082400150211     C                   eval      WFCLN_11 = WFCLN_10
082500150211     C                   update    WFCLN110
082600150211     C                   else
082700150211     C                   add       wANT          wclnANT
082800150211     C                   add       wM12          wclnM12
082900150211     C                   add       wOKS          wclnOKS
083000150211     C                   add       wR12          wclnR12
083100150211     C                   add       wR24          wclnR24
083200150211     C                   add       wRIT          wclnRIT
083300150211     C                   add       1             wclnTOT
083400150211     C                   eval      WFCLN_11 = WFCLN_10
083500150211     C                   write     WFCLN110
083600150211     C                   endif
083700150210     C*
083800150210     C                   when      *in28
083900150211     C     KEYcln_13C    chain     wfcln13l
084000150211     C                   if        %found(wfcln13l)
084100150211     C                   eval      WFCLN_10 = WFCLN_13
084200150211     C                   add       wANT          wclnANT
084300150211     C                   add       wM12          wclnM12
084400150211     C                   add       wOKS          wclnOKS
084500150211     C                   add       wR12          wclnR12
084600150211     C                   add       wR24          wclnR24
084700150211     C                   add       wRIT          wclnRIT
084800150211     C                   add       1             wclnTOT
084900150211     C                   eval      WFCLN_13 = WFCLN_10
085000150211     C                   update    WFCLN130
085100150211     C                   else
085200150211     C                   add       wANT          wclnANT
085300150211     C                   add       wM12          wclnM12
085400150211     C                   add       wOKS          wclnOKS
085500150211     C                   add       wR12          wclnR12
085600150211     C                   add       wR24          wclnR24
085700150211     C                   add       wRIT          wclnRIT
085800150211     C                   add       1             wclnTOT
085900150211     C                   eval      WFCLN_13 = WFCLN_10
086000150211     C                   write     WFCLN130
086100150211     C                   endif
086200110307     C*
086300110307     C                   endsl
086400001218     C*
086500001218     C                   ENDSR
086600001221     C*------------------------------------------------------------------------*
086700110304
086800110304
086900110304
087000110304     C*------------------------------------------------------------------------*
087100110304     C* WRITIVGD - Routine di scrittura file TIVGD (file VAS generico download)
087200110304     C*------------------------------------------------------------------------*
087300110304     C     WRITIVGD      BEGSR
087400110304     C*
087500110307     C* Leggo tutto il file WFCLN11L in QTEMP del job corrente
087600110307     C     *loval        setll     WFCLN11L
087700110307     C                   read      WFCLN11L
087800110307     C                   dow       not %eof(WFCLN11L)
087900110307     C*
088000161206     C                   clear                   tivgd000
088100110307     C                   eval      vgdDTA = %subst(WFCLN_11:1:%size(WFCLN_11))
088200110304     C                   eval      vgdTIP = TIPFILE
088300110304     C                   movel     *all'0'       vgdKSU
088400110304     C                   move      CODCLIVAS     vgdKSU
088500110304     C                   eval      vgdTSC = 'WW'
088600110304     C                   eval      vgdDAT = datcor
088700161206     C                   eval      vgdPRG = *blanks
088800161205     C                   eval      vgdPGM = procname
088900161206     C*
089000161206     C                   write     tivgd000
089100110304     C*
089200110307     C                   read      WFCLN11L
089300110307     C                   enddo
089400110307     C*
089500110304     C                   ENDSR
089600110304     C*------------------------------------------------------------------------*
089700120214
089800120214
089900120214
090000120214     C*------------------------------------------------------------------------*
090100120214     C* CARTBL - Routine di caricamento dati tabellati
090200120214     C*------------------------------------------------------------------------*
090300120214     C     CARTBL        BEGSR
090400120214     C*
090500120214     C                   Z-ADD     1             I                 4 0
090600120214     C                   MOVEL     CODCLI        KUNI(I)
090700130403     C*
090800130403     C                   SELECT
090900130403     C                   WHEN      FLGUNI = 'U'
091000120214     C                   MOVEL     '3K'          tblCOD
091100120214     C     KEYtab_P      CHAIN     TABEL                              31
091200120214     C     *IN31         DOWEQ     '0'
091300120214     C     TBLFLG        IFEQ      ' '
091400120214     C                   MOVEL     TBLUNI        DS3K
091500120214     C     §3KCKS        IFEQ      CODCLI
091600120214     C                   ADD       1             I
091700120214     C                   MOVEL     TBLKEY        wKUNI             7 0
091800120214     C     wKUNI         LOOKUP    KUNI                                   55
091900120214     C                   IF        %equal
092000120214     C                   ELSE
092100120214     C                   Z-ADD     wKUNI         KUNI(I)
092200120214     C                   ENDIF
092300120214     C                   ENDIF
092400120214     C                   ENDIF
092500120214     C     KEYtab_P      READE     TABEL                                  31
092600120214     C                   ENDDO
092700130403     C*
092800130403     C                   WHEN      FLGUNI = 'T'
092900130403     C                   CLEAR                   TIBS10DS
093000130403     C                   EVAL      D10DRF = datcor                              *data riferimento
093100130403     C                   EVAL      D10TLE = 'ST'                                *tipo legame
093200130403     C                   EVAL      D10PAF = 'F'                                 *cerca se padre
093300130403     C                   EVAL      D10COD = CODCLI
093400130403     C                   CALL      'TIBS10R'
093500130403     C                   PARM                    TIBS10DS
093600130403     C                   IF        D10ERR = *BLANKS                             *Padre
093700130403     C                   Z-ADD     1             I
093800130403     C                   DOW       skc(I) > *zeros
093900130403     C                   Z-ADD     skc(I)        KUNI(I)
094000130403     C                   ADD       1             I
094100130403     C                   ENDDO
094200130403     C                   ENDIF
094300130403     C*
094400130403     C                   ENDSL
094500120214     C*
094600120214     C                   ENDSR
094700120214     C*---------------------------------------------------------------*
094800161205
094900161205
095000161205
095100161205     C*------------------------------------------------------------------------*
095200161205     C* EXEERR - Routine di esecuzione azioni su Errore
095300161205     C*------------------------------------------------------------------------*
095400161205     C     EXEERR        BEGSR
095500161205     C*
095600161205     C                   dump(A)
095700161205     C                   rolbk
095800161205     C                   seton                                        lr
095900161205     C                   return
096000161205     C*
096100161205     C                   ENDSR
096200161205     C*------------------------------------------------------------------------*
096300120214
096400001218
096500001218
096600001218     C*------------------------------------------------------------------------*
096700001218     C* *INZSR - ROUTINE INIZIALE
096800001218     C*------------------------------------------------------------------------*
096900001218     C     *INZSR        BEGSR
097000001218     C*
097100001218     C     *ENTRY        PLIST
097200100219     C                   PARM                    KPJBA
097300001218     C*
097400100219     C                   MOVEL     KPJBU         DSINPUT
097500100219     C                   EVAL      tblKUT = 1
097600110307     C*
097700110307     C* Determino la data corrente
097800110307     C                   Z-ADD     *zeros        datcor            8 0
097900110307     C                   EVAL      datcor = %dec(%date() : *ISO)
098000001218     C*
098100001218     C* Definizioni chiavi
098200100219     C*
098300100219     C     KEYdly05_P    KLIST
098400100219     C                   KFLD                    dlyLNP
098500100219     C                   KFLD                    dlyTSP
098600100219     C                   KFLD                    dlyNAR
098700100219     C                   KFLD                    dlyCAP
098800120214     C*
098900120214     C     KEYtab_P      KLIST
099000120214     C                   KFLD                    tblKUT
099100120214     C                   KFLD                    tblCOD
099200001218     C*
099300100219     C     KEYtab_C      KLIST
099400100219     C                   KFLD                    tblKUT
099500100219     C                   KFLD                    tblCOD
099600100219     C                   KFLD                    tblKEY
099700110307     C*
099800110307     C     KEYcln_11C    KLIST
099900110307     C                   KFLD                    wclnCCM
100000110307     C                   KFLD                    wclnAAAAMM
100100110307     C                   KFLD                    wclnAAS
100200110307     C                   KFLD                    wclnLNP
100300110307     C                   KFLD                    wclnNRS
100400110307     C                   KFLD                    wclnNSP
100500030704     C*
100600110307     C     KEYcln_12C    KLIST
100700100219     C                   KFLD                    wclnCCM
100800100219     C                   KFLD                    wclnAAAAMM
100900110307     C                   KFLD                    wclnREG
101000100219     C                   KFLD                    wclnPRD
101100100219     C                   KFLD                    wclnCAD
101200100219     C                   KFLD                    wclnISO
101300100219     C                   KFLD                    wclnANOM
101400150210     C*
101500150210     C     KEYcln_13C    KLIST
101600150210     C                   KFLD                    wclnCCM
101700150210     C                   KFLD                    wclnAAAAMM
101800150210     C                   KFLD                    wclnREG
101900150210     C                   KFLD                    wclnPRD
102000100219     C*
102100001218     C                   ENDSR

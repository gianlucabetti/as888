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
001200161207     FTIVGD00F  O    E             DISK
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
009200161207     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
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
011600161206     C* Su errore
011700161206     C                   on-error
011800161207     C                   dump(A)
011900161206     C*
012000161206     C* Fine monitoring
012100161206     C                   endmon
012200001218     C*
012300001218     C                   seton                                        LR
012400110304
012500110304
012600110304
012700110304     C*------------------------------------------------------------------------*
012800110304     C* CHKPAR - Routine di controllo parametri ricevuti in input
012900110304     C*------------------------------------------------------------------------*
013000110304     C     CHKPAR        BEGSR
013100110304     C*
013200110304     C* Verifico le date
013300110304     C                   if        DATADA > *zeros AND
013400110304     C                             DATAA  = *zeros
013500110304     C                   movel     *all'9'       DATAA
013600110304     C                   endif
013700110304     C                   if        DACMDA > *zeros AND
013800110304     C                             DACMA  = *zeros
013900110304     C                   movel     *all'9'       DACMA
014000110304     C                   endif
014100110307     C*
014200110307     C* Verifico il livello d dettaglio richiesto
014300150210     C                   setoff                                       262728
014400110307     C                   select
014500110307     C                   when      TIPDET = 'C'                                 * x CAP
014600110307     C                   seton                                        26
014700110307     C                   when      TIPDET = 'S'                                 * x SPED
014800110307     C                   seton                                        27
014900150210     C                   when      TIPDET = 'P'                                 * x PROV
015000150210     C                   seton                                        28
015100110307     C                   endsl
015200110304     C*
015300110304     C                   ENDSR
015400110304     C*------------------------------------------------------------------------*
015500110304
015600110304
015700110304
015800110304     C*------------------------------------------------------------------------*
015900110304     C* PROCEDI - Routine principale
016000110304     C*------------------------------------------------------------------------*
016100110304     C     PROCEDI       BEGSR
016200110304     C*
016300110304     C* Se richiesta elaborazione x codice cliente mittente...
016400110304     C                   select
016500110304     C                   when      TIPCLI = *blanks
016600110304     C                   exsr      EXECCM
016700110304     C*
016800110304     C* Se richiesta elaborazione x codice cliente fatturazione...
016900110304     C                   when      TIPCLI = 'K'
017000111130     C                   exsr      EXEKSC
017100110304     C*
017200110304     C* Se richiesta elaborazione x entrambi: codice cliente mittente + fatturazione...
017300110304     C                   when      TIPCLI = 'E'
017400110304     C                   exsr      EXECCM
017500111130     C                   exsr      EXEKSC
017600110304     C*
017700110304     C                   endsl
017800110304     C*
017900110304     C                   ENDSR
018000110304     C*------------------------------------------------------------------------*
018100110304
018200110304
018300110304
018400110304     C*------------------------------------------------------------------------*
018500110304     C* EXECCM   - Elaborazione x codice cliente mittente
018600110304     C*------------------------------------------------------------------------*
018700110304     C     EXECCM        BEGSR
018800110304     C*
018900110304     C                   movel     'C'           wChkCli           1
019000110304     C*
019100110304     C                   select
019200110304     C* ...se richiesta elaborazione x data spedizione
019300110304     C                   when      DATADA > *zeros
019400110304     C                   exsr      SQ_CCMDSP
019500110304     C* ...se richiesta elaborazione x data consegna merce
019600110304     C                   when      DACMDA > *zeros
019700110304     C                   exsr      SQ_CCMDCM
019800110304     C                   endsl
019900110304     C*
020000110304     C                   ENDSR
020100110304     C*------------------------------------------------------------------------*
020200111130
020300111130
020400111130
020500111130     C*------------------------------------------------------------------------*
020600111130     C* EXEKSC   - Elaborazione x codice cliente fatturazione
020700111130     C*------------------------------------------------------------------------*
020800111130     C     EXEKSC        BEGSR
020900111130     C*
021000111130     C                   movel     'K'           wChkCli           1
021100111130     C*
021200111130     C                   select
021300111130     C* ...se richiesta elaborazione x data spedizione
021400111130     C                   when      DATADA > *zeros
021500111130     C                   exsr      SQ_KSCDSP
021600111130     C* ...se richiesta elaborazione x data consegna merce
021700111130     C                   when      DACMDA > *zeros
021800111130     C                   exsr      SQ_KSCDCM
021900111130     C                   endsl
022000111130     C*
022100111130     C                   ENDSR
022200111130     C*------------------------------------------------------------------------*
022300110304
022400110304
022500110304
022600110304     C*------------------------------------------------------------------------*
022700110304     C* SQ_CCMDSP - Elaborazione x codice cliente mittente + data spedizione
022800110304     C*------------------------------------------------------------------------*
022900110304     C     SQ_CCMDSP     BEGSR
023000110304     C*
023100110304     C                   z-add     KUNI(I)       wParCCM           7 0
023200110304     C*
023300110304     C/EXEC SQL
023400110304     C+ declare C_CCMDSP cursor for
023500110304     C+ select * from titas00f
023600110304     C+ where tasccm = :wParCCM and
023700110304     C+ tasaas*10000+tasmgs between :DATADA and :DATAA
023800110304     C+ union
023900110304     C+ select * from titas10f
024000110304     C+ where tasccm = :wParCCM and
024100110304     C+ tasaas*10000+tasmgs between :DATADA and :DATAA
024200110304     C+ union
024300110304     C+ select * from titasP0f
024400110304     C+ where tasccm = :wParCCM and
024500110304     C+ tasaas*10000+tasmgs between :DATADA and :DATAA
024600110304     C+ for read only
024700110304     C/END-EXEC
024800110304     C
024900110304     C/EXEC SQL
025000110304     C+ open C_CCMDSP
025100110304     C/END-EXEC
025200110304     C
025300110304     C/EXEC SQL
025400110304     C+ Fetch C_CCMDSP into :TITAS00F
025500110304     C/END-EXEC
025600110304     C*
025700110304     C                   dow       sqlcod = *zeros
025800110304     C                   exsr      verifica
025900110304     C                   if        CHKREC = 'S'
026000110304     C                   exsr      scriviWF
026100110304     C                   endif
026200110304     C
026300110304     C/EXEC SQL
026400110304     C+ Fetch C_CCMDSP into :TITAS00F
026500110304     C/END-EXEC
026600110304     C*
026700110304     C                   enddo
026800110304     C*
026900110304     C/EXEC SQL
027000110304     C+ close C_CCMDSP
027100110304     C/END-EXEC
027200110304     C*
027300110304     C*
027400110304     C                   ENDSR
027500110304     C*------------------------------------------------------------------------*
027600111130
027700111130
027800111130
027900111130     C*------------------------------------------------------------------------*
028000111130     C* SQ_KSCDSP - Elaborazione x codice cliente fatturazione + data spedizione
028100111130     C*------------------------------------------------------------------------*
028200111130     C     SQ_KSCDSP     BEGSR
028300111130     C*
028400111130     C                   z-add     KUNI(I)       wParKSC           7 0
028500111130     C*
028600111130     C/EXEC SQL
028700111130     C+ declare C_KSCDSP cursor for
028800111130     C+ select * from titas00f
028900111130     C+ where tasksc = :wParKSC and
029000111130     C+ tasaas*10000+tasmgs between :DATADA and :DATAA
029100111130     C+ union
029200111130     C+ select * from titas10f
029300111130     C+ where tasksc = :wParKSC and
029400111130     C+ tasaas*10000+tasmgs between :DATADA and :DATAA
029500111130     C+ union
029600111130     C+ select * from titasP0f
029700111130     C+ where tasksc = :wParKSC and
029800111130     C+ tasaas*10000+tasmgs between :DATADA and :DATAA
029900111130     C+ for read only
030000111130     C/END-EXEC
030100111130     C
030200111130     C/EXEC SQL
030300111130     C+ open C_KSCDSP
030400111130     C/END-EXEC
030500111130     C
030600111130     C/EXEC SQL
030700111130     C+ Fetch C_KSCDSP into :TITAS00F
030800111130     C/END-EXEC
030900111130     C*
031000111130     C                   dow       sqlcod = *zeros
031100111130     C                   exsr      verifica
031200111130     C                   if        CHKREC = 'S'
031300111130     C                   exsr      scriviWF
031400111130     C                   endif
031500111130     C
031600111130     C/EXEC SQL
031700111130     C+ Fetch C_KSCDSP into :TITAS00F
031800111130     C/END-EXEC
031900111130     C*
032000111130     C                   enddo
032100111130     C*
032200111130     C/EXEC SQL
032300111130     C+ close C_KSCDSP
032400111130     C/END-EXEC
032500111130     C*
032600111130     C*
032700111130     C                   ENDSR
032800111130     C*------------------------------------------------------------------------*
032900110304
033000110304
033100110304
033200110304     C*------------------------------------------------------------------------*
033300110304     C* SQ_CCMDCM - Elaborazione x codice cliente mittente + data consegna merce
033400110304     C*------------------------------------------------------------------------*
033500110304     C     SQ_CCMDCM     BEGSR
033600110304     C*
033700110304     C                   z-add     KUNI(I)       wParCCM           7 0
033800110304     C*
033900110304     C/EXEC SQL
034000110304     C+ declare C_CCMDCM cursor for
034100110304     C+ select * from titas00f
034200110304     C+ where tasccm = :wParCCM and
034300110304     C+ tasdcm between :DACMDA and :DACMA
034400110304     C+ union
034500110304     C+ select * from titas10f
034600110304     C+ where tasccm = :wParCCM and
034700110304     C+ tasdcm between :DACMDA and :DACMA
034800110304     C+ union
034900110304     C+ select * from titasP0f
035000110304     C+ where tasccm = :wParCCM and
035100110304     C+ tasdcm between :DACMDA and :DACMA
035200110304     C+ for read only
035300110304     C/END-EXEC
035400110304     C
035500110304     C/EXEC SQL
035600110304     C+ open C_CCMDCM
035700110304     C/END-EXEC
035800110304     C
035900110304     C/EXEC SQL
036000110304     C+ Fetch C_CCMDCM into :TITAS00F
036100110304     C/END-EXEC
036200110304     C*
036300110304     C                   dow       sqlcod = *zeros
036400110304     C                   exsr      verifica
036500110304     C                   if        CHKREC = 'S'
036600110304     C                   exsr      scriviWF
036700110304     C                   endif
036800110304     C
036900110304     C/EXEC SQL
037000110304     C+ Fetch C_CCMDCM into :TITAS00F
037100110304     C/END-EXEC
037200110304     C*
037300110304     C                   enddo
037400110304     C*
037500110304     C/EXEC SQL
037600110304     C+ close C_CCMDCM
037700110304     C/END-EXEC
037800110304     C*
037900110304     C*
038000110304     C                   ENDSR
038100110304     C*------------------------------------------------------------------------*
038200111130
038300111130
038400111130
038500111130     C*------------------------------------------------------------------------*
038600111130     C* SQ_KSCDCM - Elaborazione x codice cliente fatturazione + data consegna merce
038700111130     C*------------------------------------------------------------------------*
038800111130     C     SQ_KSCDCM     BEGSR
038900111130     C*
039000111130     C                   z-add     KUNI(I)       wParKSC           7 0
039100111130     C*
039200111130     C/EXEC SQL
039300111130     C+ declare C_KSCDCM cursor for
039400111130     C+ select * from titas00f
039500111130     C+ where tasksc = :wParKSC and
039600111130     C+ tasdcm between :DACMDA and :DACMA
039700111130     C+ union
039800111130     C+ select * from titas10f
039900111130     C+ where tasksc = :wParKSC and
040000111130     C+ tasdcm between :DACMDA and :DACMA
040100111130     C+ union
040200111130     C+ select * from titasP0f
040300111130     C+ where tasksc = :wParKSC and
040400111130     C+ tasdcm between :DACMDA and :DACMA
040500111130     C+ for read only
040600111130     C/END-EXEC
040700111130     C
040800111130     C/EXEC SQL
040900111130     C+ open C_KSCDCM
041000111130     C/END-EXEC
041100111130     C
041200111130     C/EXEC SQL
041300111130     C+ Fetch C_KSCDCM into :TITAS00F
041400111130     C/END-EXEC
041500111130     C*
041600111130     C                   dow       sqlcod = *zeros
041700111130     C                   exsr      verifica
041800111130     C                   if        CHKREC = 'S'
041900111130     C                   exsr      scriviWF
042000111130     C                   endif
042100111130     C
042200111130     C/EXEC SQL
042300111130     C+ Fetch C_KSCDCM into :TITAS00F
042400111130     C/END-EXEC
042500111130     C*
042600111130     C                   enddo
042700111130     C*
042800111130     C/EXEC SQL
042900111130     C+ close C_KSCDCM
043000111130     C/END-EXEC
043100111130     C*
043200111130     C*
043300111130     C                   ENDSR
043400111130     C*------------------------------------------------------------------------*
043500001218
043600110304
043700110304
043800110304     C*------------------------------------------------------------------------*
043900110304     C* CHKCONS  - Verifica se bolla effettivamente consegnata
044000110304     C*------------------------------------------------------------------------*
044100110304     C     CHKCONS       BEGSR
044200110304     C*
044300110304     C                   setoff                                       4041
044400110304     C                   movel     *blanks       wEsito1           1
044500110304     C                   movel     *blanks       wEsito2           1
044600110304     C*
044700110304     C                   eval      wAAS = tasAAS
044800110304     C                   eval      wLNP = tasLNP
044900110304     C                   eval      wNRS = tasNRS
045000110304     C                   eval      wNSP = tasNSP
045100110304     C*
045200110304     C* Chiamata metodo GetLblTyp
045300110304     C                   eval      wEsito1 = %char(UBLBLSPE_GetLblTyp(
045400110304     C                                                wAAS
045500110304     C                                               :wLNP
045600110304     C                                               :wNRS
045700110304     C                                               :wNSP
045800110304     C                                               :pOutLblTyp
045900110304     C                                               :pOutAnnoBO
046000110304     C                                               :pOutLineaParBO
046100110304     C                                               :pOutSerieBO
046200110304     C                                               :pOutNumSpedBO
046300110304     C                                               :pOutDcmBO
046400110304     C                                               :pOutCcaBO
046500110304     C                                               :pOutRblBO))
046600110304     C* Se trattasi d bolla nn originale reperisco la relativa bolla originale
046700110304     C                   if        pOutLblTyp <> 'O'
046800110304     C                   eval      wAAS = pOutAnnoBO
046900110304     C                   eval      wLNP = pOutLineaParBO
047000110304     C                   eval      wNRS = pOutSerieBO
047100110304     C                   eval      wNSP = pOutNumSpedBO
047200110304     C                   endif
047300110304     C*
047400110304     C* Chiamata metodo GetLastChild
047500110304     C                   eval      wEsito2 = %char(UBLBLSPE_GetLastChild(
047600110304     C                                                wAAS
047700110304     C                                               :wLNP
047800110304     C                                               :wNRS
047900110304     C                                               :wNSP
048000110304     C                                               :pOutAnnoFI
048100110304     C                                               :pOutLineaParFI
048200110304     C                                               :pOutSerieFI
048300110304     C                                               :pOutNumSpedFI
048400110304     C                                               :pOutDcmFI
048500110304     C                                               :pOutCcaFI))
048600110304     C*
048700110304     C* Considerazioni sullo stato corrente della bolla
048800110304     C                   if        (wEsito2 = '9' and pOutDcmBO > *zeros)  or
048900110304     C                             (wEsito2 = '0' and pOutDcmFI > *zeros)
049000161205     C                   seton                                        40        * bolla chiusa
049100110304     C                   endif
049200110304     C*
049300110304     C                   if        (wEsito2 = '9' and pOutDcmBO > *zeros
049400110304     C                                            and pOutCcaBO = *blanks) or
049500110304     C                             (wEsito2 = '0' and pOutDcmFI > *zeros
049600110304     C                                            and pOutCcaFI = *blanks)
049700110304     C                   seton                                        41        * bolla cons. OK
049800110304     C                   endif
049900110304     C*
050000110304     C                   ENDSR
050100110304     C*------------------------------------------------------------------------*
050200110304
050300110304
050400110304
050500110304     C*------------------------------------------------------------------------*
050600110304     C* VERIFICA - Routine di verifica validità record corrente
050700110304     C*------------------------------------------------------------------------*
050800110304     C     VERIFICA      BEGSR
050900110304     C*
051000110304     C                   movel     'S'           CHKREC            1
051100110307     C*
051200110307     C                   movel     tasaas        wrkdata8          8 0
051300110307     C                   move      tasmgs        wrkdata8
051400110307     C                   movel     wrkdata8      wrkAAAAMM         6 0
051500110304     C*
051600110304     C* Verifico le date spedizione
051700110304     C                   if        CHKREC = 'S'
051800110304     C                   if        DATADA > *zeros
051900110304     C                   movel     tasaas        wrkdata           8 0
052000110304     C                   move      tasmgs        wrkdata
052100110304     C                   if        wrkdata < DATADA or
052200110304     C                             wrkdata > DATAA
052300110304     C                   movel     'N'           CHKREC
052400110304     C                   endif
052500110304     C                   endif
052600110304     C                   endif
052700110304     C*
052800110304     C* Verifico le date spedizione
052900110304     C                   if        CHKREC = 'S'
053000110304     C                   if        DACMDA > *zeros
053100110304     C                   if        tasdcm  < DACMDA or
053200110304     C                             tasdcm  > DACMA
053300110304     C                   movel     'N'           CHKREC
053400110304     C                   endif
053500110304     C                   endif
053600110304     C                   endif
053700110304     C*
053800110304     C* Verifico il codice cliente mittente
053900110304     C                   if        CHKREC = 'S'
054000110304     C                   if        wChkCli  = 'C'      AND
054100110304     C                             tasccm  <> KUNI(I)
054200110304     C                   movel     'N'           CHKREC
054300110304     C                   endif
054400110304     C                   endif
054500110304     C*
054600110304     C* Verifico il codice cliente fatturazione (in caso d 'K')
054700110304     C                   if        CHKREC = 'S'
054800110304     C                   if        wChkCli  = 'K'      AND
054900110304     C                             tasksc  <> KUNI(I)
055000110304     C                   movel     'N'           CHKREC
055100110304     C                   endif
055200110304     C                   endif
055300110304     C*
055400110304     C* Verifico il codice cliente fatturazione (in caso d 'E')
055500110304     C                   if        CHKREC = 'S'
055600110304     C                   if        TIPCLI  = 'E'       AND
055700110304     C                             wChkCli = 'K'       AND
055800110304     C                             tasksc  = tasccm
055900110304     C                   movel     'N'           CHKREC
056000110304     C                   endif
056100110304     C                   endif
056200110304     C*
056300110304     C* Verifico la serie
056400110304     C                   if        CHKREC = 'S'
056500110307     C                   if        NUMSER <> *blanks
056600110307     C                   if        tasnrs <> %dec(NUMSER : 2 : 0)
056700110304     C                   movel     'N'           CHKREC
056800110304     C                   endif
056900110304     C                   endif
057000110304     C                   endif
057100110304     C*
057200110304     C* Verifico il tipo bolla
057300110304     C                   if        CHKREC = 'S'
057400140326     C                   select
057500140326     C                   when      TIPBOL = '99'
057600140326     C                   when      TIPBOL = '98' AND
057700140326     C                             tastbl <> 'F1' AND tastbl <> 'A2'
057800140326     C                   movel     'N'           CHKREC
057900140326     C                   when      TIPBOL <> '99' AND
058000140326     C                             TIPBOL <> '98' AND
058100140326     C                             tastbl <> TIPBOL
058200140326     C                   movel     'N'           CHKREC
058300140326     C                   endsl
058400110304     C                   endif
058500110304     C*
058600110304     C* Verifico se richiesto nel lancio che la bolla sia consegnata
058700110304     C                   if        CHKREC = 'S'
058800110304     C                   if        FLGCONS = 'S'
058900110304     C                   exsr      CHKCONS
059000110304     C                   if        *in40 = *on
059100110304     C                   else
059200110304     C                   movel     'N'           CHKREC
059300110304     C                   endif
059400110304     C                   endif
059500110304     C                   endif
059600110304     C*
059700110304     C* Verifico se richiesto nel lancio che la bolla sia consegnata OK
059800110304     C                   if        CHKREC = 'S'
059900110304     C                   if        FLGCONS = 'O'
060000110304     C                   exsr      CHKCONS
060100110304     C                   if        *in41 = *on
060200110304     C                   else
060300110304     C                   movel     'N'           CHKREC
060400110304     C                   endif
060500110304     C                   endif
060600110304     C                   endif
060700110304     C*
060800110304     C* Verifico se richiesto nel lancio che la bolla NON sia consegnata OK
060900110304     C                   if        CHKREC = 'S'
061000110304     C                   if        FLGCONS = 'K'
061100110304     C                   exsr      CHKCONS
061200110304     C                   if        *in41 = *on
061300110304     C                   movel     'N'           CHKREC
061400110304     C                   endif
061500110304     C                   endif
061600110304     C                   endif
061700110304     C*
061800110304     C* Verifico se richiesto nel lancio che la bolla NON sia consegnata
061900110304     C                   if        CHKREC = 'S'
062000110304     C                   if        FLGCONS = 'P'
062100110304     C                   exsr      CHKCONS
062200110304     C                   if        *in40 = *on
062300110304     C                   movel     'N'           CHKREC
062400110304     C                   endif
062500110304     C                   endif
062600110304     C                   endif
062700110304     C*
062800110304     C                   ENDSR
062900110304     C*------------------------------------------------------------------------*
063000001218
063100001218
063200001218
063300001218     C*------------------------------------------------------------------------*
063400110307     C* SCRIVIWF - Routine di scrittura WFCLN10F
063500001218     C*------------------------------------------------------------------------*
063600100219     C     SCRIVIWF      BEGSR
063700100219     C*
063800110307     C                   clear                   WFCLN110
063900110307     C                   clear                   WFCLN120
064000150210     C                   clear                   WFCLN130
064100110307     C                   clear                   WFCLN_10
064200110307     C                   clear                   WFCLN_11
064300110307     C                   clear                   WFCLN_12
064400150210     C                   clear                   WFCLN_13
064500100219     C*
064600100219     C                   eval      dlyLNP = tasLNP
064700100219     C                   eval      dlyTSP = tasTSP
064800120309     C*
064900120309     C                   if        dlyTSP = 'D'
065000120309     C                   eval      dlyTSP = 'C'
065100120309     C                   endif
065200120309     C*
065300100219     C                   eval      dlyNAR = tasNZD
065400100219     C                   eval      dlyCAP = tasCAD
065500100219     C     KEYdly05_P    chain     wadly05l
065600100219     C                   if        %found(wadly05l)
065700100219     C                   eval      WCLNDLYTRC = DLYTRC
065800100219     C                   endif
065900100219     C*
066000100219     C                   eval      tblcod = 'PR'
066100100219     C                   eval      tblkey = tasprd
066200100219     C     KEYtab_C      chain     tabel00f
066300100219     C                   if        %found(tabel00f)
066400100219     C                   eval      DSPR = tbluni
066500100219     C                   eval      tblcod = '14'
066600100219     C                   eval      tblkey = §PRCRE
066700100219     C     KEYtab_C      chain     tabel00f
066800100219     C                   if        %found(tabel00f)
066900100219     C                   eval      DS14 = tbluni
067000110307     C                   eval      wclnREG = §14DES
067100100219     C                   endif
067200100219     C                   endif
067300100219     C*
067400100219     C                   z-add     *zeros        wANT
067500100222     C                   z-add     *zeros        wM12
067600100219     C                   z-add     *zeros        wOKS
067700150210     C                   z-add     *zeros        wR12
067800150210     C                   z-add     *zeros        wR24
067900100219     C                   z-add     *zeros        wRIT
068000130411     C*
068100130411     C*
068200130411     C* Verifico tipo affidabilità richiesta
068300130411     C                   select
068400130411     C*
068500130411     C* - affidabilità consegna
068600130411     C                   when      TIPAFFID = 'C'
068700001218     C*
068800100219     C* ...in anticipo
068900100219     C                   select
069000130411     C                   when      tasnci >= -999 AND
069100130411     C                             tasnci <= -24
069200100219     C                   add       1             wANT
069300100222     C*
069400100222     C* ...border-line
069500130411     C                   when      tasnci >  -24  AND
069600130411     C                             tasnci <= -12
069700100222     C                   add       1             wM12
069800100219     C*
069900100219     C* ...in orario
070000130411     C                   when      tasnci >  -12  AND
070100130411     C                             tasnci <=   0
070200100219     C                   add       1             wOKS
070300150210     C*
070400150210     C* ...in ritardo entro 12 ore
070500150210     C                   when      tasnci >    0  AND
070600150210     C                             tasnci <=  12
070700150210     C                   add       1             wR12
070800150210     C*
070900150210     C* ...in ritardo entro 24 ore
071000150210     C                   when      tasnci >   12  AND
071100150210     C                             tasnci <=  24
071200150210     C                   add       1             wR24
071300150210     C*
071400150210     C* ...in ritardo "molto"
071500150210     C                   when      tasnci >   24
071600150210     C                   add       1             wRIT
071700150210     C                   endsl
071800130411     C*
071900130411     C* - affidabilità cliente (*DEFAULT)
072000130411     C                   other
072100130411     C*
072200130411     C* ...in anticipo
072300130411     C                   select
072400150210     C                   when      tasnrc >= -999 AND
072500150210     C                             tasnrc <= -24
072600130411     C                   add       1             wANT
072700130411     C*
072800130411     C* ...border-line
072900150210     C                   when      tasnrc >  -24  AND
073000150210     C                             tasnrc <= -12
073100130411     C                   add       1             wM12
073200130411     C*
073300130411     C* ...in orario
073400150210     C                   when      tasnrc >  -12  AND
073500150210     C                             tasnrc <=   0
073600130411     C                   add       1             wOKS
073700150210     C*
073800150210     C* ...in ritardo entro 12 ore
073900150210     C                   when      tasnrc >    0  AND
074000150210     C                             tasnrc <=  12
074100150210     C                   add       1             wR12
074200150210     C*
074300150210     C* ...in ritardo entro 24 ore
074400150210     C                   when      tasnrc >   12  AND
074500150210     C                             tasnrc <=  24
074600150210     C                   add       1             wR24
074700150210     C*
074800150210     C* ...in ritardo "molto"
074900150210     C                   when      tasnrc >   24
075000150210     C                   add       1             wRIT
075100150210     C                   endsl
075200130411     C*
075300130411     C                   endsl
075400100219     C*
075500100219     C* Conteggio spedizioni DIR/GIAC/CCA
075600100219     C                   eval      dtasflo = tasflo
075700100219     C                   if        tasCCA   <> *blanks OR
075800100219     C                             tasFGC   <> *blanks OR
075900100219     C                             §FLONAV  <> *blanks
076000100219     C                   eval      wclnANOM = 'S'
076100100219     C                   endif
076200100219     C*
076300111130     C                   eval      wclnCCM    = tasccm
076400111130     C                   if        wChkCli = 'K'
076500111130     C                   eval      wclnCCM    = tasksc
076600111130     C                   endif
076700130403     C*
076800130403     C* Se richiesta estrazione per "unificante" (di qualunque tipo come CCM considero sempre
076900130403     C* l'unificante)
077000130403     C
077100130403     C                   if        FLGUNI <> *blanks
077200130403     C                   eval      wclnCCM    = CODCLI
077300130403     C                   endif
077400111130     C*
077500100219     C                   eval      wclnAAAAMM = wrkAAAAMM
077600100219     C                   eval      wclnPRD    = tasprd
077700100219     C                   eval      wclnCAD    = tascad
077800100219     C                   eval      wclnISO    = tasfin
077900110307     C                   eval      wclnAAS    = tasaas
078000110307     C                   eval      wclnLNP    = taslnp
078100110307     C                   eval      wclnNRS    = tasnrs
078200110307     C                   eval      wclnNSP    = tasnsp
078300110307     C*
078400110307     C                   select
078500110307     C                   when      *in26
078600110307     C     KEYcln_12C    chain     wfcln12l
078700110307     C                   if        %found(wfcln12l)
078800110307     C                   eval      WFCLN_10 = WFCLN_12
078900100219     C                   add       wANT          wclnANT
079000100222     C                   add       wM12          wclnM12
079100100219     C                   add       wOKS          wclnOKS
079200150210     C                   add       wR12          wclnR12
079300150210     C                   add       wR24          wclnR24
079400100219     C                   add       wRIT          wclnRIT
079500100219     C                   add       1             wclnTOT
079600110307     C                   eval      WFCLN_12 = WFCLN_10
079700110307     C                   update    WFCLN120
079800100219     C                   else
079900100219     C                   add       wANT          wclnANT
080000100222     C                   add       wM12          wclnM12
080100100219     C                   add       wOKS          wclnOKS
080200150210     C                   add       wR12          wclnR12
080300150210     C                   add       wR24          wclnR24
080400100219     C                   add       wRIT          wclnRIT
080500100219     C                   add       1             wclnTOT
080600110307     C                   eval      WFCLN_12 = WFCLN_10
080700110307     C                   write     WFCLN120
080800100219     C                   endif
080900110307     C*
081000110307     C                   when      *in27
081100150211     C     KEYcln_11C    chain     wfcln11l
081200150211     C                   if        %found(wfcln11l)
081300150211     C                   eval      WFCLN_10 = WFCLN_11
081400150211     C                   add       wANT          wclnANT
081500150211     C                   add       wM12          wclnM12
081600150211     C                   add       wOKS          wclnOKS
081700150211     C                   add       wR12          wclnR12
081800150211     C                   add       wR24          wclnR24
081900150211     C                   add       wRIT          wclnRIT
082000150211     C                   add       1             wclnTOT
082100150211     C                   eval      WFCLN_11 = WFCLN_10
082200150211     C                   update    WFCLN110
082300150211     C                   else
082400150211     C                   add       wANT          wclnANT
082500150211     C                   add       wM12          wclnM12
082600150211     C                   add       wOKS          wclnOKS
082700150211     C                   add       wR12          wclnR12
082800150211     C                   add       wR24          wclnR24
082900150211     C                   add       wRIT          wclnRIT
083000150211     C                   add       1             wclnTOT
083100150211     C                   eval      WFCLN_11 = WFCLN_10
083200150211     C                   write     WFCLN110
083300150211     C                   endif
083400150210     C*
083500150210     C                   when      *in28
083600150211     C     KEYcln_13C    chain     wfcln13l
083700150211     C                   if        %found(wfcln13l)
083800150211     C                   eval      WFCLN_10 = WFCLN_13
083900150211     C                   add       wANT          wclnANT
084000150211     C                   add       wM12          wclnM12
084100150211     C                   add       wOKS          wclnOKS
084200150211     C                   add       wR12          wclnR12
084300150211     C                   add       wR24          wclnR24
084400150211     C                   add       wRIT          wclnRIT
084500150211     C                   add       1             wclnTOT
084600150211     C                   eval      WFCLN_13 = WFCLN_10
084700150211     C                   update    WFCLN130
084800150211     C                   else
084900150211     C                   add       wANT          wclnANT
085000150211     C                   add       wM12          wclnM12
085100150211     C                   add       wOKS          wclnOKS
085200150211     C                   add       wR12          wclnR12
085300150211     C                   add       wR24          wclnR24
085400150211     C                   add       wRIT          wclnRIT
085500150211     C                   add       1             wclnTOT
085600150211     C                   eval      WFCLN_13 = WFCLN_10
085700150211     C                   write     WFCLN130
085800150211     C                   endif
085900110307     C*
086000110307     C                   endsl
086100001218     C*
086200001218     C                   ENDSR
086300001221     C*------------------------------------------------------------------------*
086400110304
086500110304
086600110304
086700110304     C*------------------------------------------------------------------------*
086800110304     C* WRITIVGD - Routine di scrittura file TIVGD (file VAS generico download)
086900110304     C*------------------------------------------------------------------------*
087000110304     C     WRITIVGD      BEGSR
087100110304     C*
087200110307     C* Leggo tutto il file WFCLN11L in QTEMP del job corrente
087300110307     C     *loval        setll     WFCLN11L
087400110307     C                   read      WFCLN11L
087500110307     C                   dow       not %eof(WFCLN11L)
087600110307     C*
087700161206     C                   clear                   tivgd000
087800110307     C                   eval      vgdDTA = %subst(WFCLN_11:1:%size(WFCLN_11))
087900110304     C                   eval      vgdTIP = TIPFILE
088000110304     C                   movel     *all'0'       vgdKSU
088100110304     C                   move      CODCLIVAS     vgdKSU
088200110304     C                   eval      vgdTSC = 'WW'
088300110304     C                   eval      vgdDAT = datcor
088400161206     C                   eval      vgdPRG = *blanks
088500161205     C                   eval      vgdPGM = procname
088600161206     C*
088700161206     C                   write     tivgd000
088800110304     C*
088900110307     C                   read      WFCLN11L
089000110307     C                   enddo
089100110307     C*
089200110304     C                   ENDSR
089300110304     C*------------------------------------------------------------------------*
089400120214
089500120214
089600120214
089700120214     C*------------------------------------------------------------------------*
089800120214     C* CARTBL - Routine di caricamento dati tabellati
089900120214     C*------------------------------------------------------------------------*
090000120214     C     CARTBL        BEGSR
090100120214     C*
090200120214     C                   Z-ADD     1             I                 4 0
090300120214     C                   MOVEL     CODCLI        KUNI(I)
090400130403     C*
090500130403     C                   SELECT
090600130403     C                   WHEN      FLGUNI = 'U'
090700120214     C                   MOVEL     '3K'          tblCOD
090800120214     C     KEYtab_P      CHAIN     TABEL                              31
090900120214     C     *IN31         DOWEQ     '0'
091000120214     C     TBLFLG        IFEQ      ' '
091100120214     C                   MOVEL     TBLUNI        DS3K
091200120214     C     §3KCKS        IFEQ      CODCLI
091300120214     C                   ADD       1             I
091400120214     C                   MOVEL     TBLKEY        wKUNI             7 0
091500120214     C     wKUNI         LOOKUP    KUNI                                   55
091600120214     C                   IF        %equal
091700120214     C                   ELSE
091800120214     C                   Z-ADD     wKUNI         KUNI(I)
091900120214     C                   ENDIF
092000120214     C                   ENDIF
092100120214     C                   ENDIF
092200120214     C     KEYtab_P      READE     TABEL                                  31
092300120214     C                   ENDDO
092400130403     C*
092500130403     C                   WHEN      FLGUNI = 'T'
092600130403     C                   CLEAR                   TIBS10DS
092700130403     C                   EVAL      D10DRF = datcor                              *data riferimento
092800130403     C                   EVAL      D10TLE = 'ST'                                *tipo legame
092900130403     C                   EVAL      D10PAF = 'F'                                 *cerca se padre
093000130403     C                   EVAL      D10COD = CODCLI
093100130403     C                   CALL      'TIBS10R'
093200130403     C                   PARM                    TIBS10DS
093300130403     C                   IF        D10ERR = *BLANKS                             *Padre
093400130403     C                   Z-ADD     1             I
093500130403     C                   DOW       skc(I) > *zeros
093600130403     C                   Z-ADD     skc(I)        KUNI(I)
093700130403     C                   ADD       1             I
093800130403     C                   ENDDO
093900130403     C                   ENDIF
094000130403     C*
094100130403     C                   ENDSL
094200120214     C*
094300120214     C                   ENDSR
094400120214     C*---------------------------------------------------------------*
094500120214
094600001218
094700001218
094800001218     C*------------------------------------------------------------------------*
094900001218     C* *INZSR - ROUTINE INIZIALE
095000001218     C*------------------------------------------------------------------------*
095100001218     C     *INZSR        BEGSR
095200001218     C*
095300001218     C     *ENTRY        PLIST
095400100219     C                   PARM                    KPJBA
095500001218     C*
095600100219     C                   MOVEL     KPJBU         DSINPUT
095700100219     C                   EVAL      tblKUT = 1
095800110307     C*
095900110307     C* Determino la data corrente
096000110307     C                   Z-ADD     *zeros        datcor            8 0
096100110307     C                   EVAL      datcor = %dec(%date() : *ISO)
096200001218     C*
096300001218     C* Definizioni chiavi
096400100219     C*
096500100219     C     KEYdly05_P    KLIST
096600100219     C                   KFLD                    dlyLNP
096700100219     C                   KFLD                    dlyTSP
096800100219     C                   KFLD                    dlyNAR
096900100219     C                   KFLD                    dlyCAP
097000120214     C*
097100120214     C     KEYtab_P      KLIST
097200120214     C                   KFLD                    tblKUT
097300120214     C                   KFLD                    tblCOD
097400001218     C*
097500100219     C     KEYtab_C      KLIST
097600100219     C                   KFLD                    tblKUT
097700100219     C                   KFLD                    tblCOD
097800100219     C                   KFLD                    tblKEY
097900110307     C*
098000110307     C     KEYcln_11C    KLIST
098100110307     C                   KFLD                    wclnCCM
098200110307     C                   KFLD                    wclnAAAAMM
098300110307     C                   KFLD                    wclnAAS
098400110307     C                   KFLD                    wclnLNP
098500110307     C                   KFLD                    wclnNRS
098600110307     C                   KFLD                    wclnNSP
098700030704     C*
098800110307     C     KEYcln_12C    KLIST
098900100219     C                   KFLD                    wclnCCM
099000100219     C                   KFLD                    wclnAAAAMM
099100110307     C                   KFLD                    wclnREG
099200100219     C                   KFLD                    wclnPRD
099300100219     C                   KFLD                    wclnCAD
099400100219     C                   KFLD                    wclnISO
099500100219     C                   KFLD                    wclnANOM
099600150210     C*
099700150210     C     KEYcln_13C    KLIST
099800150210     C                   KFLD                    wclnCCM
099900150210     C                   KFLD                    wclnAAAAMM
100000150210     C                   KFLD                    wclnREG
100100150210     C                   KFLD                    wclnPRD
100200100219     C*
100300001218     C                   ENDSR

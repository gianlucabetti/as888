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
001200001218     D*--------------------
001300001218     D* DS ESTERNE
001400001218     D*--------------------
001500110307     D WFCLN_10      E DS                  EXTNAME(WFCLN10F)
001600110307     D WFCLN_11      E DS                  EXTNAME(WFCLN10F) PREFIX(F11_)
001700110307     D WFCLN_12      E DS                  EXTNAME(WFCLN10F) PREFIX(F12_)
001800150210     D WFCLN_13      E DS                  EXTNAME(WFCLN10F) PREFIX(F13_)
001900161205     D psds           sds
002000161205     D  procname         *PROC
002100110307     D TITAS00F      E DS
002200900517     D KPJBA         E DS
002300100219     D DTASFLO       E DS
002400100219     D DSPR          E DS
002500100219     D DS14          E DS
002600120214     D DS3K          E DS
002700161206     D iDSVGD        E DS                  INZ EXTNAME(TIVGD00F)
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
009200110304     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
009300110304     C/END-EXEC
009400110304     C
009500110304     C*
009600120214     C                   exsr      cartbl
009700110304     C                   exsr      chkpar
009800110304     C*
009900120214     C**** Fisso KUNI(1)
010000120214     C***                   eval      KUNI(1) = CODCLI
010100110304     C*
010200110304     C* Ciclo sulla schiera dei clienti figli dell'unificante preso in input
010300110304     C                   z-add     1             I                 4 0
010400110304     C                   sorta     KUNI
010500110304     C                   dow       KUNI(I) > *zeros
010600001218     C                   exsr      procedi
010700110307     C                   add       1             I
010800110304     C                   enddo
010900161205     C*
011000161205     C* Inizializzo variabili d wrk
011100161205     C                   movel     'N'           wProcedi          1
011200161205     C*
011300161205     C* Inizializzo la transazione
011400161205     C                   CALL      'TIS7VASR1'
011500161205     C                   PARM      '1'           iOPZ              1
011600161205     C                   PARM                    iDSVGD
011700161205     C                   PARM      *blanks       oPRG             10
011800161205     C                   PARM                    oOK               1
011900161205     C*
012000161205     C* Verifico esito chiamata
012100161205     C                   if        %error OR oOK = *off
012200161205     C                   exsr      EXEERR
012300161205     C                   endif
012400161205     C*
012500161205     C* Se OK => proseguo
012600161206     C                   if        oOK = *on
012700161205     C                   movel     'S'           wProcedi
012800161206     C* Forzo momentaneamente il progressivo  a *blanks
012900161206     C                   eval      oPRG = *blanks
013000161205     C                   endif
013100110307     C*
013200161205     C* Se OK => Popolo il file d output
013300110307     C                   if        wProcedi = 'S'
013400110307     C                   exsr      WRITIVGD
013500110307     C                   endif
013600161205     C*
013700161205     C* Finalizzo la transazione
013800161205     C                   CALL      'TIS7VASR1'
013900161205     C                   PARM      '3'           iOPZ
014000161205     C                   PARM                    iDSVGD
014100161205     C                   PARM                    oPRG
014200161205     C                   PARM                    oOK
014300161205     C*
014400161205     C* Verifico esito chiamata
014500161205     C                   if        %error OR oOK = *off
014600161205     C                   exsr      EXEERR
014700161205     C                   endif
014800001218     C*
014900001218     C                   seton                                        LR
015000110304
015100110304
015200110304
015300110304     C*------------------------------------------------------------------------*
015400110304     C* CHKPAR - Routine di controllo parametri ricevuti in input
015500110304     C*------------------------------------------------------------------------*
015600110304     C     CHKPAR        BEGSR
015700110304     C*
015800110304     C* Verifico le date
015900110304     C                   if        DATADA > *zeros AND
016000110304     C                             DATAA  = *zeros
016100110304     C                   movel     *all'9'       DATAA
016200110304     C                   endif
016300110304     C                   if        DACMDA > *zeros AND
016400110304     C                             DACMA  = *zeros
016500110304     C                   movel     *all'9'       DACMA
016600110304     C                   endif
016700110307     C*
016800110307     C* Verifico il livello d dettaglio richiesto
016900150210     C                   setoff                                       262728
017000110307     C                   select
017100110307     C                   when      TIPDET = 'C'                                 * x CAP
017200110307     C                   seton                                        26
017300110307     C                   when      TIPDET = 'S'                                 * x SPED
017400110307     C                   seton                                        27
017500150210     C                   when      TIPDET = 'P'                                 * x PROV
017600150210     C                   seton                                        28
017700110307     C                   endsl
017800110304     C*
017900110304     C                   ENDSR
018000110304     C*------------------------------------------------------------------------*
018100110304
018200110304
018300110304
018400110304     C*------------------------------------------------------------------------*
018500110304     C* PROCEDI - Routine principale
018600110304     C*------------------------------------------------------------------------*
018700110304     C     PROCEDI       BEGSR
018800110304     C*
018900110304     C* Se richiesta elaborazione x codice cliente mittente...
019000110304     C                   select
019100110304     C                   when      TIPCLI = *blanks
019200110304     C                   exsr      EXECCM
019300110304     C*
019400110304     C* Se richiesta elaborazione x codice cliente fatturazione...
019500110304     C                   when      TIPCLI = 'K'
019600111130     C                   exsr      EXEKSC
019700110304     C*
019800110304     C* Se richiesta elaborazione x entrambi: codice cliente mittente + fatturazione...
019900110304     C                   when      TIPCLI = 'E'
020000110304     C                   exsr      EXECCM
020100111130     C                   exsr      EXEKSC
020200110304     C*
020300110304     C                   endsl
020400110304     C*
020500110304     C                   ENDSR
020600110304     C*------------------------------------------------------------------------*
020700110304
020800110304
020900110304
021000110304     C*------------------------------------------------------------------------*
021100110304     C* EXECCM   - Elaborazione x codice cliente mittente
021200110304     C*------------------------------------------------------------------------*
021300110304     C     EXECCM        BEGSR
021400110304     C*
021500110304     C                   movel     'C'           wChkCli           1
021600110304     C*
021700110304     C                   select
021800110304     C* ...se richiesta elaborazione x data spedizione
021900110304     C                   when      DATADA > *zeros
022000110304     C                   exsr      SQ_CCMDSP
022100110304     C* ...se richiesta elaborazione x data consegna merce
022200110304     C                   when      DACMDA > *zeros
022300110304     C                   exsr      SQ_CCMDCM
022400110304     C                   endsl
022500110304     C*
022600110304     C                   ENDSR
022700110304     C*------------------------------------------------------------------------*
022800111130
022900111130
023000111130
023100111130     C*------------------------------------------------------------------------*
023200111130     C* EXEKSC   - Elaborazione x codice cliente fatturazione
023300111130     C*------------------------------------------------------------------------*
023400111130     C     EXEKSC        BEGSR
023500111130     C*
023600111130     C                   movel     'K'           wChkCli           1
023700111130     C*
023800111130     C                   select
023900111130     C* ...se richiesta elaborazione x data spedizione
024000111130     C                   when      DATADA > *zeros
024100111130     C                   exsr      SQ_KSCDSP
024200111130     C* ...se richiesta elaborazione x data consegna merce
024300111130     C                   when      DACMDA > *zeros
024400111130     C                   exsr      SQ_KSCDCM
024500111130     C                   endsl
024600111130     C*
024700111130     C                   ENDSR
024800111130     C*------------------------------------------------------------------------*
024900110304
025000110304
025100110304
025200110304     C*------------------------------------------------------------------------*
025300110304     C* SQ_CCMDSP - Elaborazione x codice cliente mittente + data spedizione
025400110304     C*------------------------------------------------------------------------*
025500110304     C     SQ_CCMDSP     BEGSR
025600110304     C*
025700110304     C                   z-add     KUNI(I)       wParCCM           7 0
025800110304     C*
025900110304     C/EXEC SQL
026000110304     C+ declare C_CCMDSP cursor for
026100110304     C+ select * from titas00f
026200110304     C+ where tasccm = :wParCCM and
026300110304     C+ tasaas*10000+tasmgs between :DATADA and :DATAA
026400110304     C+ union
026500110304     C+ select * from titas10f
026600110304     C+ where tasccm = :wParCCM and
026700110304     C+ tasaas*10000+tasmgs between :DATADA and :DATAA
026800110304     C+ union
026900110304     C+ select * from titasP0f
027000110304     C+ where tasccm = :wParCCM and
027100110304     C+ tasaas*10000+tasmgs between :DATADA and :DATAA
027200110304     C+ for read only
027300110304     C/END-EXEC
027400110304     C
027500110304     C/EXEC SQL
027600110304     C+ open C_CCMDSP
027700110304     C/END-EXEC
027800110304     C
027900110304     C/EXEC SQL
028000110304     C+ Fetch C_CCMDSP into :TITAS00F
028100110304     C/END-EXEC
028200110304     C*
028300110304     C                   dow       sqlcod = *zeros
028400110304     C                   exsr      verifica
028500110304     C                   if        CHKREC = 'S'
028600110304     C                   exsr      scriviWF
028700110304     C                   endif
028800110304     C
028900110304     C/EXEC SQL
029000110304     C+ Fetch C_CCMDSP into :TITAS00F
029100110304     C/END-EXEC
029200110304     C*
029300110304     C                   enddo
029400110304     C*
029500110304     C/EXEC SQL
029600110304     C+ close C_CCMDSP
029700110304     C/END-EXEC
029800110304     C*
029900110304     C*
030000110304     C                   ENDSR
030100110304     C*------------------------------------------------------------------------*
030200111130
030300111130
030400111130
030500111130     C*------------------------------------------------------------------------*
030600111130     C* SQ_KSCDSP - Elaborazione x codice cliente fatturazione + data spedizione
030700111130     C*------------------------------------------------------------------------*
030800111130     C     SQ_KSCDSP     BEGSR
030900111130     C*
031000111130     C                   z-add     KUNI(I)       wParKSC           7 0
031100111130     C*
031200111130     C/EXEC SQL
031300111130     C+ declare C_KSCDSP cursor for
031400111130     C+ select * from titas00f
031500111130     C+ where tasksc = :wParKSC and
031600111130     C+ tasaas*10000+tasmgs between :DATADA and :DATAA
031700111130     C+ union
031800111130     C+ select * from titas10f
031900111130     C+ where tasksc = :wParKSC and
032000111130     C+ tasaas*10000+tasmgs between :DATADA and :DATAA
032100111130     C+ union
032200111130     C+ select * from titasP0f
032300111130     C+ where tasksc = :wParKSC and
032400111130     C+ tasaas*10000+tasmgs between :DATADA and :DATAA
032500111130     C+ for read only
032600111130     C/END-EXEC
032700111130     C
032800111130     C/EXEC SQL
032900111130     C+ open C_KSCDSP
033000111130     C/END-EXEC
033100111130     C
033200111130     C/EXEC SQL
033300111130     C+ Fetch C_KSCDSP into :TITAS00F
033400111130     C/END-EXEC
033500111130     C*
033600111130     C                   dow       sqlcod = *zeros
033700111130     C                   exsr      verifica
033800111130     C                   if        CHKREC = 'S'
033900111130     C                   exsr      scriviWF
034000111130     C                   endif
034100111130     C
034200111130     C/EXEC SQL
034300111130     C+ Fetch C_KSCDSP into :TITAS00F
034400111130     C/END-EXEC
034500111130     C*
034600111130     C                   enddo
034700111130     C*
034800111130     C/EXEC SQL
034900111130     C+ close C_KSCDSP
035000111130     C/END-EXEC
035100111130     C*
035200111130     C*
035300111130     C                   ENDSR
035400111130     C*------------------------------------------------------------------------*
035500110304
035600110304
035700110304
035800110304     C*------------------------------------------------------------------------*
035900110304     C* SQ_CCMDCM - Elaborazione x codice cliente mittente + data consegna merce
036000110304     C*------------------------------------------------------------------------*
036100110304     C     SQ_CCMDCM     BEGSR
036200110304     C*
036300110304     C                   z-add     KUNI(I)       wParCCM           7 0
036400110304     C*
036500110304     C/EXEC SQL
036600110304     C+ declare C_CCMDCM cursor for
036700110304     C+ select * from titas00f
036800110304     C+ where tasccm = :wParCCM and
036900110304     C+ tasdcm between :DACMDA and :DACMA
037000110304     C+ union
037100110304     C+ select * from titas10f
037200110304     C+ where tasccm = :wParCCM and
037300110304     C+ tasdcm between :DACMDA and :DACMA
037400110304     C+ union
037500110304     C+ select * from titasP0f
037600110304     C+ where tasccm = :wParCCM and
037700110304     C+ tasdcm between :DACMDA and :DACMA
037800110304     C+ for read only
037900110304     C/END-EXEC
038000110304     C
038100110304     C/EXEC SQL
038200110304     C+ open C_CCMDCM
038300110304     C/END-EXEC
038400110304     C
038500110304     C/EXEC SQL
038600110304     C+ Fetch C_CCMDCM into :TITAS00F
038700110304     C/END-EXEC
038800110304     C*
038900110304     C                   dow       sqlcod = *zeros
039000110304     C                   exsr      verifica
039100110304     C                   if        CHKREC = 'S'
039200110304     C                   exsr      scriviWF
039300110304     C                   endif
039400110304     C
039500110304     C/EXEC SQL
039600110304     C+ Fetch C_CCMDCM into :TITAS00F
039700110304     C/END-EXEC
039800110304     C*
039900110304     C                   enddo
040000110304     C*
040100110304     C/EXEC SQL
040200110304     C+ close C_CCMDCM
040300110304     C/END-EXEC
040400110304     C*
040500110304     C*
040600110304     C                   ENDSR
040700110304     C*------------------------------------------------------------------------*
040800111130
040900111130
041000111130
041100111130     C*------------------------------------------------------------------------*
041200111130     C* SQ_KSCDCM - Elaborazione x codice cliente fatturazione + data consegna merce
041300111130     C*------------------------------------------------------------------------*
041400111130     C     SQ_KSCDCM     BEGSR
041500111130     C*
041600111130     C                   z-add     KUNI(I)       wParKSC           7 0
041700111130     C*
041800111130     C/EXEC SQL
041900111130     C+ declare C_KSCDCM cursor for
042000111130     C+ select * from titas00f
042100111130     C+ where tasksc = :wParKSC and
042200111130     C+ tasdcm between :DACMDA and :DACMA
042300111130     C+ union
042400111130     C+ select * from titas10f
042500111130     C+ where tasksc = :wParKSC and
042600111130     C+ tasdcm between :DACMDA and :DACMA
042700111130     C+ union
042800111130     C+ select * from titasP0f
042900111130     C+ where tasksc = :wParKSC and
043000111130     C+ tasdcm between :DACMDA and :DACMA
043100111130     C+ for read only
043200111130     C/END-EXEC
043300111130     C
043400111130     C/EXEC SQL
043500111130     C+ open C_KSCDCM
043600111130     C/END-EXEC
043700111130     C
043800111130     C/EXEC SQL
043900111130     C+ Fetch C_KSCDCM into :TITAS00F
044000111130     C/END-EXEC
044100111130     C*
044200111130     C                   dow       sqlcod = *zeros
044300111130     C                   exsr      verifica
044400111130     C                   if        CHKREC = 'S'
044500111130     C                   exsr      scriviWF
044600111130     C                   endif
044700111130     C
044800111130     C/EXEC SQL
044900111130     C+ Fetch C_KSCDCM into :TITAS00F
045000111130     C/END-EXEC
045100111130     C*
045200111130     C                   enddo
045300111130     C*
045400111130     C/EXEC SQL
045500111130     C+ close C_KSCDCM
045600111130     C/END-EXEC
045700111130     C*
045800111130     C*
045900111130     C                   ENDSR
046000111130     C*------------------------------------------------------------------------*
046100001218
046200110304
046300110304
046400110304     C*------------------------------------------------------------------------*
046500110304     C* CHKCONS  - Verifica se bolla effettivamente consegnata
046600110304     C*------------------------------------------------------------------------*
046700110304     C     CHKCONS       BEGSR
046800110304     C*
046900110304     C                   setoff                                       4041
047000110304     C                   movel     *blanks       wEsito1           1
047100110304     C                   movel     *blanks       wEsito2           1
047200110304     C*
047300110304     C                   eval      wAAS = tasAAS
047400110304     C                   eval      wLNP = tasLNP
047500110304     C                   eval      wNRS = tasNRS
047600110304     C                   eval      wNSP = tasNSP
047700110304     C*
047800110304     C* Chiamata metodo GetLblTyp
047900110304     C                   eval      wEsito1 = %char(UBLBLSPE_GetLblTyp(
048000110304     C                                                wAAS
048100110304     C                                               :wLNP
048200110304     C                                               :wNRS
048300110304     C                                               :wNSP
048400110304     C                                               :pOutLblTyp
048500110304     C                                               :pOutAnnoBO
048600110304     C                                               :pOutLineaParBO
048700110304     C                                               :pOutSerieBO
048800110304     C                                               :pOutNumSpedBO
048900110304     C                                               :pOutDcmBO
049000110304     C                                               :pOutCcaBO
049100110304     C                                               :pOutRblBO))
049200110304     C* Se trattasi d bolla nn originale reperisco la relativa bolla originale
049300110304     C                   if        pOutLblTyp <> 'O'
049400110304     C                   eval      wAAS = pOutAnnoBO
049500110304     C                   eval      wLNP = pOutLineaParBO
049600110304     C                   eval      wNRS = pOutSerieBO
049700110304     C                   eval      wNSP = pOutNumSpedBO
049800110304     C                   endif
049900110304     C*
050000110304     C* Chiamata metodo GetLastChild
050100110304     C                   eval      wEsito2 = %char(UBLBLSPE_GetLastChild(
050200110304     C                                                wAAS
050300110304     C                                               :wLNP
050400110304     C                                               :wNRS
050500110304     C                                               :wNSP
050600110304     C                                               :pOutAnnoFI
050700110304     C                                               :pOutLineaParFI
050800110304     C                                               :pOutSerieFI
050900110304     C                                               :pOutNumSpedFI
051000110304     C                                               :pOutDcmFI
051100110304     C                                               :pOutCcaFI))
051200110304     C*
051300110304     C* Considerazioni sullo stato corrente della bolla
051400110304     C                   if        (wEsito2 = '9' and pOutDcmBO > *zeros)  or
051500110304     C                             (wEsito2 = '0' and pOutDcmFI > *zeros)
051600161205     C                   seton                                        40        * bolla chiusa
051700110304     C                   endif
051800110304     C*
051900110304     C                   if        (wEsito2 = '9' and pOutDcmBO > *zeros
052000110304     C                                            and pOutCcaBO = *blanks) or
052100110304     C                             (wEsito2 = '0' and pOutDcmFI > *zeros
052200110304     C                                            and pOutCcaFI = *blanks)
052300110304     C                   seton                                        41        * bolla cons. OK
052400110304     C                   endif
052500110304     C*
052600110304     C                   ENDSR
052700110304     C*------------------------------------------------------------------------*
052800110304
052900110304
053000110304
053100110304     C*------------------------------------------------------------------------*
053200110304     C* VERIFICA - Routine di verifica validità record corrente
053300110304     C*------------------------------------------------------------------------*
053400110304     C     VERIFICA      BEGSR
053500110304     C*
053600110304     C                   movel     'S'           CHKREC            1
053700110307     C*
053800110307     C                   movel     tasaas        wrkdata8          8 0
053900110307     C                   move      tasmgs        wrkdata8
054000110307     C                   movel     wrkdata8      wrkAAAAMM         6 0
054100110304     C*
054200110304     C* Verifico le date spedizione
054300110304     C                   if        CHKREC = 'S'
054400110304     C                   if        DATADA > *zeros
054500110304     C                   movel     tasaas        wrkdata           8 0
054600110304     C                   move      tasmgs        wrkdata
054700110304     C                   if        wrkdata < DATADA or
054800110304     C                             wrkdata > DATAA
054900110304     C                   movel     'N'           CHKREC
055000110304     C                   endif
055100110304     C                   endif
055200110304     C                   endif
055300110304     C*
055400110304     C* Verifico le date spedizione
055500110304     C                   if        CHKREC = 'S'
055600110304     C                   if        DACMDA > *zeros
055700110304     C                   if        tasdcm  < DACMDA or
055800110304     C                             tasdcm  > DACMA
055900110304     C                   movel     'N'           CHKREC
056000110304     C                   endif
056100110304     C                   endif
056200110304     C                   endif
056300110304     C*
056400110304     C* Verifico il codice cliente mittente
056500110304     C                   if        CHKREC = 'S'
056600110304     C                   if        wChkCli  = 'C'      AND
056700110304     C                             tasccm  <> KUNI(I)
056800110304     C                   movel     'N'           CHKREC
056900110304     C                   endif
057000110304     C                   endif
057100110304     C*
057200110304     C* Verifico il codice cliente fatturazione (in caso d 'K')
057300110304     C                   if        CHKREC = 'S'
057400110304     C                   if        wChkCli  = 'K'      AND
057500110304     C                             tasksc  <> KUNI(I)
057600110304     C                   movel     'N'           CHKREC
057700110304     C                   endif
057800110304     C                   endif
057900110304     C*
058000110304     C* Verifico il codice cliente fatturazione (in caso d 'E')
058100110304     C                   if        CHKREC = 'S'
058200110304     C                   if        TIPCLI  = 'E'       AND
058300110304     C                             wChkCli = 'K'       AND
058400110304     C                             tasksc  = tasccm
058500110304     C                   movel     'N'           CHKREC
058600110304     C                   endif
058700110304     C                   endif
058800110304     C*
058900110304     C* Verifico la serie
059000110304     C                   if        CHKREC = 'S'
059100110307     C                   if        NUMSER <> *blanks
059200110307     C                   if        tasnrs <> %dec(NUMSER : 2 : 0)
059300110304     C                   movel     'N'           CHKREC
059400110304     C                   endif
059500110304     C                   endif
059600110304     C                   endif
059700110304     C*
059800110304     C* Verifico il tipo bolla
059900110304     C                   if        CHKREC = 'S'
060000140326     C                   select
060100140326     C                   when      TIPBOL = '99'
060200140326     C                   when      TIPBOL = '98' AND
060300140326     C                             tastbl <> 'F1' AND tastbl <> 'A2'
060400140326     C                   movel     'N'           CHKREC
060500140326     C                   when      TIPBOL <> '99' AND
060600140326     C                             TIPBOL <> '98' AND
060700140326     C                             tastbl <> TIPBOL
060800140326     C                   movel     'N'           CHKREC
060900140326     C                   endsl
061000110304     C                   endif
061100110304     C*
061200110304     C* Verifico se richiesto nel lancio che la bolla sia consegnata
061300110304     C                   if        CHKREC = 'S'
061400110304     C                   if        FLGCONS = 'S'
061500110304     C                   exsr      CHKCONS
061600110304     C                   if        *in40 = *on
061700110304     C                   else
061800110304     C                   movel     'N'           CHKREC
061900110304     C                   endif
062000110304     C                   endif
062100110304     C                   endif
062200110304     C*
062300110304     C* Verifico se richiesto nel lancio che la bolla sia consegnata OK
062400110304     C                   if        CHKREC = 'S'
062500110304     C                   if        FLGCONS = 'O'
062600110304     C                   exsr      CHKCONS
062700110304     C                   if        *in41 = *on
062800110304     C                   else
062900110304     C                   movel     'N'           CHKREC
063000110304     C                   endif
063100110304     C                   endif
063200110304     C                   endif
063300110304     C*
063400110304     C* Verifico se richiesto nel lancio che la bolla NON sia consegnata OK
063500110304     C                   if        CHKREC = 'S'
063600110304     C                   if        FLGCONS = 'K'
063700110304     C                   exsr      CHKCONS
063800110304     C                   if        *in41 = *on
063900110304     C                   movel     'N'           CHKREC
064000110304     C                   endif
064100110304     C                   endif
064200110304     C                   endif
064300110304     C*
064400110304     C* Verifico se richiesto nel lancio che la bolla NON sia consegnata
064500110304     C                   if        CHKREC = 'S'
064600110304     C                   if        FLGCONS = 'P'
064700110304     C                   exsr      CHKCONS
064800110304     C                   if        *in40 = *on
064900110304     C                   movel     'N'           CHKREC
065000110304     C                   endif
065100110304     C                   endif
065200110304     C                   endif
065300110304     C*
065400110304     C                   ENDSR
065500110304     C*------------------------------------------------------------------------*
065600001218
065700001218
065800001218
065900001218     C*------------------------------------------------------------------------*
066000110307     C* SCRIVIWF - Routine di scrittura WFCLN10F
066100001218     C*------------------------------------------------------------------------*
066200100219     C     SCRIVIWF      BEGSR
066300100219     C*
066400110307     C                   clear                   WFCLN110
066500110307     C                   clear                   WFCLN120
066600150210     C                   clear                   WFCLN130
066700110307     C                   clear                   WFCLN_10
066800110307     C                   clear                   WFCLN_11
066900110307     C                   clear                   WFCLN_12
067000150210     C                   clear                   WFCLN_13
067100100219     C*
067200100219     C                   eval      dlyLNP = tasLNP
067300100219     C                   eval      dlyTSP = tasTSP
067400120309     C*
067500120309     C                   if        dlyTSP = 'D'
067600120309     C                   eval      dlyTSP = 'C'
067700120309     C                   endif
067800120309     C*
067900100219     C                   eval      dlyNAR = tasNZD
068000100219     C                   eval      dlyCAP = tasCAD
068100100219     C     KEYdly05_P    chain     wadly05l
068200100219     C                   if        %found(wadly05l)
068300100219     C                   eval      WCLNDLYTRC = DLYTRC
068400100219     C                   endif
068500100219     C*
068600100219     C                   eval      tblcod = 'PR'
068700100219     C                   eval      tblkey = tasprd
068800100219     C     KEYtab_C      chain     tabel00f
068900100219     C                   if        %found(tabel00f)
069000100219     C                   eval      DSPR = tbluni
069100100219     C                   eval      tblcod = '14'
069200100219     C                   eval      tblkey = §PRCRE
069300100219     C     KEYtab_C      chain     tabel00f
069400100219     C                   if        %found(tabel00f)
069500100219     C                   eval      DS14 = tbluni
069600110307     C                   eval      wclnREG = §14DES
069700100219     C                   endif
069800100219     C                   endif
069900100219     C*
070000100219     C                   z-add     *zeros        wANT
070100100222     C                   z-add     *zeros        wM12
070200100219     C                   z-add     *zeros        wOKS
070300150210     C                   z-add     *zeros        wR12
070400150210     C                   z-add     *zeros        wR24
070500100219     C                   z-add     *zeros        wRIT
070600130411     C*
070700130411     C*
070800130411     C* Verifico tipo affidabilità richiesta
070900130411     C                   select
071000130411     C*
071100130411     C* - affidabilità consegna
071200130411     C                   when      TIPAFFID = 'C'
071300001218     C*
071400100219     C* ...in anticipo
071500100219     C                   select
071600130411     C                   when      tasnci >= -999 AND
071700130411     C                             tasnci <= -24
071800100219     C                   add       1             wANT
071900100222     C*
072000100222     C* ...border-line
072100130411     C                   when      tasnci >  -24  AND
072200130411     C                             tasnci <= -12
072300100222     C                   add       1             wM12
072400100219     C*
072500100219     C* ...in orario
072600130411     C                   when      tasnci >  -12  AND
072700130411     C                             tasnci <=   0
072800100219     C                   add       1             wOKS
072900150210     C*
073000150210     C* ...in ritardo entro 12 ore
073100150210     C                   when      tasnci >    0  AND
073200150210     C                             tasnci <=  12
073300150210     C                   add       1             wR12
073400150210     C*
073500150210     C* ...in ritardo entro 24 ore
073600150210     C                   when      tasnci >   12  AND
073700150210     C                             tasnci <=  24
073800150210     C                   add       1             wR24
073900150210     C*
074000150210     C* ...in ritardo "molto"
074100150210     C                   when      tasnci >   24
074200150210     C                   add       1             wRIT
074300150210     C                   endsl
074400130411     C*
074500130411     C* - affidabilità cliente (*DEFAULT)
074600130411     C                   other
074700130411     C*
074800130411     C* ...in anticipo
074900130411     C                   select
075000150210     C                   when      tasnrc >= -999 AND
075100150210     C                             tasnrc <= -24
075200130411     C                   add       1             wANT
075300130411     C*
075400130411     C* ...border-line
075500150210     C                   when      tasnrc >  -24  AND
075600150210     C                             tasnrc <= -12
075700130411     C                   add       1             wM12
075800130411     C*
075900130411     C* ...in orario
076000150210     C                   when      tasnrc >  -12  AND
076100150210     C                             tasnrc <=   0
076200130411     C                   add       1             wOKS
076300150210     C*
076400150210     C* ...in ritardo entro 12 ore
076500150210     C                   when      tasnrc >    0  AND
076600150210     C                             tasnrc <=  12
076700150210     C                   add       1             wR12
076800150210     C*
076900150210     C* ...in ritardo entro 24 ore
077000150210     C                   when      tasnrc >   12  AND
077100150210     C                             tasnrc <=  24
077200150210     C                   add       1             wR24
077300150210     C*
077400150210     C* ...in ritardo "molto"
077500150210     C                   when      tasnrc >   24
077600150210     C                   add       1             wRIT
077700150210     C                   endsl
077800130411     C*
077900130411     C                   endsl
078000100219     C*
078100100219     C* Conteggio spedizioni DIR/GIAC/CCA
078200100219     C                   eval      dtasflo = tasflo
078300100219     C                   if        tasCCA   <> *blanks OR
078400100219     C                             tasFGC   <> *blanks OR
078500100219     C                             §FLONAV  <> *blanks
078600100219     C                   eval      wclnANOM = 'S'
078700100219     C                   endif
078800100219     C*
078900111130     C                   eval      wclnCCM    = tasccm
079000111130     C                   if        wChkCli = 'K'
079100111130     C                   eval      wclnCCM    = tasksc
079200111130     C                   endif
079300130403     C*
079400130403     C* Se richiesta estrazione per "unificante" (di qualunque tipo come CCM considero sempre
079500130403     C* l'unificante)
079600130403     C
079700130403     C                   if        FLGUNI <> *blanks
079800130403     C                   eval      wclnCCM    = CODCLI
079900130403     C                   endif
080000111130     C*
080100100219     C                   eval      wclnAAAAMM = wrkAAAAMM
080200100219     C                   eval      wclnPRD    = tasprd
080300100219     C                   eval      wclnCAD    = tascad
080400100219     C                   eval      wclnISO    = tasfin
080500110307     C                   eval      wclnAAS    = tasaas
080600110307     C                   eval      wclnLNP    = taslnp
080700110307     C                   eval      wclnNRS    = tasnrs
080800110307     C                   eval      wclnNSP    = tasnsp
080900110307     C*
081000110307     C                   select
081100110307     C                   when      *in26
081200110307     C     KEYcln_12C    chain     wfcln12l
081300110307     C                   if        %found(wfcln12l)
081400110307     C                   eval      WFCLN_10 = WFCLN_12
081500100219     C                   add       wANT          wclnANT
081600100222     C                   add       wM12          wclnM12
081700100219     C                   add       wOKS          wclnOKS
081800150210     C                   add       wR12          wclnR12
081900150210     C                   add       wR24          wclnR24
082000100219     C                   add       wRIT          wclnRIT
082100100219     C                   add       1             wclnTOT
082200110307     C                   eval      WFCLN_12 = WFCLN_10
082300110307     C                   update    WFCLN120
082400100219     C                   else
082500100219     C                   add       wANT          wclnANT
082600100222     C                   add       wM12          wclnM12
082700100219     C                   add       wOKS          wclnOKS
082800150210     C                   add       wR12          wclnR12
082900150210     C                   add       wR24          wclnR24
083000100219     C                   add       wRIT          wclnRIT
083100100219     C                   add       1             wclnTOT
083200110307     C                   eval      WFCLN_12 = WFCLN_10
083300110307     C                   write     WFCLN120
083400100219     C                   endif
083500110307     C*
083600110307     C                   when      *in27
083700150211     C     KEYcln_11C    chain     wfcln11l
083800150211     C                   if        %found(wfcln11l)
083900150211     C                   eval      WFCLN_10 = WFCLN_11
084000150211     C                   add       wANT          wclnANT
084100150211     C                   add       wM12          wclnM12
084200150211     C                   add       wOKS          wclnOKS
084300150211     C                   add       wR12          wclnR12
084400150211     C                   add       wR24          wclnR24
084500150211     C                   add       wRIT          wclnRIT
084600150211     C                   add       1             wclnTOT
084700150211     C                   eval      WFCLN_11 = WFCLN_10
084800150211     C                   update    WFCLN110
084900150211     C                   else
085000150211     C                   add       wANT          wclnANT
085100150211     C                   add       wM12          wclnM12
085200150211     C                   add       wOKS          wclnOKS
085300150211     C                   add       wR12          wclnR12
085400150211     C                   add       wR24          wclnR24
085500150211     C                   add       wRIT          wclnRIT
085600150211     C                   add       1             wclnTOT
085700150211     C                   eval      WFCLN_11 = WFCLN_10
085800150211     C                   write     WFCLN110
085900150211     C                   endif
086000150210     C*
086100150210     C                   when      *in28
086200150211     C     KEYcln_13C    chain     wfcln13l
086300150211     C                   if        %found(wfcln13l)
086400150211     C                   eval      WFCLN_10 = WFCLN_13
086500150211     C                   add       wANT          wclnANT
086600150211     C                   add       wM12          wclnM12
086700150211     C                   add       wOKS          wclnOKS
086800150211     C                   add       wR12          wclnR12
086900150211     C                   add       wR24          wclnR24
087000150211     C                   add       wRIT          wclnRIT
087100150211     C                   add       1             wclnTOT
087200150211     C                   eval      WFCLN_13 = WFCLN_10
087300150211     C                   update    WFCLN130
087400150211     C                   else
087500150211     C                   add       wANT          wclnANT
087600150211     C                   add       wM12          wclnM12
087700150211     C                   add       wOKS          wclnOKS
087800150211     C                   add       wR12          wclnR12
087900150211     C                   add       wR24          wclnR24
088000150211     C                   add       wRIT          wclnRIT
088100150211     C                   add       1             wclnTOT
088200150211     C                   eval      WFCLN_13 = WFCLN_10
088300150211     C                   write     WFCLN130
088400150211     C                   endif
088500110307     C*
088600110307     C                   endsl
088700001218     C*
088800001218     C                   ENDSR
088900001221     C*------------------------------------------------------------------------*
089000110304
089100110304
089200110304
089300110304     C*------------------------------------------------------------------------*
089400110304     C* WRITIVGD - Routine di scrittura file TIVGD (file VAS generico download)
089500110304     C*------------------------------------------------------------------------*
089600110304     C     WRITIVGD      BEGSR
089700110304     C*
089800110307     C* Leggo tutto il file WFCLN11L in QTEMP del job corrente
089900110307     C     *loval        setll     WFCLN11L
090000110307     C                   read      WFCLN11L
090100110307     C                   dow       not %eof(WFCLN11L)
090200110307     C*
090300161205     C                   clear                   iDSVGD
090400110307     C                   eval      vgdDTA = %subst(WFCLN_11:1:%size(WFCLN_11))
090500110304     C                   eval      vgdTIP = TIPFILE
090600110304     C                   movel     *all'0'       vgdKSU
090700110304     C                   move      CODCLIVAS     vgdKSU
090800110304     C                   eval      vgdTSC = 'WW'
090900110304     C                   eval      vgdDAT = datcor
091000161205     C                   eval      vgdPRG = oPRG
091100161205     C                   eval      vgdPGM = procname
091200161205     C*
091300161205     C                   CALL      'TIS7VASR1'
091400161205     C                   PARM      '2'           iOPZ
091500161205     C                   PARM                    iDSVGD
091600161205     C                   PARM                    oPRG
091700161205     C                   PARM                    oOK
091800161205     C*
091900161205     C* Verifico esito chiamata
092000161205     C                   if        %error OR oOK = *off
092100161205     C                   exsr      EXEERR
092200161205     C                   endif
092300110304     C*
092400110307     C                   read      WFCLN11L
092500110307     C                   enddo
092600110307     C*
092700110304     C                   ENDSR
092800110304     C*------------------------------------------------------------------------*
092900120214
093000120214
093100120214
093200120214     C*------------------------------------------------------------------------*
093300120214     C* CARTBL - Routine di caricamento dati tabellati
093400120214     C*------------------------------------------------------------------------*
093500120214     C     CARTBL        BEGSR
093600120214     C*
093700120214     C                   Z-ADD     1             I                 4 0
093800120214     C                   MOVEL     CODCLI        KUNI(I)
093900130403     C*
094000130403     C                   SELECT
094100130403     C                   WHEN      FLGUNI = 'U'
094200120214     C                   MOVEL     '3K'          tblCOD
094300120214     C     KEYtab_P      CHAIN     TABEL                              31
094400120214     C     *IN31         DOWEQ     '0'
094500120214     C     TBLFLG        IFEQ      ' '
094600120214     C                   MOVEL     TBLUNI        DS3K
094700120214     C     §3KCKS        IFEQ      CODCLI
094800120214     C                   ADD       1             I
094900120214     C                   MOVEL     TBLKEY        wKUNI             7 0
095000120214     C     wKUNI         LOOKUP    KUNI                                   55
095100120214     C                   IF        %equal
095200120214     C                   ELSE
095300120214     C                   Z-ADD     wKUNI         KUNI(I)
095400120214     C                   ENDIF
095500120214     C                   ENDIF
095600120214     C                   ENDIF
095700120214     C     KEYtab_P      READE     TABEL                                  31
095800120214     C                   ENDDO
095900130403     C*
096000130403     C                   WHEN      FLGUNI = 'T'
096100130403     C                   CLEAR                   TIBS10DS
096200130403     C                   EVAL      D10DRF = datcor                              *data riferimento
096300130403     C                   EVAL      D10TLE = 'ST'                                *tipo legame
096400130403     C                   EVAL      D10PAF = 'F'                                 *cerca se padre
096500130403     C                   EVAL      D10COD = CODCLI
096600130403     C                   CALL      'TIBS10R'
096700130403     C                   PARM                    TIBS10DS
096800130403     C                   IF        D10ERR = *BLANKS                             *Padre
096900130403     C                   Z-ADD     1             I
097000130403     C                   DOW       skc(I) > *zeros
097100130403     C                   Z-ADD     skc(I)        KUNI(I)
097200130403     C                   ADD       1             I
097300130403     C                   ENDDO
097400130403     C                   ENDIF
097500130403     C*
097600130403     C                   ENDSL
097700120214     C*
097800120214     C                   ENDSR
097900120214     C*---------------------------------------------------------------*
098000161205
098100161205
098200161205
098300161205     C*------------------------------------------------------------------------*
098400161205     C* EXEERR - Routine di esecuzione azioni su Errore
098500161205     C*------------------------------------------------------------------------*
098600161205     C     EXEERR        BEGSR
098700161205     C*
098800161205     C                   dump(A)
098900161205     C                   rolbk
099000161205     C                   seton                                        lr
099100161205     C                   return
099200161205     C*
099300161205     C                   ENDSR
099400161205     C*------------------------------------------------------------------------*
099500120214
099600001218
099700001218
099800001218     C*------------------------------------------------------------------------*
099900001218     C* *INZSR - ROUTINE INIZIALE
100000001218     C*------------------------------------------------------------------------*
100100001218     C     *INZSR        BEGSR
100200001218     C*
100300001218     C     *ENTRY        PLIST
100400100219     C                   PARM                    KPJBA
100500001218     C*
100600100219     C                   MOVEL     KPJBU         DSINPUT
100700100219     C                   EVAL      tblKUT = 1
100800110307     C*
100900110307     C* Determino la data corrente
101000110307     C                   Z-ADD     *zeros        datcor            8 0
101100110307     C                   EVAL      datcor = %dec(%date() : *ISO)
101200001218     C*
101300001218     C* Definizioni chiavi
101400100219     C*
101500100219     C     KEYdly05_P    KLIST
101600100219     C                   KFLD                    dlyLNP
101700100219     C                   KFLD                    dlyTSP
101800100219     C                   KFLD                    dlyNAR
101900100219     C                   KFLD                    dlyCAP
102000120214     C*
102100120214     C     KEYtab_P      KLIST
102200120214     C                   KFLD                    tblKUT
102300120214     C                   KFLD                    tblCOD
102400001218     C*
102500100219     C     KEYtab_C      KLIST
102600100219     C                   KFLD                    tblKUT
102700100219     C                   KFLD                    tblCOD
102800100219     C                   KFLD                    tblKEY
102900110307     C*
103000110307     C     KEYcln_11C    KLIST
103100110307     C                   KFLD                    wclnCCM
103200110307     C                   KFLD                    wclnAAAAMM
103300110307     C                   KFLD                    wclnAAS
103400110307     C                   KFLD                    wclnLNP
103500110307     C                   KFLD                    wclnNRS
103600110307     C                   KFLD                    wclnNSP
103700030704     C*
103800110307     C     KEYcln_12C    KLIST
103900100219     C                   KFLD                    wclnCCM
104000100219     C                   KFLD                    wclnAAAAMM
104100110307     C                   KFLD                    wclnREG
104200100219     C                   KFLD                    wclnPRD
104300100219     C                   KFLD                    wclnCAD
104400100219     C                   KFLD                    wclnISO
104500100219     C                   KFLD                    wclnANOM
104600150210     C*
104700150210     C     KEYcln_13C    KLIST
104800150210     C                   KFLD                    wclnCCM
104900150210     C                   KFLD                    wclnAAAAMM
105000150210     C                   KFLD                    wclnREG
105100150210     C                   KFLD                    wclnPRD
105200100219     C*
105300001218     C                   ENDSR

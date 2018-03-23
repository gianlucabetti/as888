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
012600161205     C                   if        oOK = *on AND oPRG <> *blanks
012700161205     C                   movel     'S'           wProcedi
012800161205     C                   endif
012900110307     C*
013000161205     C* Se OK => Popolo il file d output
013100110307     C                   if        wProcedi = 'S'
013200110307     C                   exsr      WRITIVGD
013300110307     C                   endif
013400161205     C*
013500161205     C* Finalizzo la transazione
013600161205     C                   CALL      'TIS7VASR1'
013700161205     C                   PARM      '3'           iOPZ
013800161205     C                   PARM                    iDSVGD
013900161205     C                   PARM                    oPRG
014000161205     C                   PARM                    oOK
014100161205     C*
014200161205     C* Verifico esito chiamata
014300161205     C                   if        %error OR oOK = *off
014400161205     C                   exsr      EXEERR
014500161205     C                   endif
014600001218     C*
014700001218     C                   seton                                        LR
014800110304
014900110304
015000110304
015100110304     C*------------------------------------------------------------------------*
015200110304     C* CHKPAR - Routine di controllo parametri ricevuti in input
015300110304     C*------------------------------------------------------------------------*
015400110304     C     CHKPAR        BEGSR
015500110304     C*
015600110304     C* Verifico le date
015700110304     C                   if        DATADA > *zeros AND
015800110304     C                             DATAA  = *zeros
015900110304     C                   movel     *all'9'       DATAA
016000110304     C                   endif
016100110304     C                   if        DACMDA > *zeros AND
016200110304     C                             DACMA  = *zeros
016300110304     C                   movel     *all'9'       DACMA
016400110304     C                   endif
016500110307     C*
016600110307     C* Verifico il livello d dettaglio richiesto
016700150210     C                   setoff                                       262728
016800110307     C                   select
016900110307     C                   when      TIPDET = 'C'                                 * x CAP
017000110307     C                   seton                                        26
017100110307     C                   when      TIPDET = 'S'                                 * x SPED
017200110307     C                   seton                                        27
017300150210     C                   when      TIPDET = 'P'                                 * x PROV
017400150210     C                   seton                                        28
017500110307     C                   endsl
017600110304     C*
017700110304     C                   ENDSR
017800110304     C*------------------------------------------------------------------------*
017900110304
018000110304
018100110304
018200110304     C*------------------------------------------------------------------------*
018300110304     C* PROCEDI - Routine principale
018400110304     C*------------------------------------------------------------------------*
018500110304     C     PROCEDI       BEGSR
018600110304     C*
018700110304     C* Se richiesta elaborazione x codice cliente mittente...
018800110304     C                   select
018900110304     C                   when      TIPCLI = *blanks
019000110304     C                   exsr      EXECCM
019100110304     C*
019200110304     C* Se richiesta elaborazione x codice cliente fatturazione...
019300110304     C                   when      TIPCLI = 'K'
019400111130     C                   exsr      EXEKSC
019500110304     C*
019600110304     C* Se richiesta elaborazione x entrambi: codice cliente mittente + fatturazione...
019700110304     C                   when      TIPCLI = 'E'
019800110304     C                   exsr      EXECCM
019900111130     C                   exsr      EXEKSC
020000110304     C*
020100110304     C                   endsl
020200110304     C*
020300110304     C                   ENDSR
020400110304     C*------------------------------------------------------------------------*
020500110304
020600110304
020700110304
020800110304     C*------------------------------------------------------------------------*
020900110304     C* EXECCM   - Elaborazione x codice cliente mittente
021000110304     C*------------------------------------------------------------------------*
021100110304     C     EXECCM        BEGSR
021200110304     C*
021300110304     C                   movel     'C'           wChkCli           1
021400110304     C*
021500110304     C                   select
021600110304     C* ...se richiesta elaborazione x data spedizione
021700110304     C                   when      DATADA > *zeros
021800110304     C                   exsr      SQ_CCMDSP
021900110304     C* ...se richiesta elaborazione x data consegna merce
022000110304     C                   when      DACMDA > *zeros
022100110304     C                   exsr      SQ_CCMDCM
022200110304     C                   endsl
022300110304     C*
022400110304     C                   ENDSR
022500110304     C*------------------------------------------------------------------------*
022600111130
022700111130
022800111130
022900111130     C*------------------------------------------------------------------------*
023000111130     C* EXEKSC   - Elaborazione x codice cliente fatturazione
023100111130     C*------------------------------------------------------------------------*
023200111130     C     EXEKSC        BEGSR
023300111130     C*
023400111130     C                   movel     'K'           wChkCli           1
023500111130     C*
023600111130     C                   select
023700111130     C* ...se richiesta elaborazione x data spedizione
023800111130     C                   when      DATADA > *zeros
023900111130     C                   exsr      SQ_KSCDSP
024000111130     C* ...se richiesta elaborazione x data consegna merce
024100111130     C                   when      DACMDA > *zeros
024200111130     C                   exsr      SQ_KSCDCM
024300111130     C                   endsl
024400111130     C*
024500111130     C                   ENDSR
024600111130     C*------------------------------------------------------------------------*
024700110304
024800110304
024900110304
025000110304     C*------------------------------------------------------------------------*
025100110304     C* SQ_CCMDSP - Elaborazione x codice cliente mittente + data spedizione
025200110304     C*------------------------------------------------------------------------*
025300110304     C     SQ_CCMDSP     BEGSR
025400110304     C*
025500110304     C                   z-add     KUNI(I)       wParCCM           7 0
025600110304     C*
025700110304     C/EXEC SQL
025800110304     C+ declare C_CCMDSP cursor for
025900110304     C+ select * from titas00f
026000110304     C+ where tasccm = :wParCCM and
026100110304     C+ tasaas*10000+tasmgs between :DATADA and :DATAA
026200110304     C+ union
026300110304     C+ select * from titas10f
026400110304     C+ where tasccm = :wParCCM and
026500110304     C+ tasaas*10000+tasmgs between :DATADA and :DATAA
026600110304     C+ union
026700110304     C+ select * from titasP0f
026800110304     C+ where tasccm = :wParCCM and
026900110304     C+ tasaas*10000+tasmgs between :DATADA and :DATAA
027000110304     C+ for read only
027100110304     C/END-EXEC
027200110304     C
027300110304     C/EXEC SQL
027400110304     C+ open C_CCMDSP
027500110304     C/END-EXEC
027600110304     C
027700110304     C/EXEC SQL
027800110304     C+ Fetch C_CCMDSP into :TITAS00F
027900110304     C/END-EXEC
028000110304     C*
028100110304     C                   dow       sqlcod = *zeros
028200110304     C                   exsr      verifica
028300110304     C                   if        CHKREC = 'S'
028400110304     C                   exsr      scriviWF
028500110304     C                   endif
028600110304     C
028700110304     C/EXEC SQL
028800110304     C+ Fetch C_CCMDSP into :TITAS00F
028900110304     C/END-EXEC
029000110304     C*
029100110304     C                   enddo
029200110304     C*
029300110304     C/EXEC SQL
029400110304     C+ close C_CCMDSP
029500110304     C/END-EXEC
029600110304     C*
029700110304     C*
029800110304     C                   ENDSR
029900110304     C*------------------------------------------------------------------------*
030000111130
030100111130
030200111130
030300111130     C*------------------------------------------------------------------------*
030400111130     C* SQ_KSCDSP - Elaborazione x codice cliente fatturazione + data spedizione
030500111130     C*------------------------------------------------------------------------*
030600111130     C     SQ_KSCDSP     BEGSR
030700111130     C*
030800111130     C                   z-add     KUNI(I)       wParKSC           7 0
030900111130     C*
031000111130     C/EXEC SQL
031100111130     C+ declare C_KSCDSP cursor for
031200111130     C+ select * from titas00f
031300111130     C+ where tasksc = :wParKSC and
031400111130     C+ tasaas*10000+tasmgs between :DATADA and :DATAA
031500111130     C+ union
031600111130     C+ select * from titas10f
031700111130     C+ where tasksc = :wParKSC and
031800111130     C+ tasaas*10000+tasmgs between :DATADA and :DATAA
031900111130     C+ union
032000111130     C+ select * from titasP0f
032100111130     C+ where tasksc = :wParKSC and
032200111130     C+ tasaas*10000+tasmgs between :DATADA and :DATAA
032300111130     C+ for read only
032400111130     C/END-EXEC
032500111130     C
032600111130     C/EXEC SQL
032700111130     C+ open C_KSCDSP
032800111130     C/END-EXEC
032900111130     C
033000111130     C/EXEC SQL
033100111130     C+ Fetch C_KSCDSP into :TITAS00F
033200111130     C/END-EXEC
033300111130     C*
033400111130     C                   dow       sqlcod = *zeros
033500111130     C                   exsr      verifica
033600111130     C                   if        CHKREC = 'S'
033700111130     C                   exsr      scriviWF
033800111130     C                   endif
033900111130     C
034000111130     C/EXEC SQL
034100111130     C+ Fetch C_KSCDSP into :TITAS00F
034200111130     C/END-EXEC
034300111130     C*
034400111130     C                   enddo
034500111130     C*
034600111130     C/EXEC SQL
034700111130     C+ close C_KSCDSP
034800111130     C/END-EXEC
034900111130     C*
035000111130     C*
035100111130     C                   ENDSR
035200111130     C*------------------------------------------------------------------------*
035300110304
035400110304
035500110304
035600110304     C*------------------------------------------------------------------------*
035700110304     C* SQ_CCMDCM - Elaborazione x codice cliente mittente + data consegna merce
035800110304     C*------------------------------------------------------------------------*
035900110304     C     SQ_CCMDCM     BEGSR
036000110304     C*
036100110304     C                   z-add     KUNI(I)       wParCCM           7 0
036200110304     C*
036300110304     C/EXEC SQL
036400110304     C+ declare C_CCMDCM cursor for
036500110304     C+ select * from titas00f
036600110304     C+ where tasccm = :wParCCM and
036700110304     C+ tasdcm between :DACMDA and :DACMA
036800110304     C+ union
036900110304     C+ select * from titas10f
037000110304     C+ where tasccm = :wParCCM and
037100110304     C+ tasdcm between :DACMDA and :DACMA
037200110304     C+ union
037300110304     C+ select * from titasP0f
037400110304     C+ where tasccm = :wParCCM and
037500110304     C+ tasdcm between :DACMDA and :DACMA
037600110304     C+ for read only
037700110304     C/END-EXEC
037800110304     C
037900110304     C/EXEC SQL
038000110304     C+ open C_CCMDCM
038100110304     C/END-EXEC
038200110304     C
038300110304     C/EXEC SQL
038400110304     C+ Fetch C_CCMDCM into :TITAS00F
038500110304     C/END-EXEC
038600110304     C*
038700110304     C                   dow       sqlcod = *zeros
038800110304     C                   exsr      verifica
038900110304     C                   if        CHKREC = 'S'
039000110304     C                   exsr      scriviWF
039100110304     C                   endif
039200110304     C
039300110304     C/EXEC SQL
039400110304     C+ Fetch C_CCMDCM into :TITAS00F
039500110304     C/END-EXEC
039600110304     C*
039700110304     C                   enddo
039800110304     C*
039900110304     C/EXEC SQL
040000110304     C+ close C_CCMDCM
040100110304     C/END-EXEC
040200110304     C*
040300110304     C*
040400110304     C                   ENDSR
040500110304     C*------------------------------------------------------------------------*
040600111130
040700111130
040800111130
040900111130     C*------------------------------------------------------------------------*
041000111130     C* SQ_KSCDCM - Elaborazione x codice cliente fatturazione + data consegna merce
041100111130     C*------------------------------------------------------------------------*
041200111130     C     SQ_KSCDCM     BEGSR
041300111130     C*
041400111130     C                   z-add     KUNI(I)       wParKSC           7 0
041500111130     C*
041600111130     C/EXEC SQL
041700111130     C+ declare C_KSCDCM cursor for
041800111130     C+ select * from titas00f
041900111130     C+ where tasksc = :wParKSC and
042000111130     C+ tasdcm between :DACMDA and :DACMA
042100111130     C+ union
042200111130     C+ select * from titas10f
042300111130     C+ where tasksc = :wParKSC and
042400111130     C+ tasdcm between :DACMDA and :DACMA
042500111130     C+ union
042600111130     C+ select * from titasP0f
042700111130     C+ where tasksc = :wParKSC and
042800111130     C+ tasdcm between :DACMDA and :DACMA
042900111130     C+ for read only
043000111130     C/END-EXEC
043100111130     C
043200111130     C/EXEC SQL
043300111130     C+ open C_KSCDCM
043400111130     C/END-EXEC
043500111130     C
043600111130     C/EXEC SQL
043700111130     C+ Fetch C_KSCDCM into :TITAS00F
043800111130     C/END-EXEC
043900111130     C*
044000111130     C                   dow       sqlcod = *zeros
044100111130     C                   exsr      verifica
044200111130     C                   if        CHKREC = 'S'
044300111130     C                   exsr      scriviWF
044400111130     C                   endif
044500111130     C
044600111130     C/EXEC SQL
044700111130     C+ Fetch C_KSCDCM into :TITAS00F
044800111130     C/END-EXEC
044900111130     C*
045000111130     C                   enddo
045100111130     C*
045200111130     C/EXEC SQL
045300111130     C+ close C_KSCDCM
045400111130     C/END-EXEC
045500111130     C*
045600111130     C*
045700111130     C                   ENDSR
045800111130     C*------------------------------------------------------------------------*
045900001218
046000110304
046100110304
046200110304     C*------------------------------------------------------------------------*
046300110304     C* CHKCONS  - Verifica se bolla effettivamente consegnata
046400110304     C*------------------------------------------------------------------------*
046500110304     C     CHKCONS       BEGSR
046600110304     C*
046700110304     C                   setoff                                       4041
046800110304     C                   movel     *blanks       wEsito1           1
046900110304     C                   movel     *blanks       wEsito2           1
047000110304     C*
047100110304     C                   eval      wAAS = tasAAS
047200110304     C                   eval      wLNP = tasLNP
047300110304     C                   eval      wNRS = tasNRS
047400110304     C                   eval      wNSP = tasNSP
047500110304     C*
047600110304     C* Chiamata metodo GetLblTyp
047700110304     C                   eval      wEsito1 = %char(UBLBLSPE_GetLblTyp(
047800110304     C                                                wAAS
047900110304     C                                               :wLNP
048000110304     C                                               :wNRS
048100110304     C                                               :wNSP
048200110304     C                                               :pOutLblTyp
048300110304     C                                               :pOutAnnoBO
048400110304     C                                               :pOutLineaParBO
048500110304     C                                               :pOutSerieBO
048600110304     C                                               :pOutNumSpedBO
048700110304     C                                               :pOutDcmBO
048800110304     C                                               :pOutCcaBO
048900110304     C                                               :pOutRblBO))
049000110304     C* Se trattasi d bolla nn originale reperisco la relativa bolla originale
049100110304     C                   if        pOutLblTyp <> 'O'
049200110304     C                   eval      wAAS = pOutAnnoBO
049300110304     C                   eval      wLNP = pOutLineaParBO
049400110304     C                   eval      wNRS = pOutSerieBO
049500110304     C                   eval      wNSP = pOutNumSpedBO
049600110304     C                   endif
049700110304     C*
049800110304     C* Chiamata metodo GetLastChild
049900110304     C                   eval      wEsito2 = %char(UBLBLSPE_GetLastChild(
050000110304     C                                                wAAS
050100110304     C                                               :wLNP
050200110304     C                                               :wNRS
050300110304     C                                               :wNSP
050400110304     C                                               :pOutAnnoFI
050500110304     C                                               :pOutLineaParFI
050600110304     C                                               :pOutSerieFI
050700110304     C                                               :pOutNumSpedFI
050800110304     C                                               :pOutDcmFI
050900110304     C                                               :pOutCcaFI))
051000110304     C*
051100110304     C* Considerazioni sullo stato corrente della bolla
051200110304     C                   if        (wEsito2 = '9' and pOutDcmBO > *zeros)  or
051300110304     C                             (wEsito2 = '0' and pOutDcmFI > *zeros)
051400161205     C                   seton                                        40        * bolla chiusa
051500110304     C                   endif
051600110304     C*
051700110304     C                   if        (wEsito2 = '9' and pOutDcmBO > *zeros
051800110304     C                                            and pOutCcaBO = *blanks) or
051900110304     C                             (wEsito2 = '0' and pOutDcmFI > *zeros
052000110304     C                                            and pOutCcaFI = *blanks)
052100110304     C                   seton                                        41        * bolla cons. OK
052200110304     C                   endif
052300110304     C*
052400110304     C                   ENDSR
052500110304     C*------------------------------------------------------------------------*
052600110304
052700110304
052800110304
052900110304     C*------------------------------------------------------------------------*
053000110304     C* VERIFICA - Routine di verifica validità record corrente
053100110304     C*------------------------------------------------------------------------*
053200110304     C     VERIFICA      BEGSR
053300110304     C*
053400110304     C                   movel     'S'           CHKREC            1
053500110307     C*
053600110307     C                   movel     tasaas        wrkdata8          8 0
053700110307     C                   move      tasmgs        wrkdata8
053800110307     C                   movel     wrkdata8      wrkAAAAMM         6 0
053900110304     C*
054000110304     C* Verifico le date spedizione
054100110304     C                   if        CHKREC = 'S'
054200110304     C                   if        DATADA > *zeros
054300110304     C                   movel     tasaas        wrkdata           8 0
054400110304     C                   move      tasmgs        wrkdata
054500110304     C                   if        wrkdata < DATADA or
054600110304     C                             wrkdata > DATAA
054700110304     C                   movel     'N'           CHKREC
054800110304     C                   endif
054900110304     C                   endif
055000110304     C                   endif
055100110304     C*
055200110304     C* Verifico le date spedizione
055300110304     C                   if        CHKREC = 'S'
055400110304     C                   if        DACMDA > *zeros
055500110304     C                   if        tasdcm  < DACMDA or
055600110304     C                             tasdcm  > DACMA
055700110304     C                   movel     'N'           CHKREC
055800110304     C                   endif
055900110304     C                   endif
056000110304     C                   endif
056100110304     C*
056200110304     C* Verifico il codice cliente mittente
056300110304     C                   if        CHKREC = 'S'
056400110304     C                   if        wChkCli  = 'C'      AND
056500110304     C                             tasccm  <> KUNI(I)
056600110304     C                   movel     'N'           CHKREC
056700110304     C                   endif
056800110304     C                   endif
056900110304     C*
057000110304     C* Verifico il codice cliente fatturazione (in caso d 'K')
057100110304     C                   if        CHKREC = 'S'
057200110304     C                   if        wChkCli  = 'K'      AND
057300110304     C                             tasksc  <> KUNI(I)
057400110304     C                   movel     'N'           CHKREC
057500110304     C                   endif
057600110304     C                   endif
057700110304     C*
057800110304     C* Verifico il codice cliente fatturazione (in caso d 'E')
057900110304     C                   if        CHKREC = 'S'
058000110304     C                   if        TIPCLI  = 'E'       AND
058100110304     C                             wChkCli = 'K'       AND
058200110304     C                             tasksc  = tasccm
058300110304     C                   movel     'N'           CHKREC
058400110304     C                   endif
058500110304     C                   endif
058600110304     C*
058700110304     C* Verifico la serie
058800110304     C                   if        CHKREC = 'S'
058900110307     C                   if        NUMSER <> *blanks
059000110307     C                   if        tasnrs <> %dec(NUMSER : 2 : 0)
059100110304     C                   movel     'N'           CHKREC
059200110304     C                   endif
059300110304     C                   endif
059400110304     C                   endif
059500110304     C*
059600110304     C* Verifico il tipo bolla
059700110304     C                   if        CHKREC = 'S'
059800140326     C                   select
059900140326     C                   when      TIPBOL = '99'
060000140326     C                   when      TIPBOL = '98' AND
060100140326     C                             tastbl <> 'F1' AND tastbl <> 'A2'
060200140326     C                   movel     'N'           CHKREC
060300140326     C                   when      TIPBOL <> '99' AND
060400140326     C                             TIPBOL <> '98' AND
060500140326     C                             tastbl <> TIPBOL
060600140326     C                   movel     'N'           CHKREC
060700140326     C                   endsl
060800110304     C                   endif
060900110304     C*
061000110304     C* Verifico se richiesto nel lancio che la bolla sia consegnata
061100110304     C                   if        CHKREC = 'S'
061200110304     C                   if        FLGCONS = 'S'
061300110304     C                   exsr      CHKCONS
061400110304     C                   if        *in40 = *on
061500110304     C                   else
061600110304     C                   movel     'N'           CHKREC
061700110304     C                   endif
061800110304     C                   endif
061900110304     C                   endif
062000110304     C*
062100110304     C* Verifico se richiesto nel lancio che la bolla sia consegnata OK
062200110304     C                   if        CHKREC = 'S'
062300110304     C                   if        FLGCONS = 'O'
062400110304     C                   exsr      CHKCONS
062500110304     C                   if        *in41 = *on
062600110304     C                   else
062700110304     C                   movel     'N'           CHKREC
062800110304     C                   endif
062900110304     C                   endif
063000110304     C                   endif
063100110304     C*
063200110304     C* Verifico se richiesto nel lancio che la bolla NON sia consegnata OK
063300110304     C                   if        CHKREC = 'S'
063400110304     C                   if        FLGCONS = 'K'
063500110304     C                   exsr      CHKCONS
063600110304     C                   if        *in41 = *on
063700110304     C                   movel     'N'           CHKREC
063800110304     C                   endif
063900110304     C                   endif
064000110304     C                   endif
064100110304     C*
064200110304     C* Verifico se richiesto nel lancio che la bolla NON sia consegnata
064300110304     C                   if        CHKREC = 'S'
064400110304     C                   if        FLGCONS = 'P'
064500110304     C                   exsr      CHKCONS
064600110304     C                   if        *in40 = *on
064700110304     C                   movel     'N'           CHKREC
064800110304     C                   endif
064900110304     C                   endif
065000110304     C                   endif
065100110304     C*
065200110304     C                   ENDSR
065300110304     C*------------------------------------------------------------------------*
065400001218
065500001218
065600001218
065700001218     C*------------------------------------------------------------------------*
065800110307     C* SCRIVIWF - Routine di scrittura WFCLN10F
065900001218     C*------------------------------------------------------------------------*
066000100219     C     SCRIVIWF      BEGSR
066100100219     C*
066200110307     C                   clear                   WFCLN110
066300110307     C                   clear                   WFCLN120
066400150210     C                   clear                   WFCLN130
066500110307     C                   clear                   WFCLN_10
066600110307     C                   clear                   WFCLN_11
066700110307     C                   clear                   WFCLN_12
066800150210     C                   clear                   WFCLN_13
066900100219     C*
067000100219     C                   eval      dlyLNP = tasLNP
067100100219     C                   eval      dlyTSP = tasTSP
067200120309     C*
067300120309     C                   if        dlyTSP = 'D'
067400120309     C                   eval      dlyTSP = 'C'
067500120309     C                   endif
067600120309     C*
067700100219     C                   eval      dlyNAR = tasNZD
067800100219     C                   eval      dlyCAP = tasCAD
067900100219     C     KEYdly05_P    chain     wadly05l
068000100219     C                   if        %found(wadly05l)
068100100219     C                   eval      WCLNDLYTRC = DLYTRC
068200100219     C                   endif
068300100219     C*
068400100219     C                   eval      tblcod = 'PR'
068500100219     C                   eval      tblkey = tasprd
068600100219     C     KEYtab_C      chain     tabel00f
068700100219     C                   if        %found(tabel00f)
068800100219     C                   eval      DSPR = tbluni
068900100219     C                   eval      tblcod = '14'
069000100219     C                   eval      tblkey = §PRCRE
069100100219     C     KEYtab_C      chain     tabel00f
069200100219     C                   if        %found(tabel00f)
069300100219     C                   eval      DS14 = tbluni
069400110307     C                   eval      wclnREG = §14DES
069500100219     C                   endif
069600100219     C                   endif
069700100219     C*
069800100219     C                   z-add     *zeros        wANT
069900100222     C                   z-add     *zeros        wM12
070000100219     C                   z-add     *zeros        wOKS
070100150210     C                   z-add     *zeros        wR12
070200150210     C                   z-add     *zeros        wR24
070300100219     C                   z-add     *zeros        wRIT
070400130411     C*
070500130411     C*
070600130411     C* Verifico tipo affidabilità richiesta
070700130411     C                   select
070800130411     C*
070900130411     C* - affidabilità consegna
071000130411     C                   when      TIPAFFID = 'C'
071100001218     C*
071200100219     C* ...in anticipo
071300100219     C                   select
071400130411     C                   when      tasnci >= -999 AND
071500130411     C                             tasnci <= -24
071600100219     C                   add       1             wANT
071700100222     C*
071800100222     C* ...border-line
071900130411     C                   when      tasnci >  -24  AND
072000130411     C                             tasnci <= -12
072100100222     C                   add       1             wM12
072200100219     C*
072300100219     C* ...in orario
072400130411     C                   when      tasnci >  -12  AND
072500130411     C                             tasnci <=   0
072600100219     C                   add       1             wOKS
072700150210     C*
072800150210     C* ...in ritardo entro 12 ore
072900150210     C                   when      tasnci >    0  AND
073000150210     C                             tasnci <=  12
073100150210     C                   add       1             wR12
073200150210     C*
073300150210     C* ...in ritardo entro 24 ore
073400150210     C                   when      tasnci >   12  AND
073500150210     C                             tasnci <=  24
073600150210     C                   add       1             wR24
073700150210     C*
073800150210     C* ...in ritardo "molto"
073900150210     C                   when      tasnci >   24
074000150210     C                   add       1             wRIT
074100150210     C                   endsl
074200130411     C*
074300130411     C* - affidabilità cliente (*DEFAULT)
074400130411     C                   other
074500130411     C*
074600130411     C* ...in anticipo
074700130411     C                   select
074800150210     C                   when      tasnrc >= -999 AND
074900150210     C                             tasnrc <= -24
075000130411     C                   add       1             wANT
075100130411     C*
075200130411     C* ...border-line
075300150210     C                   when      tasnrc >  -24  AND
075400150210     C                             tasnrc <= -12
075500130411     C                   add       1             wM12
075600130411     C*
075700130411     C* ...in orario
075800150210     C                   when      tasnrc >  -12  AND
075900150210     C                             tasnrc <=   0
076000130411     C                   add       1             wOKS
076100150210     C*
076200150210     C* ...in ritardo entro 12 ore
076300150210     C                   when      tasnrc >    0  AND
076400150210     C                             tasnrc <=  12
076500150210     C                   add       1             wR12
076600150210     C*
076700150210     C* ...in ritardo entro 24 ore
076800150210     C                   when      tasnrc >   12  AND
076900150210     C                             tasnrc <=  24
077000150210     C                   add       1             wR24
077100150210     C*
077200150210     C* ...in ritardo "molto"
077300150210     C                   when      tasnrc >   24
077400150210     C                   add       1             wRIT
077500150210     C                   endsl
077600130411     C*
077700130411     C                   endsl
077800100219     C*
077900100219     C* Conteggio spedizioni DIR/GIAC/CCA
078000100219     C                   eval      dtasflo = tasflo
078100100219     C                   if        tasCCA   <> *blanks OR
078200100219     C                             tasFGC   <> *blanks OR
078300100219     C                             §FLONAV  <> *blanks
078400100219     C                   eval      wclnANOM = 'S'
078500100219     C                   endif
078600100219     C*
078700111130     C                   eval      wclnCCM    = tasccm
078800111130     C                   if        wChkCli = 'K'
078900111130     C                   eval      wclnCCM    = tasksc
079000111130     C                   endif
079100130403     C*
079200130403     C* Se richiesta estrazione per "unificante" (di qualunque tipo come CCM considero sempre
079300130403     C* l'unificante)
079400130403     C
079500130403     C                   if        FLGUNI <> *blanks
079600130403     C                   eval      wclnCCM    = CODCLI
079700130403     C                   endif
079800111130     C*
079900100219     C                   eval      wclnAAAAMM = wrkAAAAMM
080000100219     C                   eval      wclnPRD    = tasprd
080100100219     C                   eval      wclnCAD    = tascad
080200100219     C                   eval      wclnISO    = tasfin
080300110307     C                   eval      wclnAAS    = tasaas
080400110307     C                   eval      wclnLNP    = taslnp
080500110307     C                   eval      wclnNRS    = tasnrs
080600110307     C                   eval      wclnNSP    = tasnsp
080700110307     C*
080800110307     C                   select
080900110307     C                   when      *in26
081000110307     C     KEYcln_12C    chain     wfcln12l
081100110307     C                   if        %found(wfcln12l)
081200110307     C                   eval      WFCLN_10 = WFCLN_12
081300100219     C                   add       wANT          wclnANT
081400100222     C                   add       wM12          wclnM12
081500100219     C                   add       wOKS          wclnOKS
081600150210     C                   add       wR12          wclnR12
081700150210     C                   add       wR24          wclnR24
081800100219     C                   add       wRIT          wclnRIT
081900100219     C                   add       1             wclnTOT
082000110307     C                   eval      WFCLN_12 = WFCLN_10
082100110307     C                   update    WFCLN120
082200100219     C                   else
082300100219     C                   add       wANT          wclnANT
082400100222     C                   add       wM12          wclnM12
082500100219     C                   add       wOKS          wclnOKS
082600150210     C                   add       wR12          wclnR12
082700150210     C                   add       wR24          wclnR24
082800100219     C                   add       wRIT          wclnRIT
082900100219     C                   add       1             wclnTOT
083000110307     C                   eval      WFCLN_12 = WFCLN_10
083100110307     C                   write     WFCLN120
083200100219     C                   endif
083300110307     C*
083400110307     C                   when      *in27
083500150211     C     KEYcln_11C    chain     wfcln11l
083600150211     C                   if        %found(wfcln11l)
083700150211     C                   eval      WFCLN_10 = WFCLN_11
083800150211     C                   add       wANT          wclnANT
083900150211     C                   add       wM12          wclnM12
084000150211     C                   add       wOKS          wclnOKS
084100150211     C                   add       wR12          wclnR12
084200150211     C                   add       wR24          wclnR24
084300150211     C                   add       wRIT          wclnRIT
084400150211     C                   add       1             wclnTOT
084500150211     C                   eval      WFCLN_11 = WFCLN_10
084600150211     C                   update    WFCLN110
084700150211     C                   else
084800150211     C                   add       wANT          wclnANT
084900150211     C                   add       wM12          wclnM12
085000150211     C                   add       wOKS          wclnOKS
085100150211     C                   add       wR12          wclnR12
085200150211     C                   add       wR24          wclnR24
085300150211     C                   add       wRIT          wclnRIT
085400150211     C                   add       1             wclnTOT
085500150211     C                   eval      WFCLN_11 = WFCLN_10
085600150211     C                   write     WFCLN110
085700150211     C                   endif
085800150210     C*
085900150210     C                   when      *in28
086000150211     C     KEYcln_13C    chain     wfcln13l
086100150211     C                   if        %found(wfcln13l)
086200150211     C                   eval      WFCLN_10 = WFCLN_13
086300150211     C                   add       wANT          wclnANT
086400150211     C                   add       wM12          wclnM12
086500150211     C                   add       wOKS          wclnOKS
086600150211     C                   add       wR12          wclnR12
086700150211     C                   add       wR24          wclnR24
086800150211     C                   add       wRIT          wclnRIT
086900150211     C                   add       1             wclnTOT
087000150211     C                   eval      WFCLN_13 = WFCLN_10
087100150211     C                   update    WFCLN130
087200150211     C                   else
087300150211     C                   add       wANT          wclnANT
087400150211     C                   add       wM12          wclnM12
087500150211     C                   add       wOKS          wclnOKS
087600150211     C                   add       wR12          wclnR12
087700150211     C                   add       wR24          wclnR24
087800150211     C                   add       wRIT          wclnRIT
087900150211     C                   add       1             wclnTOT
088000150211     C                   eval      WFCLN_13 = WFCLN_10
088100150211     C                   write     WFCLN130
088200150211     C                   endif
088300110307     C*
088400110307     C                   endsl
088500001218     C*
088600001218     C                   ENDSR
088700001221     C*------------------------------------------------------------------------*
088800110304
088900110304
089000110304
089100110304     C*------------------------------------------------------------------------*
089200110304     C* WRITIVGD - Routine di scrittura file TIVGD (file VAS generico download)
089300110304     C*------------------------------------------------------------------------*
089400110304     C     WRITIVGD      BEGSR
089500110304     C*
089600110307     C* Leggo tutto il file WFCLN11L in QTEMP del job corrente
089700110307     C     *loval        setll     WFCLN11L
089800110307     C                   read      WFCLN11L
089900110307     C                   dow       not %eof(WFCLN11L)
090000110307     C*
090100161205     C                   clear                   iDSVGD
090200110307     C                   eval      vgdDTA = %subst(WFCLN_11:1:%size(WFCLN_11))
090300110304     C                   eval      vgdTIP = TIPFILE
090400110304     C                   movel     *all'0'       vgdKSU
090500110304     C                   move      CODCLIVAS     vgdKSU
090600110304     C                   eval      vgdTSC = 'WW'
090700110304     C                   eval      vgdDAT = datcor
090800161205     C                   eval      vgdPRG = oPRG
090900161205     C                   eval      vgdPGM = procname
091000161205     C*
091100161205     C                   CALL      'TIS7VASR1'
091200161205     C                   PARM      '2'           iOPZ
091300161205     C                   PARM                    iDSVGD
091400161205     C                   PARM                    oPRG
091500161205     C                   PARM                    oOK
091600161205     C*
091700161205     C* Verifico esito chiamata
091800161205     C                   if        %error OR oOK = *off
091900161205     C                   exsr      EXEERR
092000161205     C                   endif
092100110304     C*
092200110307     C                   read      WFCLN11L
092300110307     C                   enddo
092400110307     C*
092500110304     C                   ENDSR
092600110304     C*------------------------------------------------------------------------*
092700120214
092800120214
092900120214
093000120214     C*------------------------------------------------------------------------*
093100120214     C* CARTBL - Routine di caricamento dati tabellati
093200120214     C*------------------------------------------------------------------------*
093300120214     C     CARTBL        BEGSR
093400120214     C*
093500120214     C                   Z-ADD     1             I                 4 0
093600120214     C                   MOVEL     CODCLI        KUNI(I)
093700130403     C*
093800130403     C                   SELECT
093900130403     C                   WHEN      FLGUNI = 'U'
094000120214     C                   MOVEL     '3K'          tblCOD
094100120214     C     KEYtab_P      CHAIN     TABEL                              31
094200120214     C     *IN31         DOWEQ     '0'
094300120214     C     TBLFLG        IFEQ      ' '
094400120214     C                   MOVEL     TBLUNI        DS3K
094500120214     C     §3KCKS        IFEQ      CODCLI
094600120214     C                   ADD       1             I
094700120214     C                   MOVEL     TBLKEY        wKUNI             7 0
094800120214     C     wKUNI         LOOKUP    KUNI                                   55
094900120214     C                   IF        %equal
095000120214     C                   ELSE
095100120214     C                   Z-ADD     wKUNI         KUNI(I)
095200120214     C                   ENDIF
095300120214     C                   ENDIF
095400120214     C                   ENDIF
095500120214     C     KEYtab_P      READE     TABEL                                  31
095600120214     C                   ENDDO
095700130403     C*
095800130403     C                   WHEN      FLGUNI = 'T'
095900130403     C                   CLEAR                   TIBS10DS
096000130403     C                   EVAL      D10DRF = datcor                              *data riferimento
096100130403     C                   EVAL      D10TLE = 'ST'                                *tipo legame
096200130403     C                   EVAL      D10PAF = 'F'                                 *cerca se padre
096300130403     C                   EVAL      D10COD = CODCLI
096400130403     C                   CALL      'TIBS10R'
096500130403     C                   PARM                    TIBS10DS
096600130403     C                   IF        D10ERR = *BLANKS                             *Padre
096700130403     C                   Z-ADD     1             I
096800130403     C                   DOW       skc(I) > *zeros
096900130403     C                   Z-ADD     skc(I)        KUNI(I)
097000130403     C                   ADD       1             I
097100130403     C                   ENDDO
097200130403     C                   ENDIF
097300130403     C*
097400130403     C                   ENDSL
097500120214     C*
097600120214     C                   ENDSR
097700120214     C*---------------------------------------------------------------*
097800161205
097900161205
098000161205
098100161205     C*------------------------------------------------------------------------*
098200161205     C* EXEERR - Routine di esecuzione azioni su Errore
098300161205     C*------------------------------------------------------------------------*
098400161205     C     EXEERR        BEGSR
098500161205     C*
098600161205     C                   dump(A)
098700161205     C                   rolbk
098800161205     C                   seton                                        lr
098900161205     C                   return
099000161205     C*
099100161205     C                   ENDSR
099200161205     C*------------------------------------------------------------------------*
099300120214
099400001218
099500001218
099600001218     C*------------------------------------------------------------------------*
099700001218     C* *INZSR - ROUTINE INIZIALE
099800001218     C*------------------------------------------------------------------------*
099900001218     C     *INZSR        BEGSR
100000001218     C*
100100001218     C     *ENTRY        PLIST
100200100219     C                   PARM                    KPJBA
100300001218     C*
100400100219     C                   MOVEL     KPJBU         DSINPUT
100500100219     C                   EVAL      tblKUT = 1
100600110307     C*
100700110307     C* Determino la data corrente
100800110307     C                   Z-ADD     *zeros        datcor            8 0
100900110307     C                   EVAL      datcor = %dec(%date() : *ISO)
101000001218     C*
101100001218     C* Definizioni chiavi
101200100219     C*
101300100219     C     KEYdly05_P    KLIST
101400100219     C                   KFLD                    dlyLNP
101500100219     C                   KFLD                    dlyTSP
101600100219     C                   KFLD                    dlyNAR
101700100219     C                   KFLD                    dlyCAP
101800120214     C*
101900120214     C     KEYtab_P      KLIST
102000120214     C                   KFLD                    tblKUT
102100120214     C                   KFLD                    tblCOD
102200001218     C*
102300100219     C     KEYtab_C      KLIST
102400100219     C                   KFLD                    tblKUT
102500100219     C                   KFLD                    tblCOD
102600100219     C                   KFLD                    tblKEY
102700110307     C*
102800110307     C     KEYcln_11C    KLIST
102900110307     C                   KFLD                    wclnCCM
103000110307     C                   KFLD                    wclnAAAAMM
103100110307     C                   KFLD                    wclnAAS
103200110307     C                   KFLD                    wclnLNP
103300110307     C                   KFLD                    wclnNRS
103400110307     C                   KFLD                    wclnNSP
103500030704     C*
103600110307     C     KEYcln_12C    KLIST
103700100219     C                   KFLD                    wclnCCM
103800100219     C                   KFLD                    wclnAAAAMM
103900110307     C                   KFLD                    wclnREG
104000100219     C                   KFLD                    wclnPRD
104100100219     C                   KFLD                    wclnCAD
104200100219     C                   KFLD                    wclnISO
104300100219     C                   KFLD                    wclnANOM
104400150210     C*
104500150210     C     KEYcln_13C    KLIST
104600150210     C                   KFLD                    wclnCCM
104700150210     C                   KFLD                    wclnAAAAMM
104800150210     C                   KFLD                    wclnREG
104900150210     C                   KFLD                    wclnPRD
105000100219     C*
105100001218     C                   ENDSR

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
001200110304     FTIVGD00F  UF A E             DISK    USROPN
001300001218     D*--------------------
001400001218     D* DS ESTERNE
001500001218     D*--------------------
001600110307     D WFCLN_10      E DS                  EXTNAME(WFCLN10F)
001700110307     D WFCLN_11      E DS                  EXTNAME(WFCLN10F) PREFIX(F11_)
001800110307     D WFCLN_12      E DS                  EXTNAME(WFCLN10F) PREFIX(F12_)
001900150210     D WFCLN_13      E DS                  EXTNAME(WFCLN10F) PREFIX(F13_)
002000110307     D TITAS00F      E DS
002100900517     D KPJBA         E DS
002200100219     D DTASFLO       E DS
002300100219     D DSPR          E DS
002400100219     D DS14          E DS
002500120214     D DS3K          E DS
002600110307     D trul47ds      e ds
002700110307     D TIS781DS      E DS
002800110307     D TIS781DFLO    E DS
002900110307     D DVGDFLO       E DS
003000110304     D*--------------------
003100110304     D* SCHIERA PER REPERIMENTO CLIENTI "FIGLI" DI UN UNIFICANTE
003200110304     D*--------------------
003300110304     D KUNI            S              7  0 DIM(1000) DESCEND
003400130403     D*-------------------
003500130403     D* DS REPERIMENTO PADRE/FIGLI
003600130403     D*-------------------
003700130403     D TIBS10DS      E DS
003800130403     D skc                           11  0 DIM(500) overlay(D10SKC)
003900110304     D*--------------------
004000110304     D* DS DI RIDEFINIZIOINE PARAMETRI IN INPUT
004100110304     D*--------------------
004200110304     D DSINPUT         DS
004300110304     D  DATADA                        8  0
004400110304     D  DATAA                         8  0
004500110304     D  CODCLI                        7  0
004600110304     D  FLGUNI                        1
004700110304     D  FLGCONS                       1
004800110304     D  CODCLIVAS                     7  0
004900110304     D  TIPFILE                       2
005000110304     D  FLGEXE                        1
005100110304     D  TIPCLI                        1
005200110304     D  DACMDA                        8  0
005300110304     D  DACMA                         8  0
005400110304     D  TIPBOL                        2
005500110307     D  NUMSER                        2
005600110304     D  TIPDET                        1
005700130411     D  TIPAFFID                      1
005800110304     D*--------------------
005900110304
006000010605     D*--------------------
006100010605     D* VARIABILI DI WRK
006200010605     D*--------------------
006300100219     D wANT            s              7  0 inz
006400100222     D wM12            s              7  0 inz
006500100219     D wOKS            s              7  0 inz
006600150210     D wR12            s              7  0 inz
006700150210     D wR24            s              7  0 inz
006800100219     D wRIT            s              7  0 inz
006900110304
007000110304
007100110304     D*------------------
007200110304     D* VARIABILI D WRK
007300110304     D*------------------
007400110304     D  wAAS           S                   like(tasAAS) inz
007500110304     D  wLNP           S                   like(tasLNP) inz
007600110304     D  wNRS           S                   like(tasNRS) inz
007700110304     D  wNSP           S                   like(tasNSP) inz
007800110304
007900110304
008000110304     D*------------------
008100110304     D* LINKING A DEFINIZIONI ESTERNE
008200110304     D*------------------
008300110304     D/COPY GAITRASRC/SRCPROTOPR,UBLBLSPE
008400110304     D/COPY GAITRASRC/SRCPROTOPI,UBLBLSPE
008500010605
008600010605
008700920812     C*---------------------------------------------------------------*
008800001218     C* MAIN
008900001218     C*---------------------------------------------------------------*
009000110304     C*
009100110304     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
009200110304     C
009300110304     C/EXEC SQL
009400110304     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
009500110304     C/END-EXEC
009600110304     C
009700110304     C*
009800120214     C                   exsr      cartbl
009900110304     C                   exsr      chkpar
010000110304     C*
010100120214     C**** Fisso KUNI(1)
010200120214     C***                   eval      KUNI(1) = CODCLI
010300110304     C*
010400110304     C* Ciclo sulla schiera dei clienti figli dell'unificante preso in input
010500110304     C                   z-add     1             I                 4 0
010600110304     C                   sorta     KUNI
010700110304     C                   dow       KUNI(I) > *zeros
010800001218     C                   exsr      procedi
010900110307     C                   add       1             I
011000110304     C                   enddo
011100110307     C*
011200110307     C* Inizializzo variabili d wrk
011300110307     C                   movel     'N'           wProcedi          1
011400110307     C*
011500110307     C* Apro il file d output
011600110307     C                   open      tivgd00f
011700110307     C*
011800110307     C* Come prima cosa avvio il blocco elaborazione TIVGD x tipo file corrente: 'VC'
011900110307     C                   clear                   trul47ds
012000110307     C                   eval      d47opz  = 'I'
012100120927     C                   eval      d47tip  = TIPFILE
012200110307     C                   eval      d47lck  = 'N'
012300110307     C                   eval      d47chkj = 'N'
012400110307     C                   eval      d47pgm  = 'TIS7FFR'
012500110307     C                   call      'TRUL47R'
012600110307     C                   parm                    trul47ds
012700110307     C*
012800110307     C* Se elaborazione consentita => proseguo
012900120928     C***                if        d47sts <> 'A'
013000110307     C                   movel     'S'           wProcedi
013100120928     C***                endif
013200110307     C*
013300110307     C* Popolo il file d output
013400110307     C                   if        wProcedi = 'S'
013500110307     C                   exsr      WRITIVGD
013600110307     C                   endif
013700110304     C*
013800110304     C* Chiudo il file di output
013900110304     C                   close     tivgd00f
014000110304     C*
014100110304     C* Infine elimino il blocco elaborazione TIVGD x tipo file corrente: 'VC'
014200110304     C                   clear                   trul47ds
014300110304     C                   eval      d47opz  = 'F'
014400120928     C                   eval      d47tip  = TIPFILE
014500110304     C                   call      'TRUL47R'
014600110304     C                   parm                    trul47ds
014700001218     C*
014800001218     C                   seton                                        LR
014900110304
015000110304
015100110304
015200110304     C*------------------------------------------------------------------------*
015300110304     C* CHKPAR - Routine di controllo parametri ricevuti in input
015400110304     C*------------------------------------------------------------------------*
015500110304     C     CHKPAR        BEGSR
015600110304     C*
015700110304     C* Verifico le date
015800110304     C                   if        DATADA > *zeros AND
015900110304     C                             DATAA  = *zeros
016000110304     C                   movel     *all'9'       DATAA
016100110304     C                   endif
016200110304     C                   if        DACMDA > *zeros AND
016300110304     C                             DACMA  = *zeros
016400110304     C                   movel     *all'9'       DACMA
016500110304     C                   endif
016600110307     C*
016700110307     C* Verifico il livello d dettaglio richiesto
016800150210     C                   setoff                                       262728
016900110307     C                   select
017000110307     C                   when      TIPDET = 'C'                                 * x CAP
017100110307     C                   seton                                        26
017200110307     C                   when      TIPDET = 'S'                                 * x SPED
017300110307     C                   seton                                        27
017400150210     C                   when      TIPDET = 'P'                                 * x PROV
017500150210     C                   seton                                        28
017600110307     C                   endsl
017700110304     C*
017800110304     C                   ENDSR
017900110304     C*------------------------------------------------------------------------*
018000110304
018100110304
018200110304
018300110304     C*------------------------------------------------------------------------*
018400110304     C* PROCEDI - Routine principale
018500110304     C*------------------------------------------------------------------------*
018600110304     C     PROCEDI       BEGSR
018700110304     C*
018800110304     C* Se richiesta elaborazione x codice cliente mittente...
018900110304     C                   select
019000110304     C                   when      TIPCLI = *blanks
019100110304     C                   exsr      EXECCM
019200110304     C*
019300110304     C* Se richiesta elaborazione x codice cliente fatturazione...
019400110304     C                   when      TIPCLI = 'K'
019500111130     C                   exsr      EXEKSC
019600110304     C*
019700110304     C* Se richiesta elaborazione x entrambi: codice cliente mittente + fatturazione...
019800110304     C                   when      TIPCLI = 'E'
019900110304     C                   exsr      EXECCM
020000111130     C                   exsr      EXEKSC
020100110304     C*
020200110304     C                   endsl
020300110304     C*
020400110304     C                   ENDSR
020500110304     C*------------------------------------------------------------------------*
020600110304
020700110304
020800110304
020900110304     C*------------------------------------------------------------------------*
021000110304     C* EXECCM   - Elaborazione x codice cliente mittente
021100110304     C*------------------------------------------------------------------------*
021200110304     C     EXECCM        BEGSR
021300110304     C*
021400110304     C                   movel     'C'           wChkCli           1
021500110304     C*
021600110304     C                   select
021700110304     C* ...se richiesta elaborazione x data spedizione
021800110304     C                   when      DATADA > *zeros
021900110304     C                   exsr      SQ_CCMDSP
022000110304     C* ...se richiesta elaborazione x data consegna merce
022100110304     C                   when      DACMDA > *zeros
022200110304     C                   exsr      SQ_CCMDCM
022300110304     C                   endsl
022400110304     C*
022500110304     C                   ENDSR
022600110304     C*------------------------------------------------------------------------*
022700111130
022800111130
022900111130
023000111130     C*------------------------------------------------------------------------*
023100111130     C* EXEKSC   - Elaborazione x codice cliente fatturazione
023200111130     C*------------------------------------------------------------------------*
023300111130     C     EXEKSC        BEGSR
023400111130     C*
023500111130     C                   movel     'K'           wChkCli           1
023600111130     C*
023700111130     C                   select
023800111130     C* ...se richiesta elaborazione x data spedizione
023900111130     C                   when      DATADA > *zeros
024000111130     C                   exsr      SQ_KSCDSP
024100111130     C* ...se richiesta elaborazione x data consegna merce
024200111130     C                   when      DACMDA > *zeros
024300111130     C                   exsr      SQ_KSCDCM
024400111130     C                   endsl
024500111130     C*
024600111130     C                   ENDSR
024700111130     C*------------------------------------------------------------------------*
024800110304
024900110304
025000110304
025100110304     C*------------------------------------------------------------------------*
025200110304     C* SQ_CCMDSP - Elaborazione x codice cliente mittente + data spedizione
025300110304     C*------------------------------------------------------------------------*
025400110304     C     SQ_CCMDSP     BEGSR
025500110304     C*
025600110304     C                   z-add     KUNI(I)       wParCCM           7 0
025700110304     C*
025800110304     C/EXEC SQL
025900110304     C+ declare C_CCMDSP cursor for
026000110304     C+ select * from titas00f
026100110304     C+ where tasccm = :wParCCM and
026200110304     C+ tasaas*10000+tasmgs between :DATADA and :DATAA
026300110304     C+ union
026400110304     C+ select * from titas10f
026500110304     C+ where tasccm = :wParCCM and
026600110304     C+ tasaas*10000+tasmgs between :DATADA and :DATAA
026700110304     C+ union
026800110304     C+ select * from titasP0f
026900110304     C+ where tasccm = :wParCCM and
027000110304     C+ tasaas*10000+tasmgs between :DATADA and :DATAA
027100110304     C+ for read only
027200110304     C/END-EXEC
027300110304     C
027400110304     C/EXEC SQL
027500110304     C+ open C_CCMDSP
027600110304     C/END-EXEC
027700110304     C
027800110304     C/EXEC SQL
027900110304     C+ Fetch C_CCMDSP into :TITAS00F
028000110304     C/END-EXEC
028100110304     C*
028200110304     C                   dow       sqlcod = *zeros
028300110304     C                   exsr      verifica
028400110304     C                   if        CHKREC = 'S'
028500110304     C                   exsr      scriviWF
028600110304     C                   endif
028700110304     C
028800110304     C/EXEC SQL
028900110304     C+ Fetch C_CCMDSP into :TITAS00F
029000110304     C/END-EXEC
029100110304     C*
029200110304     C                   enddo
029300110304     C*
029400110304     C/EXEC SQL
029500110304     C+ close C_CCMDSP
029600110304     C/END-EXEC
029700110304     C*
029800110304     C*
029900110304     C                   ENDSR
030000110304     C*------------------------------------------------------------------------*
030100111130
030200111130
030300111130
030400111130     C*------------------------------------------------------------------------*
030500111130     C* SQ_KSCDSP - Elaborazione x codice cliente fatturazione + data spedizione
030600111130     C*------------------------------------------------------------------------*
030700111130     C     SQ_KSCDSP     BEGSR
030800111130     C*
030900111130     C                   z-add     KUNI(I)       wParKSC           7 0
031000111130     C*
031100111130     C/EXEC SQL
031200111130     C+ declare C_KSCDSP cursor for
031300111130     C+ select * from titas00f
031400111130     C+ where tasksc = :wParKSC and
031500111130     C+ tasaas*10000+tasmgs between :DATADA and :DATAA
031600111130     C+ union
031700111130     C+ select * from titas10f
031800111130     C+ where tasksc = :wParKSC and
031900111130     C+ tasaas*10000+tasmgs between :DATADA and :DATAA
032000111130     C+ union
032100111130     C+ select * from titasP0f
032200111130     C+ where tasksc = :wParKSC and
032300111130     C+ tasaas*10000+tasmgs between :DATADA and :DATAA
032400111130     C+ for read only
032500111130     C/END-EXEC
032600111130     C
032700111130     C/EXEC SQL
032800111130     C+ open C_KSCDSP
032900111130     C/END-EXEC
033000111130     C
033100111130     C/EXEC SQL
033200111130     C+ Fetch C_KSCDSP into :TITAS00F
033300111130     C/END-EXEC
033400111130     C*
033500111130     C                   dow       sqlcod = *zeros
033600111130     C                   exsr      verifica
033700111130     C                   if        CHKREC = 'S'
033800111130     C                   exsr      scriviWF
033900111130     C                   endif
034000111130     C
034100111130     C/EXEC SQL
034200111130     C+ Fetch C_KSCDSP into :TITAS00F
034300111130     C/END-EXEC
034400111130     C*
034500111130     C                   enddo
034600111130     C*
034700111130     C/EXEC SQL
034800111130     C+ close C_KSCDSP
034900111130     C/END-EXEC
035000111130     C*
035100111130     C*
035200111130     C                   ENDSR
035300111130     C*------------------------------------------------------------------------*
035400110304
035500110304
035600110304
035700110304     C*------------------------------------------------------------------------*
035800110304     C* SQ_CCMDCM - Elaborazione x codice cliente mittente + data consegna merce
035900110304     C*------------------------------------------------------------------------*
036000110304     C     SQ_CCMDCM     BEGSR
036100110304     C*
036200110304     C                   z-add     KUNI(I)       wParCCM           7 0
036300110304     C*
036400110304     C/EXEC SQL
036500110304     C+ declare C_CCMDCM cursor for
036600110304     C+ select * from titas00f
036700110304     C+ where tasccm = :wParCCM and
036800110304     C+ tasdcm between :DACMDA and :DACMA
036900110304     C+ union
037000110304     C+ select * from titas10f
037100110304     C+ where tasccm = :wParCCM and
037200110304     C+ tasdcm between :DACMDA and :DACMA
037300110304     C+ union
037400110304     C+ select * from titasP0f
037500110304     C+ where tasccm = :wParCCM and
037600110304     C+ tasdcm between :DACMDA and :DACMA
037700110304     C+ for read only
037800110304     C/END-EXEC
037900110304     C
038000110304     C/EXEC SQL
038100110304     C+ open C_CCMDCM
038200110304     C/END-EXEC
038300110304     C
038400110304     C/EXEC SQL
038500110304     C+ Fetch C_CCMDCM into :TITAS00F
038600110304     C/END-EXEC
038700110304     C*
038800110304     C                   dow       sqlcod = *zeros
038900110304     C                   exsr      verifica
039000110304     C                   if        CHKREC = 'S'
039100110304     C                   exsr      scriviWF
039200110304     C                   endif
039300110304     C
039400110304     C/EXEC SQL
039500110304     C+ Fetch C_CCMDCM into :TITAS00F
039600110304     C/END-EXEC
039700110304     C*
039800110304     C                   enddo
039900110304     C*
040000110304     C/EXEC SQL
040100110304     C+ close C_CCMDCM
040200110304     C/END-EXEC
040300110304     C*
040400110304     C*
040500110304     C                   ENDSR
040600110304     C*------------------------------------------------------------------------*
040700111130
040800111130
040900111130
041000111130     C*------------------------------------------------------------------------*
041100111130     C* SQ_KSCDCM - Elaborazione x codice cliente fatturazione + data consegna merce
041200111130     C*------------------------------------------------------------------------*
041300111130     C     SQ_KSCDCM     BEGSR
041400111130     C*
041500111130     C                   z-add     KUNI(I)       wParKSC           7 0
041600111130     C*
041700111130     C/EXEC SQL
041800111130     C+ declare C_KSCDCM cursor for
041900111130     C+ select * from titas00f
042000111130     C+ where tasksc = :wParKSC and
042100111130     C+ tasdcm between :DACMDA and :DACMA
042200111130     C+ union
042300111130     C+ select * from titas10f
042400111130     C+ where tasksc = :wParKSC and
042500111130     C+ tasdcm between :DACMDA and :DACMA
042600111130     C+ union
042700111130     C+ select * from titasP0f
042800111130     C+ where tasksc = :wParKSC and
042900111130     C+ tasdcm between :DACMDA and :DACMA
043000111130     C+ for read only
043100111130     C/END-EXEC
043200111130     C
043300111130     C/EXEC SQL
043400111130     C+ open C_KSCDCM
043500111130     C/END-EXEC
043600111130     C
043700111130     C/EXEC SQL
043800111130     C+ Fetch C_KSCDCM into :TITAS00F
043900111130     C/END-EXEC
044000111130     C*
044100111130     C                   dow       sqlcod = *zeros
044200111130     C                   exsr      verifica
044300111130     C                   if        CHKREC = 'S'
044400111130     C                   exsr      scriviWF
044500111130     C                   endif
044600111130     C
044700111130     C/EXEC SQL
044800111130     C+ Fetch C_KSCDCM into :TITAS00F
044900111130     C/END-EXEC
045000111130     C*
045100111130     C                   enddo
045200111130     C*
045300111130     C/EXEC SQL
045400111130     C+ close C_KSCDCM
045500111130     C/END-EXEC
045600111130     C*
045700111130     C*
045800111130     C                   ENDSR
045900111130     C*------------------------------------------------------------------------*
046000001218
046100110304
046200110304
046300110304     C*------------------------------------------------------------------------*
046400110304     C* CHKCONS  - Verifica se bolla effettivamente consegnata
046500110304     C*------------------------------------------------------------------------*
046600110304     C     CHKCONS       BEGSR
046700110304     C*
046800110304     C                   setoff                                       4041
046900110304     C                   movel     *blanks       wEsito1           1
047000110304     C                   movel     *blanks       wEsito2           1
047100110304     C*
047200110304     C                   eval      wAAS = tasAAS
047300110304     C                   eval      wLNP = tasLNP
047400110304     C                   eval      wNRS = tasNRS
047500110304     C                   eval      wNSP = tasNSP
047600110304     C*
047700110304     C* Chiamata metodo GetLblTyp
047800110304     C                   eval      wEsito1 = %char(UBLBLSPE_GetLblTyp(
047900110304     C                                                wAAS
048000110304     C                                               :wLNP
048100110304     C                                               :wNRS
048200110304     C                                               :wNSP
048300110304     C                                               :pOutLblTyp
048400110304     C                                               :pOutAnnoBO
048500110304     C                                               :pOutLineaParBO
048600110304     C                                               :pOutSerieBO
048700110304     C                                               :pOutNumSpedBO
048800110304     C                                               :pOutDcmBO
048900110304     C                                               :pOutCcaBO
049000110304     C                                               :pOutRblBO))
049100110304     C* Se trattasi d bolla nn originale reperisco la relativa bolla originale
049200110304     C                   if        pOutLblTyp <> 'O'
049300110304     C                   eval      wAAS = pOutAnnoBO
049400110304     C                   eval      wLNP = pOutLineaParBO
049500110304     C                   eval      wNRS = pOutSerieBO
049600110304     C                   eval      wNSP = pOutNumSpedBO
049700110304     C                   endif
049800110304     C*
049900110304     C* Chiamata metodo GetLastChild
050000110304     C                   eval      wEsito2 = %char(UBLBLSPE_GetLastChild(
050100110304     C                                                wAAS
050200110304     C                                               :wLNP
050300110304     C                                               :wNRS
050400110304     C                                               :wNSP
050500110304     C                                               :pOutAnnoFI
050600110304     C                                               :pOutLineaParFI
050700110304     C                                               :pOutSerieFI
050800110304     C                                               :pOutNumSpedFI
050900110304     C                                               :pOutDcmFI
051000110304     C                                               :pOutCcaFI))
051100110304     C*
051200110304     C* Considerazioni sullo stato corrente della bolla
051300110304     C                   if        (wEsito2 = '9' and pOutDcmBO > *zeros)  or
051400110304     C                             (wEsito2 = '0' and pOutDcmFI > *zeros)
051500110304     C                   seton                                        40        * bolla cons.
051600110304     C                   endif
051700110304     C*
051800110304     C                   if        (wEsito2 = '9' and pOutDcmBO > *zeros
051900110304     C                                            and pOutCcaBO = *blanks) or
052000110304     C                             (wEsito2 = '0' and pOutDcmFI > *zeros
052100110304     C                                            and pOutCcaFI = *blanks)
052200110304     C                   seton                                        41        * bolla cons. OK
052300110304     C                   endif
052400110304     C*
052500110304     C                   ENDSR
052600110304     C*------------------------------------------------------------------------*
052700110304
052800110304
052900110304
053000110304     C*------------------------------------------------------------------------*
053100110304     C* VERIFICA - Routine di verifica validità record corrente
053200110304     C*------------------------------------------------------------------------*
053300110304     C     VERIFICA      BEGSR
053400110304     C*
053500110304     C                   movel     'S'           CHKREC            1
053600110307     C*
053700110307     C                   movel     tasaas        wrkdata8          8 0
053800110307     C                   move      tasmgs        wrkdata8
053900110307     C                   movel     wrkdata8      wrkAAAAMM         6 0
054000110304     C*
054100110304     C* Verifico le date spedizione
054200110304     C                   if        CHKREC = 'S'
054300110304     C                   if        DATADA > *zeros
054400110304     C                   movel     tasaas        wrkdata           8 0
054500110304     C                   move      tasmgs        wrkdata
054600110304     C                   if        wrkdata < DATADA or
054700110304     C                             wrkdata > DATAA
054800110304     C                   movel     'N'           CHKREC
054900110304     C                   endif
055000110304     C                   endif
055100110304     C                   endif
055200110304     C*
055300110304     C* Verifico le date spedizione
055400110304     C                   if        CHKREC = 'S'
055500110304     C                   if        DACMDA > *zeros
055600110304     C                   if        tasdcm  < DACMDA or
055700110304     C                             tasdcm  > DACMA
055800110304     C                   movel     'N'           CHKREC
055900110304     C                   endif
056000110304     C                   endif
056100110304     C                   endif
056200110304     C*
056300110304     C* Verifico il codice cliente mittente
056400110304     C                   if        CHKREC = 'S'
056500110304     C                   if        wChkCli  = 'C'      AND
056600110304     C                             tasccm  <> KUNI(I)
056700110304     C                   movel     'N'           CHKREC
056800110304     C                   endif
056900110304     C                   endif
057000110304     C*
057100110304     C* Verifico il codice cliente fatturazione (in caso d 'K')
057200110304     C                   if        CHKREC = 'S'
057300110304     C                   if        wChkCli  = 'K'      AND
057400110304     C                             tasksc  <> KUNI(I)
057500110304     C                   movel     'N'           CHKREC
057600110304     C                   endif
057700110304     C                   endif
057800110304     C*
057900110304     C* Verifico il codice cliente fatturazione (in caso d 'E')
058000110304     C                   if        CHKREC = 'S'
058100110304     C                   if        TIPCLI  = 'E'       AND
058200110304     C                             wChkCli = 'K'       AND
058300110304     C                             tasksc  = tasccm
058400110304     C                   movel     'N'           CHKREC
058500110304     C                   endif
058600110304     C                   endif
058700110304     C*
058800110304     C* Verifico la serie
058900110304     C                   if        CHKREC = 'S'
059000110307     C                   if        NUMSER <> *blanks
059100110307     C                   if        tasnrs <> %dec(NUMSER : 2 : 0)
059200110304     C                   movel     'N'           CHKREC
059300110304     C                   endif
059400110304     C                   endif
059500110304     C                   endif
059600110304     C*
059700110304     C* Verifico il tipo bolla
059800110304     C                   if        CHKREC = 'S'
059900140326     C                   select
060000140326     C                   when      TIPBOL = '99'
060100140326     C                   when      TIPBOL = '98' AND
060200140326     C                             tastbl <> 'F1' AND tastbl <> 'A2'
060300140326     C                   movel     'N'           CHKREC
060400140326     C                   when      TIPBOL <> '99' AND
060500140326     C                             TIPBOL <> '98' AND
060600140326     C                             tastbl <> TIPBOL
060700140326     C                   movel     'N'           CHKREC
060800140326     C                   endsl
060900110304     C                   endif
061000110304     C*
061100110304     C* Verifico se richiesto nel lancio che la bolla sia consegnata
061200110304     C                   if        CHKREC = 'S'
061300110304     C                   if        FLGCONS = 'S'
061400110304     C                   exsr      CHKCONS
061500110304     C                   if        *in40 = *on
061600110304     C                   else
061700110304     C                   movel     'N'           CHKREC
061800110304     C                   endif
061900110304     C                   endif
062000110304     C                   endif
062100110304     C*
062200110304     C* Verifico se richiesto nel lancio che la bolla sia consegnata OK
062300110304     C                   if        CHKREC = 'S'
062400110304     C                   if        FLGCONS = 'O'
062500110304     C                   exsr      CHKCONS
062600110304     C                   if        *in41 = *on
062700110304     C                   else
062800110304     C                   movel     'N'           CHKREC
062900110304     C                   endif
063000110304     C                   endif
063100110304     C                   endif
063200110304     C*
063300110304     C* Verifico se richiesto nel lancio che la bolla NON sia consegnata OK
063400110304     C                   if        CHKREC = 'S'
063500110304     C                   if        FLGCONS = 'K'
063600110304     C                   exsr      CHKCONS
063700110304     C                   if        *in41 = *on
063800110304     C                   movel     'N'           CHKREC
063900110304     C                   endif
064000110304     C                   endif
064100110304     C                   endif
064200110304     C*
064300110304     C* Verifico se richiesto nel lancio che la bolla NON sia consegnata
064400110304     C                   if        CHKREC = 'S'
064500110304     C                   if        FLGCONS = 'P'
064600110304     C                   exsr      CHKCONS
064700110304     C                   if        *in40 = *on
064800110304     C                   movel     'N'           CHKREC
064900110304     C                   endif
065000110304     C                   endif
065100110304     C                   endif
065200110304     C*
065300110304     C                   ENDSR
065400110304     C*------------------------------------------------------------------------*
065500001218
065600001218
065700001218
065800001218     C*------------------------------------------------------------------------*
065900110307     C* SCRIVIWF - Routine di scrittura WFCLN10F
066000001218     C*------------------------------------------------------------------------*
066100100219     C     SCRIVIWF      BEGSR
066200100219     C*
066300110307     C                   clear                   WFCLN110
066400110307     C                   clear                   WFCLN120
066500150210     C                   clear                   WFCLN130
066600110307     C                   clear                   WFCLN_10
066700110307     C                   clear                   WFCLN_11
066800110307     C                   clear                   WFCLN_12
066900150210     C                   clear                   WFCLN_13
067000100219     C*
067100100219     C                   eval      dlyLNP = tasLNP
067200100219     C                   eval      dlyTSP = tasTSP
067300120309     C*
067400120309     C                   if        dlyTSP = 'D'
067500120309     C                   eval      dlyTSP = 'C'
067600120309     C                   endif
067700120309     C*
067800100219     C                   eval      dlyNAR = tasNZD
067900100219     C                   eval      dlyCAP = tasCAD
068000100219     C     KEYdly05_P    chain     wadly05l
068100100219     C                   if        %found(wadly05l)
068200100219     C                   eval      WCLNDLYTRC = DLYTRC
068300100219     C                   endif
068400100219     C*
068500100219     C                   eval      tblcod = 'PR'
068600100219     C                   eval      tblkey = tasprd
068700100219     C     KEYtab_C      chain     tabel00f
068800100219     C                   if        %found(tabel00f)
068900100219     C                   eval      DSPR = tbluni
069000100219     C                   eval      tblcod = '14'
069100100219     C                   eval      tblkey = §PRCRE
069200100219     C     KEYtab_C      chain     tabel00f
069300100219     C                   if        %found(tabel00f)
069400100219     C                   eval      DS14 = tbluni
069500110307     C                   eval      wclnREG = §14DES
069600100219     C                   endif
069700100219     C                   endif
069800100219     C*
069900100219     C                   z-add     *zeros        wANT
070000100222     C                   z-add     *zeros        wM12
070100100219     C                   z-add     *zeros        wOKS
070200150210     C                   z-add     *zeros        wR12
070300150210     C                   z-add     *zeros        wR24
070400100219     C                   z-add     *zeros        wRIT
070500130411     C*
070600130411     C*
070700130411     C* Verifico tipo affidabilità richiesta
070800130411     C                   select
070900130411     C*
071000130411     C* - affidabilità consegna
071100130411     C                   when      TIPAFFID = 'C'
071200001218     C*
071300100219     C* ...in anticipo
071400100219     C                   select
071500130411     C                   when      tasnci >= -999 AND
071600130411     C                             tasnci <= -24
071700100219     C                   add       1             wANT
071800100222     C*
071900100222     C* ...border-line
072000130411     C                   when      tasnci >  -24  AND
072100130411     C                             tasnci <= -12
072200100222     C                   add       1             wM12
072300100219     C*
072400100219     C* ...in orario
072500130411     C                   when      tasnci >  -12  AND
072600130411     C                             tasnci <=   0
072700100219     C                   add       1             wOKS
072800150210     C*
072900150210     C* ...in ritardo entro 12 ore
073000150210     C                   when      tasnci >    0  AND
073100150210     C                             tasnci <=  12
073200150210     C                   add       1             wR12
073300150210     C*
073400150210     C* ...in ritardo entro 24 ore
073500150210     C                   when      tasnci >   12  AND
073600150210     C                             tasnci <=  24
073700150210     C                   add       1             wR24
073800150210     C*
073900150210     C* ...in ritardo "molto"
074000150210     C                   when      tasnci >   24
074100150210     C                   add       1             wRIT
074200150210     C                   endsl
074300130411     C*
074400130411     C* - affidabilità cliente (*DEFAULT)
074500130411     C                   other
074600130411     C*
074700130411     C* ...in anticipo
074800130411     C                   select
074900150210     C                   when      tasnrc >= -999 AND
075000150210     C                             tasnrc <= -24
075100130411     C                   add       1             wANT
075200130411     C*
075300130411     C* ...border-line
075400150210     C                   when      tasnrc >  -24  AND
075500150210     C                             tasnrc <= -12
075600130411     C                   add       1             wM12
075700130411     C*
075800130411     C* ...in orario
075900150210     C                   when      tasnrc >  -12  AND
076000150210     C                             tasnrc <=   0
076100130411     C                   add       1             wOKS
076200150210     C*
076300150210     C* ...in ritardo entro 12 ore
076400150210     C                   when      tasnrc >    0  AND
076500150210     C                             tasnrc <=  12
076600150210     C                   add       1             wR12
076700150210     C*
076800150210     C* ...in ritardo entro 24 ore
076900150210     C                   when      tasnrc >   12  AND
077000150210     C                             tasnrc <=  24
077100150210     C                   add       1             wR24
077200150210     C*
077300150210     C* ...in ritardo "molto"
077400150210     C                   when      tasnrc >   24
077500150210     C                   add       1             wRIT
077600150210     C                   endsl
077700130411     C*
077800130411     C                   endsl
077900100219     C*
078000100219     C* Conteggio spedizioni DIR/GIAC/CCA
078100100219     C                   eval      dtasflo = tasflo
078200100219     C                   if        tasCCA   <> *blanks OR
078300100219     C                             tasFGC   <> *blanks OR
078400100219     C                             §FLONAV  <> *blanks
078500100219     C                   eval      wclnANOM = 'S'
078600100219     C                   endif
078700100219     C*
078800111130     C                   eval      wclnCCM    = tasccm
078900111130     C                   if        wChkCli = 'K'
079000111130     C                   eval      wclnCCM    = tasksc
079100111130     C                   endif
079200130403     C*
079300130403     C* Se richiesta estrazione per "unificante" (di qualunque tipo come CCM considero sempre
079400130403     C* l'unificante)
079500130403     C
079600130403     C                   if        FLGUNI <> *blanks
079700130403     C                   eval      wclnCCM    = CODCLI
079800130403     C                   endif
079900111130     C*
080000100219     C                   eval      wclnAAAAMM = wrkAAAAMM
080100100219     C                   eval      wclnPRD    = tasprd
080200100219     C                   eval      wclnCAD    = tascad
080300100219     C                   eval      wclnISO    = tasfin
080400110307     C                   eval      wclnAAS    = tasaas
080500110307     C                   eval      wclnLNP    = taslnp
080600110307     C                   eval      wclnNRS    = tasnrs
080700110307     C                   eval      wclnNSP    = tasnsp
080800110307     C*
080900110307     C                   select
081000110307     C                   when      *in26
081100110307     C     KEYcln_12C    chain     wfcln12l
081200110307     C                   if        %found(wfcln12l)
081300110307     C                   eval      WFCLN_10 = WFCLN_12
081400100219     C                   add       wANT          wclnANT
081500100222     C                   add       wM12          wclnM12
081600100219     C                   add       wOKS          wclnOKS
081700150210     C                   add       wR12          wclnR12
081800150210     C                   add       wR24          wclnR24
081900100219     C                   add       wRIT          wclnRIT
082000100219     C                   add       1             wclnTOT
082100110307     C                   eval      WFCLN_12 = WFCLN_10
082200110307     C                   update    WFCLN120
082300100219     C                   else
082400100219     C                   add       wANT          wclnANT
082500100222     C                   add       wM12          wclnM12
082600100219     C                   add       wOKS          wclnOKS
082700150210     C                   add       wR12          wclnR12
082800150210     C                   add       wR24          wclnR24
082900100219     C                   add       wRIT          wclnRIT
083000100219     C                   add       1             wclnTOT
083100110307     C                   eval      WFCLN_12 = WFCLN_10
083200110307     C                   write     WFCLN120
083300100219     C                   endif
083400110307     C*
083500110307     C                   when      *in27
083600110307     C                   add       wANT          wclnANT
083700110307     C                   add       wM12          wclnM12
083800110307     C                   add       wOKS          wclnOKS
083900150210     C                   add       wR12          wclnR12
084000150210     C                   add       wR24          wclnR24
084100110307     C                   add       wRIT          wclnRIT
084200110307     C                   add       1             wclnTOT
084300110307     C                   eval      WFCLN_11 = WFCLN_10
084400110307     C                   write     WFCLN110
084500150210     C*
084600150210     C                   when      *in28
084700150210     C                   add       wANT          wclnANT
084800150210     C                   add       wM12          wclnM12
084900150210     C                   add       wOKS          wclnOKS
085000150210     C                   add       wR12          wclnR12
085100150210     C                   add       wR24          wclnR24
085200150210     C                   add       wRIT          wclnRIT
085300150210     C                   add       1             wclnTOT
085400150210     C                   eval      WFCLN_13 = WFCLN_10
085500150210     C                   write     WFCLN130
085600110307     C*
085700110307     C                   endsl
085800001218     C*
085900001218     C                   ENDSR
086000001221     C*------------------------------------------------------------------------*
086100110304
086200110304
086300110304
086400110304     C*------------------------------------------------------------------------*
086500110304     C* WRITIVGD - Routine di scrittura file TIVGD (file VAS generico download)
086600110304     C*------------------------------------------------------------------------*
086700110304     C     WRITIVGD      BEGSR
086800110304     C*
086900110307     C* Leggo tutto il file WFCLN11L in QTEMP del job corrente
087000110307     C     *loval        setll     WFCLN11L
087100110307     C                   read      WFCLN11L
087200110307     C                   dow       not %eof(WFCLN11L)
087300110307     C*
087400110304     C                   clear                   tivgd000
087500110307     C                   eval      vgdDTA = %subst(WFCLN_11:1:%size(WFCLN_11))
087600110304     C                   eval      vgdTIP = TIPFILE
087700110304     C                   movel     *all'0'       vgdKSU
087800110304     C                   move      CODCLIVAS     vgdKSU
087900110304     C                   eval      vgdTSC = 'WW'
088000110304     C                   eval      vgdDAT = datcor
088100110304     C                   eval      vgdPGM = 'TIS7FFR'
088200110304     C*
088300110307     C***                clear                   DVGDFLO
088400110307     C***                movel     'P'           §VGDFELA
088500110307     C***                movel     DVGDFLO       vgdflo
088600110304     C*
088700110304     C                   write     tivgd000
088800110304     C*
088900110307     C                   read      WFCLN11L
089000110307     C                   enddo
089100110307     C*
089200110307     C* Infine forzo l'elaborazione immediata del file d output
089300110307     C***                exsr      forzadwn
089400110307     C*
089500110304     C                   ENDSR
089600110304     C*------------------------------------------------------------------------*
089700110304
089800110304
089900110304
090000110304     C*------------------------------------------------------------------------*
090100110304     C* FORZADWN - Routine di forzatura elaborazione immediata download
090200110304     C*------------------------------------------------------------------------*
090300110304     C     FORZADWN      BEGSR
090400110304     C*
090500110304     C                   clear                   tis781ds
090600110304     C                   eval      §781tip = vgdTIP
090700110304     C                   eval      §781ksu = vgdKSU
090800110304     C                   eval      §781tsc = vgdTSC
090900110304     C                   eval      §781dat = vgdDAT
091000110304     C                   eval      §781pgm = vgdPGM
091100110304     C*
091200110304     C                   clear                   TIS781DFLO
091300110304     C                   movel     'P'           §781floela
091400110304     C                   movel     TIS781DFLO    §781flo
091500110304     C*
091600110304     C* Finita l'elaborazione chiamo pgm
091700110304     C                   CALL      'TIS781C1'
091800110304     C                   parm      *blanks       esito             1
091900110304     C                   parm                    tis781ds
092000110304     C                   parm      *blanks       vgdPRG           10
092100110304     C*
092200110304     C                   ENDSR
092300110304     C*------------------------------------------------------------------------*
092400120214
092500120214
092600120214
092700120214     C*------------------------------------------------------------------------*
092800120214     C* CARTBL - Routine di caricamento dati tabellati
092900120214     C*------------------------------------------------------------------------*
093000120214     C     CARTBL        BEGSR
093100120214     C*
093200120214     C                   Z-ADD     1             I                 4 0
093300120214     C                   MOVEL     CODCLI        KUNI(I)
093400130403     C*
093500130403     C                   SELECT
093600130403     C                   WHEN      FLGUNI = 'U'
093700120214     C                   MOVEL     '3K'          tblCOD
093800120214     C     KEYtab_P      CHAIN     TABEL                              31
093900120214     C     *IN31         DOWEQ     '0'
094000120214     C     TBLFLG        IFEQ      ' '
094100120214     C                   MOVEL     TBLUNI        DS3K
094200120214     C     §3KCKS        IFEQ      CODCLI
094300120214     C                   ADD       1             I
094400120214     C                   MOVEL     TBLKEY        wKUNI             7 0
094500120214     C     wKUNI         LOOKUP    KUNI                                   55
094600120214     C                   IF        %equal
094700120214     C                   ELSE
094800120214     C                   Z-ADD     wKUNI         KUNI(I)
094900120214     C                   ENDIF
095000120214     C                   ENDIF
095100120214     C                   ENDIF
095200120214     C     KEYtab_P      READE     TABEL                                  31
095300120214     C                   ENDDO
095400130403     C*
095500130403     C                   WHEN      FLGUNI = 'T'
095600130403     C                   CLEAR                   TIBS10DS
095700130403     C                   EVAL      D10DRF = datcor                              *data riferimento
095800130403     C                   EVAL      D10TLE = 'ST'                                *tipo legame
095900130403     C                   EVAL      D10PAF = 'F'                                 *cerca se padre
096000130403     C                   EVAL      D10COD = CODCLI
096100130403     C                   CALL      'TIBS10R'
096200130403     C                   PARM                    TIBS10DS
096300130403     C                   IF        D10ERR = *BLANKS                             *Padre
096400130403     C                   Z-ADD     1             I
096500130403     C                   DOW       skc(I) > *zeros
096600130403     C                   Z-ADD     skc(I)        KUNI(I)
096700130403     C                   ADD       1             I
096800130403     C                   ENDDO
096900130403     C                   ENDIF
097000130403     C*
097100130403     C                   ENDSL
097200120214     C*
097300120214     C                   ENDSR
097400120214     C*---------------------------------------------------------------*
097500120214
097600001218
097700001218
097800001218     C*------------------------------------------------------------------------*
097900001218     C* *INZSR - ROUTINE INIZIALE
098000001218     C*------------------------------------------------------------------------*
098100001218     C     *INZSR        BEGSR
098200001218     C*
098300001218     C     *ENTRY        PLIST
098400100219     C                   PARM                    KPJBA
098500001218     C*
098600100219     C                   MOVEL     KPJBU         DSINPUT
098700100219     C                   EVAL      tblKUT = 1
098800110307     C*
098900110307     C* Determino la data corrente
099000110307     C                   Z-ADD     *zeros        datcor            8 0
099100110307     C                   EVAL      datcor = %dec(%date() : *ISO)
099200001218     C*
099300001218     C* Definizioni chiavi
099400100219     C*
099500100219     C     KEYdly05_P    KLIST
099600100219     C                   KFLD                    dlyLNP
099700100219     C                   KFLD                    dlyTSP
099800100219     C                   KFLD                    dlyNAR
099900100219     C                   KFLD                    dlyCAP
100000120214     C*
100100120214     C     KEYtab_P      KLIST
100200120214     C                   KFLD                    tblKUT
100300120214     C                   KFLD                    tblCOD
100400001218     C*
100500100219     C     KEYtab_C      KLIST
100600100219     C                   KFLD                    tblKUT
100700100219     C                   KFLD                    tblCOD
100800100219     C                   KFLD                    tblKEY
100900110307     C*
101000110307     C     KEYcln_11C    KLIST
101100110307     C                   KFLD                    wclnCCM
101200110307     C                   KFLD                    wclnAAAAMM
101300110307     C                   KFLD                    wclnAAS
101400110307     C                   KFLD                    wclnLNP
101500110307     C                   KFLD                    wclnNRS
101600110307     C                   KFLD                    wclnNSP
101700030704     C*
101800110307     C     KEYcln_12C    KLIST
101900100219     C                   KFLD                    wclnCCM
102000100219     C                   KFLD                    wclnAAAAMM
102100110307     C                   KFLD                    wclnREG
102200100219     C                   KFLD                    wclnPRD
102300100219     C                   KFLD                    wclnCAD
102400100219     C                   KFLD                    wclnISO
102500100219     C                   KFLD                    wclnANOM
102600150210     C*
102700150210     C     KEYcln_13C    KLIST
102800150210     C                   KFLD                    wclnCCM
102900150210     C                   KFLD                    wclnAAAAMM
103000150210     C                   KFLD                    wclnREG
103100150210     C                   KFLD                    wclnPRD
103200100219     C*
103300001218     C                   ENDSR

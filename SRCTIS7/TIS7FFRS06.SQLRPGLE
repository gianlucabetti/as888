000100110304     H DECEDIT('0,') DATEDIT(*DMY.)
000200110304     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP('BARTVAS')
000300110304     H*--------------------
000400001218     FTABEL00F  IF   E           K DISK
000500100219     FWADLY05L  IF   E           K DISK
000600110304     FWFCLN11L  UF A E           K DISK    PREFIX(F11_)
000700110307     F                                     RENAME(WFCLN100:WFCLN110)
000800110304     FWFCLN12L  UF A E           K DISK    PREFIX(F12_)
000900110307     F                                     RENAME(WFCLN100:WFCLN120)
001000110304     FTIVGD00F  UF A E             DISK    USROPN
001100001218     D*--------------------
001200001218     D* DS ESTERNE
001300001218     D*--------------------
001400110307     D WFCLN_10      E DS                  EXTNAME(WFCLN10F)
001500110307     D WFCLN_11      E DS                  EXTNAME(WFCLN10F) PREFIX(F11_)
001600110307     D WFCLN_12      E DS                  EXTNAME(WFCLN10F) PREFIX(F12_)
001700110307     D TITAS00F      E DS
001800900517     D KPJBA         E DS
001900100219     D DTASFLO       E DS
002000100219     D DSPR          E DS
002100100219     D DS14          E DS
002200120214     D DS3K          E DS
002300110307     D trul47ds      e ds
002400110307     D TIS781DS      E DS
002500110307     D TIS781DFLO    E DS
002600110307     D DVGDFLO       E DS
002700110304     D*--------------------
002800110304     D* SCHIERA PER REPERIMENTO CLIENTI "FIGLI" DI UN UNIFICANTE
002900110304     D*--------------------
003000110304     D KUNI            S              7  0 DIM(1000) DESCEND
003100130403     D*-------------------
003200130403     D* DS REPERIMENTO PADRE/FIGLI
003300130403     D*-------------------
003400130403     D TIBS10DS      E DS
003500130403     D skc                           11  0 DIM(500) overlay(D10SKC)
003600110304     D*--------------------
003700110304     D* DS DI RIDEFINIZIOINE PARAMETRI IN INPUT
003800110304     D*--------------------
003900110304     D DSINPUT         DS
004000110304     D  DATADA                        8  0
004100110304     D  DATAA                         8  0
004200110304     D  CODCLI                        7  0
004300110304     D  FLGUNI                        1
004400110304     D  FLGCONS                       1
004500110304     D  CODCLIVAS                     7  0
004600110304     D  TIPFILE                       2
004700110304     D  FLGEXE                        1
004800110304     D  TIPCLI                        1
004900110304     D  DACMDA                        8  0
005000110304     D  DACMA                         8  0
005100110304     D  TIPBOL                        2
005200110307     D  NUMSER                        2
005300110304     D  TIPDET                        1
005400130411     D  TIPAFFID                      1
005500110304     D*--------------------
005600110304
005700010605     D*--------------------
005800010605     D* VARIABILI DI WRK
005900010605     D*--------------------
006000100219     D wANT            s              7  0 inz
006100100222     D wM12            s              7  0 inz
006200100219     D wOKS            s              7  0 inz
006300100219     D wRIT            s              7  0 inz
006400110304
006500110304
006600110304     D*------------------
006700110304     D* VARIABILI D WRK
006800110304     D*------------------
006900110304     D  wAAS           S                   like(tasAAS) inz
007000110304     D  wLNP           S                   like(tasLNP) inz
007100110304     D  wNRS           S                   like(tasNRS) inz
007200110304     D  wNSP           S                   like(tasNSP) inz
007300110304
007400110304
007500110304     D*------------------
007600110304     D* LINKING A DEFINIZIONI ESTERNE
007700110304     D*------------------
007800110304     D/COPY GAITRASRC/SRCPROTOPR,UBLBLSPE
007900110304     D/COPY GAITRASRC/SRCPROTOPI,UBLBLSPE
008000010605
008100010605
008200920812     C*---------------------------------------------------------------*
008300001218     C* MAIN
008400001218     C*---------------------------------------------------------------*
008500110304     C*
008600110304     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
008700110304     C
008800110304     C/EXEC SQL
008900110304     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
009000110304     C/END-EXEC
009100110304     C
009200110304     C*
009300120214     C                   exsr      cartbl
009400110304     C                   exsr      chkpar
009500110304     C*
009600120214     C**** Fisso KUNI(1)
009700120214     C***                   eval      KUNI(1) = CODCLI
009800110304     C*
009900110304     C* Ciclo sulla schiera dei clienti figli dell'unificante preso in input
010000110304     C                   z-add     1             I                 4 0
010100110304     C                   sorta     KUNI
010200110304     C                   dow       KUNI(I) > *zeros
010300001218     C                   exsr      procedi
010400110307     C                   add       1             I
010500110304     C                   enddo
010600110307     C*
010700110307     C* Inizializzo variabili d wrk
010800110307     C                   movel     'N'           wProcedi          1
010900110307     C*
011000110307     C* Apro il file d output
011100110307     C                   open      tivgd00f
011200110307     C*
011300110307     C* Come prima cosa avvio il blocco elaborazione TIVGD x tipo file corrente: 'VC'
011400110307     C                   clear                   trul47ds
011500110307     C                   eval      d47opz  = 'I'
011600120927     C                   eval      d47tip  = TIPFILE
011700110307     C                   eval      d47lck  = 'N'
011800110307     C                   eval      d47chkj = 'N'
011900110307     C                   eval      d47pgm  = 'TIS7FFR'
012000110307     C                   call      'TRUL47R'
012100110307     C                   parm                    trul47ds
012200110307     C*
012300110307     C* Se elaborazione consentita => proseguo
012400120928     C***                if        d47sts <> 'A'
012500110307     C                   movel     'S'           wProcedi
012600120928     C***                endif
012700110307     C*
012800110307     C* Popolo il file d output
012900110307     C                   if        wProcedi = 'S'
013000110307     C                   exsr      WRITIVGD
013100110307     C                   endif
013200110304     C*
013300110304     C* Chiudo il file di output
013400110304     C                   close     tivgd00f
013500110304     C*
013600110304     C* Infine elimino il blocco elaborazione TIVGD x tipo file corrente: 'VC'
013700110304     C                   clear                   trul47ds
013800110304     C                   eval      d47opz  = 'F'
013900120928     C                   eval      d47tip  = TIPFILE
014000110304     C                   call      'TRUL47R'
014100110304     C                   parm                    trul47ds
014200001218     C*
014300001218     C                   seton                                        LR
014400110304
014500110304
014600110304
014700110304     C*------------------------------------------------------------------------*
014800110304     C* CHKPAR - Routine di controllo parametri ricevuti in input
014900110304     C*------------------------------------------------------------------------*
015000110304     C     CHKPAR        BEGSR
015100110304     C*
015200110304     C* Verifico le date
015300110304     C                   if        DATADA > *zeros AND
015400110304     C                             DATAA  = *zeros
015500110304     C                   movel     *all'9'       DATAA
015600110304     C                   endif
015700110304     C                   if        DACMDA > *zeros AND
015800110304     C                             DACMA  = *zeros
015900110304     C                   movel     *all'9'       DACMA
016000110304     C                   endif
016100110307     C*
016200110307     C* Verifico il livello d dettaglio richiesto
016300110307     C                   setoff                                       2627
016400110307     C                   select
016500110307     C                   when      TIPDET = 'C'                                 * x CAP
016600110307     C                   seton                                        26
016700110307     C                   when      TIPDET = 'S'                                 * x SPED
016800110307     C                   seton                                        27
016900110307     C                   endsl
017000110304     C*
017100110304     C                   ENDSR
017200110304     C*------------------------------------------------------------------------*
017300110304
017400110304
017500110304
017600110304     C*------------------------------------------------------------------------*
017700110304     C* PROCEDI - Routine principale
017800110304     C*------------------------------------------------------------------------*
017900110304     C     PROCEDI       BEGSR
018000110304     C*
018100110304     C* Se richiesta elaborazione x codice cliente mittente...
018200110304     C                   select
018300110304     C                   when      TIPCLI = *blanks
018400110304     C                   exsr      EXECCM
018500110304     C*
018600110304     C* Se richiesta elaborazione x codice cliente fatturazione...
018700110304     C                   when      TIPCLI = 'K'
018800111130     C                   exsr      EXEKSC
018900110304     C*
019000110304     C* Se richiesta elaborazione x entrambi: codice cliente mittente + fatturazione...
019100110304     C                   when      TIPCLI = 'E'
019200110304     C                   exsr      EXECCM
019300111130     C                   exsr      EXEKSC
019400110304     C*
019500110304     C                   endsl
019600110304     C*
019700110304     C                   ENDSR
019800110304     C*------------------------------------------------------------------------*
019900110304
020000110304
020100110304
020200110304     C*------------------------------------------------------------------------*
020300110304     C* EXECCM   - Elaborazione x codice cliente mittente
020400110304     C*------------------------------------------------------------------------*
020500110304     C     EXECCM        BEGSR
020600110304     C*
020700110304     C                   movel     'C'           wChkCli           1
020800110304     C*
020900110304     C                   select
021000110304     C* ...se richiesta elaborazione x data spedizione
021100110304     C                   when      DATADA > *zeros
021200110304     C                   exsr      SQ_CCMDSP
021300110304     C* ...se richiesta elaborazione x data consegna merce
021400110304     C                   when      DACMDA > *zeros
021500110304     C                   exsr      SQ_CCMDCM
021600110304     C                   endsl
021700110304     C*
021800110304     C                   ENDSR
021900110304     C*------------------------------------------------------------------------*
022000111130
022100111130
022200111130
022300111130     C*------------------------------------------------------------------------*
022400111130     C* EXEKSC   - Elaborazione x codice cliente fatturazione
022500111130     C*------------------------------------------------------------------------*
022600111130     C     EXEKSC        BEGSR
022700111130     C*
022800111130     C                   movel     'K'           wChkCli           1
022900111130     C*
023000111130     C                   select
023100111130     C* ...se richiesta elaborazione x data spedizione
023200111130     C                   when      DATADA > *zeros
023300111130     C                   exsr      SQ_KSCDSP
023400111130     C* ...se richiesta elaborazione x data consegna merce
023500111130     C                   when      DACMDA > *zeros
023600111130     C                   exsr      SQ_KSCDCM
023700111130     C                   endsl
023800111130     C*
023900111130     C                   ENDSR
024000111130     C*------------------------------------------------------------------------*
024100110304
024200110304
024300110304
024400110304     C*------------------------------------------------------------------------*
024500110304     C* SQ_CCMDSP - Elaborazione x codice cliente mittente + data spedizione
024600110304     C*------------------------------------------------------------------------*
024700110304     C     SQ_CCMDSP     BEGSR
024800110304     C*
024900110304     C                   z-add     KUNI(I)       wParCCM           7 0
025000110304     C*
025100110304     C/EXEC SQL
025200110304     C+ declare C_CCMDSP cursor for
025300110304     C+ select * from titas00f
025400110304     C+ where tasccm = :wParCCM and
025500110304     C+ tasaas*10000+tasmgs between :DATADA and :DATAA
025600110304     C+ union
025700110304     C+ select * from titas10f
025800110304     C+ where tasccm = :wParCCM and
025900110304     C+ tasaas*10000+tasmgs between :DATADA and :DATAA
026000110304     C+ union
026100110304     C+ select * from titasP0f
026200110304     C+ where tasccm = :wParCCM and
026300110304     C+ tasaas*10000+tasmgs between :DATADA and :DATAA
026400110304     C+ for read only
026500110304     C/END-EXEC
026600110304     C
026700110304     C/EXEC SQL
026800110304     C+ open C_CCMDSP
026900110304     C/END-EXEC
027000110304     C
027100110304     C/EXEC SQL
027200110304     C+ Fetch C_CCMDSP into :TITAS00F
027300110304     C/END-EXEC
027400110304     C*
027500110304     C                   dow       sqlcod = *zeros
027600110304     C                   exsr      verifica
027700110304     C                   if        CHKREC = 'S'
027800110304     C                   exsr      scriviWF
027900110304     C                   endif
028000110304     C
028100110304     C/EXEC SQL
028200110304     C+ Fetch C_CCMDSP into :TITAS00F
028300110304     C/END-EXEC
028400110304     C*
028500110304     C                   enddo
028600110304     C*
028700110304     C/EXEC SQL
028800110304     C+ close C_CCMDSP
028900110304     C/END-EXEC
029000110304     C*
029100110304     C*
029200110304     C                   ENDSR
029300110304     C*------------------------------------------------------------------------*
029400111130
029500111130
029600111130
029700111130     C*------------------------------------------------------------------------*
029800111130     C* SQ_KSCDSP - Elaborazione x codice cliente fatturazione + data spedizione
029900111130     C*------------------------------------------------------------------------*
030000111130     C     SQ_KSCDSP     BEGSR
030100111130     C*
030200111130     C                   z-add     KUNI(I)       wParKSC           7 0
030300111130     C*
030400111130     C/EXEC SQL
030500111130     C+ declare C_KSCDSP cursor for
030600111130     C+ select * from titas00f
030700111130     C+ where tasksc = :wParKSC and
030800111130     C+ tasaas*10000+tasmgs between :DATADA and :DATAA
030900111130     C+ union
031000111130     C+ select * from titas10f
031100111130     C+ where tasksc = :wParKSC and
031200111130     C+ tasaas*10000+tasmgs between :DATADA and :DATAA
031300111130     C+ union
031400111130     C+ select * from titasP0f
031500111130     C+ where tasksc = :wParKSC and
031600111130     C+ tasaas*10000+tasmgs between :DATADA and :DATAA
031700111130     C+ for read only
031800111130     C/END-EXEC
031900111130     C
032000111130     C/EXEC SQL
032100111130     C+ open C_KSCDSP
032200111130     C/END-EXEC
032300111130     C
032400111130     C/EXEC SQL
032500111130     C+ Fetch C_KSCDSP into :TITAS00F
032600111130     C/END-EXEC
032700111130     C*
032800111130     C                   dow       sqlcod = *zeros
032900111130     C                   exsr      verifica
033000111130     C                   if        CHKREC = 'S'
033100111130     C                   exsr      scriviWF
033200111130     C                   endif
033300111130     C
033400111130     C/EXEC SQL
033500111130     C+ Fetch C_KSCDSP into :TITAS00F
033600111130     C/END-EXEC
033700111130     C*
033800111130     C                   enddo
033900111130     C*
034000111130     C/EXEC SQL
034100111130     C+ close C_KSCDSP
034200111130     C/END-EXEC
034300111130     C*
034400111130     C*
034500111130     C                   ENDSR
034600111130     C*------------------------------------------------------------------------*
034700110304
034800110304
034900110304
035000110304     C*------------------------------------------------------------------------*
035100110304     C* SQ_CCMDCM - Elaborazione x codice cliente mittente + data consegna merce
035200110304     C*------------------------------------------------------------------------*
035300110304     C     SQ_CCMDCM     BEGSR
035400110304     C*
035500110304     C                   z-add     KUNI(I)       wParCCM           7 0
035600110304     C*
035700110304     C/EXEC SQL
035800110304     C+ declare C_CCMDCM cursor for
035900110304     C+ select * from titas00f
036000110304     C+ where tasccm = :wParCCM and
036100110304     C+ tasdcm between :DACMDA and :DACMA
036200110304     C+ union
036300110304     C+ select * from titas10f
036400110304     C+ where tasccm = :wParCCM and
036500110304     C+ tasdcm between :DACMDA and :DACMA
036600110304     C+ union
036700110304     C+ select * from titasP0f
036800110304     C+ where tasccm = :wParCCM and
036900110304     C+ tasdcm between :DACMDA and :DACMA
037000110304     C+ for read only
037100110304     C/END-EXEC
037200110304     C
037300110304     C/EXEC SQL
037400110304     C+ open C_CCMDCM
037500110304     C/END-EXEC
037600110304     C
037700110304     C/EXEC SQL
037800110304     C+ Fetch C_CCMDCM into :TITAS00F
037900110304     C/END-EXEC
038000110304     C*
038100110304     C                   dow       sqlcod = *zeros
038200110304     C                   exsr      verifica
038300110304     C                   if        CHKREC = 'S'
038400110304     C                   exsr      scriviWF
038500110304     C                   endif
038600110304     C
038700110304     C/EXEC SQL
038800110304     C+ Fetch C_CCMDCM into :TITAS00F
038900110304     C/END-EXEC
039000110304     C*
039100110304     C                   enddo
039200110304     C*
039300110304     C/EXEC SQL
039400110304     C+ close C_CCMDCM
039500110304     C/END-EXEC
039600110304     C*
039700110304     C*
039800110304     C                   ENDSR
039900110304     C*------------------------------------------------------------------------*
040000111130
040100111130
040200111130
040300111130     C*------------------------------------------------------------------------*
040400111130     C* SQ_KSCDCM - Elaborazione x codice cliente fatturazione + data consegna merce
040500111130     C*------------------------------------------------------------------------*
040600111130     C     SQ_KSCDCM     BEGSR
040700111130     C*
040800111130     C                   z-add     KUNI(I)       wParKSC           7 0
040900111130     C*
041000111130     C/EXEC SQL
041100111130     C+ declare C_KSCDCM cursor for
041200111130     C+ select * from titas00f
041300111130     C+ where tasksc = :wParKSC and
041400111130     C+ tasdcm between :DACMDA and :DACMA
041500111130     C+ union
041600111130     C+ select * from titas10f
041700111130     C+ where tasksc = :wParKSC and
041800111130     C+ tasdcm between :DACMDA and :DACMA
041900111130     C+ union
042000111130     C+ select * from titasP0f
042100111130     C+ where tasksc = :wParKSC and
042200111130     C+ tasdcm between :DACMDA and :DACMA
042300111130     C+ for read only
042400111130     C/END-EXEC
042500111130     C
042600111130     C/EXEC SQL
042700111130     C+ open C_KSCDCM
042800111130     C/END-EXEC
042900111130     C
043000111130     C/EXEC SQL
043100111130     C+ Fetch C_KSCDCM into :TITAS00F
043200111130     C/END-EXEC
043300111130     C*
043400111130     C                   dow       sqlcod = *zeros
043500111130     C                   exsr      verifica
043600111130     C                   if        CHKREC = 'S'
043700111130     C                   exsr      scriviWF
043800111130     C                   endif
043900111130     C
044000111130     C/EXEC SQL
044100111130     C+ Fetch C_KSCDCM into :TITAS00F
044200111130     C/END-EXEC
044300111130     C*
044400111130     C                   enddo
044500111130     C*
044600111130     C/EXEC SQL
044700111130     C+ close C_KSCDCM
044800111130     C/END-EXEC
044900111130     C*
045000111130     C*
045100111130     C                   ENDSR
045200111130     C*------------------------------------------------------------------------*
045300001218
045400110304
045500110304
045600110304     C*------------------------------------------------------------------------*
045700110304     C* CHKCONS  - Verifica se bolla effettivamente consegnata
045800110304     C*------------------------------------------------------------------------*
045900110304     C     CHKCONS       BEGSR
046000110304     C*
046100110304     C                   setoff                                       4041
046200110304     C                   movel     *blanks       wEsito1           1
046300110304     C                   movel     *blanks       wEsito2           1
046400110304     C*
046500110304     C                   eval      wAAS = tasAAS
046600110304     C                   eval      wLNP = tasLNP
046700110304     C                   eval      wNRS = tasNRS
046800110304     C                   eval      wNSP = tasNSP
046900110304     C*
047000110304     C* Chiamata metodo GetLblTyp
047100110304     C                   eval      wEsito1 = %char(UBLBLSPE_GetLblTyp(
047200110304     C                                                wAAS
047300110304     C                                               :wLNP
047400110304     C                                               :wNRS
047500110304     C                                               :wNSP
047600110304     C                                               :pOutLblTyp
047700110304     C                                               :pOutAnnoBO
047800110304     C                                               :pOutLineaParBO
047900110304     C                                               :pOutSerieBO
048000110304     C                                               :pOutNumSpedBO
048100110304     C                                               :pOutDcmBO
048200110304     C                                               :pOutCcaBO
048300110304     C                                               :pOutRblBO))
048400110304     C* Se trattasi d bolla nn originale reperisco la relativa bolla originale
048500110304     C                   if        pOutLblTyp <> 'O'
048600110304     C                   eval      wAAS = pOutAnnoBO
048700110304     C                   eval      wLNP = pOutLineaParBO
048800110304     C                   eval      wNRS = pOutSerieBO
048900110304     C                   eval      wNSP = pOutNumSpedBO
049000110304     C                   endif
049100110304     C*
049200110304     C* Chiamata metodo GetLastChild
049300110304     C                   eval      wEsito2 = %char(UBLBLSPE_GetLastChild(
049400110304     C                                                wAAS
049500110304     C                                               :wLNP
049600110304     C                                               :wNRS
049700110304     C                                               :wNSP
049800110304     C                                               :pOutAnnoFI
049900110304     C                                               :pOutLineaParFI
050000110304     C                                               :pOutSerieFI
050100110304     C                                               :pOutNumSpedFI
050200110304     C                                               :pOutDcmFI
050300110304     C                                               :pOutCcaFI))
050400110304     C*
050500110304     C* Considerazioni sullo stato corrente della bolla
050600110304     C                   if        (wEsito2 = '9' and pOutDcmBO > *zeros)  or
050700110304     C                             (wEsito2 = '0' and pOutDcmFI > *zeros)
050800110304     C                   seton                                        40        * bolla cons.
050900110304     C                   endif
051000110304     C*
051100110304     C                   if        (wEsito2 = '9' and pOutDcmBO > *zeros
051200110304     C                                            and pOutCcaBO = *blanks) or
051300110304     C                             (wEsito2 = '0' and pOutDcmFI > *zeros
051400110304     C                                            and pOutCcaFI = *blanks)
051500110304     C                   seton                                        41        * bolla cons. OK
051600110304     C                   endif
051700110304     C*
051800110304     C                   ENDSR
051900110304     C*------------------------------------------------------------------------*
052000110304
052100110304
052200110304
052300110304     C*------------------------------------------------------------------------*
052400110304     C* VERIFICA - Routine di verifica validità record corrente
052500110304     C*------------------------------------------------------------------------*
052600110304     C     VERIFICA      BEGSR
052700110304     C*
052800110304     C                   movel     'S'           CHKREC            1
052900110307     C*
053000110307     C                   movel     tasaas        wrkdata8          8 0
053100110307     C                   move      tasmgs        wrkdata8
053200110307     C                   movel     wrkdata8      wrkAAAAMM         6 0
053300110304     C*
053400110304     C* Verifico le date spedizione
053500110304     C                   if        CHKREC = 'S'
053600110304     C                   if        DATADA > *zeros
053700110304     C                   movel     tasaas        wrkdata           8 0
053800110304     C                   move      tasmgs        wrkdata
053900110304     C                   if        wrkdata < DATADA or
054000110304     C                             wrkdata > DATAA
054100110304     C                   movel     'N'           CHKREC
054200110304     C                   endif
054300110304     C                   endif
054400110304     C                   endif
054500110304     C*
054600110304     C* Verifico le date spedizione
054700110304     C                   if        CHKREC = 'S'
054800110304     C                   if        DACMDA > *zeros
054900110304     C                   if        tasdcm  < DACMDA or
055000110304     C                             tasdcm  > DACMA
055100110304     C                   movel     'N'           CHKREC
055200110304     C                   endif
055300110304     C                   endif
055400110304     C                   endif
055500110304     C*
055600110304     C* Verifico il codice cliente mittente
055700110304     C                   if        CHKREC = 'S'
055800110304     C                   if        wChkCli  = 'C'      AND
055900110304     C                             tasccm  <> KUNI(I)
056000110304     C                   movel     'N'           CHKREC
056100110304     C                   endif
056200110304     C                   endif
056300110304     C*
056400110304     C* Verifico il codice cliente fatturazione (in caso d 'K')
056500110304     C                   if        CHKREC = 'S'
056600110304     C                   if        wChkCli  = 'K'      AND
056700110304     C                             tasksc  <> KUNI(I)
056800110304     C                   movel     'N'           CHKREC
056900110304     C                   endif
057000110304     C                   endif
057100110304     C*
057200110304     C* Verifico il codice cliente fatturazione (in caso d 'E')
057300110304     C                   if        CHKREC = 'S'
057400110304     C                   if        TIPCLI  = 'E'       AND
057500110304     C                             wChkCli = 'K'       AND
057600110304     C                             tasksc  = tasccm
057700110304     C                   movel     'N'           CHKREC
057800110304     C                   endif
057900110304     C                   endif
058000110304     C*
058100110304     C* Verifico la serie
058200110304     C                   if        CHKREC = 'S'
058300110307     C                   if        NUMSER <> *blanks
058400110307     C                   if        tasnrs <> %dec(NUMSER : 2 : 0)
058500110304     C                   movel     'N'           CHKREC
058600110304     C                   endif
058700110304     C                   endif
058800110304     C                   endif
058900110304     C*
059000110304     C* Verifico il tipo bolla
059100110304     C                   if        CHKREC = 'S'
059200140326     C                   select
059300140326     C                   when      TIPBOL = '99'
059400140326     C                   when      TIPBOL = '98' AND
059500140326     C                             tastbl <> 'F1' AND tastbl <> 'A2'
059600140326     C                   movel     'N'           CHKREC
059700140326     C                   when      TIPBOL <> '99' AND
059800140326     C                             TIPBOL <> '98' AND
059900140326     C                             tastbl <> TIPBOL
060000140326     C                   movel     'N'           CHKREC
060100140326     C                   endsl
060200110304     C                   endif
060300110304     C*
060400110304     C* Verifico se richiesto nel lancio che la bolla sia consegnata
060500110304     C                   if        CHKREC = 'S'
060600110304     C                   if        FLGCONS = 'S'
060700110304     C                   exsr      CHKCONS
060800110304     C                   if        *in40 = *on
060900110304     C                   else
061000110304     C                   movel     'N'           CHKREC
061100110304     C                   endif
061200110304     C                   endif
061300110304     C                   endif
061400110304     C*
061500110304     C* Verifico se richiesto nel lancio che la bolla sia consegnata OK
061600110304     C                   if        CHKREC = 'S'
061700110304     C                   if        FLGCONS = 'O'
061800110304     C                   exsr      CHKCONS
061900110304     C                   if        *in41 = *on
062000110304     C                   else
062100110304     C                   movel     'N'           CHKREC
062200110304     C                   endif
062300110304     C                   endif
062400110304     C                   endif
062500110304     C*
062600110304     C* Verifico se richiesto nel lancio che la bolla NON sia consegnata OK
062700110304     C                   if        CHKREC = 'S'
062800110304     C                   if        FLGCONS = 'K'
062900110304     C                   exsr      CHKCONS
063000110304     C                   if        *in41 = *on
063100110304     C                   movel     'N'           CHKREC
063200110304     C                   endif
063300110304     C                   endif
063400110304     C                   endif
063500110304     C*
063600110304     C* Verifico se richiesto nel lancio che la bolla NON sia consegnata
063700110304     C                   if        CHKREC = 'S'
063800110304     C                   if        FLGCONS = 'P'
063900110304     C                   exsr      CHKCONS
064000110304     C                   if        *in40 = *on
064100110304     C                   movel     'N'           CHKREC
064200110304     C                   endif
064300110304     C                   endif
064400110304     C                   endif
064500110304     C*
064600110304     C                   ENDSR
064700110304     C*------------------------------------------------------------------------*
064800001218
064900001218
065000001218
065100001218     C*------------------------------------------------------------------------*
065200110307     C* SCRIVIWF - Routine di scrittura WFCLN10F
065300001218     C*------------------------------------------------------------------------*
065400100219     C     SCRIVIWF      BEGSR
065500100219     C*
065600110307     C                   clear                   WFCLN110
065700110307     C                   clear                   WFCLN120
065800110307     C                   clear                   WFCLN_10
065900110307     C                   clear                   WFCLN_11
066000110307     C                   clear                   WFCLN_12
066100100219     C*
066200100219     C                   eval      dlyLNP = tasLNP
066300100219     C                   eval      dlyTSP = tasTSP
066400120309     C*
066500120309     C                   if        dlyTSP = 'D'
066600120309     C                   eval      dlyTSP = 'C'
066700120309     C                   endif
066800120309     C*
066900100219     C                   eval      dlyNAR = tasNZD
067000100219     C                   eval      dlyCAP = tasCAD
067100100219     C     KEYdly05_P    chain     wadly05l
067200100219     C                   if        %found(wadly05l)
067300100219     C                   eval      WCLNDLYTRC = DLYTRC
067400100219     C                   endif
067500100219     C*
067600100219     C                   eval      tblcod = 'PR'
067700100219     C                   eval      tblkey = tasprd
067800100219     C     KEYtab_C      chain     tabel00f
067900100219     C                   if        %found(tabel00f)
068000100219     C                   eval      DSPR = tbluni
068100100219     C                   eval      tblcod = '14'
068200100219     C                   eval      tblkey = §PRCRE
068300100219     C     KEYtab_C      chain     tabel00f
068400100219     C                   if        %found(tabel00f)
068500100219     C                   eval      DS14 = tbluni
068600110307     C                   eval      wclnREG = §14DES
068700100219     C                   endif
068800100219     C                   endif
068900100219     C*
069000100219     C                   z-add     *zeros        wANT
069100100222     C                   z-add     *zeros        wM12
069200100219     C                   z-add     *zeros        wOKS
069300100219     C                   z-add     *zeros        wRIT
069400130411     C*
069500130411     C*
069600130411     C* Verifico tipo affidabilità richiesta
069700130411     C                   select
069800130411     C*
069900130411     C* - affidabilità consegna
070000130411     C                   when      TIPAFFID = 'C'
070100001218     C*
070200100219     C* ...in anticipo
070300100219     C                   select
070400130411     C                   when      tasnci >= -999 AND
070500130411     C                             tasnci <= -24
070600100219     C                   add       1             wANT
070700100222     C*
070800100222     C* ...border-line
070900130411     C                   when      tasnci >  -24  AND
071000130411     C                             tasnci <= -12
071100100222     C                   add       1             wM12
071200100219     C*
071300100219     C* ...in orario
071400130411     C                   when      tasnci >  -12  AND
071500130411     C                             tasnci <=   0
071600100219     C                   add       1             wOKS
071700100219     C*
071800100219     C* ...in ritardo
071900130411     C                   when      tasnci >    0
072000100219     C                   add       1             wRIT
072100100219     C                   endsl
072200130411     C*
072300130411     C* - affidabilità cliente (*DEFAULT)
072400130411     C                   other
072500130411     C*
072600130411     C* ...in anticipo
072700130411     C                   select
072800130411     C                   when      tasnrc >= -999 AND
072900130411     C                             tasnrc <= -24
073000130411     C                   add       1             wANT
073100130411     C*
073200130411     C* ...border-line
073300130411     C                   when      tasnrc >  -24  AND
073400130411     C                             tasnrc <= -12
073500130411     C                   add       1             wM12
073600130411     C*
073700130411     C* ...in orario
073800130411     C                   when      tasnrc >  -12  AND
073900130411     C                             tasnrc <=   0
074000130411     C                   add       1             wOKS
074100130411     C*
074200130411     C* ...in ritardo
074300130411     C                   when      tasnrc >    0
074400130411     C                   add       1             wRIT
074500130411     C                   endsl
074600130411     C*
074700130411     C                   endsl
074800100219     C*
074900100219     C* Conteggio spedizioni DIR/GIAC/CCA
075000100219     C                   eval      dtasflo = tasflo
075100100219     C                   if        tasCCA   <> *blanks OR
075200100219     C                             tasFGC   <> *blanks OR
075300100219     C                             §FLONAV  <> *blanks
075400100219     C                   eval      wclnANOM = 'S'
075500100219     C                   endif
075600100219     C*
075700111130     C                   eval      wclnCCM    = tasccm
075800111130     C                   if        wChkCli = 'K'
075900111130     C                   eval      wclnCCM    = tasksc
076000111130     C                   endif
076100130403     C*
076200130403     C* Se richiesta estrazione per "unificante" (di qualunque tipo come CCM considero sempre
076300130403     C* l'unificante)
076400130403     C
076500130403     C                   if        FLGUNI <> *blanks
076600130403     C                   eval      wclnCCM    = CODCLI
076700130403     C                   endif
076800111130     C*
076900100219     C                   eval      wclnAAAAMM = wrkAAAAMM
077000100219     C                   eval      wclnPRD    = tasprd
077100100219     C                   eval      wclnCAD    = tascad
077200100219     C                   eval      wclnISO    = tasfin
077300110307     C                   eval      wclnAAS    = tasaas
077400110307     C                   eval      wclnLNP    = taslnp
077500110307     C                   eval      wclnNRS    = tasnrs
077600110307     C                   eval      wclnNSP    = tasnsp
077700110307     C*
077800110307     C                   select
077900110307     C                   when      *in26
078000110307     C     KEYcln_12C    chain     wfcln12l
078100110307     C                   if        %found(wfcln12l)
078200110307     C                   eval      WFCLN_10 = WFCLN_12
078300100219     C                   add       wANT          wclnANT
078400100222     C                   add       wM12          wclnM12
078500100219     C                   add       wOKS          wclnOKS
078600100219     C                   add       wRIT          wclnRIT
078700100219     C                   add       1             wclnTOT
078800110307     C                   eval      WFCLN_12 = WFCLN_10
078900110307     C                   update    WFCLN120
079000100219     C                   else
079100100219     C                   add       wANT          wclnANT
079200100222     C                   add       wM12          wclnM12
079300100219     C                   add       wOKS          wclnOKS
079400100219     C                   add       wRIT          wclnRIT
079500100219     C                   add       1             wclnTOT
079600110307     C                   eval      WFCLN_12 = WFCLN_10
079700110307     C                   write     WFCLN120
079800100219     C                   endif
079900110307     C*
080000110307     C                   when      *in27
080100110307     C                   add       wANT          wclnANT
080200110307     C                   add       wM12          wclnM12
080300110307     C                   add       wOKS          wclnOKS
080400110307     C                   add       wRIT          wclnRIT
080500110307     C                   add       1             wclnTOT
080600110307     C                   eval      WFCLN_11 = WFCLN_10
080700110307     C                   write     WFCLN110
080800110307     C*
080900110307     C                   endsl
081000001218     C*
081100001218     C                   ENDSR
081200001221     C*------------------------------------------------------------------------*
081300110304
081400110304
081500110304
081600110304     C*------------------------------------------------------------------------*
081700110304     C* WRITIVGD - Routine di scrittura file TIVGD (file VAS generico download)
081800110304     C*------------------------------------------------------------------------*
081900110304     C     WRITIVGD      BEGSR
082000110304     C*
082100110307     C* Leggo tutto il file WFCLN11L in QTEMP del job corrente
082200110307     C     *loval        setll     WFCLN11L
082300110307     C                   read      WFCLN11L
082400110307     C                   dow       not %eof(WFCLN11L)
082500110307     C*
082600110304     C                   clear                   tivgd000
082700110307     C                   eval      vgdDTA = %subst(WFCLN_11:1:%size(WFCLN_11))
082800110304     C                   eval      vgdTIP = TIPFILE
082900110304     C                   movel     *all'0'       vgdKSU
083000110304     C                   move      CODCLIVAS     vgdKSU
083100110304     C                   eval      vgdTSC = 'WW'
083200110304     C                   eval      vgdDAT = datcor
083300110304     C                   eval      vgdPGM = 'TIS7FFR'
083400110304     C*
083500110307     C***                clear                   DVGDFLO
083600110307     C***                movel     'P'           §VGDFELA
083700110307     C***                movel     DVGDFLO       vgdflo
083800110304     C*
083900110304     C                   write     tivgd000
084000110304     C*
084100110307     C                   read      WFCLN11L
084200110307     C                   enddo
084300110307     C*
084400110307     C* Infine forzo l'elaborazione immediata del file d output
084500110307     C***                exsr      forzadwn
084600110307     C*
084700110304     C                   ENDSR
084800110304     C*------------------------------------------------------------------------*
084900110304
085000110304
085100110304
085200110304     C*------------------------------------------------------------------------*
085300110304     C* FORZADWN - Routine di forzatura elaborazione immediata download
085400110304     C*------------------------------------------------------------------------*
085500110304     C     FORZADWN      BEGSR
085600110304     C*
085700110304     C                   clear                   tis781ds
085800110304     C                   eval      §781tip = vgdTIP
085900110304     C                   eval      §781ksu = vgdKSU
086000110304     C                   eval      §781tsc = vgdTSC
086100110304     C                   eval      §781dat = vgdDAT
086200110304     C                   eval      §781pgm = vgdPGM
086300110304     C*
086400110304     C                   clear                   TIS781DFLO
086500110304     C                   movel     'P'           §781floela
086600110304     C                   movel     TIS781DFLO    §781flo
086700110304     C*
086800110304     C* Finita l'elaborazione chiamo pgm
086900110304     C                   CALL      'TIS781C1'
087000110304     C                   parm      *blanks       esito             1
087100110304     C                   parm                    tis781ds
087200110304     C                   parm      *blanks       vgdPRG           10
087300110304     C*
087400110304     C                   ENDSR
087500110304     C*------------------------------------------------------------------------*
087600120214
087700120214
087800120214
087900120214     C*------------------------------------------------------------------------*
088000120214     C* CARTBL - Routine di caricamento dati tabellati
088100120214     C*------------------------------------------------------------------------*
088200120214     C     CARTBL        BEGSR
088300120214     C*
088400120214     C                   Z-ADD     1             I                 4 0
088500120214     C                   MOVEL     CODCLI        KUNI(I)
088600130403     C*
088700130403     C                   SELECT
088800130403     C                   WHEN      FLGUNI = 'U'
088900120214     C                   MOVEL     '3K'          tblCOD
089000120214     C     KEYtab_P      CHAIN     TABEL                              31
089100120214     C     *IN31         DOWEQ     '0'
089200120214     C     TBLFLG        IFEQ      ' '
089300120214     C                   MOVEL     TBLUNI        DS3K
089400120214     C     §3KCKS        IFEQ      CODCLI
089500120214     C                   ADD       1             I
089600120214     C                   MOVEL     TBLKEY        wKUNI             7 0
089700120214     C     wKUNI         LOOKUP    KUNI                                   55
089800120214     C                   IF        %equal
089900120214     C                   ELSE
090000120214     C                   Z-ADD     wKUNI         KUNI(I)
090100120214     C                   ENDIF
090200120214     C                   ENDIF
090300120214     C                   ENDIF
090400120214     C     KEYtab_P      READE     TABEL                                  31
090500120214     C                   ENDDO
090600130403     C*
090700130403     C                   WHEN      FLGUNI = 'T'
090800130403     C                   CLEAR                   TIBS10DS
090900130403     C                   EVAL      D10DRF = datcor                              *data riferimento
091000130403     C                   EVAL      D10TLE = 'ST'                                *tipo legame
091100130403     C                   EVAL      D10PAF = 'F'                                 *cerca se padre
091200130403     C                   EVAL      D10COD = CODCLI
091300130403     C                   CALL      'TIBS10R'
091400130403     C                   PARM                    TIBS10DS
091500130403     C                   IF        D10ERR = *BLANKS                             *Padre
091600130403     C                   Z-ADD     1             I
091700130403     C                   DOW       skc(I) > *zeros
091800130403     C                   Z-ADD     skc(I)        KUNI(I)
091900130403     C                   ADD       1             I
092000130403     C                   ENDDO
092100130403     C                   ENDIF
092200130403     C*
092300130403     C                   ENDSL
092400120214     C*
092500120214     C                   ENDSR
092600120214     C*---------------------------------------------------------------*
092700120214
092800001218
092900001218
093000001218     C*------------------------------------------------------------------------*
093100001218     C* *INZSR - ROUTINE INIZIALE
093200001218     C*------------------------------------------------------------------------*
093300001218     C     *INZSR        BEGSR
093400001218     C*
093500001218     C     *ENTRY        PLIST
093600100219     C                   PARM                    KPJBA
093700001218     C*
093800100219     C                   MOVEL     KPJBU         DSINPUT
093900100219     C                   EVAL      tblKUT = 1
094000110307     C*
094100110307     C* Determino la data corrente
094200110307     C                   Z-ADD     *zeros        datcor            8 0
094300110307     C                   EVAL      datcor = %dec(%date() : *ISO)
094400001218     C*
094500001218     C* Definizioni chiavi
094600100219     C*
094700100219     C     KEYdly05_P    KLIST
094800100219     C                   KFLD                    dlyLNP
094900100219     C                   KFLD                    dlyTSP
095000100219     C                   KFLD                    dlyNAR
095100100219     C                   KFLD                    dlyCAP
095200120214     C*
095300120214     C     KEYtab_P      KLIST
095400120214     C                   KFLD                    tblKUT
095500120214     C                   KFLD                    tblCOD
095600001218     C*
095700100219     C     KEYtab_C      KLIST
095800100219     C                   KFLD                    tblKUT
095900100219     C                   KFLD                    tblCOD
096000100219     C                   KFLD                    tblKEY
096100110307     C*
096200110307     C     KEYcln_11C    KLIST
096300110307     C                   KFLD                    wclnCCM
096400110307     C                   KFLD                    wclnAAAAMM
096500110307     C                   KFLD                    wclnAAS
096600110307     C                   KFLD                    wclnLNP
096700110307     C                   KFLD                    wclnNRS
096800110307     C                   KFLD                    wclnNSP
096900030704     C*
097000110307     C     KEYcln_12C    KLIST
097100100219     C                   KFLD                    wclnCCM
097200100219     C                   KFLD                    wclnAAAAMM
097300110307     C                   KFLD                    wclnREG
097400100219     C                   KFLD                    wclnPRD
097500100219     C                   KFLD                    wclnCAD
097600100219     C                   KFLD                    wclnISO
097700100219     C                   KFLD                    wclnANOM
097800100219     C*
097900001218     C                   ENDSR

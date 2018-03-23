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
005400110304     D*--------------------
005500110304
005600010605     D*--------------------
005700010605     D* VARIABILI DI WRK
005800010605     D*--------------------
005900100219     D wANT            s              7  0 inz
006000100222     D wM12            s              7  0 inz
006100100219     D wOKS            s              7  0 inz
006200100219     D wRIT            s              7  0 inz
006300110304
006400110304
006500110304     D*------------------
006600110304     D* VARIABILI D WRK
006700110304     D*------------------
006800110304     D  wAAS           S                   like(tasAAS) inz
006900110304     D  wLNP           S                   like(tasLNP) inz
007000110304     D  wNRS           S                   like(tasNRS) inz
007100110304     D  wNSP           S                   like(tasNSP) inz
007200110304
007300110304
007400110304     D*------------------
007500110304     D* LINKING A DEFINIZIONI ESTERNE
007600110304     D*------------------
007700110304     D/COPY GAITRASRC/SRCPROTOPR,UBLBLSPE
007800110304     D/COPY GAITRASRC/SRCPROTOPI,UBLBLSPE
007900010605
008000010605
008100920812     C*---------------------------------------------------------------*
008200001218     C* MAIN
008300001218     C*---------------------------------------------------------------*
008400110304     C*
008500110304     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
008600110304     C
008700110304     C/EXEC SQL
008800110304     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
008900110304     C/END-EXEC
009000110304     C
009100110304     C*
009200120214     C                   exsr      cartbl
009300110304     C                   exsr      chkpar
009400110304     C*
009500120214     C**** Fisso KUNI(1)
009600120214     C***                   eval      KUNI(1) = CODCLI
009700110304     C*
009800110304     C* Ciclo sulla schiera dei clienti figli dell'unificante preso in input
009900110304     C                   z-add     1             I                 4 0
010000110304     C                   sorta     KUNI
010100110304     C                   dow       KUNI(I) > *zeros
010200001218     C                   exsr      procedi
010300110307     C                   add       1             I
010400110304     C                   enddo
010500110307     C*
010600110307     C* Inizializzo variabili d wrk
010700110307     C                   movel     'N'           wProcedi          1
010800110307     C*
010900110307     C* Apro il file d output
011000110307     C                   open      tivgd00f
011100110307     C*
011200110307     C* Come prima cosa avvio il blocco elaborazione TIVGD x tipo file corrente: 'VC'
011300110307     C                   clear                   trul47ds
011400110307     C                   eval      d47opz  = 'I'
011500120927     C                   eval      d47tip  = TIPFILE
011600110307     C                   eval      d47lck  = 'N'
011700110307     C                   eval      d47chkj = 'N'
011800110307     C                   eval      d47pgm  = 'TIS7FFR'
011900110307     C                   call      'TRUL47R'
012000110307     C                   parm                    trul47ds
012100110307     C*
012200110307     C* Se elaborazione consentita => proseguo
012300120928     C***                if        d47sts <> 'A'
012400110307     C                   movel     'S'           wProcedi
012500120928     C***                endif
012600110307     C*
012700110307     C* Popolo il file d output
012800110307     C                   if        wProcedi = 'S'
012900110307     C                   exsr      WRITIVGD
013000110307     C                   endif
013100110304     C*
013200110304     C* Chiudo il file di output
013300110304     C                   close     tivgd00f
013400110304     C*
013500110304     C* Infine elimino il blocco elaborazione TIVGD x tipo file corrente: 'VC'
013600110304     C                   clear                   trul47ds
013700110304     C                   eval      d47opz  = 'F'
013800120928     C                   eval      d47tip  = TIPFILE
013900110304     C                   call      'TRUL47R'
014000110304     C                   parm                    trul47ds
014100001218     C*
014200001218     C                   seton                                        LR
014300110304
014400110304
014500110304
014600110304     C*------------------------------------------------------------------------*
014700110304     C* CHKPAR - Routine di controllo parametri ricevuti in input
014800110304     C*------------------------------------------------------------------------*
014900110304     C     CHKPAR        BEGSR
015000110304     C*
015100110304     C* Verifico le date
015200110304     C                   if        DATADA > *zeros AND
015300110304     C                             DATAA  = *zeros
015400110304     C                   movel     *all'9'       DATAA
015500110304     C                   endif
015600110304     C                   if        DACMDA > *zeros AND
015700110304     C                             DACMA  = *zeros
015800110304     C                   movel     *all'9'       DACMA
015900110304     C                   endif
016000110307     C*
016100110307     C* Verifico il livello d dettaglio richiesto
016200110307     C                   setoff                                       2627
016300110307     C                   select
016400110307     C                   when      TIPDET = 'C'                                 * x CAP
016500110307     C                   seton                                        26
016600110307     C                   when      TIPDET = 'S'                                 * x SPED
016700110307     C                   seton                                        27
016800110307     C                   endsl
016900110304     C*
017000110304     C                   ENDSR
017100110304     C*------------------------------------------------------------------------*
017200110304
017300110304
017400110304
017500110304     C*------------------------------------------------------------------------*
017600110304     C* PROCEDI - Routine principale
017700110304     C*------------------------------------------------------------------------*
017800110304     C     PROCEDI       BEGSR
017900110304     C*
018000110304     C* Se richiesta elaborazione x codice cliente mittente...
018100110304     C                   select
018200110304     C                   when      TIPCLI = *blanks
018300110304     C                   exsr      EXECCM
018400110304     C*
018500110304     C* Se richiesta elaborazione x codice cliente fatturazione...
018600110304     C                   when      TIPCLI = 'K'
018700111130     C                   exsr      EXEKSC
018800110304     C*
018900110304     C* Se richiesta elaborazione x entrambi: codice cliente mittente + fatturazione...
019000110304     C                   when      TIPCLI = 'E'
019100110304     C                   exsr      EXECCM
019200111130     C                   exsr      EXEKSC
019300110304     C*
019400110304     C                   endsl
019500110304     C*
019600110304     C                   ENDSR
019700110304     C*------------------------------------------------------------------------*
019800110304
019900110304
020000110304
020100110304     C*------------------------------------------------------------------------*
020200110304     C* EXECCM   - Elaborazione x codice cliente mittente
020300110304     C*------------------------------------------------------------------------*
020400110304     C     EXECCM        BEGSR
020500110304     C*
020600110304     C                   movel     'C'           wChkCli           1
020700110304     C*
020800110304     C                   select
020900110304     C* ...se richiesta elaborazione x data spedizione
021000110304     C                   when      DATADA > *zeros
021100110304     C                   exsr      SQ_CCMDSP
021200110304     C* ...se richiesta elaborazione x data consegna merce
021300110304     C                   when      DACMDA > *zeros
021400110304     C                   exsr      SQ_CCMDCM
021500110304     C                   endsl
021600110304     C*
021700110304     C                   ENDSR
021800110304     C*------------------------------------------------------------------------*
021900111130
022000111130
022100111130
022200111130     C*------------------------------------------------------------------------*
022300111130     C* EXEKSC   - Elaborazione x codice cliente fatturazione
022400111130     C*------------------------------------------------------------------------*
022500111130     C     EXEKSC        BEGSR
022600111130     C*
022700111130     C                   movel     'K'           wChkCli           1
022800111130     C*
022900111130     C                   select
023000111130     C* ...se richiesta elaborazione x data spedizione
023100111130     C                   when      DATADA > *zeros
023200111130     C                   exsr      SQ_KSCDSP
023300111130     C* ...se richiesta elaborazione x data consegna merce
023400111130     C                   when      DACMDA > *zeros
023500111130     C                   exsr      SQ_KSCDCM
023600111130     C                   endsl
023700111130     C*
023800111130     C                   ENDSR
023900111130     C*------------------------------------------------------------------------*
024000110304
024100110304
024200110304
024300110304     C*------------------------------------------------------------------------*
024400110304     C* SQ_CCMDSP - Elaborazione x codice cliente mittente + data spedizione
024500110304     C*------------------------------------------------------------------------*
024600110304     C     SQ_CCMDSP     BEGSR
024700110304     C*
024800110304     C                   z-add     KUNI(I)       wParCCM           7 0
024900110304     C*
025000110304     C/EXEC SQL
025100110304     C+ declare C_CCMDSP cursor for
025200110304     C+ select * from titas00f
025300110304     C+ where tasccm = :wParCCM and
025400110304     C+ tasaas*10000+tasmgs between :DATADA and :DATAA
025500110304     C+ union
025600110304     C+ select * from titas10f
025700110304     C+ where tasccm = :wParCCM and
025800110304     C+ tasaas*10000+tasmgs between :DATADA and :DATAA
025900110304     C+ union
026000110304     C+ select * from titasP0f
026100110304     C+ where tasccm = :wParCCM and
026200110304     C+ tasaas*10000+tasmgs between :DATADA and :DATAA
026300110304     C+ for read only
026400110304     C/END-EXEC
026500110304     C
026600110304     C/EXEC SQL
026700110304     C+ open C_CCMDSP
026800110304     C/END-EXEC
026900110304     C
027000110304     C/EXEC SQL
027100110304     C+ Fetch C_CCMDSP into :TITAS00F
027200110304     C/END-EXEC
027300110304     C*
027400110304     C                   dow       sqlcod = *zeros
027500110304     C                   exsr      verifica
027600110304     C                   if        CHKREC = 'S'
027700110304     C                   exsr      scriviWF
027800110304     C                   endif
027900110304     C
028000110304     C/EXEC SQL
028100110304     C+ Fetch C_CCMDSP into :TITAS00F
028200110304     C/END-EXEC
028300110304     C*
028400110304     C                   enddo
028500110304     C*
028600110304     C/EXEC SQL
028700110304     C+ close C_CCMDSP
028800110304     C/END-EXEC
028900110304     C*
029000110304     C*
029100110304     C                   ENDSR
029200110304     C*------------------------------------------------------------------------*
029300111130
029400111130
029500111130
029600111130     C*------------------------------------------------------------------------*
029700111130     C* SQ_KSCDSP - Elaborazione x codice cliente fatturazione + data spedizione
029800111130     C*------------------------------------------------------------------------*
029900111130     C     SQ_KSCDSP     BEGSR
030000111130     C*
030100111130     C                   z-add     KUNI(I)       wParKSC           7 0
030200111130     C*
030300111130     C/EXEC SQL
030400111130     C+ declare C_KSCDSP cursor for
030500111130     C+ select * from titas00f
030600111130     C+ where tasksc = :wParKSC and
030700111130     C+ tasaas*10000+tasmgs between :DATADA and :DATAA
030800111130     C+ union
030900111130     C+ select * from titas10f
031000111130     C+ where tasksc = :wParKSC and
031100111130     C+ tasaas*10000+tasmgs between :DATADA and :DATAA
031200111130     C+ union
031300111130     C+ select * from titasP0f
031400111130     C+ where tasksc = :wParKSC and
031500111130     C+ tasaas*10000+tasmgs between :DATADA and :DATAA
031600111130     C+ for read only
031700111130     C/END-EXEC
031800111130     C
031900111130     C/EXEC SQL
032000111130     C+ open C_KSCDSP
032100111130     C/END-EXEC
032200111130     C
032300111130     C/EXEC SQL
032400111130     C+ Fetch C_KSCDSP into :TITAS00F
032500111130     C/END-EXEC
032600111130     C*
032700111130     C                   dow       sqlcod = *zeros
032800111130     C                   exsr      verifica
032900111130     C                   if        CHKREC = 'S'
033000111130     C                   exsr      scriviWF
033100111130     C                   endif
033200111130     C
033300111130     C/EXEC SQL
033400111130     C+ Fetch C_KSCDSP into :TITAS00F
033500111130     C/END-EXEC
033600111130     C*
033700111130     C                   enddo
033800111130     C*
033900111130     C/EXEC SQL
034000111130     C+ close C_KSCDSP
034100111130     C/END-EXEC
034200111130     C*
034300111130     C*
034400111130     C                   ENDSR
034500111130     C*------------------------------------------------------------------------*
034600110304
034700110304
034800110304
034900110304     C*------------------------------------------------------------------------*
035000110304     C* SQ_CCMDCM - Elaborazione x codice cliente mittente + data consegna merce
035100110304     C*------------------------------------------------------------------------*
035200110304     C     SQ_CCMDCM     BEGSR
035300110304     C*
035400110304     C                   z-add     KUNI(I)       wParCCM           7 0
035500110304     C*
035600110304     C/EXEC SQL
035700110304     C+ declare C_CCMDCM cursor for
035800110304     C+ select * from titas00f
035900110304     C+ where tasccm = :wParCCM and
036000110304     C+ tasdcm between :DACMDA and :DACMA
036100110304     C+ union
036200110304     C+ select * from titas10f
036300110304     C+ where tasccm = :wParCCM and
036400110304     C+ tasdcm between :DACMDA and :DACMA
036500110304     C+ union
036600110304     C+ select * from titasP0f
036700110304     C+ where tasccm = :wParCCM and
036800110304     C+ tasdcm between :DACMDA and :DACMA
036900110304     C+ for read only
037000110304     C/END-EXEC
037100110304     C
037200110304     C/EXEC SQL
037300110304     C+ open C_CCMDCM
037400110304     C/END-EXEC
037500110304     C
037600110304     C/EXEC SQL
037700110304     C+ Fetch C_CCMDCM into :TITAS00F
037800110304     C/END-EXEC
037900110304     C*
038000110304     C                   dow       sqlcod = *zeros
038100110304     C                   exsr      verifica
038200110304     C                   if        CHKREC = 'S'
038300110304     C                   exsr      scriviWF
038400110304     C                   endif
038500110304     C
038600110304     C/EXEC SQL
038700110304     C+ Fetch C_CCMDCM into :TITAS00F
038800110304     C/END-EXEC
038900110304     C*
039000110304     C                   enddo
039100110304     C*
039200110304     C/EXEC SQL
039300110304     C+ close C_CCMDCM
039400110304     C/END-EXEC
039500110304     C*
039600110304     C*
039700110304     C                   ENDSR
039800110304     C*------------------------------------------------------------------------*
039900111130
040000111130
040100111130
040200111130     C*------------------------------------------------------------------------*
040300111130     C* SQ_KSCDCM - Elaborazione x codice cliente fatturazione + data consegna merce
040400111130     C*------------------------------------------------------------------------*
040500111130     C     SQ_KSCDCM     BEGSR
040600111130     C*
040700111130     C                   z-add     KUNI(I)       wParKSC           7 0
040800111130     C*
040900111130     C/EXEC SQL
041000111130     C+ declare C_KSCDCM cursor for
041100111130     C+ select * from titas00f
041200111130     C+ where tasksc = :wParKSC and
041300111130     C+ tasdcm between :DACMDA and :DACMA
041400111130     C+ union
041500111130     C+ select * from titas10f
041600111130     C+ where tasksc = :wParKSC and
041700111130     C+ tasdcm between :DACMDA and :DACMA
041800111130     C+ union
041900111130     C+ select * from titasP0f
042000111130     C+ where tasksc = :wParKSC and
042100111130     C+ tasdcm between :DACMDA and :DACMA
042200111130     C+ for read only
042300111130     C/END-EXEC
042400111130     C
042500111130     C/EXEC SQL
042600111130     C+ open C_KSCDCM
042700111130     C/END-EXEC
042800111130     C
042900111130     C/EXEC SQL
043000111130     C+ Fetch C_KSCDCM into :TITAS00F
043100111130     C/END-EXEC
043200111130     C*
043300111130     C                   dow       sqlcod = *zeros
043400111130     C                   exsr      verifica
043500111130     C                   if        CHKREC = 'S'
043600111130     C                   exsr      scriviWF
043700111130     C                   endif
043800111130     C
043900111130     C/EXEC SQL
044000111130     C+ Fetch C_KSCDCM into :TITAS00F
044100111130     C/END-EXEC
044200111130     C*
044300111130     C                   enddo
044400111130     C*
044500111130     C/EXEC SQL
044600111130     C+ close C_KSCDCM
044700111130     C/END-EXEC
044800111130     C*
044900111130     C*
045000111130     C                   ENDSR
045100111130     C*------------------------------------------------------------------------*
045200001218
045300110304
045400110304
045500110304     C*------------------------------------------------------------------------*
045600110304     C* CHKCONS  - Verifica se bolla effettivamente consegnata
045700110304     C*------------------------------------------------------------------------*
045800110304     C     CHKCONS       BEGSR
045900110304     C*
046000110304     C                   setoff                                       4041
046100110304     C                   movel     *blanks       wEsito1           1
046200110304     C                   movel     *blanks       wEsito2           1
046300110304     C*
046400110304     C                   eval      wAAS = tasAAS
046500110304     C                   eval      wLNP = tasLNP
046600110304     C                   eval      wNRS = tasNRS
046700110304     C                   eval      wNSP = tasNSP
046800110304     C*
046900110304     C* Chiamata metodo GetLblTyp
047000110304     C                   eval      wEsito1 = %char(UBLBLSPE_GetLblTyp(
047100110304     C                                                wAAS
047200110304     C                                               :wLNP
047300110304     C                                               :wNRS
047400110304     C                                               :wNSP
047500110304     C                                               :pOutLblTyp
047600110304     C                                               :pOutAnnoBO
047700110304     C                                               :pOutLineaParBO
047800110304     C                                               :pOutSerieBO
047900110304     C                                               :pOutNumSpedBO
048000110304     C                                               :pOutDcmBO
048100110304     C                                               :pOutCcaBO
048200110304     C                                               :pOutRblBO))
048300110304     C* Se trattasi d bolla nn originale reperisco la relativa bolla originale
048400110304     C                   if        pOutLblTyp <> 'O'
048500110304     C                   eval      wAAS = pOutAnnoBO
048600110304     C                   eval      wLNP = pOutLineaParBO
048700110304     C                   eval      wNRS = pOutSerieBO
048800110304     C                   eval      wNSP = pOutNumSpedBO
048900110304     C                   endif
049000110304     C*
049100110304     C* Chiamata metodo GetLastChild
049200110304     C                   eval      wEsito2 = %char(UBLBLSPE_GetLastChild(
049300110304     C                                                wAAS
049400110304     C                                               :wLNP
049500110304     C                                               :wNRS
049600110304     C                                               :wNSP
049700110304     C                                               :pOutAnnoFI
049800110304     C                                               :pOutLineaParFI
049900110304     C                                               :pOutSerieFI
050000110304     C                                               :pOutNumSpedFI
050100110304     C                                               :pOutDcmFI
050200110304     C                                               :pOutCcaFI))
050300110304     C*
050400110304     C* Considerazioni sullo stato corrente della bolla
050500110304     C                   if        (wEsito2 = '9' and pOutDcmBO > *zeros)  or
050600110304     C                             (wEsito2 = '0' and pOutDcmFI > *zeros)
050700110304     C                   seton                                        40        * bolla cons.
050800110304     C                   endif
050900110304     C*
051000110304     C                   if        (wEsito2 = '9' and pOutDcmBO > *zeros
051100110304     C                                            and pOutCcaBO = *blanks) or
051200110304     C                             (wEsito2 = '0' and pOutDcmFI > *zeros
051300110304     C                                            and pOutCcaFI = *blanks)
051400110304     C                   seton                                        41        * bolla cons. OK
051500110304     C                   endif
051600110304     C*
051700110304     C                   ENDSR
051800110304     C*------------------------------------------------------------------------*
051900110304
052000110304
052100110304
052200110304     C*------------------------------------------------------------------------*
052300110304     C* VERIFICA - Routine di verifica validità record corrente
052400110304     C*------------------------------------------------------------------------*
052500110304     C     VERIFICA      BEGSR
052600110304     C*
052700110304     C                   movel     'S'           CHKREC            1
052800110307     C*
052900110307     C                   movel     tasaas        wrkdata8          8 0
053000110307     C                   move      tasmgs        wrkdata8
053100110307     C                   movel     wrkdata8      wrkAAAAMM         6 0
053200110304     C*
053300110304     C* Verifico le date spedizione
053400110304     C                   if        CHKREC = 'S'
053500110304     C                   if        DATADA > *zeros
053600110304     C                   movel     tasaas        wrkdata           8 0
053700110304     C                   move      tasmgs        wrkdata
053800110304     C                   if        wrkdata < DATADA or
053900110304     C                             wrkdata > DATAA
054000110304     C                   movel     'N'           CHKREC
054100110304     C                   endif
054200110304     C                   endif
054300110304     C                   endif
054400110304     C*
054500110304     C* Verifico le date spedizione
054600110304     C                   if        CHKREC = 'S'
054700110304     C                   if        DACMDA > *zeros
054800110304     C                   if        tasdcm  < DACMDA or
054900110304     C                             tasdcm  > DACMA
055000110304     C                   movel     'N'           CHKREC
055100110304     C                   endif
055200110304     C                   endif
055300110304     C                   endif
055400110304     C*
055500110304     C* Verifico il codice cliente mittente
055600110304     C                   if        CHKREC = 'S'
055700110304     C                   if        wChkCli  = 'C'      AND
055800110304     C                             tasccm  <> KUNI(I)
055900110304     C                   movel     'N'           CHKREC
056000110304     C                   endif
056100110304     C                   endif
056200110304     C*
056300110304     C* Verifico il codice cliente fatturazione (in caso d 'K')
056400110304     C                   if        CHKREC = 'S'
056500110304     C                   if        wChkCli  = 'K'      AND
056600110304     C                             tasksc  <> KUNI(I)
056700110304     C                   movel     'N'           CHKREC
056800110304     C                   endif
056900110304     C                   endif
057000110304     C*
057100110304     C* Verifico il codice cliente fatturazione (in caso d 'E')
057200110304     C                   if        CHKREC = 'S'
057300110304     C                   if        TIPCLI  = 'E'       AND
057400110304     C                             wChkCli = 'K'       AND
057500110304     C                             tasksc  = tasccm
057600110304     C                   movel     'N'           CHKREC
057700110304     C                   endif
057800110304     C                   endif
057900110304     C*
058000110304     C* Verifico la serie
058100110304     C                   if        CHKREC = 'S'
058200110307     C                   if        NUMSER <> *blanks
058300110307     C                   if        tasnrs <> %dec(NUMSER : 2 : 0)
058400110304     C                   movel     'N'           CHKREC
058500110304     C                   endif
058600110304     C                   endif
058700110304     C                   endif
058800110304     C*
058900110304     C* Verifico il tipo bolla
059000110304     C                   if        CHKREC = 'S'
059100110304     C                   if        TIPBOL <> *blanks
059200110307     C                   if        tastbl <> TIPBOL
059300110304     C                   movel     'N'           CHKREC
059400110304     C                   endif
059500110304     C                   endif
059600110304     C                   endif
059700110304     C*
059800110304     C* Verifico se richiesto nel lancio che la bolla sia consegnata
059900110304     C                   if        CHKREC = 'S'
060000110304     C                   if        FLGCONS = 'S'
060100110304     C                   exsr      CHKCONS
060200110304     C                   if        *in40 = *on
060300110304     C                   else
060400110304     C                   movel     'N'           CHKREC
060500110304     C                   endif
060600110304     C                   endif
060700110304     C                   endif
060800110304     C*
060900110304     C* Verifico se richiesto nel lancio che la bolla sia consegnata OK
061000110304     C                   if        CHKREC = 'S'
061100110304     C                   if        FLGCONS = 'O'
061200110304     C                   exsr      CHKCONS
061300110304     C                   if        *in41 = *on
061400110304     C                   else
061500110304     C                   movel     'N'           CHKREC
061600110304     C                   endif
061700110304     C                   endif
061800110304     C                   endif
061900110304     C*
062000110304     C* Verifico se richiesto nel lancio che la bolla NON sia consegnata OK
062100110304     C                   if        CHKREC = 'S'
062200110304     C                   if        FLGCONS = 'K'
062300110304     C                   exsr      CHKCONS
062400110304     C                   if        *in41 = *on
062500110304     C                   movel     'N'           CHKREC
062600110304     C                   endif
062700110304     C                   endif
062800110304     C                   endif
062900110304     C*
063000110304     C* Verifico se richiesto nel lancio che la bolla NON sia consegnata
063100110304     C                   if        CHKREC = 'S'
063200110304     C                   if        FLGCONS = 'P'
063300110304     C                   exsr      CHKCONS
063400110304     C                   if        *in40 = *on
063500110304     C                   movel     'N'           CHKREC
063600110304     C                   endif
063700110304     C                   endif
063800110304     C                   endif
063900110304     C*
064000110304     C                   ENDSR
064100110304     C*------------------------------------------------------------------------*
064200001218
064300001218
064400001218
064500001218     C*------------------------------------------------------------------------*
064600110307     C* SCRIVIWF - Routine di scrittura WFCLN10F
064700001218     C*------------------------------------------------------------------------*
064800100219     C     SCRIVIWF      BEGSR
064900100219     C*
065000110307     C                   clear                   WFCLN110
065100110307     C                   clear                   WFCLN120
065200110307     C                   clear                   WFCLN_10
065300110307     C                   clear                   WFCLN_11
065400110307     C                   clear                   WFCLN_12
065500100219     C*
065600100219     C                   eval      dlyLNP = tasLNP
065700100219     C                   eval      dlyTSP = tasTSP
065800120309     C*
065900120309     C                   if        dlyTSP = 'D'
066000120309     C                   eval      dlyTSP = 'C'
066100120309     C                   endif
066200120309     C*
066300100219     C                   eval      dlyNAR = tasNZD
066400100219     C                   eval      dlyCAP = tasCAD
066500100219     C     KEYdly05_P    chain     wadly05l
066600100219     C                   if        %found(wadly05l)
066700100219     C                   eval      WCLNDLYTRC = DLYTRC
066800100219     C                   endif
066900100219     C*
067000100219     C                   eval      tblcod = 'PR'
067100100219     C                   eval      tblkey = tasprd
067200100219     C     KEYtab_C      chain     tabel00f
067300100219     C                   if        %found(tabel00f)
067400100219     C                   eval      DSPR = tbluni
067500100219     C                   eval      tblcod = '14'
067600100219     C                   eval      tblkey = §PRCRE
067700100219     C     KEYtab_C      chain     tabel00f
067800100219     C                   if        %found(tabel00f)
067900100219     C                   eval      DS14 = tbluni
068000110307     C                   eval      wclnREG = §14DES
068100100219     C                   endif
068200100219     C                   endif
068300100219     C*
068400100219     C                   z-add     *zeros        wANT
068500100222     C                   z-add     *zeros        wM12
068600100219     C                   z-add     *zeros        wOKS
068700100219     C                   z-add     *zeros        wRIT
068800001218     C*
068900100219     C* ...in anticipo
069000100219     C                   select
069100100219     C                   when      tasnrc >= -999 AND
069200100219     C                             tasnrc <= -24
069300100219     C                   add       1             wANT
069400100222     C*
069500100222     C* ...border-line
069600100222     C                   when      tasnrc >  -24  AND
069700100222     C                             tasnrc <= -12
069800100222     C                   add       1             wM12
069900100219     C*
070000100219     C* ...in orario
070100100222     C                   when      tasnrc >  -12  AND
070200100219     C                             tasnrc <=   0
070300100219     C                   add       1             wOKS
070400100219     C*
070500100219     C* ...in ritardo
070600100219     C                   when      tasnrc >    0
070700100219     C                   add       1             wRIT
070800100219     C                   endsl
070900100219     C*
071000100219     C* Conteggio spedizioni DIR/GIAC/CCA
071100100219     C                   eval      dtasflo = tasflo
071200100219     C                   if        tasCCA   <> *blanks OR
071300100219     C                             tasFGC   <> *blanks OR
071400100219     C                             §FLONAV  <> *blanks
071500100219     C                   eval      wclnANOM = 'S'
071600100219     C                   endif
071700100219     C*
071800111130     C                   eval      wclnCCM    = tasccm
071900111130     C                   if        wChkCli = 'K'
072000111130     C                   eval      wclnCCM    = tasksc
072100111130     C                   endif
072200130403     C*
072300130403     C* Se richiesta estrazione per "unificante" (di qualunque tipo come CCM considero sempre
072400130403     C* l'unificante)
072500130403     C
072600130403     C                   if        FLGUNI <> *blanks
072700130403     C                   eval      wclnCCM    = CODCLI
072800130403     C                   endif
072900111130     C*
073000100219     C                   eval      wclnAAAAMM = wrkAAAAMM
073100100219     C                   eval      wclnPRD    = tasprd
073200100219     C                   eval      wclnCAD    = tascad
073300100219     C                   eval      wclnISO    = tasfin
073400110307     C                   eval      wclnAAS    = tasaas
073500110307     C                   eval      wclnLNP    = taslnp
073600110307     C                   eval      wclnNRS    = tasnrs
073700110307     C                   eval      wclnNSP    = tasnsp
073800110307     C*
073900110307     C                   select
074000110307     C                   when      *in26
074100110307     C     KEYcln_12C    chain     wfcln12l
074200110307     C                   if        %found(wfcln12l)
074300110307     C                   eval      WFCLN_10 = WFCLN_12
074400100219     C                   add       wANT          wclnANT
074500100222     C                   add       wM12          wclnM12
074600100219     C                   add       wOKS          wclnOKS
074700100219     C                   add       wRIT          wclnRIT
074800100219     C                   add       1             wclnTOT
074900110307     C                   eval      WFCLN_12 = WFCLN_10
075000110307     C                   update    WFCLN120
075100100219     C                   else
075200100219     C                   add       wANT          wclnANT
075300100222     C                   add       wM12          wclnM12
075400100219     C                   add       wOKS          wclnOKS
075500100219     C                   add       wRIT          wclnRIT
075600100219     C                   add       1             wclnTOT
075700110307     C                   eval      WFCLN_12 = WFCLN_10
075800110307     C                   write     WFCLN120
075900100219     C                   endif
076000110307     C*
076100110307     C                   when      *in27
076200110307     C                   add       wANT          wclnANT
076300110307     C                   add       wM12          wclnM12
076400110307     C                   add       wOKS          wclnOKS
076500110307     C                   add       wRIT          wclnRIT
076600110307     C                   add       1             wclnTOT
076700110307     C                   eval      WFCLN_11 = WFCLN_10
076800110307     C                   write     WFCLN110
076900110307     C*
077000110307     C                   endsl
077100001218     C*
077200001218     C                   ENDSR
077300001221     C*------------------------------------------------------------------------*
077400110304
077500110304
077600110304
077700110304     C*------------------------------------------------------------------------*
077800110304     C* WRITIVGD - Routine di scrittura file TIVGD (file VAS generico download)
077900110304     C*------------------------------------------------------------------------*
078000110304     C     WRITIVGD      BEGSR
078100110304     C*
078200110307     C* Leggo tutto il file WFCLN11L in QTEMP del job corrente
078300110307     C     *loval        setll     WFCLN11L
078400110307     C                   read      WFCLN11L
078500110307     C                   dow       not %eof(WFCLN11L)
078600110307     C*
078700110304     C                   clear                   tivgd000
078800110307     C                   eval      vgdDTA = %subst(WFCLN_11:1:%size(WFCLN_11))
078900110304     C                   eval      vgdTIP = TIPFILE
079000110304     C                   movel     *all'0'       vgdKSU
079100110304     C                   move      CODCLIVAS     vgdKSU
079200110304     C                   eval      vgdTSC = 'WW'
079300110304     C                   eval      vgdDAT = datcor
079400110304     C                   eval      vgdPGM = 'TIS7FFR'
079500110304     C*
079600110307     C***                clear                   DVGDFLO
079700110307     C***                movel     'P'           §VGDFELA
079800110307     C***                movel     DVGDFLO       vgdflo
079900110304     C*
080000110304     C                   write     tivgd000
080100110304     C*
080200110307     C                   read      WFCLN11L
080300110307     C                   enddo
080400110307     C*
080500110307     C* Infine forzo l'elaborazione immediata del file d output
080600110307     C***                exsr      forzadwn
080700110307     C*
080800110304     C                   ENDSR
080900110304     C*------------------------------------------------------------------------*
081000110304
081100110304
081200110304
081300110304     C*------------------------------------------------------------------------*
081400110304     C* FORZADWN - Routine di forzatura elaborazione immediata download
081500110304     C*------------------------------------------------------------------------*
081600110304     C     FORZADWN      BEGSR
081700110304     C*
081800110304     C                   clear                   tis781ds
081900110304     C                   eval      §781tip = vgdTIP
082000110304     C                   eval      §781ksu = vgdKSU
082100110304     C                   eval      §781tsc = vgdTSC
082200110304     C                   eval      §781dat = vgdDAT
082300110304     C                   eval      §781pgm = vgdPGM
082400110304     C*
082500110304     C                   clear                   TIS781DFLO
082600110304     C                   movel     'P'           §781floela
082700110304     C                   movel     TIS781DFLO    §781flo
082800110304     C*
082900110304     C* Finita l'elaborazione chiamo pgm
083000110304     C                   CALL      'TIS781C1'
083100110304     C                   parm      *blanks       esito             1
083200110304     C                   parm                    tis781ds
083300110304     C                   parm      *blanks       vgdPRG           10
083400110304     C*
083500110304     C                   ENDSR
083600110304     C*------------------------------------------------------------------------*
083700120214
083800120214
083900120214
084000120214     C*------------------------------------------------------------------------*
084100120214     C* CARTBL - Routine di caricamento dati tabellati
084200120214     C*------------------------------------------------------------------------*
084300120214     C     CARTBL        BEGSR
084400120214     C*
084500120214     C                   Z-ADD     1             I                 4 0
084600120214     C                   MOVEL     CODCLI        KUNI(I)
084700130403     C*
084800130403     C                   SELECT
084900130403     C                   WHEN      FLGUNI = 'U'
085000120214     C                   MOVEL     '3K'          tblCOD
085100120214     C     KEYtab_P      CHAIN     TABEL                              31
085200120214     C     *IN31         DOWEQ     '0'
085300120214     C     TBLFLG        IFEQ      ' '
085400120214     C                   MOVEL     TBLUNI        DS3K
085500120214     C     §3KCKS        IFEQ      CODCLI
085600120214     C                   ADD       1             I
085700120214     C                   MOVEL     TBLKEY        wKUNI             7 0
085800120214     C     wKUNI         LOOKUP    KUNI                                   55
085900120214     C                   IF        %equal
086000120214     C                   ELSE
086100120214     C                   Z-ADD     wKUNI         KUNI(I)
086200120214     C                   ENDIF
086300120214     C                   ENDIF
086400120214     C                   ENDIF
086500120214     C     KEYtab_P      READE     TABEL                                  31
086600120214     C                   ENDDO
086700130403     C*
086800130403     C                   WHEN      FLGUNI = 'T'
086900130403     C                   CLEAR                   TIBS10DS
087000130403     C                   EVAL      D10DRF = datcor                              *data riferimento
087100130403     C                   EVAL      D10TLE = 'ST'                                *tipo legame
087200130403     C                   EVAL      D10PAF = 'F'                                 *cerca se padre
087300130403     C                   EVAL      D10COD = CODCLI
087400130403     C                   CALL      'TIBS10R'
087500130403     C                   PARM                    TIBS10DS
087600130403     C                   IF        D10ERR = *BLANKS                             *Padre
087700130403     C                   Z-ADD     1             I
087800130403     C                   DOW       skc(I) > *zeros
087900130403     C                   Z-ADD     skc(I)        KUNI(I)
088000130403     C                   ADD       1             I
088100130403     C                   ENDDO
088200130403     C                   ENDIF
088300130403     C*
088400130403     C                   ENDSL
088500120214     C*
088600120214     C                   ENDSR
088700120214     C*---------------------------------------------------------------*
088800120214
088900001218
089000001218
089100001218     C*------------------------------------------------------------------------*
089200001218     C* *INZSR - ROUTINE INIZIALE
089300001218     C*------------------------------------------------------------------------*
089400001218     C     *INZSR        BEGSR
089500001218     C*
089600001218     C     *ENTRY        PLIST
089700100219     C                   PARM                    KPJBA
089800001218     C*
089900100219     C                   MOVEL     KPJBU         DSINPUT
090000100219     C                   EVAL      tblKUT = 1
090100110307     C*
090200110307     C* Determino la data corrente
090300110307     C                   Z-ADD     *zeros        datcor            8 0
090400110307     C                   EVAL      datcor = %dec(%date() : *ISO)
090500001218     C*
090600001218     C* Definizioni chiavi
090700100219     C*
090800100219     C     KEYdly05_P    KLIST
090900100219     C                   KFLD                    dlyLNP
091000100219     C                   KFLD                    dlyTSP
091100100219     C                   KFLD                    dlyNAR
091200100219     C                   KFLD                    dlyCAP
091300120214     C*
091400120214     C     KEYtab_P      KLIST
091500120214     C                   KFLD                    tblKUT
091600120214     C                   KFLD                    tblCOD
091700001218     C*
091800100219     C     KEYtab_C      KLIST
091900100219     C                   KFLD                    tblKUT
092000100219     C                   KFLD                    tblCOD
092100100219     C                   KFLD                    tblKEY
092200110307     C*
092300110307     C     KEYcln_11C    KLIST
092400110307     C                   KFLD                    wclnCCM
092500110307     C                   KFLD                    wclnAAAAMM
092600110307     C                   KFLD                    wclnAAS
092700110307     C                   KFLD                    wclnLNP
092800110307     C                   KFLD                    wclnNRS
092900110307     C                   KFLD                    wclnNSP
093000030704     C*
093100110307     C     KEYcln_12C    KLIST
093200100219     C                   KFLD                    wclnCCM
093300100219     C                   KFLD                    wclnAAAAMM
093400110307     C                   KFLD                    wclnREG
093500100219     C                   KFLD                    wclnPRD
093600100219     C                   KFLD                    wclnCAD
093700100219     C                   KFLD                    wclnISO
093800100219     C                   KFLD                    wclnANOM
093900100219     C*
094000001218     C                   ENDSR

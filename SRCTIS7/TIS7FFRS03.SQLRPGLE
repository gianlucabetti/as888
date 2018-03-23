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
003100110304     D*--------------------
003200110304     D* DS DI RIDEFINIZIOINE PARAMETRI IN INPUT
003300110304     D*--------------------
003400110304     D DSINPUT         DS
003500110304     D  DATADA                        8  0
003600110304     D  DATAA                         8  0
003700110304     D  CODCLI                        7  0
003800110304     D  FLGUNI                        1
003900110304     D  FLGCONS                       1
004000110304     D  CODCLIVAS                     7  0
004100110304     D  TIPFILE                       2
004200110304     D  FLGEXE                        1
004300110304     D  TIPCLI                        1
004400110304     D  DACMDA                        8  0
004500110304     D  DACMA                         8  0
004600110304     D  TIPBOL                        2
004700110307     D  NUMSER                        2
004800110304     D  TIPDET                        1
004900110304     D*--------------------
005000110304
005100010605     D*--------------------
005200010605     D* VARIABILI DI WRK
005300010605     D*--------------------
005400100219     D wANT            s              7  0 inz
005500100222     D wM12            s              7  0 inz
005600100219     D wOKS            s              7  0 inz
005700100219     D wRIT            s              7  0 inz
005800110304
005900110304
006000110304     D*------------------
006100110304     D* VARIABILI D WRK
006200110304     D*------------------
006300110304     D  wAAS           S                   like(tasAAS) inz
006400110304     D  wLNP           S                   like(tasLNP) inz
006500110304     D  wNRS           S                   like(tasNRS) inz
006600110304     D  wNSP           S                   like(tasNSP) inz
006700110304
006800110304
006900110304     D*------------------
007000110304     D* LINKING A DEFINIZIONI ESTERNE
007100110304     D*------------------
007200110304     D/COPY GAITRASRC/SRCPROTOPR,UBLBLSPE
007300110304     D/COPY GAITRASRC/SRCPROTOPI,UBLBLSPE
007400010605
007500010605
007600920812     C*---------------------------------------------------------------*
007700001218     C* MAIN
007800001218     C*---------------------------------------------------------------*
007900110304     C*
008000110304     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
008100110304     C
008200110304     C/EXEC SQL
008300110304     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
008400110304     C/END-EXEC
008500110304     C
008600110304     C*
008700120214     C                   exsr      cartbl
008800110304     C                   exsr      chkpar
008900110304     C*
009000120214     C**** Fisso KUNI(1)
009100120214     C***                   eval      KUNI(1) = CODCLI
009200110304     C*
009300110304     C* Ciclo sulla schiera dei clienti figli dell'unificante preso in input
009400110304     C                   z-add     1             I                 4 0
009500110304     C                   sorta     KUNI
009600110304     C                   dow       KUNI(I) > *zeros
009700001218     C                   exsr      procedi
009800110307     C                   add       1             I
009900110304     C                   enddo
010000110307     C*
010100110307     C* Inizializzo variabili d wrk
010200110307     C                   movel     'N'           wProcedi          1
010300110307     C*
010400110307     C* Apro il file d output
010500110307     C                   open      tivgd00f
010600110307     C*
010700110307     C* Come prima cosa avvio il blocco elaborazione TIVGD x tipo file corrente: 'VC'
010800110307     C                   clear                   trul47ds
010900110307     C                   eval      d47opz  = 'I'
011000120927     C                   eval      d47tip  = TIPFILE
011100110307     C                   eval      d47lck  = 'N'
011200110307     C                   eval      d47chkj = 'N'
011300110307     C                   eval      d47pgm  = 'TIS7FFR'
011400110307     C                   call      'TRUL47R'
011500110307     C                   parm                    trul47ds
011600110307     C*
011700110307     C* Se elaborazione consentita => proseguo
011800120928     C***                if        d47sts <> 'A'
011900110307     C                   movel     'S'           wProcedi
012000120928     C***                endif
012100110307     C*
012200110307     C* Popolo il file d output
012300110307     C                   if        wProcedi = 'S'
012400110307     C                   exsr      WRITIVGD
012500110307     C                   endif
012600110304     C*
012700110304     C* Chiudo il file di output
012800110304     C                   close     tivgd00f
012900110304     C*
013000110304     C* Infine elimino il blocco elaborazione TIVGD x tipo file corrente: 'VC'
013100110304     C                   clear                   trul47ds
013200110304     C                   eval      d47opz  = 'F'
013300120928     C                   eval      d47tip  = TIPFILE
013400110304     C                   call      'TRUL47R'
013500110304     C                   parm                    trul47ds
013600001218     C*
013700001218     C                   seton                                        LR
013800110304
013900110304
014000110304
014100110304     C*------------------------------------------------------------------------*
014200110304     C* CHKPAR - Routine di controllo parametri ricevuti in input
014300110304     C*------------------------------------------------------------------------*
014400110304     C     CHKPAR        BEGSR
014500110304     C*
014600110304     C* Verifico le date
014700110304     C                   if        DATADA > *zeros AND
014800110304     C                             DATAA  = *zeros
014900110304     C                   movel     *all'9'       DATAA
015000110304     C                   endif
015100110304     C                   if        DACMDA > *zeros AND
015200110304     C                             DACMA  = *zeros
015300110304     C                   movel     *all'9'       DACMA
015400110304     C                   endif
015500110307     C*
015600110307     C* Verifico il livello d dettaglio richiesto
015700110307     C                   setoff                                       2627
015800110307     C                   select
015900110307     C                   when      TIPDET = 'C'                                 * x CAP
016000110307     C                   seton                                        26
016100110307     C                   when      TIPDET = 'S'                                 * x SPED
016200110307     C                   seton                                        27
016300110307     C                   endsl
016400110304     C*
016500110304     C                   ENDSR
016600110304     C*------------------------------------------------------------------------*
016700110304
016800110304
016900110304
017000110304     C*------------------------------------------------------------------------*
017100110304     C* PROCEDI - Routine principale
017200110304     C*------------------------------------------------------------------------*
017300110304     C     PROCEDI       BEGSR
017400110304     C*
017500110304     C* Se richiesta elaborazione x codice cliente mittente...
017600110304     C                   select
017700110304     C                   when      TIPCLI = *blanks
017800110304     C                   exsr      EXECCM
017900110304     C*
018000110304     C* Se richiesta elaborazione x codice cliente fatturazione...
018100110304     C                   when      TIPCLI = 'K'
018200111130     C                   exsr      EXEKSC
018300110304     C*
018400110304     C* Se richiesta elaborazione x entrambi: codice cliente mittente + fatturazione...
018500110304     C                   when      TIPCLI = 'E'
018600110304     C                   exsr      EXECCM
018700111130     C                   exsr      EXEKSC
018800110304     C*
018900110304     C                   endsl
019000110304     C*
019100110304     C                   ENDSR
019200110304     C*------------------------------------------------------------------------*
019300110304
019400110304
019500110304
019600110304     C*------------------------------------------------------------------------*
019700110304     C* EXECCM   - Elaborazione x codice cliente mittente
019800110304     C*------------------------------------------------------------------------*
019900110304     C     EXECCM        BEGSR
020000110304     C*
020100110304     C                   movel     'C'           wChkCli           1
020200110304     C*
020300110304     C                   select
020400110304     C* ...se richiesta elaborazione x data spedizione
020500110304     C                   when      DATADA > *zeros
020600110304     C                   exsr      SQ_CCMDSP
020700110304     C* ...se richiesta elaborazione x data consegna merce
020800110304     C                   when      DACMDA > *zeros
020900110304     C                   exsr      SQ_CCMDCM
021000110304     C                   endsl
021100110304     C*
021200110304     C                   ENDSR
021300110304     C*------------------------------------------------------------------------*
021400111130
021500111130
021600111130
021700111130     C*------------------------------------------------------------------------*
021800111130     C* EXEKSC   - Elaborazione x codice cliente fatturazione
021900111130     C*------------------------------------------------------------------------*
022000111130     C     EXEKSC        BEGSR
022100111130     C*
022200111130     C                   movel     'K'           wChkCli           1
022300111130     C*
022400111130     C                   select
022500111130     C* ...se richiesta elaborazione x data spedizione
022600111130     C                   when      DATADA > *zeros
022700111130     C                   exsr      SQ_KSCDSP
022800111130     C* ...se richiesta elaborazione x data consegna merce
022900111130     C                   when      DACMDA > *zeros
023000111130     C                   exsr      SQ_KSCDCM
023100111130     C                   endsl
023200111130     C*
023300111130     C                   ENDSR
023400111130     C*------------------------------------------------------------------------*
023500110304
023600110304
023700110304
023800110304     C*------------------------------------------------------------------------*
023900110304     C* SQ_CCMDSP - Elaborazione x codice cliente mittente + data spedizione
024000110304     C*------------------------------------------------------------------------*
024100110304     C     SQ_CCMDSP     BEGSR
024200110304     C*
024300110304     C                   z-add     KUNI(I)       wParCCM           7 0
024400110304     C*
024500110304     C/EXEC SQL
024600110304     C+ declare C_CCMDSP cursor for
024700110304     C+ select * from titas00f
024800110304     C+ where tasccm = :wParCCM and
024900110304     C+ tasaas*10000+tasmgs between :DATADA and :DATAA
025000110304     C+ union
025100110304     C+ select * from titas10f
025200110304     C+ where tasccm = :wParCCM and
025300110304     C+ tasaas*10000+tasmgs between :DATADA and :DATAA
025400110304     C+ union
025500110304     C+ select * from titasP0f
025600110304     C+ where tasccm = :wParCCM and
025700110304     C+ tasaas*10000+tasmgs between :DATADA and :DATAA
025800110304     C+ for read only
025900110304     C/END-EXEC
026000110304     C
026100110304     C/EXEC SQL
026200110304     C+ open C_CCMDSP
026300110304     C/END-EXEC
026400110304     C
026500110304     C/EXEC SQL
026600110304     C+ Fetch C_CCMDSP into :TITAS00F
026700110304     C/END-EXEC
026800110304     C*
026900110304     C                   dow       sqlcod = *zeros
027000110304     C                   exsr      verifica
027100110304     C                   if        CHKREC = 'S'
027200110304     C                   exsr      scriviWF
027300110304     C                   endif
027400110304     C
027500110304     C/EXEC SQL
027600110304     C+ Fetch C_CCMDSP into :TITAS00F
027700110304     C/END-EXEC
027800110304     C*
027900110304     C                   enddo
028000110304     C*
028100110304     C/EXEC SQL
028200110304     C+ close C_CCMDSP
028300110304     C/END-EXEC
028400110304     C*
028500110304     C*
028600110304     C                   ENDSR
028700110304     C*------------------------------------------------------------------------*
028800111130
028900111130
029000111130
029100111130     C*------------------------------------------------------------------------*
029200111130     C* SQ_KSCDSP - Elaborazione x codice cliente fatturazione + data spedizione
029300111130     C*------------------------------------------------------------------------*
029400111130     C     SQ_KSCDSP     BEGSR
029500111130     C*
029600111130     C                   z-add     KUNI(I)       wParKSC           7 0
029700111130     C*
029800111130     C/EXEC SQL
029900111130     C+ declare C_KSCDSP cursor for
030000111130     C+ select * from titas00f
030100111130     C+ where tasksc = :wParKSC and
030200111130     C+ tasaas*10000+tasmgs between :DATADA and :DATAA
030300111130     C+ union
030400111130     C+ select * from titas10f
030500111130     C+ where tasksc = :wParKSC and
030600111130     C+ tasaas*10000+tasmgs between :DATADA and :DATAA
030700111130     C+ union
030800111130     C+ select * from titasP0f
030900111130     C+ where tasksc = :wParKSC and
031000111130     C+ tasaas*10000+tasmgs between :DATADA and :DATAA
031100111130     C+ for read only
031200111130     C/END-EXEC
031300111130     C
031400111130     C/EXEC SQL
031500111130     C+ open C_KSCDSP
031600111130     C/END-EXEC
031700111130     C
031800111130     C/EXEC SQL
031900111130     C+ Fetch C_KSCDSP into :TITAS00F
032000111130     C/END-EXEC
032100111130     C*
032200111130     C                   dow       sqlcod = *zeros
032300111130     C                   exsr      verifica
032400111130     C                   if        CHKREC = 'S'
032500111130     C                   exsr      scriviWF
032600111130     C                   endif
032700111130     C
032800111130     C/EXEC SQL
032900111130     C+ Fetch C_KSCDSP into :TITAS00F
033000111130     C/END-EXEC
033100111130     C*
033200111130     C                   enddo
033300111130     C*
033400111130     C/EXEC SQL
033500111130     C+ close C_KSCDSP
033600111130     C/END-EXEC
033700111130     C*
033800111130     C*
033900111130     C                   ENDSR
034000111130     C*------------------------------------------------------------------------*
034100110304
034200110304
034300110304
034400110304     C*------------------------------------------------------------------------*
034500110304     C* SQ_CCMDCM - Elaborazione x codice cliente mittente + data consegna merce
034600110304     C*------------------------------------------------------------------------*
034700110304     C     SQ_CCMDCM     BEGSR
034800110304     C*
034900110304     C                   z-add     KUNI(I)       wParCCM           7 0
035000110304     C*
035100110304     C/EXEC SQL
035200110304     C+ declare C_CCMDCM cursor for
035300110304     C+ select * from titas00f
035400110304     C+ where tasccm = :wParCCM and
035500110304     C+ tasdcm between :DACMDA and :DACMA
035600110304     C+ union
035700110304     C+ select * from titas10f
035800110304     C+ where tasccm = :wParCCM and
035900110304     C+ tasdcm between :DACMDA and :DACMA
036000110304     C+ union
036100110304     C+ select * from titasP0f
036200110304     C+ where tasccm = :wParCCM and
036300110304     C+ tasdcm between :DACMDA and :DACMA
036400110304     C+ for read only
036500110304     C/END-EXEC
036600110304     C
036700110304     C/EXEC SQL
036800110304     C+ open C_CCMDCM
036900110304     C/END-EXEC
037000110304     C
037100110304     C/EXEC SQL
037200110304     C+ Fetch C_CCMDCM into :TITAS00F
037300110304     C/END-EXEC
037400110304     C*
037500110304     C                   dow       sqlcod = *zeros
037600110304     C                   exsr      verifica
037700110304     C                   if        CHKREC = 'S'
037800110304     C                   exsr      scriviWF
037900110304     C                   endif
038000110304     C
038100110304     C/EXEC SQL
038200110304     C+ Fetch C_CCMDCM into :TITAS00F
038300110304     C/END-EXEC
038400110304     C*
038500110304     C                   enddo
038600110304     C*
038700110304     C/EXEC SQL
038800110304     C+ close C_CCMDCM
038900110304     C/END-EXEC
039000110304     C*
039100110304     C*
039200110304     C                   ENDSR
039300110304     C*------------------------------------------------------------------------*
039400111130
039500111130
039600111130
039700111130     C*------------------------------------------------------------------------*
039800111130     C* SQ_KSCDCM - Elaborazione x codice cliente fatturazione + data consegna merce
039900111130     C*------------------------------------------------------------------------*
040000111130     C     SQ_KSCDCM     BEGSR
040100111130     C*
040200111130     C                   z-add     KUNI(I)       wParKSC           7 0
040300111130     C*
040400111130     C/EXEC SQL
040500111130     C+ declare C_KSCDCM cursor for
040600111130     C+ select * from titas00f
040700111130     C+ where tasksc = :wParKSC and
040800111130     C+ tasdcm between :DACMDA and :DACMA
040900111130     C+ union
041000111130     C+ select * from titas10f
041100111130     C+ where tasksc = :wParKSC and
041200111130     C+ tasdcm between :DACMDA and :DACMA
041300111130     C+ union
041400111130     C+ select * from titasP0f
041500111130     C+ where tasksc = :wParKSC and
041600111130     C+ tasdcm between :DACMDA and :DACMA
041700111130     C+ for read only
041800111130     C/END-EXEC
041900111130     C
042000111130     C/EXEC SQL
042100111130     C+ open C_KSCDCM
042200111130     C/END-EXEC
042300111130     C
042400111130     C/EXEC SQL
042500111130     C+ Fetch C_KSCDCM into :TITAS00F
042600111130     C/END-EXEC
042700111130     C*
042800111130     C                   dow       sqlcod = *zeros
042900111130     C                   exsr      verifica
043000111130     C                   if        CHKREC = 'S'
043100111130     C                   exsr      scriviWF
043200111130     C                   endif
043300111130     C
043400111130     C/EXEC SQL
043500111130     C+ Fetch C_KSCDCM into :TITAS00F
043600111130     C/END-EXEC
043700111130     C*
043800111130     C                   enddo
043900111130     C*
044000111130     C/EXEC SQL
044100111130     C+ close C_KSCDCM
044200111130     C/END-EXEC
044300111130     C*
044400111130     C*
044500111130     C                   ENDSR
044600111130     C*------------------------------------------------------------------------*
044700001218
044800110304
044900110304
045000110304     C*------------------------------------------------------------------------*
045100110304     C* CHKCONS  - Verifica se bolla effettivamente consegnata
045200110304     C*------------------------------------------------------------------------*
045300110304     C     CHKCONS       BEGSR
045400110304     C*
045500110304     C                   setoff                                       4041
045600110304     C                   movel     *blanks       wEsito1           1
045700110304     C                   movel     *blanks       wEsito2           1
045800110304     C*
045900110304     C                   eval      wAAS = tasAAS
046000110304     C                   eval      wLNP = tasLNP
046100110304     C                   eval      wNRS = tasNRS
046200110304     C                   eval      wNSP = tasNSP
046300110304     C*
046400110304     C* Chiamata metodo GetLblTyp
046500110304     C                   eval      wEsito1 = %char(UBLBLSPE_GetLblTyp(
046600110304     C                                                wAAS
046700110304     C                                               :wLNP
046800110304     C                                               :wNRS
046900110304     C                                               :wNSP
047000110304     C                                               :pOutLblTyp
047100110304     C                                               :pOutAnnoBO
047200110304     C                                               :pOutLineaParBO
047300110304     C                                               :pOutSerieBO
047400110304     C                                               :pOutNumSpedBO
047500110304     C                                               :pOutDcmBO
047600110304     C                                               :pOutCcaBO
047700110304     C                                               :pOutRblBO))
047800110304     C* Se trattasi d bolla nn originale reperisco la relativa bolla originale
047900110304     C                   if        pOutLblTyp <> 'O'
048000110304     C                   eval      wAAS = pOutAnnoBO
048100110304     C                   eval      wLNP = pOutLineaParBO
048200110304     C                   eval      wNRS = pOutSerieBO
048300110304     C                   eval      wNSP = pOutNumSpedBO
048400110304     C                   endif
048500110304     C*
048600110304     C* Chiamata metodo GetLastChild
048700110304     C                   eval      wEsito2 = %char(UBLBLSPE_GetLastChild(
048800110304     C                                                wAAS
048900110304     C                                               :wLNP
049000110304     C                                               :wNRS
049100110304     C                                               :wNSP
049200110304     C                                               :pOutAnnoFI
049300110304     C                                               :pOutLineaParFI
049400110304     C                                               :pOutSerieFI
049500110304     C                                               :pOutNumSpedFI
049600110304     C                                               :pOutDcmFI
049700110304     C                                               :pOutCcaFI))
049800110304     C*
049900110304     C* Considerazioni sullo stato corrente della bolla
050000110304     C                   if        (wEsito2 = '9' and pOutDcmBO > *zeros)  or
050100110304     C                             (wEsito2 = '0' and pOutDcmFI > *zeros)
050200110304     C                   seton                                        40        * bolla cons.
050300110304     C                   endif
050400110304     C*
050500110304     C                   if        (wEsito2 = '9' and pOutDcmBO > *zeros
050600110304     C                                            and pOutCcaBO = *blanks) or
050700110304     C                             (wEsito2 = '0' and pOutDcmFI > *zeros
050800110304     C                                            and pOutCcaFI = *blanks)
050900110304     C                   seton                                        41        * bolla cons. OK
051000110304     C                   endif
051100110304     C*
051200110304     C                   ENDSR
051300110304     C*------------------------------------------------------------------------*
051400110304
051500110304
051600110304
051700110304     C*------------------------------------------------------------------------*
051800110304     C* VERIFICA - Routine di verifica validità record corrente
051900110304     C*------------------------------------------------------------------------*
052000110304     C     VERIFICA      BEGSR
052100110304     C*
052200110304     C                   movel     'S'           CHKREC            1
052300110307     C*
052400110307     C                   movel     tasaas        wrkdata8          8 0
052500110307     C                   move      tasmgs        wrkdata8
052600110307     C                   movel     wrkdata8      wrkAAAAMM         6 0
052700110304     C*
052800110304     C* Verifico le date spedizione
052900110304     C                   if        CHKREC = 'S'
053000110304     C                   if        DATADA > *zeros
053100110304     C                   movel     tasaas        wrkdata           8 0
053200110304     C                   move      tasmgs        wrkdata
053300110304     C                   if        wrkdata < DATADA or
053400110304     C                             wrkdata > DATAA
053500110304     C                   movel     'N'           CHKREC
053600110304     C                   endif
053700110304     C                   endif
053800110304     C                   endif
053900110304     C*
054000110304     C* Verifico le date spedizione
054100110304     C                   if        CHKREC = 'S'
054200110304     C                   if        DACMDA > *zeros
054300110304     C                   if        tasdcm  < DACMDA or
054400110304     C                             tasdcm  > DACMA
054500110304     C                   movel     'N'           CHKREC
054600110304     C                   endif
054700110304     C                   endif
054800110304     C                   endif
054900110304     C*
055000110304     C* Verifico il codice cliente mittente
055100110304     C                   if        CHKREC = 'S'
055200110304     C                   if        wChkCli  = 'C'      AND
055300110304     C                             tasccm  <> KUNI(I)
055400110304     C                   movel     'N'           CHKREC
055500110304     C                   endif
055600110304     C                   endif
055700110304     C*
055800110304     C* Verifico il codice cliente fatturazione (in caso d 'K')
055900110304     C                   if        CHKREC = 'S'
056000110304     C                   if        wChkCli  = 'K'      AND
056100110304     C                             tasksc  <> KUNI(I)
056200110304     C                   movel     'N'           CHKREC
056300110304     C                   endif
056400110304     C                   endif
056500110304     C*
056600110304     C* Verifico il codice cliente fatturazione (in caso d 'E')
056700110304     C                   if        CHKREC = 'S'
056800110304     C                   if        TIPCLI  = 'E'       AND
056900110304     C                             wChkCli = 'K'       AND
057000110304     C                             tasksc  = tasccm
057100110304     C                   movel     'N'           CHKREC
057200110304     C                   endif
057300110304     C                   endif
057400110304     C*
057500110304     C* Verifico la serie
057600110304     C                   if        CHKREC = 'S'
057700110307     C                   if        NUMSER <> *blanks
057800110307     C                   if        tasnrs <> %dec(NUMSER : 2 : 0)
057900110304     C                   movel     'N'           CHKREC
058000110304     C                   endif
058100110304     C                   endif
058200110304     C                   endif
058300110304     C*
058400110304     C* Verifico il tipo bolla
058500110304     C                   if        CHKREC = 'S'
058600110304     C                   if        TIPBOL <> *blanks
058700110307     C                   if        tastbl <> TIPBOL
058800110304     C                   movel     'N'           CHKREC
058900110304     C                   endif
059000110304     C                   endif
059100110304     C                   endif
059200110304     C*
059300110304     C* Verifico se richiesto nel lancio che la bolla sia consegnata
059400110304     C                   if        CHKREC = 'S'
059500110304     C                   if        FLGCONS = 'S'
059600110304     C                   exsr      CHKCONS
059700110304     C                   if        *in40 = *on
059800110304     C                   else
059900110304     C                   movel     'N'           CHKREC
060000110304     C                   endif
060100110304     C                   endif
060200110304     C                   endif
060300110304     C*
060400110304     C* Verifico se richiesto nel lancio che la bolla sia consegnata OK
060500110304     C                   if        CHKREC = 'S'
060600110304     C                   if        FLGCONS = 'O'
060700110304     C                   exsr      CHKCONS
060800110304     C                   if        *in41 = *on
060900110304     C                   else
061000110304     C                   movel     'N'           CHKREC
061100110304     C                   endif
061200110304     C                   endif
061300110304     C                   endif
061400110304     C*
061500110304     C* Verifico se richiesto nel lancio che la bolla NON sia consegnata OK
061600110304     C                   if        CHKREC = 'S'
061700110304     C                   if        FLGCONS = 'K'
061800110304     C                   exsr      CHKCONS
061900110304     C                   if        *in41 = *on
062000110304     C                   movel     'N'           CHKREC
062100110304     C                   endif
062200110304     C                   endif
062300110304     C                   endif
062400110304     C*
062500110304     C* Verifico se richiesto nel lancio che la bolla NON sia consegnata
062600110304     C                   if        CHKREC = 'S'
062700110304     C                   if        FLGCONS = 'P'
062800110304     C                   exsr      CHKCONS
062900110304     C                   if        *in40 = *on
063000110304     C                   movel     'N'           CHKREC
063100110304     C                   endif
063200110304     C                   endif
063300110304     C                   endif
063400110304     C*
063500110304     C                   ENDSR
063600110304     C*------------------------------------------------------------------------*
063700001218
063800001218
063900001218
064000001218     C*------------------------------------------------------------------------*
064100110307     C* SCRIVIWF - Routine di scrittura WFCLN10F
064200001218     C*------------------------------------------------------------------------*
064300100219     C     SCRIVIWF      BEGSR
064400100219     C*
064500110307     C                   clear                   WFCLN110
064600110307     C                   clear                   WFCLN120
064700110307     C                   clear                   WFCLN_10
064800110307     C                   clear                   WFCLN_11
064900110307     C                   clear                   WFCLN_12
065000100219     C*
065100100219     C                   eval      dlyLNP = tasLNP
065200100219     C                   eval      dlyTSP = tasTSP
065300120309     C*
065400120309     C                   if        dlyTSP = 'D'
065500120309     C                   eval      dlyTSP = 'C'
065600120309     C                   endif
065700120309     C*
065800100219     C                   eval      dlyNAR = tasNZD
065900100219     C                   eval      dlyCAP = tasCAD
066000100219     C     KEYdly05_P    chain     wadly05l
066100100219     C                   if        %found(wadly05l)
066200100219     C                   eval      WCLNDLYTRC = DLYTRC
066300100219     C                   endif
066400100219     C*
066500100219     C                   eval      tblcod = 'PR'
066600100219     C                   eval      tblkey = tasprd
066700100219     C     KEYtab_C      chain     tabel00f
066800100219     C                   if        %found(tabel00f)
066900100219     C                   eval      DSPR = tbluni
067000100219     C                   eval      tblcod = '14'
067100100219     C                   eval      tblkey = §PRCRE
067200100219     C     KEYtab_C      chain     tabel00f
067300100219     C                   if        %found(tabel00f)
067400100219     C                   eval      DS14 = tbluni
067500110307     C                   eval      wclnREG = §14DES
067600100219     C                   endif
067700100219     C                   endif
067800100219     C*
067900100219     C                   z-add     *zeros        wANT
068000100222     C                   z-add     *zeros        wM12
068100100219     C                   z-add     *zeros        wOKS
068200100219     C                   z-add     *zeros        wRIT
068300001218     C*
068400100219     C* ...in anticipo
068500100219     C                   select
068600100219     C                   when      tasnrc >= -999 AND
068700100219     C                             tasnrc <= -24
068800100219     C                   add       1             wANT
068900100222     C*
069000100222     C* ...border-line
069100100222     C                   when      tasnrc >  -24  AND
069200100222     C                             tasnrc <= -12
069300100222     C                   add       1             wM12
069400100219     C*
069500100219     C* ...in orario
069600100222     C                   when      tasnrc >  -12  AND
069700100219     C                             tasnrc <=   0
069800100219     C                   add       1             wOKS
069900100219     C*
070000100219     C* ...in ritardo
070100100219     C                   when      tasnrc >    0
070200100219     C                   add       1             wRIT
070300100219     C                   endsl
070400100219     C*
070500100219     C* Conteggio spedizioni DIR/GIAC/CCA
070600100219     C                   eval      dtasflo = tasflo
070700100219     C                   if        tasCCA   <> *blanks OR
070800100219     C                             tasFGC   <> *blanks OR
070900100219     C                             §FLONAV  <> *blanks
071000100219     C                   eval      wclnANOM = 'S'
071100100219     C                   endif
071200100219     C*
071300111130     C                   eval      wclnCCM    = tasccm
071400111130     C                   if        wChkCli = 'K'
071500111130     C                   eval      wclnCCM    = tasksc
071600111130     C                   endif
071700111130     C*
071800100219     C                   eval      wclnAAAAMM = wrkAAAAMM
071900100219     C                   eval      wclnPRD    = tasprd
072000100219     C                   eval      wclnCAD    = tascad
072100100219     C                   eval      wclnISO    = tasfin
072200110307     C                   eval      wclnAAS    = tasaas
072300110307     C                   eval      wclnLNP    = taslnp
072400110307     C                   eval      wclnNRS    = tasnrs
072500110307     C                   eval      wclnNSP    = tasnsp
072600110307     C*
072700110307     C                   select
072800110307     C                   when      *in26
072900110307     C     KEYcln_12C    chain     wfcln12l
073000110307     C                   if        %found(wfcln12l)
073100110307     C                   eval      WFCLN_10 = WFCLN_12
073200100219     C                   add       wANT          wclnANT
073300100222     C                   add       wM12          wclnM12
073400100219     C                   add       wOKS          wclnOKS
073500100219     C                   add       wRIT          wclnRIT
073600100219     C                   add       1             wclnTOT
073700110307     C                   eval      WFCLN_12 = WFCLN_10
073800110307     C                   update    WFCLN120
073900100219     C                   else
074000100219     C                   add       wANT          wclnANT
074100100222     C                   add       wM12          wclnM12
074200100219     C                   add       wOKS          wclnOKS
074300100219     C                   add       wRIT          wclnRIT
074400100219     C                   add       1             wclnTOT
074500110307     C                   eval      WFCLN_12 = WFCLN_10
074600110307     C                   write     WFCLN120
074700100219     C                   endif
074800110307     C*
074900110307     C                   when      *in27
075000110307     C                   add       wANT          wclnANT
075100110307     C                   add       wM12          wclnM12
075200110307     C                   add       wOKS          wclnOKS
075300110307     C                   add       wRIT          wclnRIT
075400110307     C                   add       1             wclnTOT
075500110307     C                   eval      WFCLN_11 = WFCLN_10
075600110307     C                   write     WFCLN110
075700110307     C*
075800110307     C                   endsl
075900001218     C*
076000001218     C                   ENDSR
076100001221     C*------------------------------------------------------------------------*
076200110304
076300110304
076400110304
076500110304     C*------------------------------------------------------------------------*
076600110304     C* WRITIVGD - Routine di scrittura file TIVGD (file VAS generico download)
076700110304     C*------------------------------------------------------------------------*
076800110304     C     WRITIVGD      BEGSR
076900110304     C*
077000110307     C* Leggo tutto il file WFCLN11L in QTEMP del job corrente
077100110307     C     *loval        setll     WFCLN11L
077200110307     C                   read      WFCLN11L
077300110307     C                   dow       not %eof(WFCLN11L)
077400110307     C*
077500110304     C                   clear                   tivgd000
077600110307     C                   eval      vgdDTA = %subst(WFCLN_11:1:%size(WFCLN_11))
077700110304     C                   eval      vgdTIP = TIPFILE
077800110304     C                   movel     *all'0'       vgdKSU
077900110304     C                   move      CODCLIVAS     vgdKSU
078000110304     C                   eval      vgdTSC = 'WW'
078100110304     C                   eval      vgdDAT = datcor
078200110304     C                   eval      vgdPGM = 'TIS7FFR'
078300110304     C*
078400110307     C***                clear                   DVGDFLO
078500110307     C***                movel     'P'           §VGDFELA
078600110307     C***                movel     DVGDFLO       vgdflo
078700110304     C*
078800110304     C                   write     tivgd000
078900110304     C*
079000110307     C                   read      WFCLN11L
079100110307     C                   enddo
079200110307     C*
079300110307     C* Infine forzo l'elaborazione immediata del file d output
079400110307     C***                exsr      forzadwn
079500110307     C*
079600110304     C                   ENDSR
079700110304     C*------------------------------------------------------------------------*
079800110304
079900110304
080000110304
080100110304     C*------------------------------------------------------------------------*
080200110304     C* FORZADWN - Routine di forzatura elaborazione immediata download
080300110304     C*------------------------------------------------------------------------*
080400110304     C     FORZADWN      BEGSR
080500110304     C*
080600110304     C                   clear                   tis781ds
080700110304     C                   eval      §781tip = vgdTIP
080800110304     C                   eval      §781ksu = vgdKSU
080900110304     C                   eval      §781tsc = vgdTSC
081000110304     C                   eval      §781dat = vgdDAT
081100110304     C                   eval      §781pgm = vgdPGM
081200110304     C*
081300110304     C                   clear                   TIS781DFLO
081400110304     C                   movel     'P'           §781floela
081500110304     C                   movel     TIS781DFLO    §781flo
081600110304     C*
081700110304     C* Finita l'elaborazione chiamo pgm
081800110304     C                   CALL      'TIS781C1'
081900110304     C                   parm      *blanks       esito             1
082000110304     C                   parm                    tis781ds
082100110304     C                   parm      *blanks       vgdPRG           10
082200110304     C*
082300110304     C                   ENDSR
082400110304     C*------------------------------------------------------------------------*
082500120214
082600120214
082700120214
082800120214     C*------------------------------------------------------------------------*
082900120214     C* CARTBL - Routine di caricamento dati tabellati
083000120214     C*------------------------------------------------------------------------*
083100120214     C     CARTBL        BEGSR
083200120214     C*
083300120214     C                   Z-ADD     1             I                 4 0
083400120214     C                   MOVEL     CODCLI        KUNI(I)
083500120214     C                   IF        FLGUNI = 'U'
083600120214     C                   MOVEL     '3K'          tblCOD
083700120214     C     KEYtab_P      CHAIN     TABEL                              31
083800120214     C     *IN31         DOWEQ     '0'
083900120214     C     TBLFLG        IFEQ      ' '
084000120214     C                   MOVEL     TBLUNI        DS3K
084100120214     C     §3KCKS        IFEQ      CODCLI
084200120214     C                   ADD       1             I
084300120214     C                   MOVEL     TBLKEY        wKUNI             7 0
084400120214     C     wKUNI         LOOKUP    KUNI                                   55
084500120214     C                   IF        %equal
084600120214     C                   ELSE
084700120214     C                   Z-ADD     wKUNI         KUNI(I)
084800120214     C                   ENDIF
084900120214     C                   ENDIF
085000120214     C                   ENDIF
085100120214     C     KEYtab_P      READE     TABEL                                  31
085200120214     C                   ENDDO
085300120214     C                   ENDIF
085400120214     C*
085500120214     C                   ENDSR
085600120214     C*---------------------------------------------------------------*
085700120214
085800001218
085900001218
086000001218     C*------------------------------------------------------------------------*
086100001218     C* *INZSR - ROUTINE INIZIALE
086200001218     C*------------------------------------------------------------------------*
086300001218     C     *INZSR        BEGSR
086400001218     C*
086500001218     C     *ENTRY        PLIST
086600100219     C                   PARM                    KPJBA
086700001218     C*
086800100219     C                   MOVEL     KPJBU         DSINPUT
086900100219     C                   EVAL      tblKUT = 1
087000110307     C*
087100110307     C* Determino la data corrente
087200110307     C                   Z-ADD     *zeros        datcor            8 0
087300110307     C                   EVAL      datcor = %dec(%date() : *ISO)
087400001218     C*
087500001218     C* Definizioni chiavi
087600100219     C*
087700100219     C     KEYdly05_P    KLIST
087800100219     C                   KFLD                    dlyLNP
087900100219     C                   KFLD                    dlyTSP
088000100219     C                   KFLD                    dlyNAR
088100100219     C                   KFLD                    dlyCAP
088200120214     C*
088300120214     C     KEYtab_P      KLIST
088400120214     C                   KFLD                    tblKUT
088500120214     C                   KFLD                    tblCOD
088600001218     C*
088700100219     C     KEYtab_C      KLIST
088800100219     C                   KFLD                    tblKUT
088900100219     C                   KFLD                    tblCOD
089000100219     C                   KFLD                    tblKEY
089100110307     C*
089200110307     C     KEYcln_11C    KLIST
089300110307     C                   KFLD                    wclnCCM
089400110307     C                   KFLD                    wclnAAAAMM
089500110307     C                   KFLD                    wclnAAS
089600110307     C                   KFLD                    wclnLNP
089700110307     C                   KFLD                    wclnNRS
089800110307     C                   KFLD                    wclnNSP
089900030704     C*
090000110307     C     KEYcln_12C    KLIST
090100100219     C                   KFLD                    wclnCCM
090200100219     C                   KFLD                    wclnAAAAMM
090300110307     C                   KFLD                    wclnREG
090400100219     C                   KFLD                    wclnPRD
090500100219     C                   KFLD                    wclnCAD
090600100219     C                   KFLD                    wclnISO
090700100219     C                   KFLD                    wclnANOM
090800100219     C*
090900001218     C                   ENDSR

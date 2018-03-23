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
002200110307     D trul47ds      e ds
002300110307     D TIS781DS      E DS
002400110307     D TIS781DFLO    E DS
002500110307     D DVGDFLO       E DS
002600110304     D*--------------------
002700110304     D* SCHIERA PER REPERIMENTO CLIENTI "FIGLI" DI UN UNIFICANTE
002800110304     D*--------------------
002900110304     D KUNI            S              7  0 DIM(1000) DESCEND
003000110304     D*--------------------
003100110304     D* DS DI RIDEFINIZIOINE PARAMETRI IN INPUT
003200110304     D*--------------------
003300110304     D DSINPUT         DS
003400110304     D  DATADA                        8  0
003500110304     D  DATAA                         8  0
003600110304     D  CODCLI                        7  0
003700110304     D  FLGUNI                        1
003800110304     D  FLGCONS                       1
003900110304     D  CODCLIVAS                     7  0
004000110304     D  TIPFILE                       2
004100110304     D  FLGEXE                        1
004200110304     D  TIPCLI                        1
004300110304     D  DACMDA                        8  0
004400110304     D  DACMA                         8  0
004500110304     D  TIPBOL                        2
004600110307     D  NUMSER                        2
004700110304     D  TIPDET                        1
004800110304     D*--------------------
004900110304
005000010605     D*--------------------
005100010605     D* VARIABILI DI WRK
005200010605     D*--------------------
005300100219     D wANT            s              7  0 inz
005400100222     D wM12            s              7  0 inz
005500100219     D wOKS            s              7  0 inz
005600100219     D wRIT            s              7  0 inz
005700110304
005800110304
005900110304     D*------------------
006000110304     D* VARIABILI D WRK
006100110304     D*------------------
006200110304     D  wAAS           S                   like(tasAAS) inz
006300110304     D  wLNP           S                   like(tasLNP) inz
006400110304     D  wNRS           S                   like(tasNRS) inz
006500110304     D  wNSP           S                   like(tasNSP) inz
006600110304
006700110304
006800110304     D*------------------
006900110304     D* LINKING A DEFINIZIONI ESTERNE
007000110304     D*------------------
007100110304     D/COPY GAITRASRC/SRCPROTOPR,UBLBLSPE
007200110304     D/COPY GAITRASRC/SRCPROTOPI,UBLBLSPE
007300010605
007400010605
007500920812     C*---------------------------------------------------------------*
007600001218     C* MAIN
007700001218     C*---------------------------------------------------------------*
007800110304     C*
007900110304     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
008000110304     C
008100110304     C/EXEC SQL
008200110304     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
008300110304     C/END-EXEC
008400110304     C
008500110304     C*
008600110304     C                   exsr      chkpar
008700110304     C*
008800110307 xxx C* Fisso KUNI(1)
008900110307     C                   eval      KUNI(1) = CODCLI
009000110304     C*
009100110304     C* Ciclo sulla schiera dei clienti figli dell'unificante preso in input
009200110304     C                   z-add     1             I                 4 0
009300110304     C                   sorta     KUNI
009400110304     C                   dow       KUNI(I) > *zeros
009500001218     C                   exsr      procedi
009600110307     C                   add       1             I
009700110304     C                   enddo
009800110307     C*
009900110307     C* Inizializzo variabili d wrk
010000110307     C                   movel     'N'           wProcedi          1
010100110307     C*
010200110307     C* Apro il file d output
010300110307     C                   open      tivgd00f
010400110307     C*
010500110307     C* Come prima cosa avvio il blocco elaborazione TIVGD x tipo file corrente: 'VC'
010600110307     C                   clear                   trul47ds
010700110307     C                   eval      d47opz  = 'I'
010800110307     C                   eval      d47tip  = 'FF'
010900110307     C                   eval      d47lck  = 'N'
011000110307     C                   eval      d47chkj = 'N'
011100110307     C                   eval      d47pgm  = 'TIS7FFR'
011200110307     C                   call      'TRUL47R'
011300110307     C                   parm                    trul47ds
011400110307     C*
011500110307     C* Se elaborazione consentita => proseguo
011600110307     C                   if        d47sts <> 'A'
011700110307     C                   movel     'S'           wProcedi
011800110307     C                   endif
011900110307     C*
012000110307     C* Popolo il file d output
012100110307     C                   if        wProcedi = 'S'
012200110307     C                   exsr      WRITIVGD
012300110307     C                   endif
012400110304     C*
012500110304     C* Chiudo il file di output
012600110304     C                   close     tivgd00f
012700110304     C*
012800110304     C* Infine elimino il blocco elaborazione TIVGD x tipo file corrente: 'VC'
012900110304     C                   clear                   trul47ds
013000110304     C                   eval      d47opz  = 'F'
013100110304     C                   eval      d47tip  = 'FF'
013200110304     C                   call      'TRUL47R'
013300110304     C                   parm                    trul47ds
013400001218     C*
013500001218     C                   seton                                        LR
013600110304
013700110304
013800110304
013900110304     C*------------------------------------------------------------------------*
014000110304     C* CHKPAR - Routine di controllo parametri ricevuti in input
014100110304     C*------------------------------------------------------------------------*
014200110304     C     CHKPAR        BEGSR
014300110304     C*
014400110304     C* Verifico le date
014500110304     C                   if        DATADA > *zeros AND
014600110304     C                             DATAA  = *zeros
014700110304     C                   movel     *all'9'       DATAA
014800110304     C                   endif
014900110304     C                   if        DACMDA > *zeros AND
015000110304     C                             DACMA  = *zeros
015100110304     C                   movel     *all'9'       DACMA
015200110304     C                   endif
015300110307     C*
015400110307     C* Verifico il livello d dettaglio richiesto
015500110307     C                   setoff                                       2627
015600110307     C                   select
015700110307     C                   when      TIPDET = 'C'                                 * x CAP
015800110307     C                   seton                                        26
015900110307     C                   when      TIPDET = 'S'                                 * x SPED
016000110307     C                   seton                                        27
016100110307     C                   endsl
016200110304     C*
016300110304     C                   ENDSR
016400110304     C*------------------------------------------------------------------------*
016500110304
016600110304
016700110304
016800110304     C*------------------------------------------------------------------------*
016900110304     C* PROCEDI - Routine principale
017000110304     C*------------------------------------------------------------------------*
017100110304     C     PROCEDI       BEGSR
017200110304     C*
017300110304     C* Se richiesta elaborazione x codice cliente mittente...
017400110304     C                   select
017500110304     C                   when      TIPCLI = *blanks
017600110304     C                   exsr      EXECCM
017700110304     C*
017800110304     C* Se richiesta elaborazione x codice cliente fatturazione...
017900110304     C                   when      TIPCLI = 'K'
018000111130     C                   exsr      EXEKSC
018100110304     C*
018200110304     C* Se richiesta elaborazione x entrambi: codice cliente mittente + fatturazione...
018300110304     C                   when      TIPCLI = 'E'
018400110304     C                   exsr      EXECCM
018500111130     C                   exsr      EXEKSC
018600110304     C*
018700110304     C                   endsl
018800110304     C*
018900110304     C                   ENDSR
019000110304     C*------------------------------------------------------------------------*
019100110304
019200110304
019300110304
019400110304     C*------------------------------------------------------------------------*
019500110304     C* EXECCM   - Elaborazione x codice cliente mittente
019600110304     C*------------------------------------------------------------------------*
019700110304     C     EXECCM        BEGSR
019800110304     C*
019900110304     C                   movel     'C'           wChkCli           1
020000110304     C*
020100110304     C                   select
020200110304     C* ...se richiesta elaborazione x data spedizione
020300110304     C                   when      DATADA > *zeros
020400110304     C                   exsr      SQ_CCMDSP
020500110304     C* ...se richiesta elaborazione x data consegna merce
020600110304     C                   when      DACMDA > *zeros
020700110304     C                   exsr      SQ_CCMDCM
020800110304     C                   endsl
020900110304     C*
021000110304     C                   ENDSR
021100110304     C*------------------------------------------------------------------------*
021200111130
021300111130
021400111130
021500111130     C*------------------------------------------------------------------------*
021600111130     C* EXEKSC   - Elaborazione x codice cliente fatturazione
021700111130     C*------------------------------------------------------------------------*
021800111130     C     EXEKSC        BEGSR
021900111130     C*
022000111130     C                   movel     'K'           wChkCli           1
022100111130     C*
022200111130     C                   select
022300111130     C* ...se richiesta elaborazione x data spedizione
022400111130     C                   when      DATADA > *zeros
022500111130     C                   exsr      SQ_KSCDSP
022600111130     C* ...se richiesta elaborazione x data consegna merce
022700111130     C                   when      DACMDA > *zeros
022800111130     C                   exsr      SQ_KSCDCM
022900111130     C                   endsl
023000111130     C*
023100111130     C                   ENDSR
023200111130     C*------------------------------------------------------------------------*
023300110304
023400110304
023500110304
023600110304     C*------------------------------------------------------------------------*
023700110304     C* SQ_CCMDSP - Elaborazione x codice cliente mittente + data spedizione
023800110304     C*------------------------------------------------------------------------*
023900110304     C     SQ_CCMDSP     BEGSR
024000110304     C*
024100110304     C                   z-add     KUNI(I)       wParCCM           7 0
024200110304     C*
024300110304     C/EXEC SQL
024400110304     C+ declare C_CCMDSP cursor for
024500110304     C+ select * from titas00f
024600110304     C+ where tasccm = :wParCCM and
024700110304     C+ tasaas*10000+tasmgs between :DATADA and :DATAA
024800110304     C+ union
024900110304     C+ select * from titas10f
025000110304     C+ where tasccm = :wParCCM and
025100110304     C+ tasaas*10000+tasmgs between :DATADA and :DATAA
025200110304     C+ union
025300110304     C+ select * from titasP0f
025400110304     C+ where tasccm = :wParCCM and
025500110304     C+ tasaas*10000+tasmgs between :DATADA and :DATAA
025600110304     C+ for read only
025700110304     C/END-EXEC
025800110304     C
025900110304     C/EXEC SQL
026000110304     C+ open C_CCMDSP
026100110304     C/END-EXEC
026200110304     C
026300110304     C/EXEC SQL
026400110304     C+ Fetch C_CCMDSP into :TITAS00F
026500110304     C/END-EXEC
026600110304     C*
026700110304     C                   dow       sqlcod = *zeros
026800110304     C                   exsr      verifica
026900110304     C                   if        CHKREC = 'S'
027000110304     C                   exsr      scriviWF
027100110304     C                   endif
027200110304     C
027300110304     C/EXEC SQL
027400110304     C+ Fetch C_CCMDSP into :TITAS00F
027500110304     C/END-EXEC
027600110304     C*
027700110304     C                   enddo
027800110304     C*
027900110304     C/EXEC SQL
028000110304     C+ close C_CCMDSP
028100110304     C/END-EXEC
028200110304     C*
028300110304     C*
028400110304     C                   ENDSR
028500110304     C*------------------------------------------------------------------------*
028600111130
028700111130
028800111130
028900111130     C*------------------------------------------------------------------------*
029000111130     C* SQ_KSCDSP - Elaborazione x codice cliente fatturazione + data spedizione
029100111130     C*------------------------------------------------------------------------*
029200111130     C     SQ_KSCDSP     BEGSR
029300111130     C*
029400111130     C                   z-add     KUNI(I)       wParKSC           7 0
029500111130     C*
029600111130     C/EXEC SQL
029700111130     C+ declare C_KSCDSP cursor for
029800111130     C+ select * from titas00f
029900111130     C+ where tasksc = :wParKSC and
030000111130     C+ tasaas*10000+tasmgs between :DATADA and :DATAA
030100111130     C+ union
030200111130     C+ select * from titas10f
030300111130     C+ where tasksc = :wParKSC and
030400111130     C+ tasaas*10000+tasmgs between :DATADA and :DATAA
030500111130     C+ union
030600111130     C+ select * from titasP0f
030700111130     C+ where tasksc = :wParKSC and
030800111130     C+ tasaas*10000+tasmgs between :DATADA and :DATAA
030900111130     C+ for read only
031000111130     C/END-EXEC
031100111130     C
031200111130     C/EXEC SQL
031300111130     C+ open C_KSCDSP
031400111130     C/END-EXEC
031500111130     C
031600111130     C/EXEC SQL
031700111130     C+ Fetch C_KSCDSP into :TITAS00F
031800111130     C/END-EXEC
031900111130     C*
032000111130     C                   dow       sqlcod = *zeros
032100111130     C                   exsr      verifica
032200111130     C                   if        CHKREC = 'S'
032300111130     C                   exsr      scriviWF
032400111130     C                   endif
032500111130     C
032600111130     C/EXEC SQL
032700111130     C+ Fetch C_KSCDSP into :TITAS00F
032800111130     C/END-EXEC
032900111130     C*
033000111130     C                   enddo
033100111130     C*
033200111130     C/EXEC SQL
033300111130     C+ close C_KSCDSP
033400111130     C/END-EXEC
033500111130     C*
033600111130     C*
033700111130     C                   ENDSR
033800111130     C*------------------------------------------------------------------------*
033900110304
034000110304
034100110304
034200110304     C*------------------------------------------------------------------------*
034300110304     C* SQ_CCMDCM - Elaborazione x codice cliente mittente + data consegna merce
034400110304     C*------------------------------------------------------------------------*
034500110304     C     SQ_CCMDCM     BEGSR
034600110304     C*
034700110304     C                   z-add     KUNI(I)       wParCCM           7 0
034800110304     C*
034900110304     C/EXEC SQL
035000110304     C+ declare C_CCMDCM cursor for
035100110304     C+ select * from titas00f
035200110304     C+ where tasccm = :wParCCM and
035300110304     C+ tasdcm between :DACMDA and :DACMA
035400110304     C+ union
035500110304     C+ select * from titas10f
035600110304     C+ where tasccm = :wParCCM and
035700110304     C+ tasdcm between :DACMDA and :DACMA
035800110304     C+ union
035900110304     C+ select * from titasP0f
036000110304     C+ where tasccm = :wParCCM and
036100110304     C+ tasdcm between :DACMDA and :DACMA
036200110304     C+ for read only
036300110304     C/END-EXEC
036400110304     C
036500110304     C/EXEC SQL
036600110304     C+ open C_CCMDCM
036700110304     C/END-EXEC
036800110304     C
036900110304     C/EXEC SQL
037000110304     C+ Fetch C_CCMDCM into :TITAS00F
037100110304     C/END-EXEC
037200110304     C*
037300110304     C                   dow       sqlcod = *zeros
037400110304     C                   exsr      verifica
037500110304     C                   if        CHKREC = 'S'
037600110304     C                   exsr      scriviWF
037700110304     C                   endif
037800110304     C
037900110304     C/EXEC SQL
038000110304     C+ Fetch C_CCMDCM into :TITAS00F
038100110304     C/END-EXEC
038200110304     C*
038300110304     C                   enddo
038400110304     C*
038500110304     C/EXEC SQL
038600110304     C+ close C_CCMDCM
038700110304     C/END-EXEC
038800110304     C*
038900110304     C*
039000110304     C                   ENDSR
039100110304     C*------------------------------------------------------------------------*
039200111130
039300111130
039400111130
039500111130     C*------------------------------------------------------------------------*
039600111130     C* SQ_KSCDCM - Elaborazione x codice cliente fatturazione + data consegna merce
039700111130     C*------------------------------------------------------------------------*
039800111130     C     SQ_KSCDCM     BEGSR
039900111130     C*
040000111130     C                   z-add     KUNI(I)       wParKSC           7 0
040100111130     C*
040200111130     C/EXEC SQL
040300111130     C+ declare C_KSCDCM cursor for
040400111130     C+ select * from titas00f
040500111130     C+ where tasksc = :wParKSC and
040600111130     C+ tasdcm between :DACMDA and :DACMA
040700111130     C+ union
040800111130     C+ select * from titas10f
040900111130     C+ where tasksc = :wParKSC and
041000111130     C+ tasdcm between :DACMDA and :DACMA
041100111130     C+ union
041200111130     C+ select * from titasP0f
041300111130     C+ where tasksc = :wParKSC and
041400111130     C+ tasdcm between :DACMDA and :DACMA
041500111130     C+ for read only
041600111130     C/END-EXEC
041700111130     C
041800111130     C/EXEC SQL
041900111130     C+ open C_KSCDCM
042000111130     C/END-EXEC
042100111130     C
042200111130     C/EXEC SQL
042300111130     C+ Fetch C_KSCDCM into :TITAS00F
042400111130     C/END-EXEC
042500111130     C*
042600111130     C                   dow       sqlcod = *zeros
042700111130     C                   exsr      verifica
042800111130     C                   if        CHKREC = 'S'
042900111130     C                   exsr      scriviWF
043000111130     C                   endif
043100111130     C
043200111130     C/EXEC SQL
043300111130     C+ Fetch C_KSCDCM into :TITAS00F
043400111130     C/END-EXEC
043500111130     C*
043600111130     C                   enddo
043700111130     C*
043800111130     C/EXEC SQL
043900111130     C+ close C_KSCDCM
044000111130     C/END-EXEC
044100111130     C*
044200111130     C*
044300111130     C                   ENDSR
044400111130     C*------------------------------------------------------------------------*
044500001218
044600110304
044700110304
044800110304     C*------------------------------------------------------------------------*
044900110304     C* CHKCONS  - Verifica se bolla effettivamente consegnata
045000110304     C*------------------------------------------------------------------------*
045100110304     C     CHKCONS       BEGSR
045200110304     C*
045300110304     C                   setoff                                       4041
045400110304     C                   movel     *blanks       wEsito1           1
045500110304     C                   movel     *blanks       wEsito2           1
045600110304     C*
045700110304     C                   eval      wAAS = tasAAS
045800110304     C                   eval      wLNP = tasLNP
045900110304     C                   eval      wNRS = tasNRS
046000110304     C                   eval      wNSP = tasNSP
046100110304     C*
046200110304     C* Chiamata metodo GetLblTyp
046300110304     C                   eval      wEsito1 = %char(UBLBLSPE_GetLblTyp(
046400110304     C                                                wAAS
046500110304     C                                               :wLNP
046600110304     C                                               :wNRS
046700110304     C                                               :wNSP
046800110304     C                                               :pOutLblTyp
046900110304     C                                               :pOutAnnoBO
047000110304     C                                               :pOutLineaParBO
047100110304     C                                               :pOutSerieBO
047200110304     C                                               :pOutNumSpedBO
047300110304     C                                               :pOutDcmBO
047400110304     C                                               :pOutCcaBO
047500110304     C                                               :pOutRblBO))
047600110304     C* Se trattasi d bolla nn originale reperisco la relativa bolla originale
047700110304     C                   if        pOutLblTyp <> 'O'
047800110304     C                   eval      wAAS = pOutAnnoBO
047900110304     C                   eval      wLNP = pOutLineaParBO
048000110304     C                   eval      wNRS = pOutSerieBO
048100110304     C                   eval      wNSP = pOutNumSpedBO
048200110304     C                   endif
048300110304     C*
048400110304     C* Chiamata metodo GetLastChild
048500110304     C                   eval      wEsito2 = %char(UBLBLSPE_GetLastChild(
048600110304     C                                                wAAS
048700110304     C                                               :wLNP
048800110304     C                                               :wNRS
048900110304     C                                               :wNSP
049000110304     C                                               :pOutAnnoFI
049100110304     C                                               :pOutLineaParFI
049200110304     C                                               :pOutSerieFI
049300110304     C                                               :pOutNumSpedFI
049400110304     C                                               :pOutDcmFI
049500110304     C                                               :pOutCcaFI))
049600110304     C*
049700110304     C* Considerazioni sullo stato corrente della bolla
049800110304     C                   if        (wEsito2 = '9' and pOutDcmBO > *zeros)  or
049900110304     C                             (wEsito2 = '0' and pOutDcmFI > *zeros)
050000110304     C                   seton                                        40        * bolla cons.
050100110304     C                   endif
050200110304     C*
050300110304     C                   if        (wEsito2 = '9' and pOutDcmBO > *zeros
050400110304     C                                            and pOutCcaBO = *blanks) or
050500110304     C                             (wEsito2 = '0' and pOutDcmFI > *zeros
050600110304     C                                            and pOutCcaFI = *blanks)
050700110304     C                   seton                                        41        * bolla cons. OK
050800110304     C                   endif
050900110304     C*
051000110304     C                   ENDSR
051100110304     C*------------------------------------------------------------------------*
051200110304
051300110304
051400110304
051500110304     C*------------------------------------------------------------------------*
051600110304     C* VERIFICA - Routine di verifica validità record corrente
051700110304     C*------------------------------------------------------------------------*
051800110304     C     VERIFICA      BEGSR
051900110304     C*
052000110304     C                   movel     'S'           CHKREC            1
052100110307     C*
052200110307     C                   movel     tasaas        wrkdata8          8 0
052300110307     C                   move      tasmgs        wrkdata8
052400110307     C                   movel     wrkdata8      wrkAAAAMM         6 0
052500110304     C*
052600110304     C* Verifico le date spedizione
052700110304     C                   if        CHKREC = 'S'
052800110304     C                   if        DATADA > *zeros
052900110304     C                   movel     tasaas        wrkdata           8 0
053000110304     C                   move      tasmgs        wrkdata
053100110304     C                   if        wrkdata < DATADA or
053200110304     C                             wrkdata > DATAA
053300110304     C                   movel     'N'           CHKREC
053400110304     C                   endif
053500110304     C                   endif
053600110304     C                   endif
053700110304     C*
053800110304     C* Verifico le date spedizione
053900110304     C                   if        CHKREC = 'S'
054000110304     C                   if        DACMDA > *zeros
054100110304     C                   if        tasdcm  < DACMDA or
054200110304     C                             tasdcm  > DACMA
054300110304     C                   movel     'N'           CHKREC
054400110304     C                   endif
054500110304     C                   endif
054600110304     C                   endif
054700110304     C*
054800110304     C* Verifico il codice cliente mittente
054900110304     C                   if        CHKREC = 'S'
055000110304     C                   if        wChkCli  = 'C'      AND
055100110304     C                             tasccm  <> KUNI(I)
055200110304     C                   movel     'N'           CHKREC
055300110304     C                   endif
055400110304     C                   endif
055500110304     C*
055600110304     C* Verifico il codice cliente fatturazione (in caso d 'K')
055700110304     C                   if        CHKREC = 'S'
055800110304     C                   if        wChkCli  = 'K'      AND
055900110304     C                             tasksc  <> KUNI(I)
056000110304     C                   movel     'N'           CHKREC
056100110304     C                   endif
056200110304     C                   endif
056300110304     C*
056400110304     C* Verifico il codice cliente fatturazione (in caso d 'E')
056500110304     C                   if        CHKREC = 'S'
056600110304     C                   if        TIPCLI  = 'E'       AND
056700110304     C                             wChkCli = 'K'       AND
056800110304     C                             tasksc  = tasccm
056900110304     C                   movel     'N'           CHKREC
057000110304     C                   endif
057100110304     C                   endif
057200110304     C*
057300110304     C* Verifico la serie
057400110304     C                   if        CHKREC = 'S'
057500110307     C                   if        NUMSER <> *blanks
057600110307     C                   if        tasnrs <> %dec(NUMSER : 2 : 0)
057700110304     C                   movel     'N'           CHKREC
057800110304     C                   endif
057900110304     C                   endif
058000110304     C                   endif
058100110304     C*
058200110304     C* Verifico il tipo bolla
058300110304     C                   if        CHKREC = 'S'
058400110304     C                   if        TIPBOL <> *blanks
058500110307     C                   if        tastbl <> TIPBOL
058600110304     C                   movel     'N'           CHKREC
058700110304     C                   endif
058800110304     C                   endif
058900110304     C                   endif
059000110304     C*
059100110304     C* Verifico se richiesto nel lancio che la bolla sia consegnata
059200110304     C                   if        CHKREC = 'S'
059300110304     C                   if        FLGCONS = 'S'
059400110304     C                   exsr      CHKCONS
059500110304     C                   if        *in40 = *on
059600110304     C                   else
059700110304     C                   movel     'N'           CHKREC
059800110304     C                   endif
059900110304     C                   endif
060000110304     C                   endif
060100110304     C*
060200110304     C* Verifico se richiesto nel lancio che la bolla sia consegnata OK
060300110304     C                   if        CHKREC = 'S'
060400110304     C                   if        FLGCONS = 'O'
060500110304     C                   exsr      CHKCONS
060600110304     C                   if        *in41 = *on
060700110304     C                   else
060800110304     C                   movel     'N'           CHKREC
060900110304     C                   endif
061000110304     C                   endif
061100110304     C                   endif
061200110304     C*
061300110304     C* Verifico se richiesto nel lancio che la bolla NON sia consegnata OK
061400110304     C                   if        CHKREC = 'S'
061500110304     C                   if        FLGCONS = 'K'
061600110304     C                   exsr      CHKCONS
061700110304     C                   if        *in41 = *on
061800110304     C                   movel     'N'           CHKREC
061900110304     C                   endif
062000110304     C                   endif
062100110304     C                   endif
062200110304     C*
062300110304     C* Verifico se richiesto nel lancio che la bolla NON sia consegnata
062400110304     C                   if        CHKREC = 'S'
062500110304     C                   if        FLGCONS = 'P'
062600110304     C                   exsr      CHKCONS
062700110304     C                   if        *in40 = *on
062800110304     C                   movel     'N'           CHKREC
062900110304     C                   endif
063000110304     C                   endif
063100110304     C                   endif
063200110304     C*
063300110304     C                   ENDSR
063400110304     C*------------------------------------------------------------------------*
063500001218
063600001218
063700001218
063800001218     C*------------------------------------------------------------------------*
063900110307     C* SCRIVIWF - Routine di scrittura WFCLN10F
064000001218     C*------------------------------------------------------------------------*
064100100219     C     SCRIVIWF      BEGSR
064200100219     C*
064300110307     C                   clear                   WFCLN110
064400110307     C                   clear                   WFCLN120
064500110307     C                   clear                   WFCLN_10
064600110307     C                   clear                   WFCLN_11
064700110307     C                   clear                   WFCLN_12
064800100219     C*
064900100219     C                   eval      dlyLNP = tasLNP
065000100219     C                   eval      dlyTSP = tasTSP
065100100219     C                   eval      dlyNAR = tasNZD
065200100219     C                   eval      dlyCAP = tasCAD
065300100219     C     KEYdly05_P    chain     wadly05l
065400100219     C                   if        %found(wadly05l)
065500100219     C                   eval      WCLNDLYTRC = DLYTRC
065600100219     C                   endif
065700100219     C*
065800100219     C                   eval      tblcod = 'PR'
065900100219     C                   eval      tblkey = tasprd
066000100219     C     KEYtab_C      chain     tabel00f
066100100219     C                   if        %found(tabel00f)
066200100219     C                   eval      DSPR = tbluni
066300100219     C                   eval      tblcod = '14'
066400100219     C                   eval      tblkey = §PRCRE
066500100219     C     KEYtab_C      chain     tabel00f
066600100219     C                   if        %found(tabel00f)
066700100219     C                   eval      DS14 = tbluni
066800110307     C                   eval      wclnREG = §14DES
066900100219     C                   endif
067000100219     C                   endif
067100100219     C*
067200100219     C                   z-add     *zeros        wANT
067300100222     C                   z-add     *zeros        wM12
067400100219     C                   z-add     *zeros        wOKS
067500100219     C                   z-add     *zeros        wRIT
067600001218     C*
067700100219     C* ...in anticipo
067800100219     C                   select
067900100219     C                   when      tasnrc >= -999 AND
068000100219     C                             tasnrc <= -24
068100100219     C                   add       1             wANT
068200100222     C*
068300100222     C* ...border-line
068400100222     C                   when      tasnrc >  -24  AND
068500100222     C                             tasnrc <= -12
068600100222     C                   add       1             wM12
068700100219     C*
068800100219     C* ...in orario
068900100222     C                   when      tasnrc >  -12  AND
069000100219     C                             tasnrc <=   0
069100100219     C                   add       1             wOKS
069200100219     C*
069300100219     C* ...in ritardo
069400100219     C                   when      tasnrc >    0
069500100219     C                   add       1             wRIT
069600100219     C                   endsl
069700100219     C*
069800100219     C* Conteggio spedizioni DIR/GIAC/CCA
069900100219     C                   eval      dtasflo = tasflo
070000100219     C                   if        tasCCA   <> *blanks OR
070100100219     C                             tasFGC   <> *blanks OR
070200100219     C                             §FLONAV  <> *blanks
070300100219     C                   eval      wclnANOM = 'S'
070400100219     C                   endif
070500100219     C*
070600111130     C                   eval      wclnCCM    = tasccm
070700111130     C                   if        wChkCli = 'K'
070800111130     C                   eval      wclnCCM    = tasksc
070900111130     C                   endif
071000111130     C*
071100100219     C                   eval      wclnAAAAMM = wrkAAAAMM
071200100219     C                   eval      wclnPRD    = tasprd
071300100219     C                   eval      wclnCAD    = tascad
071400100219     C                   eval      wclnISO    = tasfin
071500110307     C                   eval      wclnAAS    = tasaas
071600110307     C                   eval      wclnLNP    = taslnp
071700110307     C                   eval      wclnNRS    = tasnrs
071800110307     C                   eval      wclnNSP    = tasnsp
071900110307     C*
072000110307     C                   select
072100110307     C                   when      *in26
072200110307     C     KEYcln_12C    chain     wfcln12l
072300110307     C                   if        %found(wfcln12l)
072400110307     C                   eval      WFCLN_10 = WFCLN_12
072500100219     C                   add       wANT          wclnANT
072600100222     C                   add       wM12          wclnM12
072700100219     C                   add       wOKS          wclnOKS
072800100219     C                   add       wRIT          wclnRIT
072900100219     C                   add       1             wclnTOT
073000110307     C                   eval      WFCLN_12 = WFCLN_10
073100110307     C                   update    WFCLN120
073200100219     C                   else
073300100219     C                   add       wANT          wclnANT
073400100222     C                   add       wM12          wclnM12
073500100219     C                   add       wOKS          wclnOKS
073600100219     C                   add       wRIT          wclnRIT
073700100219     C                   add       1             wclnTOT
073800110307     C                   eval      WFCLN_12 = WFCLN_10
073900110307     C                   write     WFCLN120
074000100219     C                   endif
074100110307     C*
074200110307     C                   when      *in27
074300110307     C                   add       wANT          wclnANT
074400110307     C                   add       wM12          wclnM12
074500110307     C                   add       wOKS          wclnOKS
074600110307     C                   add       wRIT          wclnRIT
074700110307     C                   add       1             wclnTOT
074800110307     C                   eval      WFCLN_11 = WFCLN_10
074900110307     C                   write     WFCLN110
075000110307     C*
075100110307     C                   endsl
075200001218     C*
075300001218     C                   ENDSR
075400001221     C*------------------------------------------------------------------------*
075500110304
075600110304
075700110304
075800110304     C*------------------------------------------------------------------------*
075900110304     C* WRITIVGD - Routine di scrittura file TIVGD (file VAS generico download)
076000110304     C*------------------------------------------------------------------------*
076100110304     C     WRITIVGD      BEGSR
076200110304     C*
076300110307     C* Leggo tutto il file WFCLN11L in QTEMP del job corrente
076400110307     C     *loval        setll     WFCLN11L
076500110307     C                   read      WFCLN11L
076600110307     C                   dow       not %eof(WFCLN11L)
076700110307     C*
076800110304     C                   clear                   tivgd000
076900110307     C                   eval      vgdDTA = %subst(WFCLN_11:1:%size(WFCLN_11))
077000110304     C                   eval      vgdTIP = TIPFILE
077100110304     C                   movel     *all'0'       vgdKSU
077200110304     C                   move      CODCLIVAS     vgdKSU
077300110304     C                   eval      vgdTSC = 'WW'
077400110304     C                   eval      vgdDAT = datcor
077500110304     C                   eval      vgdPGM = 'TIS7FFR'
077600110304     C*
077700110307     C***                clear                   DVGDFLO
077800110307     C***                movel     'P'           §VGDFELA
077900110307     C***                movel     DVGDFLO       vgdflo
078000110304     C*
078100110304     C                   write     tivgd000
078200110304     C*
078300110307     C                   read      WFCLN11L
078400110307     C                   enddo
078500110307     C*
078600110307     C* Infine forzo l'elaborazione immediata del file d output
078700110307     C***                exsr      forzadwn
078800110307     C*
078900110304     C                   ENDSR
079000110304     C*------------------------------------------------------------------------*
079100110304
079200110304
079300110304
079400110304     C*------------------------------------------------------------------------*
079500110304     C* FORZADWN - Routine di forzatura elaborazione immediata download
079600110304     C*------------------------------------------------------------------------*
079700110304     C     FORZADWN      BEGSR
079800110304     C*
079900110304     C                   clear                   tis781ds
080000110304     C                   eval      §781tip = vgdTIP
080100110304     C                   eval      §781ksu = vgdKSU
080200110304     C                   eval      §781tsc = vgdTSC
080300110304     C                   eval      §781dat = vgdDAT
080400110304     C                   eval      §781pgm = vgdPGM
080500110304     C*
080600110304     C                   clear                   TIS781DFLO
080700110304     C                   movel     'P'           §781floela
080800110304     C                   movel     TIS781DFLO    §781flo
080900110304     C*
081000110304     C* Finita l'elaborazione chiamo pgm
081100110304     C                   CALL      'TIS781C1'
081200110304     C                   parm      *blanks       esito             1
081300110304     C                   parm                    tis781ds
081400110304     C                   parm      *blanks       vgdPRG           10
081500110304     C*
081600110304     C                   ENDSR
081700110304     C*------------------------------------------------------------------------*
081800110304
081900001218
082000001218
082100001218     C*------------------------------------------------------------------------*
082200001218     C* *INZSR - ROUTINE INIZIALE
082300001218     C*------------------------------------------------------------------------*
082400001218     C     *INZSR        BEGSR
082500001218     C*
082600001218     C     *ENTRY        PLIST
082700100219     C                   PARM                    KPJBA
082800001218     C*
082900100219     C                   MOVEL     KPJBU         DSINPUT
083000100219     C                   EVAL      tblKUT = 1
083100110307     C*
083200110307     C* Determino la data corrente
083300110307     C                   Z-ADD     *zeros        datcor            8 0
083400110307     C                   EVAL      datcor = %dec(%date() : *ISO)
083500001218     C*
083600001218     C* Definizioni chiavi
083700100219     C*
083800100219     C     KEYdly05_P    KLIST
083900100219     C                   KFLD                    dlyLNP
084000100219     C                   KFLD                    dlyTSP
084100100219     C                   KFLD                    dlyNAR
084200100219     C                   KFLD                    dlyCAP
084300001218     C*
084400100219     C     KEYtab_C      KLIST
084500100219     C                   KFLD                    tblKUT
084600100219     C                   KFLD                    tblCOD
084700100219     C                   KFLD                    tblKEY
084800110307     C*
084900110307     C     KEYcln_11C    KLIST
085000110307     C                   KFLD                    wclnCCM
085100110307     C                   KFLD                    wclnAAAAMM
085200110307     C                   KFLD                    wclnAAS
085300110307     C                   KFLD                    wclnLNP
085400110307     C                   KFLD                    wclnNRS
085500110307     C                   KFLD                    wclnNSP
085600030704     C*
085700110307     C     KEYcln_12C    KLIST
085800100219     C                   KFLD                    wclnCCM
085900100219     C                   KFLD                    wclnAAAAMM
086000110307     C                   KFLD                    wclnREG
086100100219     C                   KFLD                    wclnPRD
086200100219     C                   KFLD                    wclnCAD
086300100219     C                   KFLD                    wclnISO
086400100219     C                   KFLD                    wclnANOM
086500100219     C*
086600001218     C                   ENDSR

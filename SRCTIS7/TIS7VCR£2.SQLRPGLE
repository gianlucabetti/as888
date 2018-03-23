000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200120427     H DFTACTGRP(*NO) BNDDIR('UBBNDDIR') ACTGRP(*CALLER)
000300011017     H*         - RITORNO DATI AL CLIENTE  da sede
000400000000     H*--------------------------------------------------------------*
000500001218     FTABEL00F  IF   E           K DISK
000600001218     FTITA430C  IF   E           K DISK
000700001218     FTITAA30C  IF   E           K DISK
000800001218     FFNEVB01L  IF   E           K DISK
000900001219     FTNCSB03L  IF   E           K DISK
001000050310     FTIGCP51L  IF   E           K DISK
001100001222     FFNVAC00T  UF A E             DISK    USROPN
001200050823     FTIVGD00F  UF A E             DISK    USROPN
001300971126     D*--------------------------------------------------------------*
001400001218     D* SCHIERA PER CARICAMENTO CODICI E TIPI MANCATE CONSEGNE
001500001218     D*--------------------
001600001218     D C2A             S              3    DIM(300)                             cd.manc.cons.
001700001218     D T2A             S              1    DIM(300)                             tp.manc.cons.
001800040312     D COD3A           S              2    DIM(300)                             codici bolle x VAC
001900001218     D*--------------------
002000001218     D* SCHIERA PER REPERIMENTO CLIENTI "FIGLI" DI UN UNIFICANTE
002100001218     D*--------------------
002200090407     D KUNI            S              7  0 DIM(1000) DESCEND
002300001218     D*--------------------
002400001218     D* DS ESTERNE
002500001218     D*--------------------
002600100512     D TITAS00F      E DS
002700900517     D KPJBA         E DS
002800921023     D DS3K          E DS
002900941228     D DS2A          E DS
003000040312     D DS3A          E DS
003100000526     D DSBL4J        E DS
003200050823     D FNVACDS       E DS                  EXTNAME(FNVAC00T)
003300050909     D trul47ds      e ds
003400070627     D TIS781DS      E DS
003500080905     D TIS781DFLO    E DS
003600070627     D DVGDFLO       E DS
003700001218     D*--------------------
003800001218     D* DS DI RIDEFINIZIOINE PARAMETRI IN INPUT
003900001218     D*--------------------
004000050823     D DSINPUT         DS
004100050823     D  DATADA                        8  0
004200050823     D  DATAA                         8  0
004300050823     D  CODCLI                        7  0
004400050823     D  FLGUNI                        1
004500100512     D  FLGCONS                       1
004600050823     D  FLGVAS                        1
004700050823     D  FLGOPZ                        1
004800150707     D  CODCLIVAS                     7
004900100312     D  TIPFILE                       2
005000070627     D  FLGEXE                        1
005100100512     D  TIPCLI                        1
005200071228     D  FLGNOE                        1
005300081016     D  KAAS                          4  0
005400081016     D  KLNP                          3  0
005500081016     D  KNRS                          2  0
005600081016     D  KNSP                          7  0
005700100512     D  DACMDA                        8  0
005800100512     D  DACMA                         8  0
005900001218     D*--------------------
006000100512
006100100512
006200100512     D*------------------
006300100512     D* VARIABILI D WRK
006400100512     D*------------------
006500100512     D  wAAS           S                   like(tasAAS) inz
006600100512     D  wLNP           S                   like(tasLNP) inz
006700100512     D  wNRS           S                   like(tasNRS) inz
006800100512     D  wNSP           S                   like(tasNSP) inz
006900100512
007000001218
007100100512     D*------------------
007200100512     D* LINKING A DEFINIZIONI ESTERNE
007300100512     D*------------------
007400100512     D/COPY GAITRASRC/SRCPROTOPR,UBLBLSPE
007500100512     D/COPY GAITRASRC/SRCPROTOPI,UBLBLSPE
007600001218
007700001218
007800920812     C*---------------------------------------------------------------*
007900001218     C* MAIN
008000001218     C*---------------------------------------------------------------*
008100100512     C*
008200100512     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
008300100512     C
008400100512     C/EXEC SQL
008500100512     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
008600100512     C/END-EXEC
008700100512     C
008800100512     C*
008900001218     C                   exsr      cartbl
009000001218     C                   exsr      chkpar
009100001218     C                   exsr      procedi
009200070627     C*
009300070627     C                   if        FLGOPZ = '2'
009400070627     C                   exsr      forzaDWN
009500070627     C                   endif
009600001218     C*
009700001218     C                   seton                                        LR
009800001218
009900001218
010000001218
010100001218     C*------------------------------------------------------------------------*
010200001218     C* CHKPAR - Routine di controllo parametri ricevuti in input
010300001218     C*------------------------------------------------------------------------*
010400001218     C     CHKPAR        BEGSR
010500001218     C*
010600001218     C* Verifico le date
010700100512     C                   if        DATADA > *zeros AND
010800100512     C                             DATAA  = *zeros
010900001218     C                   movel     *all'9'       DATAA
011000001218     C                   endif
011100100512     C                   if        DACMDA > *zeros AND
011200100512     C                             DACMA  = *zeros
011300100512     C                   movel     *all'9'       DACMA
011400100512     C                   endif
011500050823     C*
011600050823     C* Verifico la modalità d scrittura output richiesta:
011700050823     C*   1 = crea membro "RIC" in file FNVAC00T
011800050823     C*   2 = scrivi record in file TIVGD00F
011900050823     C                   setoff                                       2526
012000050823     C                   if        FLGOPZ = '1'
012100050823     C                   seton                                        25
012200050823     C                   endif
012300050823     C                   if        FLGOPZ = '2'
012400050823     C                   seton                                        26
012500050823     C                   endif
012600001218     C*
012700001218     C                   ENDSR
012800001218     C*------------------------------------------------------------------------*
012900001218
013000001218
013100001218
013200001218     C*------------------------------------------------------------------------*
013300001218     C* PROCEDI - Routine principale
013400001218     C*------------------------------------------------------------------------*
013500001218     C     PROCEDI       BEGSR
013600050909     C*
013700050909     C* Inizializzo variabili d wrk
013800050909     C                   movel     'N'           wProcedi          1
013900050909     C*
014000050909     C* Apro il file di output
014100050909     C                   if        *in25 = *on
014200050909     C                   open      fnvac00t
014300050909     C                   movel     'S'           wProcedi
014400050909     C                   endif
014500050909     C*
014600050909     C                   if        *in26 = *on
014700050909     C                   open      tivgd00f
014800050909     C*
014900050909     C* Come prima cosa avvio il blocco elaborazione TIVGD x tipo file corrente: 'VC'
015000050909     C                   clear                   trul47ds
015100050909     C                   eval      d47opz  = 'I'
015200120927     C                   eval      d47tip  = TIPFILE
015300050909     C                   eval      d47lck  = 'N'
015400050909     C                   eval      d47chkj = 'N'
015500050909     C                   eval      d47pgm  = 'TIS7VCR'
015600050909     C                   call      'TRUL47R'
015700050909     C                   parm                    trul47ds
015800050909     C*
015900050909     C* Se elaborazione consentita => proseguo
016000120928     C***                if        d47sts <> 'A'
016100050909     C                   movel     'S'           wProcedi
016200120928     C***                endif
016300050909     C                   endif
016400050909     C*
016500050909     C* Se ok a procedere => elaboro
016600050909     C                   if        wProcedi = 'S'
016700001218     C*
016800001218     C* Ciclo sulla schiera dei clienti figli dell'unificante preso in input
016900001218     C                   z-add     1             I
017000090407     C                   sorta     KUNI
017100001218     C                   dow       KUNI(I) > *zeros
017200070718     C*
017300070718     C* Se richiesta elaborazione x codice cliente mittente...
017400100512     C                   select
017500100512     C                   when      TIPCLI = *blanks
017600100512     C                   exsr      EXECCM
017700070718     C*
017800070718     C* Se richiesta elaborazione x codice cliente fatturazione...
017900100512     C                   when      TIPCLI = 'K'
018000100512     C                   exsr      EXEKSC
018100100512     C*
018200100512     C* Se richiesta elaborazione x entrambi: codice cliente mittente + fatturazione...
018300100512     C                   when      TIPCLI = 'E'
018400100512     C                   exsr      EXECCM
018500100512     C                   exsr      EXEKSC
018600100512     C*
018700100512     C                   endsl
018800001218     C*
018900001218     C                   add       1             I
019000001218     C                   enddo
019100050909     C*
019200050909     C                   endif
019300001218     C*
019400001218     C* Chiudo il file di output
019500050823     C   25              close     fnvac00t
019600050823     C   26              close     tivgd00f
019700050909     C*
019800050909     C* Infine elimino il blocco elaborazione TIVGD x tipo file corrente: 'VC'
019900050909     C                   clear                   trul47ds
020000050909     C                   eval      d47opz  = 'F'
020100120928     C                   eval      d47tip  = TIPFILE
020200050909     C                   call      'TRUL47R'
020300050909     C                   parm                    trul47ds
020400001218     C*
020500001218     C                   ENDSR
020600001218     C*------------------------------------------------------------------------*
020700100512
020800100512
020900100512
021000100512     C*------------------------------------------------------------------------*
021100100512     C* EXECCM   - Elaborazione x codice cliente mittente
021200100512     C*------------------------------------------------------------------------*
021300100512     C     EXECCM        BEGSR
021400100521     C*
021500100521     C                   movel     'C'           wChkCli           1
021600100512     C*
021700100512     C                   select
021800100512     C* ...se richiesta elaborazione x data spedizione
021900100512     C                   when      DATADA > *zeros
022000100512     C                   exsr      SQ_CCMDSP
022100100512     C* ...se richiesta elaborazione x data consegna merce
022200100512     C                   when      DACMDA > *zeros
022300100512     C                   exsr      SQ_CCMDCM
022400100512     C                   endsl
022500100512     C*
022600100512     C                   ENDSR
022700100512     C*------------------------------------------------------------------------*
022800100512
022900100512
023000100512
023100100512     C*------------------------------------------------------------------------*
023200100512     C* EXEKSC   - Elaborazione x codice cliente fatturazione
023300100512     C*------------------------------------------------------------------------*
023400100512     C     EXEKSC        BEGSR
023500100521     C*
023600100521     C                   movel     'K'           wChkCli           1
023700100512     C*
023800100512     C                   select
023900100512     C* ...se richiesta elaborazione x data spedizione
024000100512     C                   when      DATADA > *zeros
024100100512     C                   exsr      SQ_KSCDSP
024200100512     C* ...se richiesta elaborazione x data consegna merce
024300100512     C                   when      DACMDA > *zeros
024400100512     C                   exsr      SQ_KSCDCM
024500100512     C                   endsl
024600100512     C*
024700100512     C                   ENDSR
024800100512     C*------------------------------------------------------------------------*
024900100512
025000100512
025100100512
025200100512     C*------------------------------------------------------------------------*
025300100512     C* SQ_CCMDSP - Elaborazione x codice cliente mittente + data spedizione
025400100512     C*------------------------------------------------------------------------*
025500100512     C     SQ_CCMDSP     BEGSR
025600100512     C*
025700100512     C                   z-add     KUNI(I)       wParCCM           7 0
025800100512     C*
025900100512     C/EXEC SQL
026000100512     C+ declare C_CCMDSP cursor for
026100100512     C+ select * from titas00f
026200100512     C+ where tasccm = :wParCCM and
026300100512     C+ tasaas*10000+tasmgs between :DATADA and :DATAA
026400100512     C+ union
026500100512     C+ select * from titas10f
026600100512     C+ where tasccm = :wParCCM and
026700100512     C+ tasaas*10000+tasmgs between :DATADA and :DATAA
026800100512     C+ union
026900100512     C+ select * from titasP0f
027000100512     C+ where tasccm = :wParCCM and
027100100512     C+ tasaas*10000+tasmgs between :DATADA and :DATAA
027200100512     C+ for read only
027300100512     C/END-EXEC
027400100512     C
027500100512     C/EXEC SQL
027600100512     C+ open C_CCMDSP
027700100512     C/END-EXEC
027800100512     C
027900100512     C/EXEC SQL
028000100512     C+ Fetch C_CCMDSP into :TITAS00F
028100100512     C/END-EXEC
028200100512     C*
028300100512     C                   dow       sqlcod = *zeros
028400100512     C                   exsr      verifica
028500100512     C                   if        CHKREC = 'S'
028600100512     C                   exsr      scriviVAC
028700100512     C                   endif
028800100512     C
028900100512     C/EXEC SQL
029000100512     C+ Fetch C_CCMDSP into :TITAS00F
029100100512     C/END-EXEC
029200100512     C*
029300100512     C                   enddo
029400100512     C*
029500100512     C/EXEC SQL
029600100512     C+ close C_CCMDSP
029700100512     C/END-EXEC
029800100512     C*
029900100512     C*
030000100512     C                   ENDSR
030100100512     C*------------------------------------------------------------------------*
030200100512
030300100512
030400100512
030500100512     C*------------------------------------------------------------------------*
030600100512     C* SQ_CCMDCM - Elaborazione x codice cliente mittente + data consegna merce
030700100512     C*------------------------------------------------------------------------*
030800100512     C     SQ_CCMDCM     BEGSR
030900100512     C*
031000100512     C                   z-add     KUNI(I)       wParCCM           7 0
031100100512     C*
031200100512     C/EXEC SQL
031300100512     C+ declare C_CCMDCM cursor for
031400100512     C+ select * from titas00f
031500100512     C+ where tasccm = :wParCCM and
031600100512     C+ tasdcm between :DACMDA and :DACMA
031700100512     C+ union
031800100512     C+ select * from titas10f
031900100512     C+ where tasccm = :wParCCM and
032000100512     C+ tasdcm between :DACMDA and :DACMA
032100100512     C+ union
032200100512     C+ select * from titasP0f
032300100512     C+ where tasccm = :wParCCM and
032400100512     C+ tasdcm between :DACMDA and :DACMA
032500100512     C+ for read only
032600100512     C/END-EXEC
032700100512     C
032800100512     C/EXEC SQL
032900100512     C+ open C_CCMDCM
033000100512     C/END-EXEC
033100100512     C
033200100512     C/EXEC SQL
033300100512     C+ Fetch C_CCMDCM into :TITAS00F
033400100512     C/END-EXEC
033500100512     C*
033600100512     C                   dow       sqlcod = *zeros
033700100512     C                   exsr      verifica
033800100512     C                   if        CHKREC = 'S'
033900100512     C                   exsr      scriviVAC
034000100512     C                   endif
034100100512     C
034200100512     C/EXEC SQL
034300100512     C+ Fetch C_CCMDCM into :TITAS00F
034400100512     C/END-EXEC
034500100512     C*
034600100512     C                   enddo
034700100512     C*
034800100512     C/EXEC SQL
034900100512     C+ close C_CCMDCM
035000100512     C/END-EXEC
035100100512     C*
035200100512     C*
035300100512     C                   ENDSR
035400100512     C*------------------------------------------------------------------------*
035500100512
035600100512
035700100512
035800100512     C*------------------------------------------------------------------------*
035900100512     C* SQ_KSCDSP - Elaborazione x codice cliente fatturazione + data spedizione
036000100512     C*------------------------------------------------------------------------*
036100100512     C     SQ_KSCDSP     BEGSR
036200100512     C*
036300100512     C                   z-add     KUNI(I)       wParKSC           7 0
036400100512     C*
036500100512     C/EXEC SQL
036600100512     C+ declare C_KSCDSP cursor for
036700100512     C+ select * from titas00f
036800100512     C+ where tasksc = :wParKSC and
036900100512     C+ tasaas*10000+tasmgs between :DATADA and :DATAA
037000100512     C+ union
037100100512     C+ select * from titas10f
037200100512     C+ where tasksc = :wParKSC and
037300100512     C+ tasaas*10000+tasmgs between :DATADA and :DATAA
037400100512     C+ union
037500100512     C+ select * from titasP0f
037600100512     C+ where tasksc = :wParKSC and
037700100512     C+ tasaas*10000+tasmgs between :DATADA and :DATAA
037800100512     C+ for read only
037900100512     C/END-EXEC
038000100512     C
038100100512     C/EXEC SQL
038200100512     C+ open C_KSCDSP
038300100512     C/END-EXEC
038400100512     C
038500100512     C/EXEC SQL
038600100512     C+ Fetch C_KSCDSP into :TITAS00F
038700100512     C/END-EXEC
038800100512     C*
038900100512     C                   dow       sqlcod = *zeros
039000100512     C                   exsr      verifica
039100100512     C                   if        CHKREC = 'S'
039200100512     C                   exsr      scriviVAC
039300100512     C                   endif
039400100512     C
039500100512     C/EXEC SQL
039600100512     C+ Fetch C_KSCDSP into :TITAS00F
039700100512     C/END-EXEC
039800100512     C*
039900100512     C                   enddo
040000100512     C*
040100100512     C/EXEC SQL
040200100512     C+ close C_KSCDSP
040300100512     C/END-EXEC
040400100512     C*
040500100512     C*
040600100512     C                   ENDSR
040700100512     C*------------------------------------------------------------------------*
040800100512
040900100512
041000100512
041100100512     C*------------------------------------------------------------------------*
041200100512     C* SQ_KSCDCM - Elaborazione x codice cliente fatturazione + data consegna merce
041300100512     C*------------------------------------------------------------------------*
041400100512     C     SQ_KSCDCM     BEGSR
041500100512     C*
041600100512     C                   z-add     KUNI(I)       wParKSC           7 0
041700100512     C*
041800100512     C/EXEC SQL
041900100512     C+ declare C_KSCDCM cursor for
042000100512     C+ select * from titas00f
042100100512     C+ where tasksc = :wParKSC and
042200100512     C+ tasdcm between :DACMDA and :DACMA
042300100512     C+ union
042400100512     C+ select * from titas10f
042500100512     C+ where tasksc = :wParKSC and
042600100512     C+ tasdcm between :DACMDA and :DACMA
042700100512     C+ union
042800100512     C+ select * from titasP0f
042900100512     C+ where tasksc = :wParKSC and
043000100512     C+ tasdcm between :DACMDA and :DACMA
043100100512     C+ for read only
043200100512     C/END-EXEC
043300100512     C
043400100512     C/EXEC SQL
043500100512     C+ open C_KSCDCM
043600100512     C/END-EXEC
043700100512     C
043800100512     C/EXEC SQL
043900100512     C+ Fetch C_KSCDCM into :TITAS00F
044000100512     C/END-EXEC
044100100512     C*
044200100512     C                   dow       sqlcod = *zeros
044300100512     C                   exsr      verifica
044400100512     C                   if        CHKREC = 'S'
044500100512     C                   exsr      scriviVAC
044600100512     C                   endif
044700100512     C
044800100512     C/EXEC SQL
044900100512     C+ Fetch C_KSCDCM into :TITAS00F
045000100512     C/END-EXEC
045100100512     C*
045200100512     C                   enddo
045300100512     C*
045400100512     C/EXEC SQL
045500100512     C+ close C_KSCDCM
045600100512     C/END-EXEC
045700100512     C*
045800100512     C*
045900100512     C                   ENDSR
046000100512     C*------------------------------------------------------------------------*
046100100512
046200100512
046300100512
046400100512     C*------------------------------------------------------------------------*
046500100512     C* CHKCONS  - Verifica se bolla effettivamente consegnata
046600100512     C*------------------------------------------------------------------------*
046700100512     C     CHKCONS       BEGSR
046800100512     C*
046900100512     C                   setoff                                       4041
047000100512     C                   movel     *blanks       wEsito1           1
047100100512     C                   movel     *blanks       wEsito2           1
047200100512     C*
047300100512     C                   eval      wAAS = tasAAS
047400100512     C                   eval      wLNP = tasLNP
047500100512     C                   eval      wNRS = tasNRS
047600100512     C                   eval      wNSP = tasNSP
047700100512     C*
047800100512     C* Chiamata metodo GetLblTyp
047900100512     C                   eval      wEsito1 = %char(UBLBLSPE_GetLblTyp(
048000100512     C                                                wAAS
048100100512     C                                               :wLNP
048200100512     C                                               :wNRS
048300100512     C                                               :wNSP
048400100512     C                                               :pOutLblTyp
048500100512     C                                               :pOutAnnoBO
048600100512     C                                               :pOutLineaParBO
048700100512     C                                               :pOutSerieBO
048800100512     C                                               :pOutNumSpedBO
048900100512     C                                               :pOutDcmBO
049000100512     C                                               :pOutCcaBO
049100100512     C                                               :pOutRblBO))
049200100512     C* Se trattasi d bolla nn originale reperisco la relativa bolla originale
049300100512     C                   if        pOutLblTyp <> 'O'
049400100512     C                   eval      wAAS = pOutAnnoBO
049500100512     C                   eval      wLNP = pOutLineaParBO
049600100512     C                   eval      wNRS = pOutSerieBO
049700100512     C                   eval      wNSP = pOutNumSpedBO
049800100512     C                   endif
049900100512     C*
050000100512     C* Chiamata metodo GetLastChild
050100100512     C                   eval      wEsito2 = %char(UBLBLSPE_GetLastChild(
050200100512     C                                                wAAS
050300100512     C                                               :wLNP
050400100512     C                                               :wNRS
050500100512     C                                               :wNSP
050600100512     C                                               :pOutAnnoFI
050700100512     C                                               :pOutLineaParFI
050800100512     C                                               :pOutSerieFI
050900100512     C                                               :pOutNumSpedFI
051000100512     C                                               :pOutDcmFI
051100100512     C                                               :pOutCcaFI))
051200100512     C*
051300100512     C* Considerazioni sullo stato corrente della bolla
051400100512     C                   if        (wEsito2 = '9' and pOutDcmBO > *zeros)  or
051500100512     C                             (wEsito2 = '0' and pOutDcmFI > *zeros)
051600100512     C                   seton                                        40        * bolla cons.
051700100512     C                   endif
051800100512     C*
051900100512     C                   if        (wEsito2 = '9' and pOutDcmBO > *zeros
052000100512     C                                            and pOutCcaBO = *blanks) or
052100100512     C                             (wEsito2 = '0' and pOutDcmFI > *zeros
052200100512     C                                            and pOutCcaFI = *blanks)
052300100512     C                   seton                                        41        * bolla cons. OK
052400100512     C                   endif
052500100512     C*
052600100512     C                   ENDSR
052700100512     C*------------------------------------------------------------------------*
052800001218
052900001218
053000001218
053100001218     C*------------------------------------------------------------------------*
053200001218     C* VERIFICA - Routine di verifica validità record corrente
053300001218     C*------------------------------------------------------------------------*
053400001218     C     VERIFICA      BEGSR
053500001218     C*
053600001218     C                   movel     'S'           CHKREC            1
053700001218     C*
053800100512     C* Verifico le date spedizione
053900001218     C                   if        CHKREC = 'S'
054000100512     C                   if        DATADA > *zeros
054100001218     C                   movel     tasaas        wrkdata           8 0
054200001218     C                   move      tasmgs        wrkdata
054300001218     C                   if        wrkdata < DATADA or
054400001218     C                             wrkdata > DATAA
054500001218     C                   movel     'N'           CHKREC
054600100512     C                   endif
054700001218     C                   endif
054800001218     C                   endif
054900100512     C*
055000100512     C* Verifico le date spedizione
055100100512     C                   if        CHKREC = 'S'
055200100512     C                   if        DACMDA > *zeros
055300100512     C                   if        tasdcm  < DACMDA or
055400100512     C                             tasdcm  > DACMA
055500100512     C                   movel     'N'           CHKREC
055600100512     C                   endif
055700100512     C                   endif
055800100512     C                   endif
055900100521     C*
056000100521     C* Verifico il codice cliente mittente
056100100521     C                   if        CHKREC = 'S'
056200100521     C                   if        wChkCli  = 'C'      AND
056300100521     C                             tasccm  <> KUNI(I)
056400100521     C                   movel     'N'           CHKREC
056500100521     C                   endif
056600100521     C                   endif
056700070718     C*
056800100517     C* Verifico il codice cliente fatturazione (in caso d 'K')
056900070718     C                   if        CHKREC = 'S'
057000100521     C                   if        wChkCli  = 'K'      AND
057100100521     C                             tasksc  <> KUNI(I)
057200070718     C                   movel     'N'           CHKREC
057300070718     C                   endif
057400070718     C                   endif
057500100517     C*
057600100517     C* Verifico il codice cliente fatturazione (in caso d 'E')
057700100517     C                   if        CHKREC = 'S'
057800100521     C                   if        TIPCLI  = 'E'       AND
057900100521     C                             wChkCli = 'K'       AND
058000100521     C                             tasksc  = tasccm
058100100517     C                   movel     'N'           CHKREC
058200100517     C                   endif
058300100517     C                   endif
058400001218     C*
058500020923     C* Verifico che la bolla abbia la data consegna se trattasi di bolla PT
058600001218     C                   if        CHKREC = 'S'
058700020923     C                   if        tasTSP = 'P'
058800020923     C                   if        tasDCM > *zeros
058900001218     C                   else
059000001218     C                   movel     'N'           CHKREC
059100001218     C                   endif
059200020923     C                   endif
059300001218     C                   endif
059400020920     C*
059500040322     C* Verifico che il tipo bolla sia da considerare ai fini della rigenerazione dell'FNVAC
059600020920     C                   if        CHKREC = 'S'
059700040322     C     tasCBO        lookup    COD3A                                  55
059800081016     C                   if        %equal
059900040322     C                   else
060000020920     C                   movel     'N'           CHKREC
060100020920     C                   endif
060200020920     C                   endif
060300010301     C*
060400010301     C* Verifico che la bolla se POSTE non abbia il codice CA = 5
060500010301     C                   if        CHKREC = 'S'
060600010301     C                   if        tasTSP = 'P' and tasCCA = '5'
060700010301     C                   movel     'N'           CHKREC
060800010301     C                   endif
060900010301     C                   endif
061000100512     C*
061100040322     C* Verifico se richiesto nel lancio che la bolla sia consegnata
061200040322     C                   if        CHKREC = 'S'
061300040322     C                   if        FLGCONS = 'S'
061400100512     C                   exsr      CHKCONS
061500100512     C                   if        *in40 = *on
061600040322     C                   else
061700040322     C                   movel     'N'           CHKREC
061800040322     C                   endif
061900040322     C                   endif
062000040322     C                   endif
062100100512     C*
062200100512     C* Verifico se richiesto nel lancio che la bolla sia consegnata OK
062300100512     C                   if        CHKREC = 'S'
062400100512     C                   if        FLGCONS = 'O'
062500100512     C                   exsr      CHKCONS
062600100512     C                   if        *in41 = *on
062700100512     C                   else
062800100512     C                   movel     'N'           CHKREC
062900100512     C                   endif
063000100512     C                   endif
063100100512     C                   endif
063200100513     C*
063300100907     C* Verifico se richiesto nel lancio che la bolla NON sia consegnata OK
063400100513     C                   if        CHKREC = 'S'
063500100513     C                   if        FLGCONS = 'K'
063600100513     C                   exsr      CHKCONS
063700100513     C                   if        *in41 = *on
063800100513     C                   movel     'N'           CHKREC
063900100513     C                   endif
064000100513     C                   endif
064100100513     C                   endif
064200100907     C*
064300100907     C* Verifico se richiesto nel lancio che la bolla NON sia consegnata
064400100907     C                   if        CHKREC = 'S'
064500100907     C                   if        FLGCONS = 'P'
064600100907     C                   exsr      CHKCONS
064700100907     C                   if        *in40 = *on
064800100907     C                   movel     'N'           CHKREC
064900100907     C                   endif
065000100907     C                   endif
065100100907     C                   endif
065200040322     C*
065300040322     C* Verifico se richiesto nel lancio che la bolla provenga da scambio dati VAS cliente
065400040322     C                   if        CHKREC = 'S'
065500040322     C                   if        FLGVAS = 'S'
065600040322     C                   if        tasSOP = '§' OR
065700040322     C                             tasSOP = ':' OR
065800040322     C                             tasSOP = '%'
065900040322     C                   else
066000040322     C                   movel     'N'           CHKREC
066100040322     C                   endif
066200040322     C                   endif
066300040322     C                   endif
066400081016     C*
066500081016     C* Verifico se richiesto che la bolla corrente sia la singola bolla richiesta a video
066600081016     C                   if        CHKREC = 'S'
066700081016     C                   if        KAAS   <> *zeros OR
066800081016     C                             KLNP   <> *zeros OR
066900081016     C                             KNRS   <> *zeros OR
067000081016     C                             KNSP   <> *zeros
067100081016     C                   if        tasAAS = KAAS AND
067200081016     C                             tasLNP = KLNP AND
067300081016     C                             tasNRS = KNRS AND
067400081016     C                             tasNSP = KNSP
067500081016     C                   else
067600081016     C                   movel     'N'           CHKREC
067700081016     C                   endif
067800081016     C                   endif
067900081016     C                   endif
068000001218     C*
068100001218     C                   ENDSR
068200001218     C*------------------------------------------------------------------------*
068300001218
068400001218
068500001218
068600001218     C*------------------------------------------------------------------------*
068700001218     C* SCRIVIVAC - Routine di scrittura file esiti di consegna (FNVAC)
068800001218     C*------------------------------------------------------------------------*
068900001218     C     SCRIVIVAC     BEGSR
069000001218     C*
069100001218     C                   CLEAR                   fnvac000
069200001218     C*
069300001218     C                   MOVEL     tasAAS        VACAAS
069400001218     C                   MOVEL     tasLNP        VACLNP
069500001218     C                   MOVEL     tasNRS        VACNRS
069600001218     C                   MOVEL     tasNSP        VACNSP
069700001218     C                   MOVEL     tasMGS        VACMGS
069800001218     C                   MOVEL     tasCBO        VACCBO
069900001218     C                   MOVEL     tasLNA        VACLNA
070000010223     C                   MOVEL     tasRSD        VACRSD
070100010223     C                   MOVEL     tasPRD        VACPRD
070200010301     C*
070300001218     C* Reperisco dall'estensione anagrafica bolle di SEDE i dati del Destinatario
070400001218     C                   EXSR      REPDEST
070500001218     C*
070600001218     C                   MOVEL     tasGC1        VACGC1
070700001218     C                   MOVEL     tasGC2        VACGC2
070800001218     C                   MOVEL     tasCTR        VACCTR
070900001218     C                   MOVEL     tasCTS        VACCTS
071000001218     C                   MOVEL     tasFTM        VACFTM
071100001218     C                   MOVEL     tasFIN        VACFIN
071200001218     C                   MOVEL     tasFAP        VACFAP
071300001218     C                   MOVEL     tasTSP        VACTSP
071400001218     C* Effettuo considerazioni particolare per bolla "POSTE"
071500001218     C     tasTSP        IFEQ      'P'                                          BOLLA POSTE
071600001218     C                   EXSR      REPPTIAS
071700001218     C                   EXSR      REPPTGIA
071800001218     C                   Z-ADD     tasNCD        VACDCR
071900001218     C                   ELSE
072000001218     C                   MOVEL     tasIAS        VACIAS
072100001218     C                   MOVEL     tasVAS        VACVAS
072200001218     C                   MOVEL     tasDCR        VACDCR
072300050310     C     KGCP          CHAIN     TIGCP51L
072400050310     C                   IF        %found(TIGCP51L)
072500001218     C                   MOVEL     GCPAGC        VACDAG
072600001218     C                   MOVE      GCPMGC        VACDAG
072700001218     C                   ENDIF
072800001218     C                   ENDIF
072900001218     C*
073000001218     C                   MOVEL     tasNAS        VACNAS
073100001218     C                   MOVEL     tasNCL        VACNCL
073200001218     C                   MOVEL     tasPKF        VACPKB
073300001218     C                   MOVEL     tasVLF        VACVLB
073400001218     C                   MOVEL     tasQFT        VACQFT
073500001218     C*
073600001218     C                   EXSR      REPCSB
073700001218     C*
073800001218     C                   MOVEL     tasCCM        VACCCM
073900001218     C                   MOVEL     tasRMN        VACRMN
074000010301     C*
074100001218     C                   EXSR      REPRMA
074200010301     C*
074300001218     C                   MOVEL     tasFFD        VACFFD
074400001218     C                   MOVEL     tasTCR        VACTCR
074500001218     C                   MOVEL     tasHCR        VACHCR
074600001218     C                   MOVEL     tasFTC        VACTC1
074700001218     C                   MOVEL     tasTC2        VACTC2
074800001218     C                   MOVEL     tasDCM        VACDCM
074900001218     C                   MOVEL     tasHMC        VACHMC
075000001218     C*
075100010301     C                   IF        tasTSP = 'P' and tasCCA = '7'
075200010301     C                   MOVEL     *blanks       VACCCA
075300010301     C                   ELSE
075400100910     C                   MOVEL     tasCCA        VACCCA
075500100910     C*
075600100910     C* Se CCA ancora nn valorizzato => reperisco l'ultimo evento
075700100910     C                   IF        VACCCA = *blanks
075800100910     C                   EXSR      REPLASTEV
075900010301     C                   ENDIF
076000100910     C                   ENDIF
076100010301     C*
076200120427     C* Reperisco l'eventuale lasciato avviso
076300001218     C                   EXSR      REPLAV
076400120427     C*
076500120427     C* Effettuo particolari operazioni in caso d DIROTTAMENTO
076600120427     C***                IF        VACCCA = '1' AND
076700120427     C***                          VACDCM = *zeros
076800120427     C***                EXSR      REPDIR
076900120427     C***                ENDIF
077000001218     C*
077100020923     C* Verifico che in caso di bolla NON PT siano valorizzate o la data consegna...
077200020923     C* ... o la data lasciato avviso o la data giacenza o la consegna anomala
077300020923     C                   IF        tasTSP <> 'P'
077400071228     C*
077500071228     C* Se richiesto nel lancio anche bolle senza eventi => gestisco...
077600071228     C* ...altrimenti standard
077700071228     C                   IF        FLGNOE  = 'N'
077800020923     C                   IF        vacDCM  > *zeros OR
077900020923     C                             vacDLA  > *zeros OR
078000020923     C                             vacDAG  > *zeros OR
078100020923     C                             vacCCA <> *blanks
078200071228     C   25              WRITE     FNVAC000
078300071228     C   26              exsr      WRITIVGD
078400020923     C                   endif
078500071228     C                   else
078600071228     C   25              WRITE     FNVAC000
078700071228     C   26              exsr      WRITIVGD
078800071228     C                   endif
078900020923     C                   else
079000050823     C   25              WRITE     FNVAC000
079100050823     C   26              exsr      WRITIVGD
079200020923     C                   endif
079300001218     C*
079400001218     C                   ENDSR
079500120427
079600120427
079700120427
079800120427     C*------------------------------------------------------------------------*
079900120427     C* REPDIR - Reperimento data DIROTTAMENTO
080000120427     C*------------------------------------------------------------------------*
080100120427     C     REPDIR        BEGSR
080200120427     C*
080300120427     C                   movel     *blanks       wEsito1           1
080400120427     C*
080500120427     C* Chiamata metodo GetLblTyp
080600120427     C                   eval      wEsito1 = %char(UBLBLSPE_GetLblTyp(
080700120427     C                                                tasAAS
080800120427     C                                               :tasLNP
080900120427     C                                               :tasNRS
081000120427     C                                               :tasNSP
081100120427     C                                               :pOutLblTyp
081200120427     C                                               :pOutAnnoBO
081300120427     C                                               :pOutLineaParBO
081400120427     C                                               :pOutSerieBO
081500120427     C                                               :pOutNumSpedBO
081600120427     C                                               :pOutDcmBO
081700120427     C                                               :pOutCcaBO
081800120427     C                                               :pOutRblBO))
081900120427     C*
082000120427     C* Se reperimento effettuato e trattasi d dirottamento => recupero la data
082100120427     C                   if        wEsito1 = '0' and pOutCcaBO = '1'
082200120427     C                   eval      VACDCM = pOutDcmBO
082300120427     C                   endif
082400120427     C*
082500120427     C                   ENDSR
082600120427     C*------------------------------------------------------------------------*
082700100910
082800100910
082900100910
083000100910
083100100910     C*------------------------------------------------------------------------*
083200100910     C* REPLASTEV - Routine di reperimento ultimo evento
083300100910     C*------------------------------------------------------------------------*
083400100910     C     REPLASTEV     BEGSR
083500100910     C*
083600100910     C     KEVB          SETGT     FNEVB01L
083700100910     C     KEVB          READPE    FNEVB01L                               34
083800100910     C*
083900100910     C* Devo cercare l'ultimo evento di tipo giacenza
084000100910     C                   IF        *IN34 = *OFF
084100100910     C*
084200100910     C* Evento 'MIC' considero solo se data evento >= data corrente
084300100910     C                   IF        EVBDEV >= datcor
084400100910     C* - MIC
084500100910     C                   IF        EVBCEV = 'MIC'
084600100910     C                   MOVEL     'C'           VACCCA
084700100910     C                   MOVEL     evbDEV        VACDCM
084800100910     C                   ENDIF
084900100910     C                   ENDIF
085000100910     C*
085100100910     C                   ENDIF
085200100910     C*
085300100910     C                   ENDSR
085400100910     C*------------------------------------------------------------------------*
085500050823
085600050823
085700050823
085800050823
085900050823     C*------------------------------------------------------------------------*
086000050823     C* WRITIVGD - Routine di scrittura file TIVGD (file VAS generico download)
086100050823     C*------------------------------------------------------------------------*
086200050823     C     WRITIVGD      BEGSR
086300050823     C*
086400050823     C                   clear                   tivgd000
086500080407     C                   eval      vgdDTA = %subst(FNVACDS:1:%size(FNVACDS))
086600100312     C                   eval      vgdTIP = TIPFILE
086700050823     C                   movel     *all'0'       vgdKSU
086800070525     C                   move      CODCLIVAS     vgdKSU
086900050823     C                   eval      vgdTSC = 'WW'
087000050823     C                   eval      vgdDAT = datcor
087100050901     C                   eval      vgdPGM = 'TIS7VCR'
087200070627     C*
087300070627     C                   clear                   DVGDFLO
087400070627     C                   movel     'P'           §VGDFELA
087500070627     C                   movel     DVGDFLO       vgdflo
087600070627     C*
087700050823     C                   write     tivgd000
087800050823     C*
087900050823     C                   ENDSR
088000050823     C*------------------------------------------------------------------------*
088100070627
088200070627
088300070627
088400070627     C*------------------------------------------------------------------------*
088500070627     C* FORZADWN - Routine di forzatura elaborazione immediata download
088600070627     C*------------------------------------------------------------------------*
088700070627     C     FORZADWN      BEGSR
088800070627     C*
088900070627     C                   clear                   tis781ds
089000070627     C                   eval      §781tip = vgdTIP
089100070627     C                   eval      §781ksu = vgdKSU
089200070627     C                   eval      §781tsc = vgdTSC
089300070627     C                   eval      §781dat = vgdDAT
089400070627     C                   eval      §781pgm = vgdPGM
089500080905     C*
089600080905     C                   clear                   TIS781DFLO
089700080905     C                   movel     'P'           §781floela
089800080905     C                   movel     TIS781DFLO    §781flo
089900070627     C*
090000070627     C* Finita l'elaborazione chiamo pgm
090100070627     C                   CALL      'TIS781C1'
090200070627     C                   parm      *blanks       esito             1
090300070627     C                   parm                    tis781ds
090400070627     C                   parm      *blanks       vgdPRG           10
090500070627     C*
090600070627     C                   ENDSR
090700070627     C*------------------------------------------------------------------------*
090800001218
090900001218
091000001218
091100001218
091200001218     C*------------------------------------------------------------------------*
091300001218     C* REPPTIAS - Routine di reperimento dati relativi all'importo/divisa da assicurare x POSTE
091400001218     C*------------------------------------------------------------------------*
091500001218     C     REPPTIAS      BEGSR
091600001218     C*
091700060215     C* Prendo importo da assicurare da TITA4 t.r.=J
091800001218     C                   movel     'J'           KTRC
091900001218     C     KTA4          chain     tita430c
092000001218     C                   if        %found(tita430c)
092100001218     C                   movel     ta4NOT        DSBL4J
092200001218     C                   z-add     §B4IAS        vacIAS
092300001218     C                   movel     §B4VAS        vacVAS
092400001218     C                   endif
092500001218     C*
092600001218     C                   ENDSR
092700001218     C*------------------------------------------------------------------------*
092800001218
092900001218
093000001218
093100001218
093200001218     C*------------------------------------------------------------------------*
093300001218     C* REPRMA - Routine di reperimento dati relativi al riferimento mittente alfabetico
093400001218     C*------------------------------------------------------------------------*
093500001218     C     REPRMA        BEGSR
093600001218     C*
093700060215     C* Prendo importo da assicurare da TITA4 t.r.=A
093800001218     C                   movel     'A'           KTRC
093900001218     C     KTA4          chain     tita430c
094000001218     C                   if        %found(tita430c)
094100001218     C                   eval      vacRMA = %subst(ta4NOT:1:15)
094200001218     C                   endif
094300001218     C*
094400001218     C                   ENDSR
094500001218     C*------------------------------------------------------------------------*
094600001218
094700001218
094800001218
094900001218
095000001218     C*------------------------------------------------------------------------*
095100001218     C* REPDEST - Routine di reperimento dati relativi al destinatario della bolla (TITAA)
095200001218     C*           ... e ai dati del mittente originale
095300001218     C*------------------------------------------------------------------------*
095400001218     C     REPDEST       BEGSR
095500001218     C*
095600010223     C* Prendo il tipo anagrafica = D
095700010223     C*                  movel     'D'           KTRC
095800010223     C*    KTAA          chain     titaa30c
095900010223     C*                  if        %found(titaa30c)
096000010223     C*                  movel     taaRSC        vacRSD
096100010223     C*                  movel     taaPRV        vacPRD
096200010223     C*                  endif
096300001218     C*
096400010223     C* Prendo il tipo anagrafica = O
096500001218     C                   movel     'O'           KTRC
096600001218     C     KTAA          chain     titaa30c
096700001218     C                   if        %found(titaa30c)
096800001218     C                   movel     taaRSC        vacRMO
096900001218     C                   endif
097000001218     C*
097100001218     C                   ENDSR
097200001218     C*------------------------------------------------------------------------*
097300001218
097400001218
097500001218
097600001218
097700001218     C*------------------------------------------------------------------------*
097800001218     C* REPCSB - Routine di reperimento dati relativi al contrassegno
097900001218     C*------------------------------------------------------------------------*
098000001218     C     REPCSB        BEGSR
098100001218     C*
098200001219     C     KCSB          chain     tncsb03l
098300001219     C                   if        %found(tncsb03l)
098400001218     C                   if        csbSTA = 9 and tasTSP <> 'P'
098500001218     C                   else
098600001218     C                   movel     csbTPA        vactic
098700001218     C                   move      csbTPI        vactic
098800001218     C                   z-add     csbCAS        vacCAS
098900001218     C                   movel     csbVCA        vacVCA
099000001218     C                   endif
099100001218     C                   endif
099200001218     C*
099300001218     C                   ENDSR
099400001218     C*------------------------------------------------------------------------*
099500001218
099600001218
099700001218
099800001218
099900001218     C*------------------------------------------------------------------------*
100000001218     C* REPPTGIA - Routine di reperimento dati relativi alla giacenza x POSTE
100100001218     C*------------------------------------------------------------------------*
100200001218     C     REPPTGIA      BEGSR
100300001218     C*
100400001218     C* Reperisco la data di giacenza dagli eventi poichè non vengono
100500001218     C* gestite le giacenze per le bolle poste
100600001218     C     KEVB          SETGT     FNEVB01L
100700001218     C     KEVB          READPE    FNEVB01L                               34
100800001218     C*
100900001218     C* Devo cercare l'ultimo evento di tipo giacenza
101000001218     C     *IN34         DOWEQ     *OFF
101100001218     C                   Z-ADD     1             W
101200001218     C     EVBCEV        LOOKUP    C2A(W)                                 42
101300001218     C     *IN42         IFEQ      '1'
101400001218     C     T2A(W)        ANDEQ     'G'
101500001218     C                   Z-ADD     EVBDEV        vacDAG
101600001218     C                   LEAVE
101700001218     C                   ELSE
101800001218     C     KEVB          READPE    FNEVB000                               34
101900001218     C                   ENDIF
102000001218     C                   ENDDO
102100001218     C*
102200001218     C                   ENDSR
102300001218     C*------------------------------------------------------------------------*
102400001218
102500001218
102600001218
102700001218
102800001218     C*------------------------------------------------------------------------*
102900001218     C* REPLAV - Routine di reperimento dati relativi al lasciato avviso
103000001218     C*------------------------------------------------------------------------*
103100001218     C     REPLAV        BEGSR
103200001218     C*
103300001218     C* Pererisco l'ultimo evento Lasciato Avviso
103400001218     C                   MOVEL     '2A'          COD
103500001218     C     KEVB          SETGT     FNEVB01L
103600001218     C     KEVB          READPE    FNEVB01L                               34
103700001218     C*
103800001218     C     *IN34         DOWEQ     *OFF
103900001218     C                   MOVEL(P)  EVBCEV        KEY
104000001218     C     KTAB2         CHAIN     TABEL                              30
104100001218     C     *IN30         IFEQ      *OFF
104200001218     C                   MOVEL     TBLUNI        DS2A
104300001218     C     §2AFTC        IFEQ      'A'
104400001218     C                   Z-ADD     EVBDEV        vacDLA
104500001218     C                   LEAVE
104600001218     C                   ENDIF
104700001218     C                   ENDIF
104800001218     C     KEVB          READPE    FNEVB000                               34
104900001218     C                   ENDDO
105000001218     C*
105100001218     C                   ENDSR
105200001218     C*------------------------------------------------------------------------*
105300001218
105400001218
105500001218
105600001218
105700001218     C*------------------------------------------------------------------------*
105800001218     C* CARTBL - Routine di caricamento dati tabellati
105900001218     C*------------------------------------------------------------------------*
106000001218     C     CARTBL        BEGSR
106100001218     C*
106200001218     C                   Z-ADD     0             W                 4 0
106300001218     C                   MOVEA     *HIVAL        T2A
106400001218     C                   MOVEA     *HIVAL        C2A
106500001218     C                   MOVEL     '2A'          COD
106600001218     C     KTAB          CHAIN     TABEL                              31
106700001218     C     *IN31         DOWEQ     '0'
106800001218     C     TBLFLG        IFEQ      ' '
106900001218     C                   MOVEL     TBLUNI        DS2A
107000001218     C     §2AFTC        IFNE      *BLANKS
107100001218     C                   ADD       1             W
107200001218     C                   MOVEL     §2AFTC        T2A(W)
107300001218     C                   MOVEL     TBLKEY        C2A(W)
107400001218     C                   ENDIF
107500001218     C                   ENDIF
107600001218     C     KTAB          READE     TABEL                                  31
107700001218     C                   ENDDO
107800001218     C*
107900081016     C                   Z-ADD     1             I                 4 0
108000011017     C                   MOVEL     CODCLI        KUNI(I)
108100081016     C                   IF        FLGUNI = 'U'
108200001218     C                   MOVEL     '3K'          COD
108300001218     C     KTAB          CHAIN     TABEL                              31
108400001218     C     *IN31         DOWEQ     '0'
108500001218     C     TBLFLG        IFEQ      ' '
108600001218     C                   MOVEL     TBLUNI        DS3K
108700010301     C     §3KCKS        IFEQ      CODCLI
108800001218     C                   ADD       1             I
108900081016     C                   MOVEL     TBLKEY        wKUNI             7 0
109000081016     C     wKUNI         LOOKUP    KUNI                                   55
109100081016     C                   IF        %equal
109200081016     C                   ELSE
109300081016     C                   Z-ADD     wKUNI         KUNI(I)
109400081016     C                   ENDIF
109500001218     C                   ENDIF
109600001218     C                   ENDIF
109700001218     C     KTAB          READE     TABEL                                  31
109800001218     C                   ENDDO
109900010301     C                   ENDIF
110000040312     C*
110100040312     C                   Z-ADD     0             J                 4 0
110200040312     C                   MOVEL     '3A'          COD
110300040312     C     KTAB          CHAIN     TABEL                              31
110400040312     C     *IN31         DOWEQ     '0'
110500040312     C     TBLFLG        IFEQ      ' '
110600040312     C                   MOVEL     TBLUNI        DS3A
110700040312     C     §3AVAC        IFEQ      'N'
110800040312     C                   ELSE
110900040312     C                   ADD       1             J
111000040312     C                   MOVEL     TBLKEY        COD3A(J)
111100040312     C                   ENDIF
111200040312     C                   ENDIF
111300040312     C     KTAB          READE     TABEL                                  31
111400040312     C                   ENDDO
111500001218     C*
111600001218     C                   ENDSR
111700001218     C*---------------------------------------------------------------*
111800001218
111900001218
112000001218
112100001218
112200001218     C*------------------------------------------------------------------------*
112300001218     C* *INZSR - ROUTINE INIZIALE
112400001218     C*------------------------------------------------------------------------*
112500001218     C     *INZSR        BEGSR
112600001218     C*
112700001218     C     *ENTRY        PLIST
112800100512     C                   PARM                    PARAM            71
112900001218     C*
113000001218     C                   MOVEL     PARAM         DSINPUT
113100001218     C                   Z-ADD     1             CODUT
113200001218     C*
113300001218     C* Definizioni chiavi
113400001218     C     KTA4          KLIST
113500001218     C                   KFLD                    tasAAS
113600001218     C                   KFLD                    tasLNP
113700001218     C                   KFLD                    tasNRS
113800001218     C                   KFLD                    tasNSP
113900001218     C                   KFLD                    KTRC              1
114000001218     C*
114100001218     C     KTAA          KLIST
114200001218     C                   KFLD                    tasAAS
114300001218     C                   KFLD                    tasLNP
114400001218     C                   KFLD                    tasNRS
114500001218     C                   KFLD                    tasNSP
114600001218     C                   KFLD                    KTRC
114700001218     C*
114800001218     C     KCSB          KLIST
114900001218     C                   KFLD                    tasAAS
115000001218     C                   KFLD                    tasLNP
115100001218     C                   KFLD                    tasNRS
115200001218     C                   KFLD                    tasNSP
115300001218     C*
115400001218     C     KEVB          KLIST
115500001218     C                   KFLD                    tasAAS
115600001218     C                   KFLD                    tasLNP
115700001218     C                   KFLD                    tasNRS
115800001218     C                   KFLD                    tasNSP
115900001218     C*
116000001218     C     KGCP          KLIST
116100001218     C                   KFLD                    tasAAS
116200001218     C                   KFLD                    tasLNP
116300001218     C                   KFLD                    tasNRS
116400001218     C                   KFLD                    tasNSP
116500001218     C*
116600001218     C     KTAB          KLIST
116700001218     C                   KFLD                    CODUT             1 0
116800001218     C                   KFLD                    COD               2
116900001218     C*
117000001218     C     KTAB2         KLIST
117100001218     C                   KFLD                    CODUT             1 0
117200001218     C                   KFLD                    COD               2
117300001218     C                   KFLD                    KEY               8
117400001218     C*
117500001218     C* Determino la data corrente
117600100312     C                   Z-ADD     *zeros        DATCOR            8 0
117700100312     C                   EVAL      DATCOR = %dec(%date() : *ISO)
117800001218     C*
117900001218     C                   ENDSR

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
001100151019     FFIAR531C  IF   E           K DISK
001200001222     FFNVAC00T  UF A E             DISK    USROPN
001300050823     FTIVGD00F  UF A E             DISK    USROPN
001400971126     D*--------------------------------------------------------------*
001500001218     D* SCHIERA PER CARICAMENTO CODICI E TIPI MANCATE CONSEGNE
001600001218     D*--------------------
001700001218     D C2A             S              3    DIM(300)                             cd.manc.cons.
001800001218     D T2A             S              1    DIM(300)                             tp.manc.cons.
001900040312     D COD3A           S              2    DIM(300)                             codici bolle x VAC
002000001218     D*--------------------
002100001218     D* SCHIERA PER REPERIMENTO CLIENTI "FIGLI" DI UN UNIFICANTE
002200001218     D*--------------------
002300090407     D KUNI            S              7  0 DIM(1000) DESCEND
002400001218     D*--------------------
002500001218     D* DS ESTERNE
002600001218     D*--------------------
002700100512     D TITAS00F      E DS
002800900517     D KPJBA         E DS
002900921023     D DS3K          E DS
003000941228     D DS2A          E DS
003100040312     D DS3A          E DS
003200000526     D DSBL4J        E DS
003300151019     D DAR5FAT       E DS
003400050823     D FNVACDS       E DS                  EXTNAME(FNVAC00T)
003500050909     D trul47ds      e ds
003600070627     D TIS781DS      E DS
003700080905     D TIS781DFLO    E DS
003800070627     D DVGDFLO       E DS
003900001218     D*--------------------
004000001218     D* DS DI RIDEFINIZIOINE PARAMETRI IN INPUT
004100001218     D*--------------------
004200050823     D DSINPUT         DS
004300050823     D  DATADA                        8  0
004400050823     D  DATAA                         8  0
004500050823     D  CODCLI                        7  0
004600050823     D  FLGUNI                        1
004700100512     D  FLGCONS                       1
004800050823     D  FLGVAS                        1
004900050823     D  FLGOPZ                        1
005000150707     D  CODCLIVAS                     7
005100100312     D  TIPFILE                       2
005200070627     D  FLGEXE                        1
005300100512     D  TIPCLI                        1
005400071228     D  FLGNOE                        1
005500081016     D  KAAS                          4  0
005600081016     D  KLNP                          3  0
005700081016     D  KNRS                          2  0
005800081016     D  KNSP                          7  0
005900100512     D  DACMDA                        8  0
006000100512     D  DACMA                         8  0
006100001218     D*--------------------
006200100512
006300100512
006400100512     D*------------------
006500100512     D* VARIABILI D WRK
006600100512     D*------------------
006700100512     D  wAAS           S                   like(tasAAS) inz
006800100512     D  wLNP           S                   like(tasLNP) inz
006900100512     D  wNRS           S                   like(tasNRS) inz
007000100512     D  wNSP           S                   like(tasNSP) inz
007100100512
007200001218
007300100512     D*------------------
007400100512     D* LINKING A DEFINIZIONI ESTERNE
007500100512     D*------------------
007600100512     D/COPY GAITRASRC/SRCPROTOPR,UBLBLSPE
007700100512     D/COPY GAITRASRC/SRCPROTOPI,UBLBLSPE
007800001218
007900001218
008000920812     C*---------------------------------------------------------------*
008100001218     C* MAIN
008200001218     C*---------------------------------------------------------------*
008300100512     C*
008400100512     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
008500100512     C
008600100512     C/EXEC SQL
008700100512     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
008800100512     C/END-EXEC
008900100512     C
009000100512     C*
009100001218     C                   exsr      cartbl
009200001218     C                   exsr      chkpar
009300001218     C                   exsr      procedi
009400070627     C*
009500070627     C                   if        FLGOPZ = '2'
009600070627     C                   exsr      forzaDWN
009700070627     C                   endif
009800001218     C*
009900001218     C                   seton                                        LR
010000001218
010100001218
010200001218
010300001218     C*------------------------------------------------------------------------*
010400001218     C* CHKPAR - Routine di controllo parametri ricevuti in input
010500001218     C*------------------------------------------------------------------------*
010600001218     C     CHKPAR        BEGSR
010700001218     C*
010800001218     C* Verifico le date
010900100512     C                   if        DATADA > *zeros AND
011000100512     C                             DATAA  = *zeros
011100001218     C                   movel     *all'9'       DATAA
011200001218     C                   endif
011300100512     C                   if        DACMDA > *zeros AND
011400100512     C                             DACMA  = *zeros
011500100512     C                   movel     *all'9'       DACMA
011600100512     C                   endif
011700050823     C*
011800050823     C* Verifico la modalità d scrittura output richiesta:
011900050823     C*   1 = crea membro "RIC" in file FNVAC00T
012000050823     C*   2 = scrivi record in file TIVGD00F
012100050823     C                   setoff                                       2526
012200050823     C                   if        FLGOPZ = '1'
012300050823     C                   seton                                        25
012400050823     C                   endif
012500050823     C                   if        FLGOPZ = '2'
012600050823     C                   seton                                        26
012700050823     C                   endif
012800001218     C*
012900001218     C                   ENDSR
013000001218     C*------------------------------------------------------------------------*
013100001218
013200001218
013300001218
013400001218     C*------------------------------------------------------------------------*
013500001218     C* PROCEDI - Routine principale
013600001218     C*------------------------------------------------------------------------*
013700001218     C     PROCEDI       BEGSR
013800050909     C*
013900050909     C* Inizializzo variabili d wrk
014000050909     C                   movel     'N'           wProcedi          1
014100050909     C*
014200050909     C* Apro il file di output
014300050909     C                   if        *in25 = *on
014400050909     C                   open      fnvac00t
014500050909     C                   movel     'S'           wProcedi
014600050909     C                   endif
014700050909     C*
014800050909     C                   if        *in26 = *on
014900050909     C                   open      tivgd00f
015000050909     C*
015100050909     C* Come prima cosa avvio il blocco elaborazione TIVGD x tipo file corrente: 'VC'
015200050909     C                   clear                   trul47ds
015300050909     C                   eval      d47opz  = 'I'
015400120927     C                   eval      d47tip  = TIPFILE
015500050909     C                   eval      d47lck  = 'N'
015600050909     C                   eval      d47chkj = 'N'
015700050909     C                   eval      d47pgm  = 'TIS7VCR'
015800050909     C                   call      'TRUL47R'
015900050909     C                   parm                    trul47ds
016000050909     C*
016100050909     C* Se elaborazione consentita => proseguo
016200120928     C***                if        d47sts <> 'A'
016300050909     C                   movel     'S'           wProcedi
016400120928     C***                endif
016500050909     C                   endif
016600050909     C*
016700050909     C* Se ok a procedere => elaboro
016800050909     C                   if        wProcedi = 'S'
016900001218     C*
017000001218     C* Ciclo sulla schiera dei clienti figli dell'unificante preso in input
017100001218     C                   z-add     1             I
017200090407     C                   sorta     KUNI
017300001218     C                   dow       KUNI(I) > *zeros
017400070718     C*
017500070718     C* Se richiesta elaborazione x codice cliente mittente...
017600100512     C                   select
017700100512     C                   when      TIPCLI = *blanks
017800100512     C                   exsr      EXECCM
017900070718     C*
018000070718     C* Se richiesta elaborazione x codice cliente fatturazione...
018100100512     C                   when      TIPCLI = 'K'
018200100512     C                   exsr      EXEKSC
018300100512     C*
018400100512     C* Se richiesta elaborazione x entrambi: codice cliente mittente + fatturazione...
018500100512     C                   when      TIPCLI = 'E'
018600100512     C                   exsr      EXECCM
018700100512     C                   exsr      EXEKSC
018800100512     C*
018900100512     C                   endsl
019000001218     C*
019100001218     C                   add       1             I
019200001218     C                   enddo
019300050909     C*
019400050909     C                   endif
019500001218     C*
019600001218     C* Chiudo il file di output
019700050823     C   25              close     fnvac00t
019800050823     C   26              close     tivgd00f
019900050909     C*
020000050909     C* Infine elimino il blocco elaborazione TIVGD x tipo file corrente: 'VC'
020100050909     C                   clear                   trul47ds
020200050909     C                   eval      d47opz  = 'F'
020300120928     C                   eval      d47tip  = TIPFILE
020400050909     C                   call      'TRUL47R'
020500050909     C                   parm                    trul47ds
020600001218     C*
020700001218     C                   ENDSR
020800001218     C*------------------------------------------------------------------------*
020900100512
021000100512
021100100512
021200100512     C*------------------------------------------------------------------------*
021300100512     C* EXECCM   - Elaborazione x codice cliente mittente
021400100512     C*------------------------------------------------------------------------*
021500100512     C     EXECCM        BEGSR
021600100521     C*
021700100521     C                   movel     'C'           wChkCli           1
021800100512     C*
021900100512     C                   select
022000100512     C* ...se richiesta elaborazione x data spedizione
022100100512     C                   when      DATADA > *zeros
022200100512     C                   exsr      SQ_CCMDSP
022300100512     C* ...se richiesta elaborazione x data consegna merce
022400100512     C                   when      DACMDA > *zeros
022500100512     C                   exsr      SQ_CCMDCM
022600100512     C                   endsl
022700100512     C*
022800100512     C                   ENDSR
022900100512     C*------------------------------------------------------------------------*
023000100512
023100100512
023200100512
023300100512     C*------------------------------------------------------------------------*
023400100512     C* EXEKSC   - Elaborazione x codice cliente fatturazione
023500100512     C*------------------------------------------------------------------------*
023600100512     C     EXEKSC        BEGSR
023700100521     C*
023800100521     C                   movel     'K'           wChkCli           1
023900100512     C*
024000100512     C                   select
024100100512     C* ...se richiesta elaborazione x data spedizione
024200100512     C                   when      DATADA > *zeros
024300100512     C                   exsr      SQ_KSCDSP
024400100512     C* ...se richiesta elaborazione x data consegna merce
024500100512     C                   when      DACMDA > *zeros
024600100512     C                   exsr      SQ_KSCDCM
024700100512     C                   endsl
024800100512     C*
024900100512     C                   ENDSR
025000100512     C*------------------------------------------------------------------------*
025100100512
025200100512
025300100512
025400100512     C*------------------------------------------------------------------------*
025500100512     C* SQ_CCMDSP - Elaborazione x codice cliente mittente + data spedizione
025600100512     C*------------------------------------------------------------------------*
025700100512     C     SQ_CCMDSP     BEGSR
025800100512     C*
025900100512     C                   z-add     KUNI(I)       wParCCM           7 0
026000100512     C*
026100100512     C/EXEC SQL
026200100512     C+ declare C_CCMDSP cursor for
026300100512     C+ select * from titas00f
026400100512     C+ where tasccm = :wParCCM and
026500100512     C+ tasaas*10000+tasmgs between :DATADA and :DATAA
026600100512     C+ union
026700100512     C+ select * from titas10f
026800100512     C+ where tasccm = :wParCCM and
026900100512     C+ tasaas*10000+tasmgs between :DATADA and :DATAA
027000100512     C+ union
027100100512     C+ select * from titasP0f
027200100512     C+ where tasccm = :wParCCM and
027300100512     C+ tasaas*10000+tasmgs between :DATADA and :DATAA
027400100512     C+ for read only
027500100512     C/END-EXEC
027600100512     C
027700100512     C/EXEC SQL
027800100512     C+ open C_CCMDSP
027900100512     C/END-EXEC
028000100512     C
028100100512     C/EXEC SQL
028200100512     C+ Fetch C_CCMDSP into :TITAS00F
028300100512     C/END-EXEC
028400100512     C*
028500100512     C                   dow       sqlcod = *zeros
028600100512     C                   exsr      verifica
028700100512     C                   if        CHKREC = 'S'
028800100512     C                   exsr      scriviVAC
028900100512     C                   endif
029000100512     C
029100100512     C/EXEC SQL
029200100512     C+ Fetch C_CCMDSP into :TITAS00F
029300100512     C/END-EXEC
029400100512     C*
029500100512     C                   enddo
029600100512     C*
029700100512     C/EXEC SQL
029800100512     C+ close C_CCMDSP
029900100512     C/END-EXEC
030000100512     C*
030100100512     C*
030200100512     C                   ENDSR
030300100512     C*------------------------------------------------------------------------*
030400100512
030500100512
030600100512
030700100512     C*------------------------------------------------------------------------*
030800100512     C* SQ_CCMDCM - Elaborazione x codice cliente mittente + data consegna merce
030900100512     C*------------------------------------------------------------------------*
031000100512     C     SQ_CCMDCM     BEGSR
031100100512     C*
031200100512     C                   z-add     KUNI(I)       wParCCM           7 0
031300100512     C*
031400100512     C/EXEC SQL
031500100512     C+ declare C_CCMDCM cursor for
031600100512     C+ select * from titas00f
031700100512     C+ where tasccm = :wParCCM and
031800100512     C+ tasdcm between :DACMDA and :DACMA
031900100512     C+ union
032000100512     C+ select * from titas10f
032100100512     C+ where tasccm = :wParCCM and
032200100512     C+ tasdcm between :DACMDA and :DACMA
032300100512     C+ union
032400100512     C+ select * from titasP0f
032500100512     C+ where tasccm = :wParCCM and
032600100512     C+ tasdcm between :DACMDA and :DACMA
032700100512     C+ for read only
032800100512     C/END-EXEC
032900100512     C
033000100512     C/EXEC SQL
033100100512     C+ open C_CCMDCM
033200100512     C/END-EXEC
033300100512     C
033400100512     C/EXEC SQL
033500100512     C+ Fetch C_CCMDCM into :TITAS00F
033600100512     C/END-EXEC
033700100512     C*
033800100512     C                   dow       sqlcod = *zeros
033900100512     C                   exsr      verifica
034000100512     C                   if        CHKREC = 'S'
034100100512     C                   exsr      scriviVAC
034200100512     C                   endif
034300100512     C
034400100512     C/EXEC SQL
034500100512     C+ Fetch C_CCMDCM into :TITAS00F
034600100512     C/END-EXEC
034700100512     C*
034800100512     C                   enddo
034900100512     C*
035000100512     C/EXEC SQL
035100100512     C+ close C_CCMDCM
035200100512     C/END-EXEC
035300100512     C*
035400100512     C*
035500100512     C                   ENDSR
035600100512     C*------------------------------------------------------------------------*
035700100512
035800100512
035900100512
036000100512     C*------------------------------------------------------------------------*
036100100512     C* SQ_KSCDSP - Elaborazione x codice cliente fatturazione + data spedizione
036200100512     C*------------------------------------------------------------------------*
036300100512     C     SQ_KSCDSP     BEGSR
036400100512     C*
036500100512     C                   z-add     KUNI(I)       wParKSC           7 0
036600100512     C*
036700100512     C/EXEC SQL
036800100512     C+ declare C_KSCDSP cursor for
036900100512     C+ select * from titas00f
037000100512     C+ where tasksc = :wParKSC and
037100100512     C+ tasaas*10000+tasmgs between :DATADA and :DATAA
037200100512     C+ union
037300100512     C+ select * from titas10f
037400100512     C+ where tasksc = :wParKSC and
037500100512     C+ tasaas*10000+tasmgs between :DATADA and :DATAA
037600100512     C+ union
037700100512     C+ select * from titasP0f
037800100512     C+ where tasksc = :wParKSC and
037900100512     C+ tasaas*10000+tasmgs between :DATADA and :DATAA
038000100512     C+ for read only
038100100512     C/END-EXEC
038200100512     C
038300100512     C/EXEC SQL
038400100512     C+ open C_KSCDSP
038500100512     C/END-EXEC
038600100512     C
038700100512     C/EXEC SQL
038800100512     C+ Fetch C_KSCDSP into :TITAS00F
038900100512     C/END-EXEC
039000100512     C*
039100100512     C                   dow       sqlcod = *zeros
039200100512     C                   exsr      verifica
039300100512     C                   if        CHKREC = 'S'
039400100512     C                   exsr      scriviVAC
039500100512     C                   endif
039600100512     C
039700100512     C/EXEC SQL
039800100512     C+ Fetch C_KSCDSP into :TITAS00F
039900100512     C/END-EXEC
040000100512     C*
040100100512     C                   enddo
040200100512     C*
040300100512     C/EXEC SQL
040400100512     C+ close C_KSCDSP
040500100512     C/END-EXEC
040600100512     C*
040700100512     C*
040800100512     C                   ENDSR
040900100512     C*------------------------------------------------------------------------*
041000100512
041100100512
041200100512
041300100512     C*------------------------------------------------------------------------*
041400100512     C* SQ_KSCDCM - Elaborazione x codice cliente fatturazione + data consegna merce
041500100512     C*------------------------------------------------------------------------*
041600100512     C     SQ_KSCDCM     BEGSR
041700100512     C*
041800100512     C                   z-add     KUNI(I)       wParKSC           7 0
041900100512     C*
042000100512     C/EXEC SQL
042100100512     C+ declare C_KSCDCM cursor for
042200100512     C+ select * from titas00f
042300100512     C+ where tasksc = :wParKSC and
042400100512     C+ tasdcm between :DACMDA and :DACMA
042500100512     C+ union
042600100512     C+ select * from titas10f
042700100512     C+ where tasksc = :wParKSC and
042800100512     C+ tasdcm between :DACMDA and :DACMA
042900100512     C+ union
043000100512     C+ select * from titasP0f
043100100512     C+ where tasksc = :wParKSC and
043200100512     C+ tasdcm between :DACMDA and :DACMA
043300100512     C+ for read only
043400100512     C/END-EXEC
043500100512     C
043600100512     C/EXEC SQL
043700100512     C+ open C_KSCDCM
043800100512     C/END-EXEC
043900100512     C
044000100512     C/EXEC SQL
044100100512     C+ Fetch C_KSCDCM into :TITAS00F
044200100512     C/END-EXEC
044300100512     C*
044400100512     C                   dow       sqlcod = *zeros
044500100512     C                   exsr      verifica
044600100512     C                   if        CHKREC = 'S'
044700100512     C                   exsr      scriviVAC
044800100512     C                   endif
044900100512     C
045000100512     C/EXEC SQL
045100100512     C+ Fetch C_KSCDCM into :TITAS00F
045200100512     C/END-EXEC
045300100512     C*
045400100512     C                   enddo
045500100512     C*
045600100512     C/EXEC SQL
045700100512     C+ close C_KSCDCM
045800100512     C/END-EXEC
045900100512     C*
046000100512     C*
046100100512     C                   ENDSR
046200100512     C*------------------------------------------------------------------------*
046300100512
046400100512
046500100512
046600100512     C*------------------------------------------------------------------------*
046700100512     C* CHKCONS  - Verifica se bolla effettivamente consegnata
046800100512     C*------------------------------------------------------------------------*
046900100512     C     CHKCONS       BEGSR
047000100512     C*
047100100512     C                   setoff                                       4041
047200100512     C                   movel     *blanks       wEsito1           1
047300100512     C                   movel     *blanks       wEsito2           1
047400100512     C*
047500100512     C                   eval      wAAS = tasAAS
047600100512     C                   eval      wLNP = tasLNP
047700100512     C                   eval      wNRS = tasNRS
047800100512     C                   eval      wNSP = tasNSP
047900100512     C*
048000100512     C* Chiamata metodo GetLblTyp
048100100512     C                   eval      wEsito1 = %char(UBLBLSPE_GetLblTyp(
048200100512     C                                                wAAS
048300100512     C                                               :wLNP
048400100512     C                                               :wNRS
048500100512     C                                               :wNSP
048600100512     C                                               :pOutLblTyp
048700100512     C                                               :pOutAnnoBO
048800100512     C                                               :pOutLineaParBO
048900100512     C                                               :pOutSerieBO
049000100512     C                                               :pOutNumSpedBO
049100100512     C                                               :pOutDcmBO
049200100512     C                                               :pOutCcaBO
049300100512     C                                               :pOutRblBO))
049400100512     C* Se trattasi d bolla nn originale reperisco la relativa bolla originale
049500100512     C                   if        pOutLblTyp <> 'O'
049600100512     C                   eval      wAAS = pOutAnnoBO
049700100512     C                   eval      wLNP = pOutLineaParBO
049800100512     C                   eval      wNRS = pOutSerieBO
049900100512     C                   eval      wNSP = pOutNumSpedBO
050000100512     C                   endif
050100100512     C*
050200100512     C* Chiamata metodo GetLastChild
050300100512     C                   eval      wEsito2 = %char(UBLBLSPE_GetLastChild(
050400100512     C                                                wAAS
050500100512     C                                               :wLNP
050600100512     C                                               :wNRS
050700100512     C                                               :wNSP
050800100512     C                                               :pOutAnnoFI
050900100512     C                                               :pOutLineaParFI
051000100512     C                                               :pOutSerieFI
051100100512     C                                               :pOutNumSpedFI
051200100512     C                                               :pOutDcmFI
051300100512     C                                               :pOutCcaFI))
051400100512     C*
051500100512     C* Considerazioni sullo stato corrente della bolla
051600100512     C                   if        (wEsito2 = '9' and pOutDcmBO > *zeros)  or
051700100512     C                             (wEsito2 = '0' and pOutDcmFI > *zeros)
051800100512     C                   seton                                        40        * bolla cons.
051900100512     C                   endif
052000100512     C*
052100100512     C                   if        (wEsito2 = '9' and pOutDcmBO > *zeros
052200100512     C                                            and pOutCcaBO = *blanks) or
052300100512     C                             (wEsito2 = '0' and pOutDcmFI > *zeros
052400100512     C                                            and pOutCcaFI = *blanks)
052500100512     C                   seton                                        41        * bolla cons. OK
052600100512     C                   endif
052700100512     C*
052800100512     C                   ENDSR
052900100512     C*------------------------------------------------------------------------*
053000001218
053100001218
053200001218
053300001218     C*------------------------------------------------------------------------*
053400001218     C* VERIFICA - Routine di verifica validità record corrente
053500001218     C*------------------------------------------------------------------------*
053600001218     C     VERIFICA      BEGSR
053700001218     C*
053800001218     C                   movel     'S'           CHKREC            1
053900001218     C*
054000100512     C* Verifico le date spedizione
054100001218     C                   if        CHKREC = 'S'
054200100512     C                   if        DATADA > *zeros
054300001218     C                   movel     tasaas        wrkdata           8 0
054400001218     C                   move      tasmgs        wrkdata
054500001218     C                   if        wrkdata < DATADA or
054600001218     C                             wrkdata > DATAA
054700001218     C                   movel     'N'           CHKREC
054800100512     C                   endif
054900001218     C                   endif
055000001218     C                   endif
055100100512     C*
055200100512     C* Verifico le date spedizione
055300100512     C                   if        CHKREC = 'S'
055400100512     C                   if        DACMDA > *zeros
055500100512     C                   if        tasdcm  < DACMDA or
055600100512     C                             tasdcm  > DACMA
055700100512     C                   movel     'N'           CHKREC
055800100512     C                   endif
055900100512     C                   endif
056000100512     C                   endif
056100100521     C*
056200100521     C* Verifico il codice cliente mittente
056300100521     C                   if        CHKREC = 'S'
056400100521     C                   if        wChkCli  = 'C'      AND
056500100521     C                             tasccm  <> KUNI(I)
056600100521     C                   movel     'N'           CHKREC
056700100521     C                   endif
056800100521     C                   endif
056900070718     C*
057000100517     C* Verifico il codice cliente fatturazione (in caso d 'K')
057100070718     C                   if        CHKREC = 'S'
057200100521     C                   if        wChkCli  = 'K'      AND
057300100521     C                             tasksc  <> KUNI(I)
057400070718     C                   movel     'N'           CHKREC
057500070718     C                   endif
057600070718     C                   endif
057700100517     C*
057800100517     C* Verifico il codice cliente fatturazione (in caso d 'E')
057900100517     C                   if        CHKREC = 'S'
058000100521     C                   if        TIPCLI  = 'E'       AND
058100100521     C                             wChkCli = 'K'       AND
058200100521     C                             tasksc  = tasccm
058300100517     C                   movel     'N'           CHKREC
058400100517     C                   endif
058500100517     C                   endif
058600001218     C*
058700020923     C* Verifico che la bolla abbia la data consegna se trattasi di bolla PT
058800001218     C                   if        CHKREC = 'S'
058900020923     C                   if        tasTSP = 'P'
059000020923     C                   if        tasDCM > *zeros
059100001218     C                   else
059200001218     C                   movel     'N'           CHKREC
059300001218     C                   endif
059400020923     C                   endif
059500001218     C                   endif
059600020920     C*
059700040322     C* Verifico che il tipo bolla sia da considerare ai fini della rigenerazione dell'FNVAC
059800020920     C                   if        CHKREC = 'S'
059900040322     C     tasCBO        lookup    COD3A                                  55
060000081016     C                   if        %equal
060100040322     C                   else
060200020920     C                   movel     'N'           CHKREC
060300020920     C                   endif
060400020920     C                   endif
060500010301     C*
060600010301     C* Verifico che la bolla se POSTE non abbia il codice CA = 5
060700010301     C                   if        CHKREC = 'S'
060800010301     C                   if        tasTSP = 'P' and tasCCA = '5'
060900010301     C                   movel     'N'           CHKREC
061000010301     C                   endif
061100010301     C                   endif
061200100512     C*
061300040322     C* Verifico se richiesto nel lancio che la bolla sia consegnata
061400040322     C                   if        CHKREC = 'S'
061500040322     C                   if        FLGCONS = 'S'
061600100512     C                   exsr      CHKCONS
061700100512     C                   if        *in40 = *on
061800040322     C                   else
061900040322     C                   movel     'N'           CHKREC
062000040322     C                   endif
062100040322     C                   endif
062200040322     C                   endif
062300100512     C*
062400100512     C* Verifico se richiesto nel lancio che la bolla sia consegnata OK
062500100512     C                   if        CHKREC = 'S'
062600100512     C                   if        FLGCONS = 'O'
062700100512     C                   exsr      CHKCONS
062800100512     C                   if        *in41 = *on
062900100512     C                   else
063000100512     C                   movel     'N'           CHKREC
063100100512     C                   endif
063200100512     C                   endif
063300100512     C                   endif
063400100513     C*
063500100907     C* Verifico se richiesto nel lancio che la bolla NON sia consegnata OK
063600100513     C                   if        CHKREC = 'S'
063700100513     C                   if        FLGCONS = 'K'
063800100513     C                   exsr      CHKCONS
063900100513     C                   if        *in41 = *on
064000100513     C                   movel     'N'           CHKREC
064100100513     C                   endif
064200100513     C                   endif
064300100513     C                   endif
064400100907     C*
064500100907     C* Verifico se richiesto nel lancio che la bolla NON sia consegnata
064600100907     C                   if        CHKREC = 'S'
064700100907     C                   if        FLGCONS = 'P'
064800100907     C                   exsr      CHKCONS
064900100907     C                   if        *in40 = *on
065000100907     C                   movel     'N'           CHKREC
065100100907     C                   endif
065200100907     C                   endif
065300100907     C                   endif
065400040322     C*
065500040322     C* Verifico se richiesto nel lancio che la bolla provenga da scambio dati VAS cliente
065600040322     C                   if        CHKREC = 'S'
065700040322     C                   if        FLGVAS = 'S'
065800040322     C                   if        tasSOP = '§' OR
065900040322     C                             tasSOP = ':' OR
066000040322     C                             tasSOP = '%'
066100040322     C                   else
066200040322     C                   movel     'N'           CHKREC
066300040322     C                   endif
066400040322     C                   endif
066500040322     C                   endif
066600081016     C*
066700081016     C* Verifico se richiesto che la bolla corrente sia la singola bolla richiesta a video
066800081016     C                   if        CHKREC = 'S'
066900081016     C                   if        KAAS   <> *zeros OR
067000081016     C                             KLNP   <> *zeros OR
067100081016     C                             KNRS   <> *zeros OR
067200081016     C                             KNSP   <> *zeros
067300081016     C                   if        tasAAS = KAAS AND
067400081016     C                             tasLNP = KLNP AND
067500081016     C                             tasNRS = KNRS AND
067600081016     C                             tasNSP = KNSP
067700081016     C                   else
067800081016     C                   movel     'N'           CHKREC
067900081016     C                   endif
068000081016     C                   endif
068100081016     C                   endif
068200001218     C*
068300001218     C                   ENDSR
068400001218     C*------------------------------------------------------------------------*
068500001218
068600001218
068700001218
068800001218     C*------------------------------------------------------------------------*
068900001218     C* SCRIVIVAC - Routine di scrittura file esiti di consegna (FNVAC)
069000001218     C*------------------------------------------------------------------------*
069100001218     C     SCRIVIVAC     BEGSR
069200001218     C*
069300001218     C                   CLEAR                   fnvac000
069400001218     C*
069500001218     C                   MOVEL     tasAAS        VACAAS
069600001218     C                   MOVEL     tasLNP        VACLNP
069700001218     C                   MOVEL     tasNRS        VACNRS
069800001218     C                   MOVEL     tasNSP        VACNSP
069900001218     C                   MOVEL     tasMGS        VACMGS
070000001218     C                   MOVEL     tasCBO        VACCBO
070100001218     C                   MOVEL     tasLNA        VACLNA
070200010223     C                   MOVEL     tasRSD        VACRSD
070300010223     C                   MOVEL     tasPRD        VACPRD
070400010301     C*
070500001218     C* Reperisco dall'estensione anagrafica bolle di SEDE i dati del Destinatario
070600001218     C                   EXSR      REPDEST
070700151019     C*
070800151019     C* Eseguo eventuali "forzature"
070900151019     C                   EXSR      RTVAR5FAT
071000001218     C*
071100001218     C                   MOVEL     tasGC1        VACGC1
071200001218     C                   MOVEL     tasGC2        VACGC2
071300001218     C                   MOVEL     tasCTR        VACCTR
071400001218     C                   MOVEL     tasCTS        VACCTS
071500001218     C                   MOVEL     tasFTM        VACFTM
071600001218     C                   MOVEL     tasFIN        VACFIN
071700001218     C                   MOVEL     tasFAP        VACFAP
071800001218     C                   MOVEL     tasTSP        VACTSP
071900001218     C* Effettuo considerazioni particolare per bolla "POSTE"
072000001218     C     tasTSP        IFEQ      'P'                                          BOLLA POSTE
072100001218     C                   EXSR      REPPTIAS
072200001218     C                   EXSR      REPPTGIA
072300001218     C                   Z-ADD     tasNCD        VACDCR
072400001218     C                   ELSE
072500001218     C                   MOVEL     tasIAS        VACIAS
072600001218     C                   MOVEL     tasVAS        VACVAS
072700001218     C                   MOVEL     tasDCR        VACDCR
072800050310     C     KGCP          CHAIN     TIGCP51L
072900050310     C                   IF        %found(TIGCP51L)
073000001218     C                   MOVEL     GCPAGC        VACDAG
073100001218     C                   MOVE      GCPMGC        VACDAG
073200001218     C                   ENDIF
073300001218     C                   ENDIF
073400001218     C*
073500001218     C                   MOVEL     tasNAS        VACNAS
073600001218     C                   MOVEL     tasNCL        VACNCL
073700001218     C                   MOVEL     tasPKF        VACPKB
073800001218     C                   MOVEL     tasVLF        VACVLB
073900001218     C                   MOVEL     tasQFT        VACQFT
074000001218     C*
074100001218     C                   EXSR      REPCSB
074200001218     C*
074300001218     C                   MOVEL     tasCCM        VACCCM
074400001218     C                   MOVEL     tasRMN        VACRMN
074500010301     C*
074600001218     C                   EXSR      REPRMA
074700010301     C*
074800001218     C                   MOVEL     tasFFD        VACFFD
074900001218     C                   MOVEL     tasTCR        VACTCR
075000001218     C                   MOVEL     tasHCR        VACHCR
075100001218     C                   MOVEL     tasFTC        VACTC1
075200001218     C                   MOVEL     tasTC2        VACTC2
075300001218     C                   MOVEL     tasDCM        VACDCM
075400001218     C                   MOVEL     tasHMC        VACHMC
075500001218     C*
075600010301     C                   IF        tasTSP = 'P' and tasCCA = '7'
075700010301     C                   MOVEL     *blanks       VACCCA
075800010301     C                   ELSE
075900100910     C                   MOVEL     tasCCA        VACCCA
076000100910     C*
076100100910     C* Se CCA ancora nn valorizzato => reperisco l'ultimo evento
076200100910     C                   IF        VACCCA = *blanks
076300100910     C                   EXSR      REPLASTEV
076400010301     C                   ENDIF
076500100910     C                   ENDIF
076600010301     C*
076700120427     C* Reperisco l'eventuale lasciato avviso
076800001218     C                   EXSR      REPLAV
076900120427     C*
077000120427     C* Effettuo particolari operazioni in caso d DIROTTAMENTO
077100120427     C***                IF        VACCCA = '1' AND
077200120427     C***                          VACDCM = *zeros
077300120427     C***                EXSR      REPDIR
077400120427     C***                ENDIF
077500001218     C*
077600020923     C* Verifico che in caso di bolla NON PT siano valorizzate o la data consegna...
077700020923     C* ... o la data lasciato avviso o la data giacenza o la consegna anomala
077800020923     C                   IF        tasTSP <> 'P'
077900071228     C*
078000071228     C* Se richiesto nel lancio anche bolle senza eventi => gestisco...
078100071228     C* ...altrimenti standard
078200071228     C                   IF        FLGNOE  = 'N'
078300020923     C                   IF        vacDCM  > *zeros OR
078400020923     C                             vacDLA  > *zeros OR
078500020923     C                             vacDAG  > *zeros OR
078600020923     C                             vacCCA <> *blanks
078700071228     C   25              WRITE     FNVAC000
078800071228     C   26              exsr      WRITIVGD
078900020923     C                   endif
079000071228     C                   else
079100071228     C   25              WRITE     FNVAC000
079200071228     C   26              exsr      WRITIVGD
079300071228     C                   endif
079400020923     C                   else
079500050823     C   25              WRITE     FNVAC000
079600050823     C   26              exsr      WRITIVGD
079700020923     C                   endif
079800001218     C*
079900001218     C                   ENDSR
080000120427
080100120427
080200120427
080300120427     C*------------------------------------------------------------------------*
080400120427     C* REPDIR - Reperimento data DIROTTAMENTO
080500120427     C*------------------------------------------------------------------------*
080600120427     C     REPDIR        BEGSR
080700120427     C*
080800120427     C                   movel     *blanks       wEsito1           1
080900120427     C*
081000120427     C* Chiamata metodo GetLblTyp
081100120427     C                   eval      wEsito1 = %char(UBLBLSPE_GetLblTyp(
081200120427     C                                                tasAAS
081300120427     C                                               :tasLNP
081400120427     C                                               :tasNRS
081500120427     C                                               :tasNSP
081600120427     C                                               :pOutLblTyp
081700120427     C                                               :pOutAnnoBO
081800120427     C                                               :pOutLineaParBO
081900120427     C                                               :pOutSerieBO
082000120427     C                                               :pOutNumSpedBO
082100120427     C                                               :pOutDcmBO
082200120427     C                                               :pOutCcaBO
082300120427     C                                               :pOutRblBO))
082400120427     C*
082500120427     C* Se reperimento effettuato e trattasi d dirottamento => recupero la data
082600120427     C                   if        wEsito1 = '0' and pOutCcaBO = '1'
082700120427     C                   eval      VACDCM = pOutDcmBO
082800120427     C                   endif
082900120427     C*
083000120427     C                   ENDSR
083100120427     C*------------------------------------------------------------------------*
083200100910
083300100910
083400100910
083500100910
083600100910     C*------------------------------------------------------------------------*
083700100910     C* REPLASTEV - Routine di reperimento ultimo evento
083800100910     C*------------------------------------------------------------------------*
083900100910     C     REPLASTEV     BEGSR
084000100910     C*
084100100910     C     KEVB          SETGT     FNEVB01L
084200100910     C     KEVB          READPE    FNEVB01L                               34
084300100910     C*
084400100910     C* Devo cercare l'ultimo evento di tipo giacenza
084500100910     C                   IF        *IN34 = *OFF
084600100910     C*
084700100910     C* Evento 'MIC' considero solo se data evento >= data corrente
084800100910     C                   IF        EVBDEV >= datcor
084900100910     C* - MIC
085000100910     C                   IF        EVBCEV = 'MIC'
085100100910     C                   MOVEL     'C'           VACCCA
085200100910     C                   MOVEL     evbDEV        VACDCM
085300100910     C                   ENDIF
085400100910     C                   ENDIF
085500100910     C*
085600100910     C                   ENDIF
085700100910     C*
085800100910     C                   ENDSR
085900100910     C*------------------------------------------------------------------------*
086000050823
086100050823
086200050823
086300050823
086400050823     C*------------------------------------------------------------------------*
086500050823     C* WRITIVGD - Routine di scrittura file TIVGD (file VAS generico download)
086600050823     C*------------------------------------------------------------------------*
086700050823     C     WRITIVGD      BEGSR
086800050823     C*
086900050823     C                   clear                   tivgd000
087000080407     C                   eval      vgdDTA = %subst(FNVACDS:1:%size(FNVACDS))
087100100312     C                   eval      vgdTIP = TIPFILE
087200050823     C                   movel     *all'0'       vgdKSU
087300070525     C                   move      CODCLIVAS     vgdKSU
087400050823     C                   eval      vgdTSC = 'WW'
087500050823     C                   eval      vgdDAT = datcor
087600050901     C                   eval      vgdPGM = 'TIS7VCR'
087700070627     C*
087800070627     C                   clear                   DVGDFLO
087900070627     C                   movel     'P'           §VGDFELA
088000070627     C                   movel     DVGDFLO       vgdflo
088100070627     C*
088200050823     C                   write     tivgd000
088300050823     C*
088400050823     C                   ENDSR
088500050823     C*------------------------------------------------------------------------*
088600070627
088700070627
088800070627
088900070627     C*------------------------------------------------------------------------*
089000070627     C* FORZADWN - Routine di forzatura elaborazione immediata download
089100070627     C*------------------------------------------------------------------------*
089200070627     C     FORZADWN      BEGSR
089300070627     C*
089400070627     C                   clear                   tis781ds
089500070627     C                   eval      §781tip = vgdTIP
089600070627     C                   eval      §781ksu = vgdKSU
089700070627     C                   eval      §781tsc = vgdTSC
089800070627     C                   eval      §781dat = vgdDAT
089900070627     C                   eval      §781pgm = vgdPGM
090000080905     C*
090100080905     C                   clear                   TIS781DFLO
090200080905     C                   movel     'P'           §781floela
090300080905     C                   movel     TIS781DFLO    §781flo
090400070627     C*
090500070627     C* Finita l'elaborazione chiamo pgm
090600070627     C                   CALL      'TIS781C1'
090700070627     C                   parm      *blanks       esito             1
090800070627     C                   parm                    tis781ds
090900070627     C                   parm      *blanks       vgdPRG           10
091000070627     C*
091100070627     C                   ENDSR
091200070627     C*------------------------------------------------------------------------*
091300001218
091400001218
091500001218
091600001218
091700001218     C*------------------------------------------------------------------------*
091800001218     C* REPPTIAS - Routine di reperimento dati relativi all'importo/divisa da assicurare x POSTE
091900001218     C*------------------------------------------------------------------------*
092000001218     C     REPPTIAS      BEGSR
092100001218     C*
092200060215     C* Prendo importo da assicurare da TITA4 t.r.=J
092300001218     C                   movel     'J'           KTRC
092400001218     C     KTA4          chain     tita430c
092500001218     C                   if        %found(tita430c)
092600001218     C                   movel     ta4NOT        DSBL4J
092700001218     C                   z-add     §B4IAS        vacIAS
092800001218     C                   movel     §B4VAS        vacVAS
092900001218     C                   endif
093000001218     C*
093100001218     C                   ENDSR
093200001218     C*------------------------------------------------------------------------*
093300001218
093400001218
093500001218
093600001218
093700001218     C*------------------------------------------------------------------------*
093800001218     C* REPRMA - Routine di reperimento dati relativi al riferimento mittente alfabetico
093900001218     C*------------------------------------------------------------------------*
094000001218     C     REPRMA        BEGSR
094100001218     C*
094200060215     C* Prendo importo da assicurare da TITA4 t.r.=A
094300001218     C                   movel     'A'           KTRC
094400001218     C     KTA4          chain     tita430c
094500001218     C                   if        %found(tita430c)
094600001218     C                   eval      vacRMA = %subst(ta4NOT:1:15)
094700001218     C                   endif
094800001218     C*
094900001218     C                   ENDSR
095000001218     C*------------------------------------------------------------------------*
095100001218
095200001218
095300001218
095400001218
095500001218     C*------------------------------------------------------------------------*
095600001218     C* REPDEST - Routine di reperimento dati relativi al destinatario della bolla (TITAA)
095700001218     C*           ... e ai dati del mittente originale
095800001218     C*------------------------------------------------------------------------*
095900001218     C     REPDEST       BEGSR
096000001218     C*
096100010223     C* Prendo il tipo anagrafica = D
096200010223     C*                  movel     'D'           KTRC
096300010223     C*    KTAA          chain     titaa30c
096400010223     C*                  if        %found(titaa30c)
096500010223     C*                  movel     taaRSC        vacRSD
096600010223     C*                  movel     taaPRV        vacPRD
096700010223     C*                  endif
096800001218     C*
096900010223     C* Prendo il tipo anagrafica = O
097000001218     C                   movel     'O'           KTRC
097100001218     C     KTAA          chain     titaa30c
097200001218     C                   if        %found(titaa30c)
097300001218     C                   movel     taaRSC        vacRMO
097400001218     C                   endif
097500001218     C*
097600001218     C                   ENDSR
097700001218     C*------------------------------------------------------------------------*
097800001218
097900001218
098000001218
098100001218
098200001218     C*------------------------------------------------------------------------*
098300001218     C* REPCSB - Routine di reperimento dati relativi al contrassegno
098400001218     C*------------------------------------------------------------------------*
098500001218     C     REPCSB        BEGSR
098600001218     C*
098700001219     C     KCSB          chain     tncsb03l
098800001219     C                   if        %found(tncsb03l)
098900001218     C                   if        csbSTA = 9 and tasTSP <> 'P'
099000001218     C                   else
099100001218     C                   movel     csbTPA        vactic
099200001218     C                   move      csbTPI        vactic
099300001218     C                   z-add     csbCAS        vacCAS
099400001218     C                   movel     csbVCA        vacVCA
099500001218     C                   endif
099600001218     C                   endif
099700001218     C*
099800001218     C                   ENDSR
099900001218     C*------------------------------------------------------------------------*
100000001218
100100001218
100200001218
100300001218
100400001218     C*------------------------------------------------------------------------*
100500001218     C* REPPTGIA - Routine di reperimento dati relativi alla giacenza x POSTE
100600001218     C*------------------------------------------------------------------------*
100700001218     C     REPPTGIA      BEGSR
100800001218     C*
100900001218     C* Reperisco la data di giacenza dagli eventi poichè non vengono
101000001218     C* gestite le giacenze per le bolle poste
101100001218     C     KEVB          SETGT     FNEVB01L
101200001218     C     KEVB          READPE    FNEVB01L                               34
101300001218     C*
101400001218     C* Devo cercare l'ultimo evento di tipo giacenza
101500001218     C     *IN34         DOWEQ     *OFF
101600001218     C                   Z-ADD     1             W
101700001218     C     EVBCEV        LOOKUP    C2A(W)                                 42
101800001218     C     *IN42         IFEQ      '1'
101900001218     C     T2A(W)        ANDEQ     'G'
102000001218     C                   Z-ADD     EVBDEV        vacDAG
102100001218     C                   LEAVE
102200001218     C                   ELSE
102300001218     C     KEVB          READPE    FNEVB000                               34
102400001218     C                   ENDIF
102500001218     C                   ENDDO
102600001218     C*
102700001218     C                   ENDSR
102800001218     C*------------------------------------------------------------------------*
102900001218
103000001218
103100001218
103200001218
103300001218     C*------------------------------------------------------------------------*
103400001218     C* REPLAV - Routine di reperimento dati relativi al lasciato avviso
103500001218     C*------------------------------------------------------------------------*
103600001218     C     REPLAV        BEGSR
103700001218     C*
103800001218     C* Pererisco l'ultimo evento Lasciato Avviso
103900001218     C                   MOVEL     '2A'          COD
104000001218     C     KEVB          SETGT     FNEVB01L
104100001218     C     KEVB          READPE    FNEVB01L                               34
104200001218     C*
104300001218     C     *IN34         DOWEQ     *OFF
104400001218     C                   MOVEL(P)  EVBCEV        KEY
104500001218     C     KTAB2         CHAIN     TABEL                              30
104600001218     C     *IN30         IFEQ      *OFF
104700001218     C                   MOVEL     TBLUNI        DS2A
104800001218     C     §2AFTC        IFEQ      'A'
104900001218     C                   Z-ADD     EVBDEV        vacDLA
105000001218     C                   LEAVE
105100001218     C                   ENDIF
105200001218     C                   ENDIF
105300001218     C     KEVB          READPE    FNEVB000                               34
105400001218     C                   ENDDO
105500001218     C*
105600001218     C                   ENDSR
105700001218     C*------------------------------------------------------------------------*
105800001218
105900001218
106000001218
106100001218
106200001218     C*------------------------------------------------------------------------*
106300001218     C* CARTBL - Routine di caricamento dati tabellati
106400001218     C*------------------------------------------------------------------------*
106500001218     C     CARTBL        BEGSR
106600001218     C*
106700001218     C                   Z-ADD     0             W                 4 0
106800001218     C                   MOVEA     *HIVAL        T2A
106900001218     C                   MOVEA     *HIVAL        C2A
107000001218     C                   MOVEL     '2A'          COD
107100001218     C     KTAB          CHAIN     TABEL                              31
107200001218     C     *IN31         DOWEQ     '0'
107300001218     C     TBLFLG        IFEQ      ' '
107400001218     C                   MOVEL     TBLUNI        DS2A
107500001218     C     §2AFTC        IFNE      *BLANKS
107600001218     C                   ADD       1             W
107700001218     C                   MOVEL     §2AFTC        T2A(W)
107800001218     C                   MOVEL     TBLKEY        C2A(W)
107900001218     C                   ENDIF
108000001218     C                   ENDIF
108100001218     C     KTAB          READE     TABEL                                  31
108200001218     C                   ENDDO
108300001218     C*
108400081016     C                   Z-ADD     1             I                 4 0
108500011017     C                   MOVEL     CODCLI        KUNI(I)
108600081016     C                   IF        FLGUNI = 'U'
108700001218     C                   MOVEL     '3K'          COD
108800001218     C     KTAB          CHAIN     TABEL                              31
108900001218     C     *IN31         DOWEQ     '0'
109000001218     C     TBLFLG        IFEQ      ' '
109100001218     C                   MOVEL     TBLUNI        DS3K
109200010301     C     §3KCKS        IFEQ      CODCLI
109300001218     C                   ADD       1             I
109400081016     C                   MOVEL     TBLKEY        wKUNI             7 0
109500081016     C     wKUNI         LOOKUP    KUNI                                   55
109600081016     C                   IF        %equal
109700081016     C                   ELSE
109800081016     C                   Z-ADD     wKUNI         KUNI(I)
109900081016     C                   ENDIF
110000001218     C                   ENDIF
110100001218     C                   ENDIF
110200001218     C     KTAB          READE     TABEL                                  31
110300001218     C                   ENDDO
110400010301     C                   ENDIF
110500040312     C*
110600040312     C                   Z-ADD     0             J                 4 0
110700040312     C                   MOVEL     '3A'          COD
110800040312     C     KTAB          CHAIN     TABEL                              31
110900040312     C     *IN31         DOWEQ     '0'
111000040312     C     TBLFLG        IFEQ      ' '
111100040312     C                   MOVEL     TBLUNI        DS3A
111200040312     C     §3AVAC        IFEQ      'N'
111300040312     C                   ELSE
111400040312     C                   ADD       1             J
111500040312     C                   MOVEL     TBLKEY        COD3A(J)
111600040312     C                   ENDIF
111700040312     C                   ENDIF
111800040312     C     KTAB          READE     TABEL                                  31
111900040312     C                   ENDDO
112000001218     C*
112100001218     C                   ENDSR
112200001218     C*---------------------------------------------------------------*
112300151019
112400151019
112500151019
112600151019     C     RTVAR5FAT     BEGSR
112700151019     C*
112800151019     C* Se, presente, considero il peso/volume DESUNTO al pari del VDL TOTALE
112900151019     C                   SETOFF                                       7071
113000151019     C                   EVAL      ar5TRD = 'FAT'
113100151019     C                   CLEAR                   DAR5FAT
113200151019     C     KEYar531_P    CHAIN     FIAR531C
113300151019     C                   IF        %found(FIAR531C)
113400151019     C                   MOVEL     AR5UNI        DAR5FAT
113500151019     C                   IF        §AR5PKTAS > *zeros
113600151019     C                   SETON                                        70
113700151019     C                   EVAL      tasPKF = §AR5PKTAS
113800151019     C                   ENDIF
113900151019     C                   IF        §AR5VLTAS > *zeros
114000151019     C                   SETON                                        71
114100151019     C***                EVAL      tasVLF = §AR5VLTAS
114200151019     C                   ENDIF
114300151019     C                   ENDIF
114400151019     C*
114500151019     C                   ENDSR
114600001218
114700001218
114800001218
114900001218
115000001218     C*------------------------------------------------------------------------*
115100001218     C* *INZSR - ROUTINE INIZIALE
115200001218     C*------------------------------------------------------------------------*
115300001218     C     *INZSR        BEGSR
115400001218     C*
115500001218     C     *ENTRY        PLIST
115600100512     C                   PARM                    PARAM            71
115700001218     C*
115800001218     C                   MOVEL     PARAM         DSINPUT
115900001218     C                   Z-ADD     1             CODUT
116000001218     C*
116100001218     C* Definizioni chiavi
116200001218     C     KTA4          KLIST
116300001218     C                   KFLD                    tasAAS
116400001218     C                   KFLD                    tasLNP
116500001218     C                   KFLD                    tasNRS
116600001218     C                   KFLD                    tasNSP
116700001218     C                   KFLD                    KTRC              1
116800001218     C*
116900001218     C     KTAA          KLIST
117000001218     C                   KFLD                    tasAAS
117100001218     C                   KFLD                    tasLNP
117200001218     C                   KFLD                    tasNRS
117300001218     C                   KFLD                    tasNSP
117400001218     C                   KFLD                    KTRC
117500001218     C*
117600001218     C     KCSB          KLIST
117700001218     C                   KFLD                    tasAAS
117800001218     C                   KFLD                    tasLNP
117900001218     C                   KFLD                    tasNRS
118000001218     C                   KFLD                    tasNSP
118100001218     C*
118200001218     C     KEVB          KLIST
118300001218     C                   KFLD                    tasAAS
118400001218     C                   KFLD                    tasLNP
118500001218     C                   KFLD                    tasNRS
118600001218     C                   KFLD                    tasNSP
118700001218     C*
118800001218     C     KGCP          KLIST
118900001218     C                   KFLD                    tasAAS
119000001218     C                   KFLD                    tasLNP
119100001218     C                   KFLD                    tasNRS
119200001218     C                   KFLD                    tasNSP
119300001218     C*
119400001218     C     KTAB          KLIST
119500001218     C                   KFLD                    CODUT             1 0
119600001218     C                   KFLD                    COD               2
119700001218     C*
119800001218     C     KTAB2         KLIST
119900001218     C                   KFLD                    CODUT             1 0
120000001218     C                   KFLD                    COD               2
120100001218     C                   KFLD                    KEY               8
120200151019     C*
120300151019     C* FIAR531C - Parziale
120400151019     C     KEYar531_P    KLIST
120500151019     C                   KFLD                    tasAAS
120600151019     C                   KFLD                    tasLNP
120700151019     C                   KFLD                    tasNRS
120800151019     C                   KFLD                    tasNSP
120900151019     C                   KFLD                    ar5TRD
121000001218     C*
121100001218     C* Determino la data corrente
121200100312     C                   Z-ADD     *zeros        DATCOR            8 0
121300100312     C                   EVAL      DATCOR = %dec(%date() : *ISO)
121400001218     C*
121500001218     C                   ENDSR

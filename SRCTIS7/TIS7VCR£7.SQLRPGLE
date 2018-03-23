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
003900160208     D DTA4A         E DS
004000001218     D*--------------------
004100001218     D* DS DI RIDEFINIZIOINE PARAMETRI IN INPUT
004200001218     D*--------------------
004300050823     D DSINPUT         DS
004400050823     D  DATADA                        8  0
004500050823     D  DATAA                         8  0
004600050823     D  CODCLI                        7  0
004700050823     D  FLGUNI                        1
004800100512     D  FLGCONS                       1
004900050823     D  FLGVAS                        1
005000050823     D  FLGOPZ                        1
005100150707     D  CODCLIVAS                     7
005200100312     D  TIPFILE                       2
005300070627     D  FLGEXE                        1
005400100512     D  TIPCLI                        1
005500071228     D  FLGNOE                        1
005600081016     D  KAAS                          4  0
005700081016     D  KLNP                          3  0
005800081016     D  KNRS                          2  0
005900081016     D  KNSP                          7  0
006000100512     D  DACMDA                        8  0
006100100512     D  DACMA                         8  0
006200001218     D*--------------------
006300100512
006400100512
006500100512     D*------------------
006600100512     D* VARIABILI D WRK
006700100512     D*------------------
006800100512     D  wAAS           S                   like(tasAAS) inz
006900100512     D  wLNP           S                   like(tasLNP) inz
007000100512     D  wNRS           S                   like(tasNRS) inz
007100100512     D  wNSP           S                   like(tasNSP) inz
007200100512
007300001218
007400100512     D*------------------
007500100512     D* LINKING A DEFINIZIONI ESTERNE
007600100512     D*------------------
007700100512     D/COPY GAITRASRC/SRCPROTOPR,UBLBLSPE
007800100512     D/COPY GAITRASRC/SRCPROTOPI,UBLBLSPE
007900160208     D/COPY GAITRASRC/SRCPROTOPI,UBTA400R
008000001218
008100001218
008200920812     C*---------------------------------------------------------------*
008300001218     C* MAIN
008400001218     C*---------------------------------------------------------------*
008500100512     C*
008600100512     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
008700100512     C
008800100512     C/EXEC SQL
008900100512     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
009000100512     C/END-EXEC
009100100512     C
009200100512     C*
009300161202     C* Inizializzo la transazione
009400161202     C*
009500001218     C                   exsr      cartbl
009600001218     C                   exsr      chkpar
009700001218     C                   exsr      procedi
009800160208     C*
009900160208     C* Finita l'elaborazione chiamo driver reperimento TITA4 in chiusura
010000160208     C                   CALL      'UBTA400R'
010100160208     C                   PARM                    UBTA4IOPZ
010200160208     C                   PARM      'C'           UBTA4ITLA
010300160208     C                   PARM                    UBTA4IAAS
010400160208     C                   PARM                    UBTA4ILNP
010500160208     C                   PARM                    UBTA4INRS
010600160208     C                   PARM                    UBTA4INSP
010700160208     C                   PARM                    UBTA4ITRC
010800160208     C                   PARM                    UBTA4OERR
010900160208     C                   PARM                    UBTA4ODS
011000160208     C                   PARM                    UBTA4OLEN
011100160208     C                   PARM                    UBTA4ODATI
011200070627     C*
011300070627     C                   if        FLGOPZ = '2'
011400161205     C***                exsr      forzaDWN
011500070627     C                   endif
011600001218     C*
011700001218     C                   seton                                        LR
011800001218
011900001218
012000001218
012100001218     C*------------------------------------------------------------------------*
012200001218     C* CHKPAR - Routine di controllo parametri ricevuti in input
012300001218     C*------------------------------------------------------------------------*
012400001218     C     CHKPAR        BEGSR
012500001218     C*
012600001218     C* Verifico le date
012700100512     C                   if        DATADA > *zeros AND
012800100512     C                             DATAA  = *zeros
012900001218     C                   movel     *all'9'       DATAA
013000001218     C                   endif
013100100512     C                   if        DACMDA > *zeros AND
013200100512     C                             DACMA  = *zeros
013300100512     C                   movel     *all'9'       DACMA
013400100512     C                   endif
013500050823     C*
013600050823     C* Verifico la modalità d scrittura output richiesta:
013700050823     C*   1 = crea membro "RIC" in file FNVAC00T
013800050823     C*   2 = scrivi record in file TIVGD00F
013900050823     C                   setoff                                       2526
014000050823     C                   if        FLGOPZ = '1'
014100050823     C                   seton                                        25
014200050823     C                   endif
014300050823     C                   if        FLGOPZ = '2'
014400050823     C                   seton                                        26
014500050823     C                   endif
014600001218     C*
014700001218     C                   ENDSR
014800001218     C*------------------------------------------------------------------------*
014900001218
015000001218
015100001218
015200001218     C*------------------------------------------------------------------------*
015300001218     C* PROCEDI - Routine principale
015400001218     C*------------------------------------------------------------------------*
015500001218     C     PROCEDI       BEGSR
015600050909     C*
015700050909     C* Inizializzo variabili d wrk
015800050909     C                   movel     'N'           wProcedi          1
015900050909     C*
016000050909     C* Apro il file di output
016100050909     C                   if        *in25 = *on
016200050909     C                   open      fnvac00t
016300050909     C                   movel     'S'           wProcedi
016400050909     C                   endif
016500050909     C*
016600050909     C                   if        *in26 = *on
016700050909     C                   open      tivgd00f
016800050909     C*
016900050909     C* Come prima cosa avvio il blocco elaborazione TIVGD x tipo file corrente: 'VC'
017000050909     C                   clear                   trul47ds
017100050909     C                   eval      d47opz  = 'I'
017200120927     C                   eval      d47tip  = TIPFILE
017300050909     C                   eval      d47lck  = 'N'
017400050909     C                   eval      d47chkj = 'N'
017500050909     C                   eval      d47pgm  = 'TIS7VCR'
017600050909     C                   call      'TRUL47R'
017700050909     C                   parm                    trul47ds
017800050909     C*
017900050909     C* Se elaborazione consentita => proseguo
018000120928     C***                if        d47sts <> 'A'
018100050909     C                   movel     'S'           wProcedi
018200120928     C***                endif
018300050909     C                   endif
018400050909     C*
018500050909     C* Se ok a procedere => elaboro
018600050909     C                   if        wProcedi = 'S'
018700001218     C*
018800001218     C* Ciclo sulla schiera dei clienti figli dell'unificante preso in input
018900001218     C                   z-add     1             I
019000090407     C                   sorta     KUNI
019100001218     C                   dow       KUNI(I) > *zeros
019200070718     C*
019300070718     C* Se richiesta elaborazione x codice cliente mittente...
019400100512     C                   select
019500100512     C                   when      TIPCLI = *blanks
019600100512     C                   exsr      EXECCM
019700070718     C*
019800070718     C* Se richiesta elaborazione x codice cliente fatturazione...
019900100512     C                   when      TIPCLI = 'K'
020000100512     C                   exsr      EXEKSC
020100100512     C*
020200100512     C* Se richiesta elaborazione x entrambi: codice cliente mittente + fatturazione...
020300100512     C                   when      TIPCLI = 'E'
020400100512     C                   exsr      EXECCM
020500100512     C                   exsr      EXEKSC
020600100512     C*
020700100512     C                   endsl
020800001218     C*
020900001218     C                   add       1             I
021000001218     C                   enddo
021100050909     C*
021200050909     C                   endif
021300001218     C*
021400001218     C* Chiudo il file di output
021500050823     C   25              close     fnvac00t
021600050823     C   26              close     tivgd00f
021700050909     C*
021800050909     C* Infine elimino il blocco elaborazione TIVGD x tipo file corrente: 'VC'
021900050909     C                   clear                   trul47ds
022000050909     C                   eval      d47opz  = 'F'
022100120928     C                   eval      d47tip  = TIPFILE
022200050909     C                   call      'TRUL47R'
022300050909     C                   parm                    trul47ds
022400001218     C*
022500001218     C                   ENDSR
022600001218     C*------------------------------------------------------------------------*
022700100512
022800100512
022900100512
023000100512     C*------------------------------------------------------------------------*
023100100512     C* EXECCM   - Elaborazione x codice cliente mittente
023200100512     C*------------------------------------------------------------------------*
023300100512     C     EXECCM        BEGSR
023400100521     C*
023500100521     C                   movel     'C'           wChkCli           1
023600100512     C*
023700100512     C                   select
023800100512     C* ...se richiesta elaborazione x data spedizione
023900100512     C                   when      DATADA > *zeros
024000100512     C                   exsr      SQ_CCMDSP
024100100512     C* ...se richiesta elaborazione x data consegna merce
024200100512     C                   when      DACMDA > *zeros
024300100512     C                   exsr      SQ_CCMDCM
024400100512     C                   endsl
024500100512     C*
024600100512     C                   ENDSR
024700100512     C*------------------------------------------------------------------------*
024800100512
024900100512
025000100512
025100100512     C*------------------------------------------------------------------------*
025200100512     C* EXEKSC   - Elaborazione x codice cliente fatturazione
025300100512     C*------------------------------------------------------------------------*
025400100512     C     EXEKSC        BEGSR
025500100521     C*
025600100521     C                   movel     'K'           wChkCli           1
025700100512     C*
025800100512     C                   select
025900100512     C* ...se richiesta elaborazione x data spedizione
026000100512     C                   when      DATADA > *zeros
026100100512     C                   exsr      SQ_KSCDSP
026200100512     C* ...se richiesta elaborazione x data consegna merce
026300100512     C                   when      DACMDA > *zeros
026400100512     C                   exsr      SQ_KSCDCM
026500100512     C                   endsl
026600100512     C*
026700100512     C                   ENDSR
026800100512     C*------------------------------------------------------------------------*
026900100512
027000100512
027100100512
027200100512     C*------------------------------------------------------------------------*
027300100512     C* SQ_CCMDSP - Elaborazione x codice cliente mittente + data spedizione
027400100512     C*------------------------------------------------------------------------*
027500100512     C     SQ_CCMDSP     BEGSR
027600100512     C*
027700100512     C                   z-add     KUNI(I)       wParCCM           7 0
027800100512     C*
027900100512     C/EXEC SQL
028000100512     C+ declare C_CCMDSP cursor for
028100100512     C+ select * from titas00f
028200100512     C+ where tasccm = :wParCCM and
028300100512     C+ tasaas*10000+tasmgs between :DATADA and :DATAA
028400100512     C+ union
028500100512     C+ select * from titas10f
028600100512     C+ where tasccm = :wParCCM and
028700100512     C+ tasaas*10000+tasmgs between :DATADA and :DATAA
028800100512     C+ union
028900100512     C+ select * from titasP0f
029000100512     C+ where tasccm = :wParCCM and
029100100512     C+ tasaas*10000+tasmgs between :DATADA and :DATAA
029200100512     C+ for read only
029300100512     C/END-EXEC
029400100512     C
029500100512     C/EXEC SQL
029600100512     C+ open C_CCMDSP
029700100512     C/END-EXEC
029800100512     C
029900100512     C/EXEC SQL
030000100512     C+ Fetch C_CCMDSP into :TITAS00F
030100100512     C/END-EXEC
030200100512     C*
030300100512     C                   dow       sqlcod = *zeros
030400100512     C                   exsr      verifica
030500100512     C                   if        CHKREC = 'S'
030600100512     C                   exsr      scriviVAC
030700100512     C                   endif
030800100512     C
030900100512     C/EXEC SQL
031000100512     C+ Fetch C_CCMDSP into :TITAS00F
031100100512     C/END-EXEC
031200100512     C*
031300100512     C                   enddo
031400100512     C*
031500100512     C/EXEC SQL
031600100512     C+ close C_CCMDSP
031700100512     C/END-EXEC
031800100512     C*
031900100512     C*
032000100512     C                   ENDSR
032100100512     C*------------------------------------------------------------------------*
032200100512
032300100512
032400100512
032500100512     C*------------------------------------------------------------------------*
032600100512     C* SQ_CCMDCM - Elaborazione x codice cliente mittente + data consegna merce
032700100512     C*------------------------------------------------------------------------*
032800100512     C     SQ_CCMDCM     BEGSR
032900100512     C*
033000100512     C                   z-add     KUNI(I)       wParCCM           7 0
033100100512     C*
033200100512     C/EXEC SQL
033300100512     C+ declare C_CCMDCM cursor for
033400100512     C+ select * from titas00f
033500100512     C+ where tasccm = :wParCCM and
033600100512     C+ tasdcm between :DACMDA and :DACMA
033700100512     C+ union
033800100512     C+ select * from titas10f
033900100512     C+ where tasccm = :wParCCM and
034000100512     C+ tasdcm between :DACMDA and :DACMA
034100100512     C+ union
034200100512     C+ select * from titasP0f
034300100512     C+ where tasccm = :wParCCM and
034400100512     C+ tasdcm between :DACMDA and :DACMA
034500100512     C+ for read only
034600100512     C/END-EXEC
034700100512     C
034800100512     C/EXEC SQL
034900100512     C+ open C_CCMDCM
035000100512     C/END-EXEC
035100100512     C
035200100512     C/EXEC SQL
035300100512     C+ Fetch C_CCMDCM into :TITAS00F
035400100512     C/END-EXEC
035500100512     C*
035600100512     C                   dow       sqlcod = *zeros
035700100512     C                   exsr      verifica
035800100512     C                   if        CHKREC = 'S'
035900100512     C                   exsr      scriviVAC
036000100512     C                   endif
036100100512     C
036200100512     C/EXEC SQL
036300100512     C+ Fetch C_CCMDCM into :TITAS00F
036400100512     C/END-EXEC
036500100512     C*
036600100512     C                   enddo
036700100512     C*
036800100512     C/EXEC SQL
036900100512     C+ close C_CCMDCM
037000100512     C/END-EXEC
037100100512     C*
037200100512     C*
037300100512     C                   ENDSR
037400100512     C*------------------------------------------------------------------------*
037500100512
037600100512
037700100512
037800100512     C*------------------------------------------------------------------------*
037900100512     C* SQ_KSCDSP - Elaborazione x codice cliente fatturazione + data spedizione
038000100512     C*------------------------------------------------------------------------*
038100100512     C     SQ_KSCDSP     BEGSR
038200100512     C*
038300100512     C                   z-add     KUNI(I)       wParKSC           7 0
038400100512     C*
038500100512     C/EXEC SQL
038600100512     C+ declare C_KSCDSP cursor for
038700100512     C+ select * from titas00f
038800100512     C+ where tasksc = :wParKSC and
038900100512     C+ tasaas*10000+tasmgs between :DATADA and :DATAA
039000100512     C+ union
039100100512     C+ select * from titas10f
039200100512     C+ where tasksc = :wParKSC and
039300100512     C+ tasaas*10000+tasmgs between :DATADA and :DATAA
039400100512     C+ union
039500100512     C+ select * from titasP0f
039600100512     C+ where tasksc = :wParKSC and
039700100512     C+ tasaas*10000+tasmgs between :DATADA and :DATAA
039800100512     C+ for read only
039900100512     C/END-EXEC
040000100512     C
040100100512     C/EXEC SQL
040200100512     C+ open C_KSCDSP
040300100512     C/END-EXEC
040400100512     C
040500100512     C/EXEC SQL
040600100512     C+ Fetch C_KSCDSP into :TITAS00F
040700100512     C/END-EXEC
040800100512     C*
040900100512     C                   dow       sqlcod = *zeros
041000100512     C                   exsr      verifica
041100100512     C                   if        CHKREC = 'S'
041200100512     C                   exsr      scriviVAC
041300100512     C                   endif
041400100512     C
041500100512     C/EXEC SQL
041600100512     C+ Fetch C_KSCDSP into :TITAS00F
041700100512     C/END-EXEC
041800100512     C*
041900100512     C                   enddo
042000100512     C*
042100100512     C/EXEC SQL
042200100512     C+ close C_KSCDSP
042300100512     C/END-EXEC
042400100512     C*
042500100512     C*
042600100512     C                   ENDSR
042700100512     C*------------------------------------------------------------------------*
042800100512
042900100512
043000100512
043100100512     C*------------------------------------------------------------------------*
043200100512     C* SQ_KSCDCM - Elaborazione x codice cliente fatturazione + data consegna merce
043300100512     C*------------------------------------------------------------------------*
043400100512     C     SQ_KSCDCM     BEGSR
043500100512     C*
043600100512     C                   z-add     KUNI(I)       wParKSC           7 0
043700100512     C*
043800100512     C/EXEC SQL
043900100512     C+ declare C_KSCDCM cursor for
044000100512     C+ select * from titas00f
044100100512     C+ where tasksc = :wParKSC and
044200100512     C+ tasdcm between :DACMDA and :DACMA
044300100512     C+ union
044400100512     C+ select * from titas10f
044500100512     C+ where tasksc = :wParKSC and
044600100512     C+ tasdcm between :DACMDA and :DACMA
044700100512     C+ union
044800100512     C+ select * from titasP0f
044900100512     C+ where tasksc = :wParKSC and
045000100512     C+ tasdcm between :DACMDA and :DACMA
045100100512     C+ for read only
045200100512     C/END-EXEC
045300100512     C
045400100512     C/EXEC SQL
045500100512     C+ open C_KSCDCM
045600100512     C/END-EXEC
045700100512     C
045800100512     C/EXEC SQL
045900100512     C+ Fetch C_KSCDCM into :TITAS00F
046000100512     C/END-EXEC
046100100512     C*
046200100512     C                   dow       sqlcod = *zeros
046300100512     C                   exsr      verifica
046400100512     C                   if        CHKREC = 'S'
046500100512     C                   exsr      scriviVAC
046600100512     C                   endif
046700100512     C
046800100512     C/EXEC SQL
046900100512     C+ Fetch C_KSCDCM into :TITAS00F
047000100512     C/END-EXEC
047100100512     C*
047200100512     C                   enddo
047300100512     C*
047400100512     C/EXEC SQL
047500100512     C+ close C_KSCDCM
047600100512     C/END-EXEC
047700100512     C*
047800100512     C*
047900100512     C                   ENDSR
048000100512     C*------------------------------------------------------------------------*
048100100512
048200100512
048300100512
048400100512     C*------------------------------------------------------------------------*
048500100512     C* CHKCONS  - Verifica se bolla effettivamente consegnata
048600100512     C*------------------------------------------------------------------------*
048700100512     C     CHKCONS       BEGSR
048800100512     C*
048900100512     C                   setoff                                       4041
049000100512     C                   movel     *blanks       wEsito1           1
049100100512     C                   movel     *blanks       wEsito2           1
049200100512     C*
049300100512     C                   eval      wAAS = tasAAS
049400100512     C                   eval      wLNP = tasLNP
049500100512     C                   eval      wNRS = tasNRS
049600100512     C                   eval      wNSP = tasNSP
049700100512     C*
049800100512     C* Chiamata metodo GetLblTyp
049900100512     C                   eval      wEsito1 = %char(UBLBLSPE_GetLblTyp(
050000100512     C                                                wAAS
050100100512     C                                               :wLNP
050200100512     C                                               :wNRS
050300100512     C                                               :wNSP
050400100512     C                                               :pOutLblTyp
050500100512     C                                               :pOutAnnoBO
050600100512     C                                               :pOutLineaParBO
050700100512     C                                               :pOutSerieBO
050800100512     C                                               :pOutNumSpedBO
050900100512     C                                               :pOutDcmBO
051000100512     C                                               :pOutCcaBO
051100100512     C                                               :pOutRblBO))
051200100512     C* Se trattasi d bolla nn originale reperisco la relativa bolla originale
051300100512     C                   if        pOutLblTyp <> 'O'
051400100512     C                   eval      wAAS = pOutAnnoBO
051500100512     C                   eval      wLNP = pOutLineaParBO
051600100512     C                   eval      wNRS = pOutSerieBO
051700100512     C                   eval      wNSP = pOutNumSpedBO
051800100512     C                   endif
051900100512     C*
052000100512     C* Chiamata metodo GetLastChild
052100100512     C                   eval      wEsito2 = %char(UBLBLSPE_GetLastChild(
052200100512     C                                                wAAS
052300100512     C                                               :wLNP
052400100512     C                                               :wNRS
052500100512     C                                               :wNSP
052600100512     C                                               :pOutAnnoFI
052700100512     C                                               :pOutLineaParFI
052800100512     C                                               :pOutSerieFI
052900100512     C                                               :pOutNumSpedFI
053000100512     C                                               :pOutDcmFI
053100100512     C                                               :pOutCcaFI))
053200100512     C*
053300100512     C* Considerazioni sullo stato corrente della bolla
053400100512     C                   if        (wEsito2 = '9' and pOutDcmBO > *zeros)  or
053500100512     C                             (wEsito2 = '0' and pOutDcmFI > *zeros)
053600100512     C                   seton                                        40        * bolla cons.
053700100512     C                   endif
053800100512     C*
053900100512     C                   if        (wEsito2 = '9' and pOutDcmBO > *zeros
054000100512     C                                            and pOutCcaBO = *blanks) or
054100100512     C                             (wEsito2 = '0' and pOutDcmFI > *zeros
054200100512     C                                            and pOutCcaFI = *blanks)
054300100512     C                   seton                                        41        * bolla cons. OK
054400100512     C                   endif
054500100512     C*
054600100512     C                   ENDSR
054700100512     C*------------------------------------------------------------------------*
054800001218
054900001218
055000001218
055100001218     C*------------------------------------------------------------------------*
055200001218     C* VERIFICA - Routine di verifica validità record corrente
055300001218     C*------------------------------------------------------------------------*
055400001218     C     VERIFICA      BEGSR
055500001218     C*
055600001218     C                   movel     'S'           CHKREC            1
055700001218     C*
055800100512     C* Verifico le date spedizione
055900001218     C                   if        CHKREC = 'S'
056000100512     C                   if        DATADA > *zeros
056100001218     C                   movel     tasaas        wrkdata           8 0
056200001218     C                   move      tasmgs        wrkdata
056300001218     C                   if        wrkdata < DATADA or
056400001218     C                             wrkdata > DATAA
056500001218     C                   movel     'N'           CHKREC
056600100512     C                   endif
056700001218     C                   endif
056800001218     C                   endif
056900100512     C*
057000100512     C* Verifico le date spedizione
057100100512     C                   if        CHKREC = 'S'
057200100512     C                   if        DACMDA > *zeros
057300100512     C                   if        tasdcm  < DACMDA or
057400100512     C                             tasdcm  > DACMA
057500100512     C                   movel     'N'           CHKREC
057600100512     C                   endif
057700100512     C                   endif
057800100512     C                   endif
057900100521     C*
058000100521     C* Verifico il codice cliente mittente
058100100521     C                   if        CHKREC = 'S'
058200100521     C                   if        wChkCli  = 'C'      AND
058300100521     C                             tasccm  <> KUNI(I)
058400100521     C                   movel     'N'           CHKREC
058500100521     C                   endif
058600100521     C                   endif
058700070718     C*
058800100517     C* Verifico il codice cliente fatturazione (in caso d 'K')
058900070718     C                   if        CHKREC = 'S'
059000100521     C                   if        wChkCli  = 'K'      AND
059100100521     C                             tasksc  <> KUNI(I)
059200070718     C                   movel     'N'           CHKREC
059300070718     C                   endif
059400070718     C                   endif
059500100517     C*
059600100517     C* Verifico il codice cliente fatturazione (in caso d 'E')
059700100517     C                   if        CHKREC = 'S'
059800100521     C                   if        TIPCLI  = 'E'       AND
059900100521     C                             wChkCli = 'K'       AND
060000100521     C                             tasksc  = tasccm
060100100517     C                   movel     'N'           CHKREC
060200100517     C                   endif
060300100517     C                   endif
060400001218     C*
060500020923     C* Verifico che la bolla abbia la data consegna se trattasi di bolla PT
060600001218     C                   if        CHKREC = 'S'
060700020923     C                   if        tasTSP = 'P'
060800020923     C                   if        tasDCM > *zeros
060900001218     C                   else
061000001218     C                   movel     'N'           CHKREC
061100001218     C                   endif
061200020923     C                   endif
061300001218     C                   endif
061400020920     C*
061500040322     C* Verifico che il tipo bolla sia da considerare ai fini della rigenerazione dell'FNVAC
061600020920     C                   if        CHKREC = 'S'
061700040322     C     tasCBO        lookup    COD3A                                  55
061800081016     C                   if        %equal
061900040322     C                   else
062000020920     C                   movel     'N'           CHKREC
062100020920     C                   endif
062200020920     C                   endif
062300010301     C*
062400010301     C* Verifico che la bolla se POSTE non abbia il codice CA = 5
062500010301     C                   if        CHKREC = 'S'
062600010301     C                   if        tasTSP = 'P' and tasCCA = '5'
062700010301     C                   movel     'N'           CHKREC
062800010301     C                   endif
062900010301     C                   endif
063000100512     C*
063100040322     C* Verifico se richiesto nel lancio che la bolla sia consegnata
063200040322     C                   if        CHKREC = 'S'
063300040322     C                   if        FLGCONS = 'S'
063400100512     C                   exsr      CHKCONS
063500100512     C                   if        *in40 = *on
063600040322     C                   else
063700040322     C                   movel     'N'           CHKREC
063800040322     C                   endif
063900040322     C                   endif
064000040322     C                   endif
064100100512     C*
064200100512     C* Verifico se richiesto nel lancio che la bolla sia consegnata OK
064300100512     C                   if        CHKREC = 'S'
064400100512     C                   if        FLGCONS = 'O'
064500100512     C                   exsr      CHKCONS
064600100512     C                   if        *in41 = *on
064700100512     C                   else
064800100512     C                   movel     'N'           CHKREC
064900100512     C                   endif
065000100512     C                   endif
065100100512     C                   endif
065200100513     C*
065300100907     C* Verifico se richiesto nel lancio che la bolla NON sia consegnata OK
065400100513     C                   if        CHKREC = 'S'
065500100513     C                   if        FLGCONS = 'K'
065600100513     C                   exsr      CHKCONS
065700100513     C                   if        *in41 = *on
065800100513     C                   movel     'N'           CHKREC
065900100513     C                   endif
066000100513     C                   endif
066100100513     C                   endif
066200100907     C*
066300100907     C* Verifico se richiesto nel lancio che la bolla NON sia consegnata
066400100907     C                   if        CHKREC = 'S'
066500100907     C                   if        FLGCONS = 'P'
066600100907     C                   exsr      CHKCONS
066700100907     C                   if        *in40 = *on
066800100907     C                   movel     'N'           CHKREC
066900100907     C                   endif
067000100907     C                   endif
067100100907     C                   endif
067200040322     C*
067300040322     C* Verifico se richiesto nel lancio che la bolla provenga da scambio dati VAS cliente
067400040322     C                   if        CHKREC = 'S'
067500040322     C                   if        FLGVAS = 'S'
067600040322     C                   if        tasSOP = '§' OR
067700040322     C                             tasSOP = ':' OR
067800040322     C                             tasSOP = '%'
067900040322     C                   else
068000040322     C                   movel     'N'           CHKREC
068100040322     C                   endif
068200040322     C                   endif
068300040322     C                   endif
068400081016     C*
068500081016     C* Verifico se richiesto che la bolla corrente sia la singola bolla richiesta a video
068600081016     C                   if        CHKREC = 'S'
068700081016     C                   if        KAAS   <> *zeros OR
068800081016     C                             KLNP   <> *zeros OR
068900081016     C                             KNRS   <> *zeros OR
069000081016     C                             KNSP   <> *zeros
069100081016     C                   if        tasAAS = KAAS AND
069200081016     C                             tasLNP = KLNP AND
069300081016     C                             tasNRS = KNRS AND
069400081016     C                             tasNSP = KNSP
069500081016     C                   else
069600081016     C                   movel     'N'           CHKREC
069700081016     C                   endif
069800081016     C                   endif
069900081016     C                   endif
070000001218     C*
070100001218     C                   ENDSR
070200001218     C*------------------------------------------------------------------------*
070300001218
070400001218
070500001218
070600001218     C*------------------------------------------------------------------------*
070700001218     C* SCRIVIVAC - Routine di scrittura file esiti di consegna (FNVAC)
070800001218     C*------------------------------------------------------------------------*
070900001218     C     SCRIVIVAC     BEGSR
071000001218     C*
071100001218     C                   CLEAR                   fnvac000
071200001218     C*
071300001218     C                   MOVEL     tasAAS        VACAAS
071400001218     C                   MOVEL     tasLNP        VACLNP
071500001218     C                   MOVEL     tasNRS        VACNRS
071600001218     C                   MOVEL     tasNSP        VACNSP
071700001218     C                   MOVEL     tasMGS        VACMGS
071800001218     C                   MOVEL     tasCBO        VACCBO
071900001218     C                   MOVEL     tasLNA        VACLNA
072000010223     C                   MOVEL     tasRSD        VACRSD
072100010223     C                   MOVEL     tasPRD        VACPRD
072200010301     C*
072300001218     C* Reperisco dall'estensione anagrafica bolle di SEDE i dati del Destinatario
072400001218     C                   EXSR      REPDEST
072500151019     C*
072600151019     C* Eseguo eventuali "forzature"
072700151019     C                   EXSR      RTVAR5FAT
072800160208     C*
072900160208     C* Reprrisco NATURA MERCE
073000160208     C                   EXSR      RTVNAS
073100001218     C*
073200001218     C                   MOVEL     tasGC1        VACGC1
073300001218     C                   MOVEL     tasGC2        VACGC2
073400001218     C                   MOVEL     tasCTR        VACCTR
073500001218     C                   MOVEL     tasCTS        VACCTS
073600001218     C                   MOVEL     tasFTM        VACFTM
073700001218     C                   MOVEL     tasFIN        VACFIN
073800001218     C                   MOVEL     tasFAP        VACFAP
073900001218     C                   MOVEL     tasTSP        VACTSP
074000001218     C* Effettuo considerazioni particolare per bolla "POSTE"
074100001218     C     tasTSP        IFEQ      'P'                                          BOLLA POSTE
074200001218     C                   EXSR      REPPTIAS
074300001218     C                   EXSR      REPPTGIA
074400001218     C                   Z-ADD     tasNCD        VACDCR
074500001218     C                   ELSE
074600001218     C                   MOVEL     tasIAS        VACIAS
074700001218     C                   MOVEL     tasVAS        VACVAS
074800001218     C                   MOVEL     tasDCR        VACDCR
074900050310     C     KGCP          CHAIN     TIGCP51L
075000050310     C                   IF        %found(TIGCP51L)
075100001218     C                   MOVEL     GCPAGC        VACDAG
075200001218     C                   MOVE      GCPMGC        VACDAG
075300001218     C                   ENDIF
075400001218     C                   ENDIF
075500001218     C*
075600160208     C                   MOVEL     §TA4ANAS      VACNAS
075700001218     C                   MOVEL     tasNCL        VACNCL
075800001218     C                   MOVEL     tasPKF        VACPKB
075900001218     C                   MOVEL     tasVLF        VACVLB
076000001218     C                   MOVEL     tasQFT        VACQFT
076100001218     C*
076200001218     C                   EXSR      REPCSB
076300001218     C*
076400001218     C                   MOVEL     tasCCM        VACCCM
076500001218     C                   MOVEL     tasRMN        VACRMN
076600010301     C*
076700001218     C                   EXSR      REPRMA
076800010301     C*
076900001218     C                   MOVEL     tasFFD        VACFFD
077000001218     C                   MOVEL     tasTCR        VACTCR
077100001218     C                   MOVEL     tasHCR        VACHCR
077200001218     C                   MOVEL     tasFTC        VACTC1
077300001218     C                   MOVEL     tasTC2        VACTC2
077400001218     C                   MOVEL     tasDCM        VACDCM
077500001218     C                   MOVEL     tasHMC        VACHMC
077600001218     C*
077700010301     C                   IF        tasTSP = 'P' and tasCCA = '7'
077800010301     C                   MOVEL     *blanks       VACCCA
077900010301     C                   ELSE
078000100910     C                   MOVEL     tasCCA        VACCCA
078100100910     C*
078200100910     C* Se CCA ancora nn valorizzato => reperisco l'ultimo evento
078300100910     C                   IF        VACCCA = *blanks
078400100910     C                   EXSR      REPLASTEV
078500010301     C                   ENDIF
078600100910     C                   ENDIF
078700010301     C*
078800120427     C* Reperisco l'eventuale lasciato avviso
078900001218     C                   EXSR      REPLAV
079000120427     C*
079100120427     C* Effettuo particolari operazioni in caso d DIROTTAMENTO
079200120427     C***                IF        VACCCA = '1' AND
079300120427     C***                          VACDCM = *zeros
079400120427     C***                EXSR      REPDIR
079500120427     C***                ENDIF
079600001218     C*
079700020923     C* Verifico che in caso di bolla NON PT siano valorizzate o la data consegna...
079800020923     C* ... o la data lasciato avviso o la data giacenza o la consegna anomala
079900020923     C                   IF        tasTSP <> 'P'
080000071228     C*
080100071228     C* Se richiesto nel lancio anche bolle senza eventi => gestisco...
080200071228     C* ...altrimenti standard
080300071228     C                   IF        FLGNOE  = 'N'
080400020923     C                   IF        vacDCM  > *zeros OR
080500020923     C                             vacDLA  > *zeros OR
080600020923     C                             vacDAG  > *zeros OR
080700020923     C                             vacCCA <> *blanks
080800071228     C   25              WRITE     FNVAC000
080900071228     C   26              exsr      WRITIVGD
081000020923     C                   endif
081100071228     C                   else
081200071228     C   25              WRITE     FNVAC000
081300071228     C   26              exsr      WRITIVGD
081400071228     C                   endif
081500020923     C                   else
081600050823     C   25              WRITE     FNVAC000
081700050823     C   26              exsr      WRITIVGD
081800020923     C                   endif
081900001218     C*
082000001218     C                   ENDSR
082100120427
082200120427
082300120427
082400120427     C*------------------------------------------------------------------------*
082500120427     C* REPDIR - Reperimento data DIROTTAMENTO
082600120427     C*------------------------------------------------------------------------*
082700120427     C     REPDIR        BEGSR
082800120427     C*
082900120427     C                   movel     *blanks       wEsito1           1
083000120427     C*
083100120427     C* Chiamata metodo GetLblTyp
083200120427     C                   eval      wEsito1 = %char(UBLBLSPE_GetLblTyp(
083300120427     C                                                tasAAS
083400120427     C                                               :tasLNP
083500120427     C                                               :tasNRS
083600120427     C                                               :tasNSP
083700120427     C                                               :pOutLblTyp
083800120427     C                                               :pOutAnnoBO
083900120427     C                                               :pOutLineaParBO
084000120427     C                                               :pOutSerieBO
084100120427     C                                               :pOutNumSpedBO
084200120427     C                                               :pOutDcmBO
084300120427     C                                               :pOutCcaBO
084400120427     C                                               :pOutRblBO))
084500120427     C*
084600120427     C* Se reperimento effettuato e trattasi d dirottamento => recupero la data
084700120427     C                   if        wEsito1 = '0' and pOutCcaBO = '1'
084800120427     C                   eval      VACDCM = pOutDcmBO
084900120427     C                   endif
085000120427     C*
085100120427     C                   ENDSR
085200120427     C*------------------------------------------------------------------------*
085300100910
085400100910
085500100910
085600100910
085700100910     C*------------------------------------------------------------------------*
085800100910     C* REPLASTEV - Routine di reperimento ultimo evento
085900100910     C*------------------------------------------------------------------------*
086000100910     C     REPLASTEV     BEGSR
086100100910     C*
086200100910     C     KEVB          SETGT     FNEVB01L
086300100910     C     KEVB          READPE    FNEVB01L                               34
086400100910     C*
086500100910     C* Devo cercare l'ultimo evento di tipo giacenza
086600100910     C                   IF        *IN34 = *OFF
086700100910     C*
086800100910     C* Evento 'MIC' considero solo se data evento >= data corrente
086900100910     C                   IF        EVBDEV >= datcor
087000100910     C* - MIC
087100100910     C                   IF        EVBCEV = 'MIC'
087200100910     C                   MOVEL     'C'           VACCCA
087300100910     C                   MOVEL     evbDEV        VACDCM
087400100910     C                   ENDIF
087500100910     C                   ENDIF
087600100910     C*
087700100910     C                   ENDIF
087800100910     C*
087900100910     C                   ENDSR
088000100910     C*------------------------------------------------------------------------*
088100050823
088200050823
088300050823
088400050823
088500050823     C*------------------------------------------------------------------------*
088600050823     C* WRITIVGD - Routine di scrittura file TIVGD (file VAS generico download)
088700050823     C*------------------------------------------------------------------------*
088800050823     C     WRITIVGD      BEGSR
088900050823     C*
089000050823     C                   clear                   tivgd000
089100080407     C                   eval      vgdDTA = %subst(FNVACDS:1:%size(FNVACDS))
089200100312     C                   eval      vgdTIP = TIPFILE
089300050823     C                   movel     *all'0'       vgdKSU
089400070525     C                   move      CODCLIVAS     vgdKSU
089500050823     C                   eval      vgdTSC = 'WW'
089600050823     C                   eval      vgdDAT = datcor
089700050901     C                   eval      vgdPGM = 'TIS7VCR'
089800070627     C*
089900070627     C                   clear                   DVGDFLO
090000070627     C                   movel     'P'           §VGDFELA
090100070627     C                   movel     DVGDFLO       vgdflo
090200070627     C*
090300050823     C                   write     tivgd000
090400050823     C*
090500050823     C                   ENDSR
090600050823     C*------------------------------------------------------------------------*
090700070627
090800070627
090900070627
091000070627     C*------------------------------------------------------------------------*
091100070627     C* FORZADWN - Routine di forzatura elaborazione immediata download
091200070627     C*------------------------------------------------------------------------*
091300070627     C     FORZADWN      BEGSR
091400070627     C*
091500070627     C                   clear                   tis781ds
091600070627     C                   eval      §781tip = vgdTIP
091700070627     C                   eval      §781ksu = vgdKSU
091800070627     C                   eval      §781tsc = vgdTSC
091900070627     C                   eval      §781dat = vgdDAT
092000070627     C                   eval      §781pgm = vgdPGM
092100080905     C*
092200080905     C                   clear                   TIS781DFLO
092300080905     C                   movel     'P'           §781floela
092400080905     C                   movel     TIS781DFLO    §781flo
092500070627     C*
092600070627     C* Finita l'elaborazione chiamo pgm
092700070627     C                   CALL      'TIS781C1'
092800070627     C                   parm      *blanks       esito             1
092900070627     C                   parm                    tis781ds
093000070627     C                   parm      *blanks       vgdPRG           10
093100070627     C*
093200070627     C                   ENDSR
093300070627     C*------------------------------------------------------------------------*
093400001218
093500001218
093600001218
093700001218
093800001218     C*------------------------------------------------------------------------*
093900001218     C* REPPTIAS - Routine di reperimento dati relativi all'importo/divisa da assicurare x POSTE
094000001218     C*------------------------------------------------------------------------*
094100001218     C     REPPTIAS      BEGSR
094200001218     C*
094300060215     C* Prendo importo da assicurare da TITA4 t.r.=J
094400001218     C                   movel     'J'           KTRC
094500001218     C     KTA4          chain     tita430c
094600001218     C                   if        %found(tita430c)
094700001218     C                   movel     ta4NOT        DSBL4J
094800001218     C                   z-add     §B4IAS        vacIAS
094900001218     C                   movel     §B4VAS        vacVAS
095000001218     C                   endif
095100001218     C*
095200001218     C                   ENDSR
095300001218     C*------------------------------------------------------------------------*
095400001218
095500001218
095600001218
095700001218
095800001218     C*------------------------------------------------------------------------*
095900001218     C* REPRMA - Routine di reperimento dati relativi al riferimento mittente alfabetico
096000001218     C*------------------------------------------------------------------------*
096100001218     C     REPRMA        BEGSR
096200001218     C*
096300060215     C* Prendo importo da assicurare da TITA4 t.r.=A
096400001218     C                   movel     'A'           KTRC
096500001218     C     KTA4          chain     tita430c
096600001218     C                   if        %found(tita430c)
096700001218     C                   eval      vacRMA = %subst(ta4NOT:1:15)
096800001218     C                   endif
096900001218     C*
097000001218     C                   ENDSR
097100001218     C*------------------------------------------------------------------------*
097200001218
097300001218
097400001218
097500001218
097600001218     C*------------------------------------------------------------------------*
097700001218     C* REPDEST - Routine di reperimento dati relativi al destinatario della bolla (TITAA)
097800001218     C*           ... e ai dati del mittente originale
097900001218     C*------------------------------------------------------------------------*
098000001218     C     REPDEST       BEGSR
098100001218     C*
098200010223     C* Prendo il tipo anagrafica = D
098300010223     C*                  movel     'D'           KTRC
098400010223     C*    KTAA          chain     titaa30c
098500010223     C*                  if        %found(titaa30c)
098600010223     C*                  movel     taaRSC        vacRSD
098700010223     C*                  movel     taaPRV        vacPRD
098800010223     C*                  endif
098900001218     C*
099000010223     C* Prendo il tipo anagrafica = O
099100001218     C                   movel     'O'           KTRC
099200001218     C     KTAA          chain     titaa30c
099300001218     C                   if        %found(titaa30c)
099400001218     C                   movel     taaRSC        vacRMO
099500001218     C                   endif
099600001218     C*
099700001218     C                   ENDSR
099800001218     C*------------------------------------------------------------------------*
099900001218
100000001218
100100001218
100200001218
100300001218     C*------------------------------------------------------------------------*
100400001218     C* REPCSB - Routine di reperimento dati relativi al contrassegno
100500001218     C*------------------------------------------------------------------------*
100600001218     C     REPCSB        BEGSR
100700001218     C*
100800001219     C     KCSB          chain     tncsb03l
100900001219     C                   if        %found(tncsb03l)
101000001218     C                   if        csbSTA = 9 and tasTSP <> 'P'
101100001218     C                   else
101200001218     C                   movel     csbTPA        vactic
101300001218     C                   move      csbTPI        vactic
101400001218     C                   z-add     csbCAS        vacCAS
101500001218     C                   movel     csbVCA        vacVCA
101600001218     C                   endif
101700001218     C                   endif
101800001218     C*
101900001218     C                   ENDSR
102000001218     C*------------------------------------------------------------------------*
102100001218
102200001218
102300001218
102400001218
102500001218     C*------------------------------------------------------------------------*
102600001218     C* REPPTGIA - Routine di reperimento dati relativi alla giacenza x POSTE
102700001218     C*------------------------------------------------------------------------*
102800001218     C     REPPTGIA      BEGSR
102900001218     C*
103000001218     C* Reperisco la data di giacenza dagli eventi poichè non vengono
103100001218     C* gestite le giacenze per le bolle poste
103200001218     C     KEVB          SETGT     FNEVB01L
103300001218     C     KEVB          READPE    FNEVB01L                               34
103400001218     C*
103500001218     C* Devo cercare l'ultimo evento di tipo giacenza
103600001218     C     *IN34         DOWEQ     *OFF
103700001218     C                   Z-ADD     1             W
103800001218     C     EVBCEV        LOOKUP    C2A(W)                                 42
103900001218     C     *IN42         IFEQ      '1'
104000001218     C     T2A(W)        ANDEQ     'G'
104100001218     C                   Z-ADD     EVBDEV        vacDAG
104200001218     C                   LEAVE
104300001218     C                   ELSE
104400001218     C     KEVB          READPE    FNEVB000                               34
104500001218     C                   ENDIF
104600001218     C                   ENDDO
104700001218     C*
104800001218     C                   ENDSR
104900001218     C*------------------------------------------------------------------------*
105000001218
105100001218
105200001218
105300001218
105400001218     C*------------------------------------------------------------------------*
105500001218     C* REPLAV - Routine di reperimento dati relativi al lasciato avviso
105600001218     C*------------------------------------------------------------------------*
105700001218     C     REPLAV        BEGSR
105800001218     C*
105900001218     C* Pererisco l'ultimo evento Lasciato Avviso
106000001218     C                   MOVEL     '2A'          COD
106100001218     C     KEVB          SETGT     FNEVB01L
106200001218     C     KEVB          READPE    FNEVB01L                               34
106300001218     C*
106400001218     C     *IN34         DOWEQ     *OFF
106500001218     C                   MOVEL(P)  EVBCEV        KEY
106600001218     C     KTAB2         CHAIN     TABEL                              30
106700001218     C     *IN30         IFEQ      *OFF
106800001218     C                   MOVEL     TBLUNI        DS2A
106900001218     C     §2AFTC        IFEQ      'A'
107000001218     C                   Z-ADD     EVBDEV        vacDLA
107100001218     C                   LEAVE
107200001218     C                   ENDIF
107300001218     C                   ENDIF
107400001218     C     KEVB          READPE    FNEVB000                               34
107500001218     C                   ENDDO
107600001218     C*
107700001218     C                   ENDSR
107800001218     C*------------------------------------------------------------------------*
107900001218
108000001218
108100001218
108200001218
108300001218     C*------------------------------------------------------------------------*
108400001218     C* CARTBL - Routine di caricamento dati tabellati
108500001218     C*------------------------------------------------------------------------*
108600001218     C     CARTBL        BEGSR
108700001218     C*
108800001218     C                   Z-ADD     0             W                 4 0
108900001218     C                   MOVEA     *HIVAL        T2A
109000001218     C                   MOVEA     *HIVAL        C2A
109100001218     C                   MOVEL     '2A'          COD
109200001218     C     KTAB          CHAIN     TABEL                              31
109300001218     C     *IN31         DOWEQ     '0'
109400001218     C     TBLFLG        IFEQ      ' '
109500001218     C                   MOVEL     TBLUNI        DS2A
109600001218     C     §2AFTC        IFNE      *BLANKS
109700001218     C                   ADD       1             W
109800001218     C                   MOVEL     §2AFTC        T2A(W)
109900001218     C                   MOVEL     TBLKEY        C2A(W)
110000001218     C                   ENDIF
110100001218     C                   ENDIF
110200001218     C     KTAB          READE     TABEL                                  31
110300001218     C                   ENDDO
110400001218     C*
110500081016     C                   Z-ADD     1             I                 4 0
110600011017     C                   MOVEL     CODCLI        KUNI(I)
110700081016     C                   IF        FLGUNI = 'U'
110800001218     C                   MOVEL     '3K'          COD
110900001218     C     KTAB          CHAIN     TABEL                              31
111000001218     C     *IN31         DOWEQ     '0'
111100001218     C     TBLFLG        IFEQ      ' '
111200001218     C                   MOVEL     TBLUNI        DS3K
111300010301     C     §3KCKS        IFEQ      CODCLI
111400001218     C                   ADD       1             I
111500081016     C                   MOVEL     TBLKEY        wKUNI             7 0
111600081016     C     wKUNI         LOOKUP    KUNI                                   55
111700081016     C                   IF        %equal
111800081016     C                   ELSE
111900081016     C                   Z-ADD     wKUNI         KUNI(I)
112000081016     C                   ENDIF
112100001218     C                   ENDIF
112200001218     C                   ENDIF
112300001218     C     KTAB          READE     TABEL                                  31
112400001218     C                   ENDDO
112500010301     C                   ENDIF
112600040312     C*
112700040312     C                   Z-ADD     0             J                 4 0
112800040312     C                   MOVEL     '3A'          COD
112900040312     C     KTAB          CHAIN     TABEL                              31
113000040312     C     *IN31         DOWEQ     '0'
113100040312     C     TBLFLG        IFEQ      ' '
113200040312     C                   MOVEL     TBLUNI        DS3A
113300040312     C     §3AVAC        IFEQ      'N'
113400040312     C                   ELSE
113500040312     C                   ADD       1             J
113600040312     C                   MOVEL     TBLKEY        COD3A(J)
113700040312     C                   ENDIF
113800040312     C                   ENDIF
113900040312     C     KTAB          READE     TABEL                                  31
114000040312     C                   ENDDO
114100001218     C*
114200001218     C                   ENDSR
114300001218     C*---------------------------------------------------------------*
114400151019
114500151019
114600151019
114700151019     C     RTVAR5FAT     BEGSR
114800151019     C*
114900151019     C* Se, presente, considero il peso/volume DESUNTO al pari del VDL TOTALE
115000151019     C                   SETOFF                                       7071
115100151019     C                   EVAL      ar5TRD = 'FAT'
115200151019     C                   CLEAR                   DAR5FAT
115300151019     C     KEYar531_P    CHAIN     FIAR531C
115400151019     C                   IF        %found(FIAR531C)
115500151019     C                   MOVEL     AR5UNI        DAR5FAT
115600151019     C                   IF        §AR5PKTAS > *zeros
115700151019     C                   SETON                                        70
115800151019     C                   EVAL      tasPKF = §AR5PKTAS
115900151019     C                   ENDIF
116000151019     C                   IF        §AR5VLTAS > *zeros
116100151019     C                   SETON                                        71
116200160505     C                   EVAL      tasVLF = §AR5VLTAS
116300151019     C                   ENDIF
116400151019     C                   ENDIF
116500151019     C*
116600151019     C                   ENDSR
116700160208
116800160208
116900160208
117000160208     C     RTVNAS        BEGSR
117100160208     C*
117200160208     C* Inizializzo le DS relative ai tipi record del TITA4 da gestire
117300160208     C                   CLEAR                   DTA4A
117400160208     C*
117500160208     C* Reperisco dal tipo record 'A' del TITA4 la natura merce della bolla corrente
117600160208     C                   CALL      'UBTA400R'
117700160208     C                   PARM      *blanks       UBTA4IOPZ
117800160208     C                   PARM      *blanks       UBTA4ITLA
117900160208     C                   PARM      tasAAS        UBTA4IAAS
118000160208     C                   PARM      tasLNP        UBTA4ILNP
118100160208     C                   PARM      tasNRS        UBTA4INRS
118200160208     C                   PARM      tasNSP        UBTA4INSP
118300160208     C                   PARM      'A'           UBTA4ITRC
118400160208     C                   PARM                    UBTA4OERR
118500160208     C                   PARM                    UBTA4ODS
118600160208     C                   PARM                    UBTA4OLEN
118700160208     C                   PARM                    UBTA4ODATI
118800160208     C*
118900160208     C                   IF        UBTA4OERR = *zeros
119000160208     C                   SELECT
119100160208     C* Gestione output tipo record 'A'
119200160208     C                   WHEN      UBTA4ODS = 'DTA4A'
119300160208     C                   EVAL      DTA4A = %subst(UBTA4ODATI:1:UBTA4OLEN)
119400160208     C                   ENDSL
119500160208     C*
119600160208     C                   ENDIF
119700160208     C*
119800160208     C                   ENDSR
119900001218
120000001218
120100001218
120200001218     C*------------------------------------------------------------------------*
120300001218     C* *INZSR - ROUTINE INIZIALE
120400001218     C*------------------------------------------------------------------------*
120500001218     C     *INZSR        BEGSR
120600001218     C*
120700001218     C     *ENTRY        PLIST
120800100512     C                   PARM                    PARAM            71
120900001218     C*
121000001218     C                   MOVEL     PARAM         DSINPUT
121100001218     C                   Z-ADD     1             CODUT
121200001218     C*
121300001218     C* Definizioni chiavi
121400001218     C     KTA4          KLIST
121500001218     C                   KFLD                    tasAAS
121600001218     C                   KFLD                    tasLNP
121700001218     C                   KFLD                    tasNRS
121800001218     C                   KFLD                    tasNSP
121900001218     C                   KFLD                    KTRC              1
122000001218     C*
122100001218     C     KTAA          KLIST
122200001218     C                   KFLD                    tasAAS
122300001218     C                   KFLD                    tasLNP
122400001218     C                   KFLD                    tasNRS
122500001218     C                   KFLD                    tasNSP
122600001218     C                   KFLD                    KTRC
122700001218     C*
122800001218     C     KCSB          KLIST
122900001218     C                   KFLD                    tasAAS
123000001218     C                   KFLD                    tasLNP
123100001218     C                   KFLD                    tasNRS
123200001218     C                   KFLD                    tasNSP
123300001218     C*
123400001218     C     KEVB          KLIST
123500001218     C                   KFLD                    tasAAS
123600001218     C                   KFLD                    tasLNP
123700001218     C                   KFLD                    tasNRS
123800001218     C                   KFLD                    tasNSP
123900001218     C*
124000001218     C     KGCP          KLIST
124100001218     C                   KFLD                    tasAAS
124200001218     C                   KFLD                    tasLNP
124300001218     C                   KFLD                    tasNRS
124400001218     C                   KFLD                    tasNSP
124500001218     C*
124600001218     C     KTAB          KLIST
124700001218     C                   KFLD                    CODUT             1 0
124800001218     C                   KFLD                    COD               2
124900001218     C*
125000001218     C     KTAB2         KLIST
125100001218     C                   KFLD                    CODUT             1 0
125200001218     C                   KFLD                    COD               2
125300001218     C                   KFLD                    KEY               8
125400151019     C*
125500151019     C* FIAR531C - Parziale
125600151019     C     KEYar531_P    KLIST
125700151019     C                   KFLD                    tasAAS
125800151019     C                   KFLD                    tasLNP
125900151019     C                   KFLD                    tasNRS
126000151019     C                   KFLD                    tasNSP
126100151019     C                   KFLD                    ar5TRD
126200001218     C*
126300001218     C* Determino la data corrente
126400100312     C                   Z-ADD     *zeros        DATCOR            8 0
126500100312     C                   EVAL      DATCOR = %dec(%date() : *ISO)
126600001218     C*
126700001218     C                   ENDSR

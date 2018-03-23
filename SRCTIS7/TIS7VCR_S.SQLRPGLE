000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200161205     H DFTACTGRP(*NO) BNDDIR('UBBNDDIR') ACTGRP(*CALLER)
000300011017     H*         - RITORNO DATI AL CLIENTE  da sede
000400000000     H*--------------------------------------------------------------*
000500001218     FTABEL00F  IF   E           K DISK
000600001218     FTITA430C  IF   E           K DISK
000700001218     FTITAA30C  IF   E           K DISK
000800001218     FFNEVB01L  IF   E           K DISK
000900001219     FTNCSB03L  IF   E           K DISK
001000050310     FTIGCP51L  IF   E           K DISK
001100151019     FFIAR531C  IF   E           K DISK
001200161205     FFNVAC00T  UF A E             DISK    USROPN COMMIT
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
002600161205     D psds           sds
002700161205     D  procname         *PROC
002800100512     D TITAS00F      E DS
002900900517     D KPJBA         E DS
003000921023     D DS3K          E DS
003100941228     D DS2A          E DS
003200040312     D DS3A          E DS
003300000526     D DSBL4J        E DS
003400151019     D DAR5FAT       E DS
003500050823     D FNVACDS       E DS                  EXTNAME(FNVAC00T)
003600161206     D iDSVGD        E DS                  INZ EXTNAME(TIVGD00F)
003700160208     D DTA4A         E DS
003800001218     D*--------------------
003900001218     D* DS DI RIDEFINIZIOINE PARAMETRI IN INPUT
004000001218     D*--------------------
004100050823     D DSINPUT         DS
004200050823     D  DATADA                        8  0
004300050823     D  DATAA                         8  0
004400050823     D  CODCLI                        7  0
004500050823     D  FLGUNI                        1
004600100512     D  FLGCONS                       1
004700050823     D  FLGVAS                        1
004800050823     D  FLGOPZ                        1
004900150707     D  CODCLIVAS                     7
005000100312     D  TIPFILE                       2
005100070627     D  FLGEXE                        1
005200100512     D  TIPCLI                        1
005300071228     D  FLGNOE                        1
005400081016     D  KAAS                          4  0
005500081016     D  KLNP                          3  0
005600081016     D  KNRS                          2  0
005700081016     D  KNSP                          7  0
005800100512     D  DACMDA                        8  0
005900100512     D  DACMA                         8  0
006000001218     D*--------------------
006100100512
006200100512
006300100512     D*------------------
006400100512     D* VARIABILI D WRK
006500100512     D*------------------
006600100512     D  wAAS           S                   like(tasAAS) inz
006700100512     D  wLNP           S                   like(tasLNP) inz
006800100512     D  wNRS           S                   like(tasNRS) inz
006900100512     D  wNSP           S                   like(tasNSP) inz
007000100512
007100001218
007200100512     D*------------------
007300100512     D* LINKING A DEFINIZIONI ESTERNE
007400100512     D*------------------
007500100512     D/COPY GAITRASRC/SRCPROTOPR,UBLBLSPE
007600100512     D/COPY GAITRASRC/SRCPROTOPI,UBLBLSPE
007700160208     D/COPY GAITRASRC/SRCPROTOPI,UBTA400R
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
009000161202     C*
009100001218     C                   exsr      cartbl
009200001218     C                   exsr      chkpar
009300001218     C                   exsr      procedi
009400161205     C                   exsr      endela
009500001218     C*
009600001218     C                   seton                                        LR
009700001218
009800001218
009900001218
010000001218     C*------------------------------------------------------------------------*
010100001218     C* CHKPAR - Routine di controllo parametri ricevuti in input
010200001218     C*------------------------------------------------------------------------*
010300001218     C     CHKPAR        BEGSR
010400001218     C*
010500001218     C* Verifico le date
010600100512     C                   if        DATADA > *zeros AND
010700100512     C                             DATAA  = *zeros
010800001218     C                   movel     *all'9'       DATAA
010900001218     C                   endif
011000100512     C                   if        DACMDA > *zeros AND
011100100512     C                             DACMA  = *zeros
011200100512     C                   movel     *all'9'       DACMA
011300100512     C                   endif
011400050823     C*
011500050823     C* Verifico la modalità d scrittura output richiesta:
011600050823     C*   1 = crea membro "RIC" in file FNVAC00T
011700050823     C*   2 = scrivi record in file TIVGD00F
011800050823     C                   setoff                                       2526
011900050823     C                   if        FLGOPZ = '1'
012000050823     C                   seton                                        25
012100050823     C                   endif
012200050823     C                   if        FLGOPZ = '2'
012300050823     C                   seton                                        26
012400050823     C                   endif
012500001218     C*
012600001218     C                   ENDSR
012700001218     C*------------------------------------------------------------------------*
012800001218
012900001218
013000001218
013100001218     C*------------------------------------------------------------------------*
013200001218     C* PROCEDI - Routine principale
013300001218     C*------------------------------------------------------------------------*
013400001218     C     PROCEDI       BEGSR
013500050909     C*
013600050909     C* Inizializzo variabili d wrk
013700050909     C                   movel     'N'           wProcedi          1
013800050909     C*
013900050909     C* Apro il file di output
014000161202     C                   if        *in25
014100050909     C                   open      fnvac00t
014200050909     C                   movel     'S'           wProcedi
014300050909     C                   endif
014400161202     C*
014500161202     C* Inizializzo la transazione
014600161202     C                   if        *in26
014700161202     C                   CALL      'TIS7VASR1'
014800161202     C                   PARM      '1'           iOPZ              1
014900161202     C                   PARM                    iDSVGD
015000161205     C                   PARM      *blanks       oPRG             10
015100161202     C                   PARM                    oOK               1
015200161205     C*
015300161205     C* Verifico esito chiamata
015400161205     C                   if        %error OR oOK = *off
015500161205     C                   exsr      EXEERR
015600161205     C                   endif
015700161202     C*
015800161202     C* Se OK => proseguo
015900161206     C                   if        oOK = *on
016000161202     C                   movel     'S'           wProcedi
016100161206     C* Forzo momentaneamente il progressivo  a *blanks
016200161206     C                   eval      oPRG = *blanks
016300161202     C                   endif
016400050909     C                   endif
016500050909     C*
016600050909     C* Se ok a procedere => elaboro
016700050909     C                   if        wProcedi = 'S'
016800001218     C*
016900001218     C* Ciclo sulla schiera dei clienti figli dell'unificante preso in input
017000001218     C                   z-add     1             I
017100090407     C                   sorta     KUNI
017200001218     C                   dow       KUNI(I) > *zeros
017300070718     C*
017400070718     C* Se richiesta elaborazione x codice cliente mittente...
017500100512     C                   select
017600100512     C                   when      TIPCLI = *blanks
017700100512     C                   exsr      EXECCM
017800070718     C*
017900070718     C* Se richiesta elaborazione x codice cliente fatturazione...
018000100512     C                   when      TIPCLI = 'K'
018100100512     C                   exsr      EXEKSC
018200100512     C*
018300100512     C* Se richiesta elaborazione x entrambi: codice cliente mittente + fatturazione...
018400100512     C                   when      TIPCLI = 'E'
018500100512     C                   exsr      EXECCM
018600100512     C                   exsr      EXEKSC
018700100512     C*
018800100512     C                   endsl
018900001218     C*
019000001218     C                   add       1             I
019100001218     C                   enddo
019200050909     C*
019300050909     C                   endif
019400001218     C*
019500001218     C* Chiudo il file di output
019600050823     C   25              close     fnvac00t
019700161202     C*
019800161202     C* Finalizzo la transazione
019900161202     C   26              CALL      'TIS7VASR1'
020000161205     C                   PARM      '3'           iOPZ
020100161205     C                   PARM                    iDSVGD
020200161206     C                   PARM                    oPRG
020300161205     C                   PARM                    oOK
020400161205     C*
020500161205     C* Verifico esito chiamata
020600161205     C                   if        %error OR oOK = *off
020700161205     C                   exsr      EXEERR
020800161205     C                   endif
020900001218     C*
021000001218     C                   ENDSR
021100001218     C*------------------------------------------------------------------------*
021200100512
021300100512
021400100512
021500100512     C*------------------------------------------------------------------------*
021600161205     C* EXECCM   - Elaborazione x codice cliente mittente
021700100512     C*------------------------------------------------------------------------*
021800100512     C     EXECCM        BEGSR
021900100521     C*
022000100521     C                   movel     'C'           wChkCli           1
022100100512     C*
022200100512     C                   select
022300100512     C* ...se richiesta elaborazione x data spedizione
022400100512     C                   when      DATADA > *zeros
022500100512     C                   exsr      SQ_CCMDSP
022600100512     C* ...se richiesta elaborazione x data consegna merce
022700100512     C                   when      DACMDA > *zeros
022800100512     C                   exsr      SQ_CCMDCM
022900100512     C                   endsl
023000100512     C*
023100100512     C                   ENDSR
023200100512     C*------------------------------------------------------------------------*
023300100512
023400100512
023500100512
023600100512     C*------------------------------------------------------------------------*
023700161205     C* EXEKSC   - Elaborazione x codice cliente fatturazione
023800100512     C*------------------------------------------------------------------------*
023900100512     C     EXEKSC        BEGSR
024000100521     C*
024100100521     C                   movel     'K'           wChkCli           1
024200100512     C*
024300100512     C                   select
024400100512     C* ...se richiesta elaborazione x data spedizione
024500100512     C                   when      DATADA > *zeros
024600100512     C                   exsr      SQ_KSCDSP
024700100512     C* ...se richiesta elaborazione x data consegna merce
024800100512     C                   when      DACMDA > *zeros
024900100512     C                   exsr      SQ_KSCDCM
025000100512     C                   endsl
025100100512     C*
025200100512     C                   ENDSR
025300100512     C*------------------------------------------------------------------------*
025400100512
025500100512
025600100512
025700100512     C*------------------------------------------------------------------------*
025800161205     C* SQ_CCMDSP - Elaborazione x codice cliente mittente + data spedizione
025900100512     C*------------------------------------------------------------------------*
026000100512     C     SQ_CCMDSP     BEGSR
026100100512     C*
026200100512     C                   z-add     KUNI(I)       wParCCM           7 0
026300100512     C*
026400100512     C/EXEC SQL
026500100512     C+ declare C_CCMDSP cursor for
026600100512     C+ select * from titas00f
026700100512     C+ where tasccm = :wParCCM and
026800100512     C+ tasaas*10000+tasmgs between :DATADA and :DATAA
026900100512     C+ union
027000100512     C+ select * from titas10f
027100100512     C+ where tasccm = :wParCCM and
027200100512     C+ tasaas*10000+tasmgs between :DATADA and :DATAA
027300100512     C+ union
027400100512     C+ select * from titasP0f
027500100512     C+ where tasccm = :wParCCM and
027600100512     C+ tasaas*10000+tasmgs between :DATADA and :DATAA
027700100512     C+ for read only
027800100512     C/END-EXEC
027900100512     C
028000100512     C/EXEC SQL
028100100512     C+ open C_CCMDSP
028200100512     C/END-EXEC
028300100512     C
028400100512     C/EXEC SQL
028500100512     C+ Fetch C_CCMDSP into :TITAS00F
028600100512     C/END-EXEC
028700100512     C*
028800100512     C                   dow       sqlcod = *zeros
028900100512     C                   exsr      verifica
029000100512     C                   if        CHKREC = 'S'
029100100512     C                   exsr      scriviVAC
029200100512     C                   endif
029300100512     C
029400100512     C/EXEC SQL
029500100512     C+ Fetch C_CCMDSP into :TITAS00F
029600100512     C/END-EXEC
029700100512     C*
029800100512     C                   enddo
029900100512     C*
030000100512     C/EXEC SQL
030100100512     C+ close C_CCMDSP
030200100512     C/END-EXEC
030300100512     C*
030400100512     C*
030500100512     C                   ENDSR
030600100512     C*------------------------------------------------------------------------*
030700100512
030800100512
030900100512
031000100512     C*------------------------------------------------------------------------*
031100161205     C* SQ_CCMDCM - Elaborazione x codice cliente mittente + data consegna merce
031200100512     C*------------------------------------------------------------------------*
031300100512     C     SQ_CCMDCM     BEGSR
031400100512     C*
031500100512     C                   z-add     KUNI(I)       wParCCM           7 0
031600100512     C*
031700100512     C/EXEC SQL
031800100512     C+ declare C_CCMDCM cursor for
031900100512     C+ select * from titas00f
032000100512     C+ where tasccm = :wParCCM and
032100100512     C+ tasdcm between :DACMDA and :DACMA
032200100512     C+ union
032300100512     C+ select * from titas10f
032400100512     C+ where tasccm = :wParCCM and
032500100512     C+ tasdcm between :DACMDA and :DACMA
032600100512     C+ union
032700100512     C+ select * from titasP0f
032800100512     C+ where tasccm = :wParCCM and
032900100512     C+ tasdcm between :DACMDA and :DACMA
033000100512     C+ for read only
033100100512     C/END-EXEC
033200100512     C
033300100512     C/EXEC SQL
033400100512     C+ open C_CCMDCM
033500100512     C/END-EXEC
033600100512     C
033700100512     C/EXEC SQL
033800100512     C+ Fetch C_CCMDCM into :TITAS00F
033900100512     C/END-EXEC
034000100512     C*
034100100512     C                   dow       sqlcod = *zeros
034200100512     C                   exsr      verifica
034300100512     C                   if        CHKREC = 'S'
034400100512     C                   exsr      scriviVAC
034500100512     C                   endif
034600100512     C
034700100512     C/EXEC SQL
034800100512     C+ Fetch C_CCMDCM into :TITAS00F
034900100512     C/END-EXEC
035000100512     C*
035100100512     C                   enddo
035200100512     C*
035300100512     C/EXEC SQL
035400100512     C+ close C_CCMDCM
035500100512     C/END-EXEC
035600100512     C*
035700100512     C*
035800100512     C                   ENDSR
035900100512     C*------------------------------------------------------------------------*
036000100512
036100100512
036200100512
036300100512     C*------------------------------------------------------------------------*
036400161205     C* SQ_KSCDSP - Elaborazione x codice cliente fatturazione + data spedizione
036500100512     C*------------------------------------------------------------------------*
036600100512     C     SQ_KSCDSP     BEGSR
036700100512     C*
036800100512     C                   z-add     KUNI(I)       wParKSC           7 0
036900100512     C*
037000100512     C/EXEC SQL
037100100512     C+ declare C_KSCDSP cursor for
037200100512     C+ select * from titas00f
037300100512     C+ where tasksc = :wParKSC and
037400100512     C+ tasaas*10000+tasmgs between :DATADA and :DATAA
037500100512     C+ union
037600100512     C+ select * from titas10f
037700100512     C+ where tasksc = :wParKSC and
037800100512     C+ tasaas*10000+tasmgs between :DATADA and :DATAA
037900100512     C+ union
038000100512     C+ select * from titasP0f
038100100512     C+ where tasksc = :wParKSC and
038200100512     C+ tasaas*10000+tasmgs between :DATADA and :DATAA
038300100512     C+ for read only
038400100512     C/END-EXEC
038500100512     C
038600100512     C/EXEC SQL
038700100512     C+ open C_KSCDSP
038800100512     C/END-EXEC
038900100512     C
039000100512     C/EXEC SQL
039100100512     C+ Fetch C_KSCDSP into :TITAS00F
039200100512     C/END-EXEC
039300100512     C*
039400100512     C                   dow       sqlcod = *zeros
039500100512     C                   exsr      verifica
039600100512     C                   if        CHKREC = 'S'
039700100512     C                   exsr      scriviVAC
039800100512     C                   endif
039900100512     C
040000100512     C/EXEC SQL
040100100512     C+ Fetch C_KSCDSP into :TITAS00F
040200100512     C/END-EXEC
040300100512     C*
040400100512     C                   enddo
040500100512     C*
040600100512     C/EXEC SQL
040700100512     C+ close C_KSCDSP
040800100512     C/END-EXEC
040900100512     C*
041000100512     C*
041100100512     C                   ENDSR
041200100512     C*------------------------------------------------------------------------*
041300100512
041400100512
041500100512
041600100512     C*------------------------------------------------------------------------*
041700161205     C* SQ_KSCDCM - Elaborazione x codice cliente fatturazione + data consegna merce
041800100512     C*------------------------------------------------------------------------*
041900100512     C     SQ_KSCDCM     BEGSR
042000100512     C*
042100100512     C                   z-add     KUNI(I)       wParKSC           7 0
042200100512     C*
042300100512     C/EXEC SQL
042400100512     C+ declare C_KSCDCM cursor for
042500100512     C+ select * from titas00f
042600100512     C+ where tasksc = :wParKSC and
042700100512     C+ tasdcm between :DACMDA and :DACMA
042800100512     C+ union
042900100512     C+ select * from titas10f
043000100512     C+ where tasksc = :wParKSC and
043100100512     C+ tasdcm between :DACMDA and :DACMA
043200100512     C+ union
043300100512     C+ select * from titasP0f
043400100512     C+ where tasksc = :wParKSC and
043500100512     C+ tasdcm between :DACMDA and :DACMA
043600100512     C+ for read only
043700100512     C/END-EXEC
043800100512     C
043900100512     C/EXEC SQL
044000100512     C+ open C_KSCDCM
044100100512     C/END-EXEC
044200100512     C
044300100512     C/EXEC SQL
044400100512     C+ Fetch C_KSCDCM into :TITAS00F
044500100512     C/END-EXEC
044600100512     C*
044700100512     C                   dow       sqlcod = *zeros
044800100512     C                   exsr      verifica
044900100512     C                   if        CHKREC = 'S'
045000100512     C                   exsr      scriviVAC
045100100512     C                   endif
045200100512     C
045300100512     C/EXEC SQL
045400100512     C+ Fetch C_KSCDCM into :TITAS00F
045500100512     C/END-EXEC
045600100512     C*
045700100512     C                   enddo
045800100512     C*
045900100512     C/EXEC SQL
046000100512     C+ close C_KSCDCM
046100100512     C/END-EXEC
046200100512     C*
046300100512     C*
046400100512     C                   ENDSR
046500100512     C*------------------------------------------------------------------------*
046600100512
046700100512
046800100512
046900100512     C*------------------------------------------------------------------------*
047000161205     C* CHKCONS  - Verifica se bolla effettivamente consegnata
047100100512     C*------------------------------------------------------------------------*
047200100512     C     CHKCONS       BEGSR
047300100512     C*
047400100512     C                   setoff                                       4041
047500100512     C                   movel     *blanks       wEsito1           1
047600100512     C                   movel     *blanks       wEsito2           1
047700100512     C*
047800100512     C                   eval      wAAS = tasAAS
047900100512     C                   eval      wLNP = tasLNP
048000100512     C                   eval      wNRS = tasNRS
048100100512     C                   eval      wNSP = tasNSP
048200100512     C*
048300100512     C* Chiamata metodo GetLblTyp
048400100512     C                   eval      wEsito1 = %char(UBLBLSPE_GetLblTyp(
048500100512     C                                                wAAS
048600100512     C                                               :wLNP
048700100512     C                                               :wNRS
048800100512     C                                               :wNSP
048900100512     C                                               :pOutLblTyp
049000100512     C                                               :pOutAnnoBO
049100100512     C                                               :pOutLineaParBO
049200100512     C                                               :pOutSerieBO
049300100512     C                                               :pOutNumSpedBO
049400100512     C                                               :pOutDcmBO
049500100512     C                                               :pOutCcaBO
049600100512     C                                               :pOutRblBO))
049700100512     C* Se trattasi d bolla nn originale reperisco la relativa bolla originale
049800100512     C                   if        pOutLblTyp <> 'O'
049900100512     C                   eval      wAAS = pOutAnnoBO
050000100512     C                   eval      wLNP = pOutLineaParBO
050100100512     C                   eval      wNRS = pOutSerieBO
050200100512     C                   eval      wNSP = pOutNumSpedBO
050300100512     C                   endif
050400100512     C*
050500100512     C* Chiamata metodo GetLastChild
050600100512     C                   eval      wEsito2 = %char(UBLBLSPE_GetLastChild(
050700100512     C                                                wAAS
050800100512     C                                               :wLNP
050900100512     C                                               :wNRS
051000100512     C                                               :wNSP
051100100512     C                                               :pOutAnnoFI
051200100512     C                                               :pOutLineaParFI
051300100512     C                                               :pOutSerieFI
051400100512     C                                               :pOutNumSpedFI
051500100512     C                                               :pOutDcmFI
051600100512     C                                               :pOutCcaFI))
051700100512     C*
051800100512     C* Considerazioni sullo stato corrente della bolla
051900100512     C                   if        (wEsito2 = '9' and pOutDcmBO > *zeros)  or
052000100512     C                             (wEsito2 = '0' and pOutDcmFI > *zeros)
052100161202     C                   seton                                        40        * bolla chiusa
052200100512     C                   endif
052300100512     C*
052400100512     C                   if        (wEsito2 = '9' and pOutDcmBO > *zeros
052500100512     C                                            and pOutCcaBO = *blanks) or
052600100512     C                             (wEsito2 = '0' and pOutDcmFI > *zeros
052700100512     C                                            and pOutCcaFI = *blanks)
052800100512     C                   seton                                        41        * bolla cons. OK
052900100512     C                   endif
053000100512     C*
053100100512     C                   ENDSR
053200100512     C*------------------------------------------------------------------------*
053300001218
053400001218
053500001218
053600001218     C*------------------------------------------------------------------------*
053700161205     C* VERIFICA - Routine di verifica validità record corrente
053800001218     C*------------------------------------------------------------------------*
053900001218     C     VERIFICA      BEGSR
054000001218     C*
054100001218     C                   movel     'S'           CHKREC            1
054200001218     C*
054300100512     C* Verifico le date spedizione
054400001218     C                   if        CHKREC = 'S'
054500100512     C                   if        DATADA > *zeros
054600001218     C                   movel     tasaas        wrkdata           8 0
054700001218     C                   move      tasmgs        wrkdata
054800001218     C                   if        wrkdata < DATADA or
054900001218     C                             wrkdata > DATAA
055000001218     C                   movel     'N'           CHKREC
055100100512     C                   endif
055200001218     C                   endif
055300001218     C                   endif
055400100512     C*
055500100512     C* Verifico le date spedizione
055600100512     C                   if        CHKREC = 'S'
055700100512     C                   if        DACMDA > *zeros
055800100512     C                   if        tasdcm  < DACMDA or
055900100512     C                             tasdcm  > DACMA
056000100512     C                   movel     'N'           CHKREC
056100100512     C                   endif
056200100512     C                   endif
056300100512     C                   endif
056400100521     C*
056500100521     C* Verifico il codice cliente mittente
056600100521     C                   if        CHKREC = 'S'
056700100521     C                   if        wChkCli  = 'C'      AND
056800100521     C                             tasccm  <> KUNI(I)
056900100521     C                   movel     'N'           CHKREC
057000100521     C                   endif
057100100521     C                   endif
057200070718     C*
057300100517     C* Verifico il codice cliente fatturazione (in caso d 'K')
057400070718     C                   if        CHKREC = 'S'
057500100521     C                   if        wChkCli  = 'K'      AND
057600100521     C                             tasksc  <> KUNI(I)
057700070718     C                   movel     'N'           CHKREC
057800070718     C                   endif
057900070718     C                   endif
058000100517     C*
058100100517     C* Verifico il codice cliente fatturazione (in caso d 'E')
058200100517     C                   if        CHKREC = 'S'
058300100521     C                   if        TIPCLI  = 'E'       AND
058400100521     C                             wChkCli = 'K'       AND
058500100521     C                             tasksc  = tasccm
058600100517     C                   movel     'N'           CHKREC
058700100517     C                   endif
058800100517     C                   endif
058900001218     C*
059000020923     C* Verifico che la bolla abbia la data consegna se trattasi di bolla PT
059100001218     C                   if        CHKREC = 'S'
059200020923     C                   if        tasTSP = 'P'
059300020923     C                   if        tasDCM > *zeros
059400001218     C                   else
059500001218     C                   movel     'N'           CHKREC
059600001218     C                   endif
059700020923     C                   endif
059800001218     C                   endif
059900020920     C*
060000040322     C* Verifico che il tipo bolla sia da considerare ai fini della rigenerazione dell'FNVAC
060100020920     C                   if        CHKREC = 'S'
060200040322     C     tasCBO        lookup    COD3A                                  55
060300081016     C                   if        %equal
060400040322     C                   else
060500020920     C                   movel     'N'           CHKREC
060600020920     C                   endif
060700020920     C                   endif
060800010301     C*
060900010301     C* Verifico che la bolla se POSTE non abbia il codice CA = 5
061000010301     C                   if        CHKREC = 'S'
061100010301     C                   if        tasTSP = 'P' and tasCCA = '5'
061200010301     C                   movel     'N'           CHKREC
061300010301     C                   endif
061400010301     C                   endif
061500100512     C*
061600040322     C* Verifico se richiesto nel lancio che la bolla sia consegnata
061700040322     C                   if        CHKREC = 'S'
061800040322     C                   if        FLGCONS = 'S'
061900100512     C                   exsr      CHKCONS
062000100512     C                   if        *in40 = *on
062100040322     C                   else
062200040322     C                   movel     'N'           CHKREC
062300040322     C                   endif
062400040322     C                   endif
062500040322     C                   endif
062600100512     C*
062700100512     C* Verifico se richiesto nel lancio che la bolla sia consegnata OK
062800100512     C                   if        CHKREC = 'S'
062900100512     C                   if        FLGCONS = 'O'
063000100512     C                   exsr      CHKCONS
063100100512     C                   if        *in41 = *on
063200100512     C                   else
063300100512     C                   movel     'N'           CHKREC
063400100512     C                   endif
063500100512     C                   endif
063600100512     C                   endif
063700100513     C*
063800100907     C* Verifico se richiesto nel lancio che la bolla NON sia consegnata OK
063900100513     C                   if        CHKREC = 'S'
064000100513     C                   if        FLGCONS = 'K'
064100100513     C                   exsr      CHKCONS
064200100513     C                   if        *in41 = *on
064300100513     C                   movel     'N'           CHKREC
064400100513     C                   endif
064500100513     C                   endif
064600100513     C                   endif
064700100907     C*
064800100907     C* Verifico se richiesto nel lancio che la bolla NON sia consegnata
064900100907     C                   if        CHKREC = 'S'
065000100907     C                   if        FLGCONS = 'P'
065100100907     C                   exsr      CHKCONS
065200100907     C                   if        *in40 = *on
065300100907     C                   movel     'N'           CHKREC
065400100907     C                   endif
065500100907     C                   endif
065600100907     C                   endif
065700040322     C*
065800040322     C* Verifico se richiesto nel lancio che la bolla provenga da scambio dati VAS cliente
065900040322     C                   if        CHKREC = 'S'
066000040322     C                   if        FLGVAS = 'S'
066100040322     C                   if        tasSOP = '§' OR
066200040322     C                             tasSOP = ':' OR
066300040322     C                             tasSOP = '%'
066400040322     C                   else
066500040322     C                   movel     'N'           CHKREC
066600040322     C                   endif
066700040322     C                   endif
066800040322     C                   endif
066900081016     C*
067000081016     C* Verifico se richiesto che la bolla corrente sia la singola bolla richiesta a video
067100081016     C                   if        CHKREC = 'S'
067200081016     C                   if        KAAS   <> *zeros OR
067300081016     C                             KLNP   <> *zeros OR
067400081016     C                             KNRS   <> *zeros OR
067500081016     C                             KNSP   <> *zeros
067600081016     C                   if        tasAAS = KAAS AND
067700081016     C                             tasLNP = KLNP AND
067800081016     C                             tasNRS = KNRS AND
067900081016     C                             tasNSP = KNSP
068000081016     C                   else
068100081016     C                   movel     'N'           CHKREC
068200081016     C                   endif
068300081016     C                   endif
068400081016     C                   endif
068500001218     C*
068600001218     C                   ENDSR
068700001218     C*------------------------------------------------------------------------*
068800001218
068900001218
069000001218
069100001218     C*------------------------------------------------------------------------*
069200001218     C* SCRIVIVAC - Routine di scrittura file esiti di consegna (FNVAC)
069300001218     C*------------------------------------------------------------------------*
069400001218     C     SCRIVIVAC     BEGSR
069500001218     C*
069600001218     C                   CLEAR                   fnvac000
069700001218     C*
069800001218     C                   MOVEL     tasAAS        VACAAS
069900001218     C                   MOVEL     tasLNP        VACLNP
070000001218     C                   MOVEL     tasNRS        VACNRS
070100001218     C                   MOVEL     tasNSP        VACNSP
070200001218     C                   MOVEL     tasMGS        VACMGS
070300001218     C                   MOVEL     tasCBO        VACCBO
070400001218     C                   MOVEL     tasLNA        VACLNA
070500010223     C                   MOVEL     tasRSD        VACRSD
070600010223     C                   MOVEL     tasPRD        VACPRD
070700010301     C*
070800001218     C* Reperisco dall'estensione anagrafica bolle di SEDE i dati del Destinatario
070900001218     C                   EXSR      REPDEST
071000151019     C*
071100151019     C* Eseguo eventuali "forzature"
071200151019     C                   EXSR      RTVAR5FAT
071300160208     C*
071400160208     C* Reprrisco NATURA MERCE
071500160208     C                   EXSR      RTVNAS
071600001218     C*
071700001218     C                   MOVEL     tasGC1        VACGC1
071800001218     C                   MOVEL     tasGC2        VACGC2
071900001218     C                   MOVEL     tasCTR        VACCTR
072000001218     C                   MOVEL     tasCTS        VACCTS
072100001218     C                   MOVEL     tasFTM        VACFTM
072200001218     C                   MOVEL     tasFIN        VACFIN
072300001218     C                   MOVEL     tasFAP        VACFAP
072400001218     C                   MOVEL     tasTSP        VACTSP
072500001218     C* Effettuo considerazioni particolare per bolla "POSTE"
072600001218     C     tasTSP        IFEQ      'P'                                          BOLLA POSTE
072700001218     C                   EXSR      REPPTIAS
072800001218     C                   EXSR      REPPTGIA
072900001218     C                   Z-ADD     tasNCD        VACDCR
073000001218     C                   ELSE
073100001218     C                   MOVEL     tasIAS        VACIAS
073200001218     C                   MOVEL     tasVAS        VACVAS
073300001218     C                   MOVEL     tasDCR        VACDCR
073400050310     C     KGCP          CHAIN     TIGCP51L
073500050310     C                   IF        %found(TIGCP51L)
073600001218     C                   MOVEL     GCPAGC        VACDAG
073700001218     C                   MOVE      GCPMGC        VACDAG
073800001218     C                   ENDIF
073900001218     C                   ENDIF
074000001218     C*
074100160208     C                   MOVEL     §TA4ANAS      VACNAS
074200001218     C                   MOVEL     tasNCL        VACNCL
074300001218     C                   MOVEL     tasPKF        VACPKB
074400001218     C                   MOVEL     tasVLF        VACVLB
074500001218     C                   MOVEL     tasQFT        VACQFT
074600001218     C*
074700001218     C                   EXSR      REPCSB
074800001218     C*
074900001218     C                   MOVEL     tasCCM        VACCCM
075000001218     C                   MOVEL     tasRMN        VACRMN
075100010301     C*
075200001218     C                   EXSR      REPRMA
075300010301     C*
075400001218     C                   MOVEL     tasFFD        VACFFD
075500001218     C                   MOVEL     tasTCR        VACTCR
075600001218     C                   MOVEL     tasHCR        VACHCR
075700001218     C                   MOVEL     tasFTC        VACTC1
075800001218     C                   MOVEL     tasTC2        VACTC2
075900001218     C                   MOVEL     tasDCM        VACDCM
076000001218     C                   MOVEL     tasHMC        VACHMC
076100001218     C*
076200010301     C                   IF        tasTSP = 'P' and tasCCA = '7'
076300010301     C                   MOVEL     *blanks       VACCCA
076400010301     C                   ELSE
076500100910     C                   MOVEL     tasCCA        VACCCA
076600100910     C*
076700100910     C* Se CCA ancora nn valorizzato => reperisco l'ultimo evento
076800100910     C                   IF        VACCCA = *blanks
076900100910     C                   EXSR      REPLASTEV
077000010301     C                   ENDIF
077100100910     C                   ENDIF
077200010301     C*
077300120427     C* Reperisco l'eventuale lasciato avviso
077400001218     C                   EXSR      REPLAV
077500120427     C*
077600120427     C* Effettuo particolari operazioni in caso d DIROTTAMENTO
077700120427     C***                IF        VACCCA = '1' AND
077800120427     C***                          VACDCM = *zeros
077900120427     C***                EXSR      REPDIR
078000120427     C***                ENDIF
078100001218     C*
078200020923     C* Verifico che in caso di bolla NON PT siano valorizzate o la data consegna...
078300020923     C* ... o la data lasciato avviso o la data giacenza o la consegna anomala
078400020923     C                   IF        tasTSP <> 'P'
078500071228     C*
078600071228     C* Se richiesto nel lancio anche bolle senza eventi => gestisco...
078700071228     C* ...altrimenti standard
078800071228     C                   IF        FLGNOE  = 'N'
078900020923     C                   IF        vacDCM  > *zeros OR
079000020923     C                             vacDLA  > *zeros OR
079100020923     C                             vacDAG  > *zeros OR
079200020923     C                             vacCCA <> *blanks
079300071228     C   25              WRITE     FNVAC000
079400161205     C   26              exsr      WRITIVGD
079500020923     C                   endif
079600071228     C                   else
079700071228     C   25              WRITE     FNVAC000
079800161205     C   26              exsr      WRITIVGD
079900071228     C                   endif
080000020923     C                   else
080100050823     C   25              WRITE     FNVAC000
080200161205     C   26              exsr      WRITIVGD
080300020923     C                   endif
080400001218     C*
080500001218     C                   ENDSR
080600120427
080700120427
080800120427
080900120427     C*------------------------------------------------------------------------*
081000161205     C* REPDIR - Reperimento data DIROTTAMENTO
081100120427     C*------------------------------------------------------------------------*
081200120427     C     REPDIR        BEGSR
081300120427     C*
081400120427     C                   movel     *blanks       wEsito1           1
081500120427     C*
081600120427     C* Chiamata metodo GetLblTyp
081700120427     C                   eval      wEsito1 = %char(UBLBLSPE_GetLblTyp(
081800120427     C                                                tasAAS
081900120427     C                                               :tasLNP
082000120427     C                                               :tasNRS
082100120427     C                                               :tasNSP
082200120427     C                                               :pOutLblTyp
082300120427     C                                               :pOutAnnoBO
082400120427     C                                               :pOutLineaParBO
082500120427     C                                               :pOutSerieBO
082600120427     C                                               :pOutNumSpedBO
082700120427     C                                               :pOutDcmBO
082800120427     C                                               :pOutCcaBO
082900120427     C                                               :pOutRblBO))
083000120427     C*
083100120427     C* Se reperimento effettuato e trattasi d dirottamento => recupero la data
083200120427     C                   if        wEsito1 = '0' and pOutCcaBO = '1'
083300120427     C                   eval      VACDCM = pOutDcmBO
083400120427     C                   endif
083500120427     C*
083600120427     C                   ENDSR
083700120427     C*------------------------------------------------------------------------*
083800100910
083900100910
084000100910
084100100910     C*------------------------------------------------------------------------*
084200161205     C* REPLASTEV - Routine di reperimento ultimo evento
084300100910     C*------------------------------------------------------------------------*
084400100910     C     REPLASTEV     BEGSR
084500100910     C*
084600100910     C     KEVB          SETGT     FNEVB01L
084700100910     C     KEVB          READPE    FNEVB01L                               34
084800100910     C*
084900100910     C* Devo cercare l'ultimo evento di tipo giacenza
085000100910     C                   IF        *IN34 = *OFF
085100100910     C*
085200100910     C* Evento 'MIC' considero solo se data evento >= data corrente
085300100910     C                   IF        EVBDEV >= datcor
085400100910     C* - MIC
085500100910     C                   IF        EVBCEV = 'MIC'
085600100910     C                   MOVEL     'C'           VACCCA
085700100910     C                   MOVEL     evbDEV        VACDCM
085800100910     C                   ENDIF
085900100910     C                   ENDIF
086000100910     C*
086100100910     C                   ENDIF
086200100910     C*
086300100910     C                   ENDSR
086400100910     C*------------------------------------------------------------------------*
086500050823
086600050823
086700050823
086800050823     C*------------------------------------------------------------------------*
086900161205     C* WRITIVGD - Routine di scrittura file TIVGD (file VAS generico download)
087000050823     C*------------------------------------------------------------------------*
087100161205     C     WRITIVGD      BEGSR
087200050823     C*
087300161205     C                   clear                   iDSVGD
087400080407     C                   eval      vgdDTA = %subst(FNVACDS:1:%size(FNVACDS))
087500100312     C                   eval      vgdTIP = TIPFILE
087600050823     C                   movel     *all'0'       vgdKSU
087700070525     C                   move      CODCLIVAS     vgdKSU
087800050823     C                   eval      vgdTSC = 'WW'
087900050823     C                   eval      vgdDAT = datcor
088000161205     C                   eval      vgdPRG = oPRG
088100161205     C                   eval      vgdPGM = procname
088200070627     C*
088300161205     C                   CALL      'TIS7VASR1'
088400161205     C                   PARM      '2'           iOPZ
088500161205     C                   PARM                    iDSVGD
088600161206     C                   PARM                    oPRG
088700161205     C                   PARM                    oOK
088800161205     C*
088900161205     C* Verifico esito chiamata
089000161205     C                   if        %error OR oOK = *off
089100161205     C                   exsr      EXEERR
089200161205     C                   endif
089300161205     C*
089400050823     C                   ENDSR
089500050823     C*------------------------------------------------------------------------*
089600001218
089700001218
089800161202
089900001218     C*------------------------------------------------------------------------*
090000001218     C* REPPTIAS - Routine di reperimento dati relativi all'importo/divisa da assicurare x POSTE
090100001218     C*------------------------------------------------------------------------*
090200001218     C     REPPTIAS      BEGSR
090300001218     C*
090400060215     C* Prendo importo da assicurare da TITA4 t.r.=J
090500001218     C                   movel     'J'           KTRC
090600001218     C     KTA4          chain     tita430c
090700001218     C                   if        %found(tita430c)
090800001218     C                   movel     ta4NOT        DSBL4J
090900001218     C                   z-add     §B4IAS        vacIAS
091000001218     C                   movel     §B4VAS        vacVAS
091100001218     C                   endif
091200001218     C*
091300001218     C                   ENDSR
091400001218     C*------------------------------------------------------------------------*
091500001218
091600001218
091700001218
091800001218     C*------------------------------------------------------------------------*
091900001218     C* REPRMA - Routine di reperimento dati relativi al riferimento mittente alfabetico
092000001218     C*------------------------------------------------------------------------*
092100001218     C     REPRMA        BEGSR
092200001218     C*
092300060215     C* Prendo importo da assicurare da TITA4 t.r.=A
092400001218     C                   movel     'A'           KTRC
092500001218     C     KTA4          chain     tita430c
092600001218     C                   if        %found(tita430c)
092700001218     C                   eval      vacRMA = %subst(ta4NOT:1:15)
092800001218     C                   endif
092900001218     C*
093000001218     C                   ENDSR
093100001218     C*------------------------------------------------------------------------*
093200001218
093300001218
093400001218
093500001218     C*------------------------------------------------------------------------*
093600001218     C* REPDEST - Routine di reperimento dati relativi al destinatario della bolla (TITAA)
093700001218     C*           ... e ai dati del mittente originale
093800001218     C*------------------------------------------------------------------------*
093900001218     C     REPDEST       BEGSR
094000001218     C*
094100010223     C* Prendo il tipo anagrafica = D
094200010223     C*                  movel     'D'           KTRC
094300010223     C*    KTAA          chain     titaa30c
094400010223     C*                  if        %found(titaa30c)
094500010223     C*                  movel     taaRSC        vacRSD
094600010223     C*                  movel     taaPRV        vacPRD
094700010223     C*                  endif
094800001218     C*
094900010223     C* Prendo il tipo anagrafica = O
095000001218     C                   movel     'O'           KTRC
095100001218     C     KTAA          chain     titaa30c
095200001218     C                   if        %found(titaa30c)
095300001218     C                   movel     taaRSC        vacRMO
095400001218     C                   endif
095500001218     C*
095600001218     C                   ENDSR
095700001218     C*------------------------------------------------------------------------*
095800001218
095900001218
096000001218
096100001218     C*------------------------------------------------------------------------*
096200001218     C* REPCSB - Routine di reperimento dati relativi al contrassegno
096300001218     C*------------------------------------------------------------------------*
096400001218     C     REPCSB        BEGSR
096500001218     C*
096600001219     C     KCSB          chain     tncsb03l
096700001219     C                   if        %found(tncsb03l)
096800001218     C                   if        csbSTA = 9 and tasTSP <> 'P'
096900001218     C                   else
097000001218     C                   movel     csbTPA        vactic
097100001218     C                   move      csbTPI        vactic
097200001218     C                   z-add     csbCAS        vacCAS
097300001218     C                   movel     csbVCA        vacVCA
097400001218     C                   endif
097500001218     C                   endif
097600001218     C*
097700001218     C                   ENDSR
097800001218     C*------------------------------------------------------------------------*
097900001218
098000001218
098100001218
098200001218     C*------------------------------------------------------------------------*
098300001218     C* REPPTGIA - Routine di reperimento dati relativi alla giacenza x POSTE
098400001218     C*------------------------------------------------------------------------*
098500001218     C     REPPTGIA      BEGSR
098600001218     C*
098700001218     C* Reperisco la data di giacenza dagli eventi poichè non vengono
098800001218     C* gestite le giacenze per le bolle poste
098900001218     C     KEVB          SETGT     FNEVB01L
099000001218     C     KEVB          READPE    FNEVB01L                               34
099100001218     C*
099200001218     C* Devo cercare l'ultimo evento di tipo giacenza
099300001218     C     *IN34         DOWEQ     *OFF
099400001218     C                   Z-ADD     1             W
099500001218     C     EVBCEV        LOOKUP    C2A(W)                                 42
099600001218     C     *IN42         IFEQ      '1'
099700001218     C     T2A(W)        ANDEQ     'G'
099800001218     C                   Z-ADD     EVBDEV        vacDAG
099900001218     C                   LEAVE
100000001218     C                   ELSE
100100001218     C     KEVB          READPE    FNEVB000                               34
100200001218     C                   ENDIF
100300001218     C                   ENDDO
100400001218     C*
100500001218     C                   ENDSR
100600001218     C*------------------------------------------------------------------------*
100700001218
100800001218
100900001218
101000001218
101100001218     C*------------------------------------------------------------------------*
101200001218     C* REPLAV - Routine di reperimento dati relativi al lasciato avviso
101300001218     C*------------------------------------------------------------------------*
101400001218     C     REPLAV        BEGSR
101500001218     C*
101600001218     C* Pererisco l'ultimo evento Lasciato Avviso
101700001218     C                   MOVEL     '2A'          COD
101800001218     C     KEVB          SETGT     FNEVB01L
101900001218     C     KEVB          READPE    FNEVB01L                               34
102000001218     C*
102100001218     C     *IN34         DOWEQ     *OFF
102200001218     C                   MOVEL(P)  EVBCEV        KEY
102300001218     C     KTAB2         CHAIN     TABEL                              30
102400001218     C     *IN30         IFEQ      *OFF
102500001218     C                   MOVEL     TBLUNI        DS2A
102600001218     C     §2AFTC        IFEQ      'A'
102700001218     C                   Z-ADD     EVBDEV        vacDLA
102800001218     C                   LEAVE
102900001218     C                   ENDIF
103000001218     C                   ENDIF
103100001218     C     KEVB          READPE    FNEVB000                               34
103200001218     C                   ENDDO
103300001218     C*
103400001218     C                   ENDSR
103500001218     C*------------------------------------------------------------------------*
103600001218
103700001218
103800001218
103900001218     C*------------------------------------------------------------------------*
104000001218     C* CARTBL - Routine di caricamento dati tabellati
104100001218     C*------------------------------------------------------------------------*
104200001218     C     CARTBL        BEGSR
104300001218     C*
104400001218     C                   Z-ADD     0             W                 4 0
104500001218     C                   MOVEA     *HIVAL        T2A
104600001218     C                   MOVEA     *HIVAL        C2A
104700001218     C                   MOVEL     '2A'          COD
104800001218     C     KTAB          CHAIN     TABEL                              31
104900001218     C     *IN31         DOWEQ     '0'
105000001218     C     TBLFLG        IFEQ      ' '
105100001218     C                   MOVEL     TBLUNI        DS2A
105200001218     C     §2AFTC        IFNE      *BLANKS
105300001218     C                   ADD       1             W
105400001218     C                   MOVEL     §2AFTC        T2A(W)
105500001218     C                   MOVEL     TBLKEY        C2A(W)
105600001218     C                   ENDIF
105700001218     C                   ENDIF
105800001218     C     KTAB          READE     TABEL                                  31
105900001218     C                   ENDDO
106000001218     C*
106100081016     C                   Z-ADD     1             I                 4 0
106200011017     C                   MOVEL     CODCLI        KUNI(I)
106300081016     C                   IF        FLGUNI = 'U'
106400001218     C                   MOVEL     '3K'          COD
106500001218     C     KTAB          CHAIN     TABEL                              31
106600001218     C     *IN31         DOWEQ     '0'
106700001218     C     TBLFLG        IFEQ      ' '
106800001218     C                   MOVEL     TBLUNI        DS3K
106900010301     C     §3KCKS        IFEQ      CODCLI
107000001218     C                   ADD       1             I
107100081016     C                   MOVEL     TBLKEY        wKUNI             7 0
107200081016     C     wKUNI         LOOKUP    KUNI                                   55
107300081016     C                   IF        %equal
107400081016     C                   ELSE
107500081016     C                   Z-ADD     wKUNI         KUNI(I)
107600081016     C                   ENDIF
107700001218     C                   ENDIF
107800001218     C                   ENDIF
107900001218     C     KTAB          READE     TABEL                                  31
108000001218     C                   ENDDO
108100010301     C                   ENDIF
108200040312     C*
108300040312     C                   Z-ADD     0             J                 4 0
108400040312     C                   MOVEL     '3A'          COD
108500040312     C     KTAB          CHAIN     TABEL                              31
108600040312     C     *IN31         DOWEQ     '0'
108700040312     C     TBLFLG        IFEQ      ' '
108800040312     C                   MOVEL     TBLUNI        DS3A
108900040312     C     §3AVAC        IFEQ      'N'
109000040312     C                   ELSE
109100040312     C                   ADD       1             J
109200040312     C                   MOVEL     TBLKEY        COD3A(J)
109300040312     C                   ENDIF
109400040312     C                   ENDIF
109500040312     C     KTAB          READE     TABEL                                  31
109600040312     C                   ENDDO
109700001218     C*
109800001218     C                   ENDSR
109900001218     C*---------------------------------------------------------------*
110000151019
110100151019
110200151019
110300151019     C     RTVAR5FAT     BEGSR
110400151019     C*
110500151019     C* Se, presente, considero il peso/volume DESUNTO al pari del VDL TOTALE
110600151019     C                   SETOFF                                       7071
110700151019     C                   EVAL      ar5TRD = 'FAT'
110800151019     C                   CLEAR                   DAR5FAT
110900151019     C     KEYar531_P    CHAIN     FIAR531C
111000151019     C                   IF        %found(FIAR531C)
111100151019     C                   MOVEL     AR5UNI        DAR5FAT
111200151019     C                   IF        §AR5PKTAS > *zeros
111300151019     C                   SETON                                        70
111400151019     C                   EVAL      tasPKF = §AR5PKTAS
111500151019     C                   ENDIF
111600151019     C                   IF        §AR5VLTAS > *zeros
111700151019     C                   SETON                                        71
111800160505     C                   EVAL      tasVLF = §AR5VLTAS
111900151019     C                   ENDIF
112000151019     C                   ENDIF
112100151019     C*
112200151019     C                   ENDSR
112300160208
112400160208
112500160208
112600160208     C     RTVNAS        BEGSR
112700160208     C*
112800160208     C* Inizializzo le DS relative ai tipi record del TITA4 da gestire
112900160208     C                   CLEAR                   DTA4A
113000160208     C*
113100160208     C* Reperisco dal tipo record 'A' del TITA4 la natura merce della bolla corrente
113200160208     C                   CALL      'UBTA400R'
113300160208     C                   PARM      *blanks       UBTA4IOPZ
113400160208     C                   PARM      *blanks       UBTA4ITLA
113500160208     C                   PARM      tasAAS        UBTA4IAAS
113600160208     C                   PARM      tasLNP        UBTA4ILNP
113700160208     C                   PARM      tasNRS        UBTA4INRS
113800160208     C                   PARM      tasNSP        UBTA4INSP
113900160208     C                   PARM      'A'           UBTA4ITRC
114000160208     C                   PARM                    UBTA4OERR
114100160208     C                   PARM                    UBTA4ODS
114200160208     C                   PARM                    UBTA4OLEN
114300160208     C                   PARM                    UBTA4ODATI
114400160208     C*
114500160208     C                   IF        UBTA4OERR = *zeros
114600160208     C                   SELECT
114700160208     C* Gestione output tipo record 'A'
114800160208     C                   WHEN      UBTA4ODS = 'DTA4A'
114900160208     C                   EVAL      DTA4A = %subst(UBTA4ODATI:1:UBTA4OLEN)
115000160208     C                   ENDSL
115100160208     C*
115200160208     C                   ENDIF
115300160208     C*
115400160208     C                   ENDSR
115500161205
115600161205
115700161205
115800161205     C*------------------------------------------------------------------------*
115900161205     C* ENDELA - Routine di lanci di chiusura finali
116000161205     C*------------------------------------------------------------------------*
116100161205     C     ENDELA        BEGSR
116200161205     C*
116300161205     C* Finita l'elaborazione chiamo driver reperimento TITA4 in chiusura
116400161205     C                   CALL      'UBTA400R'
116500161205     C                   PARM                    UBTA4IOPZ
116600161205     C                   PARM      'C'           UBTA4ITLA
116700161205     C                   PARM                    UBTA4IAAS
116800161205     C                   PARM                    UBTA4ILNP
116900161205     C                   PARM                    UBTA4INRS
117000161205     C                   PARM                    UBTA4INSP
117100161205     C                   PARM                    UBTA4ITRC
117200161205     C                   PARM                    UBTA4OERR
117300161205     C                   PARM                    UBTA4ODS
117400161205     C                   PARM                    UBTA4OLEN
117500161205     C                   PARM                    UBTA4ODATI
117600161205     C*
117700161205     C                   ENDSR
117800161205     C*------------------------------------------------------------------------*
117900161205
118000161205
118100161205
118200161205     C*------------------------------------------------------------------------*
118300161205     C* EXEERR - Routine di esecuzione azioni su Errore
118400161205     C*------------------------------------------------------------------------*
118500161205     C     EXEERR        BEGSR
118600161205     C*
118700161205     C                   dump(A)
118800161205     C                   rolbk
118900161205     C                   seton                                        lr
119000161205     C                   return
119100161205     C*
119200161205     C                   ENDSR
119300161205     C*------------------------------------------------------------------------*
119400001218
119500001218
119600001218
119700001218     C*------------------------------------------------------------------------*
119800001218     C* *INZSR - ROUTINE INIZIALE
119900001218     C*------------------------------------------------------------------------*
120000001218     C     *INZSR        BEGSR
120100001218     C*
120200001218     C     *ENTRY        PLIST
120300100512     C                   PARM                    PARAM            71
120400001218     C*
120500001218     C                   MOVEL     PARAM         DSINPUT
120600001218     C                   Z-ADD     1             CODUT
120700001218     C*
120800001218     C* Definizioni chiavi
120900001218     C     KTA4          KLIST
121000001218     C                   KFLD                    tasAAS
121100001218     C                   KFLD                    tasLNP
121200001218     C                   KFLD                    tasNRS
121300001218     C                   KFLD                    tasNSP
121400001218     C                   KFLD                    KTRC              1
121500001218     C*
121600001218     C     KTAA          KLIST
121700001218     C                   KFLD                    tasAAS
121800001218     C                   KFLD                    tasLNP
121900001218     C                   KFLD                    tasNRS
122000001218     C                   KFLD                    tasNSP
122100001218     C                   KFLD                    KTRC
122200001218     C*
122300001218     C     KCSB          KLIST
122400001218     C                   KFLD                    tasAAS
122500001218     C                   KFLD                    tasLNP
122600001218     C                   KFLD                    tasNRS
122700001218     C                   KFLD                    tasNSP
122800001218     C*
122900001218     C     KEVB          KLIST
123000001218     C                   KFLD                    tasAAS
123100001218     C                   KFLD                    tasLNP
123200001218     C                   KFLD                    tasNRS
123300001218     C                   KFLD                    tasNSP
123400001218     C*
123500001218     C     KGCP          KLIST
123600001218     C                   KFLD                    tasAAS
123700001218     C                   KFLD                    tasLNP
123800001218     C                   KFLD                    tasNRS
123900001218     C                   KFLD                    tasNSP
124000001218     C*
124100001218     C     KTAB          KLIST
124200001218     C                   KFLD                    CODUT             1 0
124300001218     C                   KFLD                    COD               2
124400001218     C*
124500001218     C     KTAB2         KLIST
124600001218     C                   KFLD                    CODUT             1 0
124700001218     C                   KFLD                    COD               2
124800001218     C                   KFLD                    KEY               8
124900151019     C*
125000151019     C* FIAR531C - Parziale
125100151019     C     KEYar531_P    KLIST
125200151019     C                   KFLD                    tasAAS
125300151019     C                   KFLD                    tasLNP
125400151019     C                   KFLD                    tasNRS
125500151019     C                   KFLD                    tasNSP
125600151019     C                   KFLD                    ar5TRD
125700001218     C*
125800001218     C* Determino la data corrente
125900100312     C                   Z-ADD     *zeros        DATCOR            8 0
126000100312     C                   EVAL      DATCOR = %dec(%date() : *ISO)
126100001218     C*
126200001218     C                   ENDSR

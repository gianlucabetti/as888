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
015900161205     C                   if        oOK = *on AND oPRG <> *blanks
016000161202     C                   movel     'S'           wProcedi
016100161202     C                   endif
016200050909     C                   endif
016300050909     C*
016400050909     C* Se ok a procedere => elaboro
016500050909     C                   if        wProcedi = 'S'
016600001218     C*
016700001218     C* Ciclo sulla schiera dei clienti figli dell'unificante preso in input
016800001218     C                   z-add     1             I
016900090407     C                   sorta     KUNI
017000001218     C                   dow       KUNI(I) > *zeros
017100070718     C*
017200070718     C* Se richiesta elaborazione x codice cliente mittente...
017300100512     C                   select
017400100512     C                   when      TIPCLI = *blanks
017500100512     C                   exsr      EXECCM
017600070718     C*
017700070718     C* Se richiesta elaborazione x codice cliente fatturazione...
017800100512     C                   when      TIPCLI = 'K'
017900100512     C                   exsr      EXEKSC
018000100512     C*
018100100512     C* Se richiesta elaborazione x entrambi: codice cliente mittente + fatturazione...
018200100512     C                   when      TIPCLI = 'E'
018300100512     C                   exsr      EXECCM
018400100512     C                   exsr      EXEKSC
018500100512     C*
018600100512     C                   endsl
018700001218     C*
018800001218     C                   add       1             I
018900001218     C                   enddo
019000050909     C*
019100050909     C                   endif
019200001218     C*
019300001218     C* Chiudo il file di output
019400050823     C   25              close     fnvac00t
019500161202     C*
019600161202     C* Finalizzo la transazione
019700161202     C   26              CALL      'TIS7VASR1'
019800161205     C                   PARM      '3'           iOPZ
019900161205     C                   PARM                    iDSVGD
020000161205     C                   PARM                    oPRG
020100161205     C                   PARM                    oOK
020200161205     C*
020300161205     C* Verifico esito chiamata
020400161205     C                   if        %error OR oOK = *off
020500161205     C                   exsr      EXEERR
020600161205     C                   endif
020700001218     C*
020800001218     C                   ENDSR
020900001218     C*------------------------------------------------------------------------*
021000100512
021100100512
021200100512
021300100512     C*------------------------------------------------------------------------*
021400161205     C* EXECCM   - Elaborazione x codice cliente mittente
021500100512     C*------------------------------------------------------------------------*
021600100512     C     EXECCM        BEGSR
021700100521     C*
021800100521     C                   movel     'C'           wChkCli           1
021900100512     C*
022000100512     C                   select
022100100512     C* ...se richiesta elaborazione x data spedizione
022200100512     C                   when      DATADA > *zeros
022300100512     C                   exsr      SQ_CCMDSP
022400100512     C* ...se richiesta elaborazione x data consegna merce
022500100512     C                   when      DACMDA > *zeros
022600100512     C                   exsr      SQ_CCMDCM
022700100512     C                   endsl
022800100512     C*
022900100512     C                   ENDSR
023000100512     C*------------------------------------------------------------------------*
023100100512
023200100512
023300100512
023400100512     C*------------------------------------------------------------------------*
023500161205     C* EXEKSC   - Elaborazione x codice cliente fatturazione
023600100512     C*------------------------------------------------------------------------*
023700100512     C     EXEKSC        BEGSR
023800100521     C*
023900100521     C                   movel     'K'           wChkCli           1
024000100512     C*
024100100512     C                   select
024200100512     C* ...se richiesta elaborazione x data spedizione
024300100512     C                   when      DATADA > *zeros
024400100512     C                   exsr      SQ_KSCDSP
024500100512     C* ...se richiesta elaborazione x data consegna merce
024600100512     C                   when      DACMDA > *zeros
024700100512     C                   exsr      SQ_KSCDCM
024800100512     C                   endsl
024900100512     C*
025000100512     C                   ENDSR
025100100512     C*------------------------------------------------------------------------*
025200100512
025300100512
025400100512
025500100512     C*------------------------------------------------------------------------*
025600161205     C* SQ_CCMDSP - Elaborazione x codice cliente mittente + data spedizione
025700100512     C*------------------------------------------------------------------------*
025800100512     C     SQ_CCMDSP     BEGSR
025900100512     C*
026000100512     C                   z-add     KUNI(I)       wParCCM           7 0
026100100512     C*
026200100512     C/EXEC SQL
026300100512     C+ declare C_CCMDSP cursor for
026400100512     C+ select * from titas00f
026500100512     C+ where tasccm = :wParCCM and
026600100512     C+ tasaas*10000+tasmgs between :DATADA and :DATAA
026700100512     C+ union
026800100512     C+ select * from titas10f
026900100512     C+ where tasccm = :wParCCM and
027000100512     C+ tasaas*10000+tasmgs between :DATADA and :DATAA
027100100512     C+ union
027200100512     C+ select * from titasP0f
027300100512     C+ where tasccm = :wParCCM and
027400100512     C+ tasaas*10000+tasmgs between :DATADA and :DATAA
027500100512     C+ for read only
027600100512     C/END-EXEC
027700100512     C
027800100512     C/EXEC SQL
027900100512     C+ open C_CCMDSP
028000100512     C/END-EXEC
028100100512     C
028200100512     C/EXEC SQL
028300100512     C+ Fetch C_CCMDSP into :TITAS00F
028400100512     C/END-EXEC
028500100512     C*
028600100512     C                   dow       sqlcod = *zeros
028700100512     C                   exsr      verifica
028800100512     C                   if        CHKREC = 'S'
028900100512     C                   exsr      scriviVAC
029000100512     C                   endif
029100100512     C
029200100512     C/EXEC SQL
029300100512     C+ Fetch C_CCMDSP into :TITAS00F
029400100512     C/END-EXEC
029500100512     C*
029600100512     C                   enddo
029700100512     C*
029800100512     C/EXEC SQL
029900100512     C+ close C_CCMDSP
030000100512     C/END-EXEC
030100100512     C*
030200100512     C*
030300100512     C                   ENDSR
030400100512     C*------------------------------------------------------------------------*
030500100512
030600100512
030700100512
030800100512     C*------------------------------------------------------------------------*
030900161205     C* SQ_CCMDCM - Elaborazione x codice cliente mittente + data consegna merce
031000100512     C*------------------------------------------------------------------------*
031100100512     C     SQ_CCMDCM     BEGSR
031200100512     C*
031300100512     C                   z-add     KUNI(I)       wParCCM           7 0
031400100512     C*
031500100512     C/EXEC SQL
031600100512     C+ declare C_CCMDCM cursor for
031700100512     C+ select * from titas00f
031800100512     C+ where tasccm = :wParCCM and
031900100512     C+ tasdcm between :DACMDA and :DACMA
032000100512     C+ union
032100100512     C+ select * from titas10f
032200100512     C+ where tasccm = :wParCCM and
032300100512     C+ tasdcm between :DACMDA and :DACMA
032400100512     C+ union
032500100512     C+ select * from titasP0f
032600100512     C+ where tasccm = :wParCCM and
032700100512     C+ tasdcm between :DACMDA and :DACMA
032800100512     C+ for read only
032900100512     C/END-EXEC
033000100512     C
033100100512     C/EXEC SQL
033200100512     C+ open C_CCMDCM
033300100512     C/END-EXEC
033400100512     C
033500100512     C/EXEC SQL
033600100512     C+ Fetch C_CCMDCM into :TITAS00F
033700100512     C/END-EXEC
033800100512     C*
033900100512     C                   dow       sqlcod = *zeros
034000100512     C                   exsr      verifica
034100100512     C                   if        CHKREC = 'S'
034200100512     C                   exsr      scriviVAC
034300100512     C                   endif
034400100512     C
034500100512     C/EXEC SQL
034600100512     C+ Fetch C_CCMDCM into :TITAS00F
034700100512     C/END-EXEC
034800100512     C*
034900100512     C                   enddo
035000100512     C*
035100100512     C/EXEC SQL
035200100512     C+ close C_CCMDCM
035300100512     C/END-EXEC
035400100512     C*
035500100512     C*
035600100512     C                   ENDSR
035700100512     C*------------------------------------------------------------------------*
035800100512
035900100512
036000100512
036100100512     C*------------------------------------------------------------------------*
036200161205     C* SQ_KSCDSP - Elaborazione x codice cliente fatturazione + data spedizione
036300100512     C*------------------------------------------------------------------------*
036400100512     C     SQ_KSCDSP     BEGSR
036500100512     C*
036600100512     C                   z-add     KUNI(I)       wParKSC           7 0
036700100512     C*
036800100512     C/EXEC SQL
036900100512     C+ declare C_KSCDSP cursor for
037000100512     C+ select * from titas00f
037100100512     C+ where tasksc = :wParKSC and
037200100512     C+ tasaas*10000+tasmgs between :DATADA and :DATAA
037300100512     C+ union
037400100512     C+ select * from titas10f
037500100512     C+ where tasksc = :wParKSC and
037600100512     C+ tasaas*10000+tasmgs between :DATADA and :DATAA
037700100512     C+ union
037800100512     C+ select * from titasP0f
037900100512     C+ where tasksc = :wParKSC and
038000100512     C+ tasaas*10000+tasmgs between :DATADA and :DATAA
038100100512     C+ for read only
038200100512     C/END-EXEC
038300100512     C
038400100512     C/EXEC SQL
038500100512     C+ open C_KSCDSP
038600100512     C/END-EXEC
038700100512     C
038800100512     C/EXEC SQL
038900100512     C+ Fetch C_KSCDSP into :TITAS00F
039000100512     C/END-EXEC
039100100512     C*
039200100512     C                   dow       sqlcod = *zeros
039300100512     C                   exsr      verifica
039400100512     C                   if        CHKREC = 'S'
039500100512     C                   exsr      scriviVAC
039600100512     C                   endif
039700100512     C
039800100512     C/EXEC SQL
039900100512     C+ Fetch C_KSCDSP into :TITAS00F
040000100512     C/END-EXEC
040100100512     C*
040200100512     C                   enddo
040300100512     C*
040400100512     C/EXEC SQL
040500100512     C+ close C_KSCDSP
040600100512     C/END-EXEC
040700100512     C*
040800100512     C*
040900100512     C                   ENDSR
041000100512     C*------------------------------------------------------------------------*
041100100512
041200100512
041300100512
041400100512     C*------------------------------------------------------------------------*
041500161205     C* SQ_KSCDCM - Elaborazione x codice cliente fatturazione + data consegna merce
041600100512     C*------------------------------------------------------------------------*
041700100512     C     SQ_KSCDCM     BEGSR
041800100512     C*
041900100512     C                   z-add     KUNI(I)       wParKSC           7 0
042000100512     C*
042100100512     C/EXEC SQL
042200100512     C+ declare C_KSCDCM cursor for
042300100512     C+ select * from titas00f
042400100512     C+ where tasksc = :wParKSC and
042500100512     C+ tasdcm between :DACMDA and :DACMA
042600100512     C+ union
042700100512     C+ select * from titas10f
042800100512     C+ where tasksc = :wParKSC and
042900100512     C+ tasdcm between :DACMDA and :DACMA
043000100512     C+ union
043100100512     C+ select * from titasP0f
043200100512     C+ where tasksc = :wParKSC and
043300100512     C+ tasdcm between :DACMDA and :DACMA
043400100512     C+ for read only
043500100512     C/END-EXEC
043600100512     C
043700100512     C/EXEC SQL
043800100512     C+ open C_KSCDCM
043900100512     C/END-EXEC
044000100512     C
044100100512     C/EXEC SQL
044200100512     C+ Fetch C_KSCDCM into :TITAS00F
044300100512     C/END-EXEC
044400100512     C*
044500100512     C                   dow       sqlcod = *zeros
044600100512     C                   exsr      verifica
044700100512     C                   if        CHKREC = 'S'
044800100512     C                   exsr      scriviVAC
044900100512     C                   endif
045000100512     C
045100100512     C/EXEC SQL
045200100512     C+ Fetch C_KSCDCM into :TITAS00F
045300100512     C/END-EXEC
045400100512     C*
045500100512     C                   enddo
045600100512     C*
045700100512     C/EXEC SQL
045800100512     C+ close C_KSCDCM
045900100512     C/END-EXEC
046000100512     C*
046100100512     C*
046200100512     C                   ENDSR
046300100512     C*------------------------------------------------------------------------*
046400100512
046500100512
046600100512
046700100512     C*------------------------------------------------------------------------*
046800161205     C* CHKCONS  - Verifica se bolla effettivamente consegnata
046900100512     C*------------------------------------------------------------------------*
047000100512     C     CHKCONS       BEGSR
047100100512     C*
047200100512     C                   setoff                                       4041
047300100512     C                   movel     *blanks       wEsito1           1
047400100512     C                   movel     *blanks       wEsito2           1
047500100512     C*
047600100512     C                   eval      wAAS = tasAAS
047700100512     C                   eval      wLNP = tasLNP
047800100512     C                   eval      wNRS = tasNRS
047900100512     C                   eval      wNSP = tasNSP
048000100512     C*
048100100512     C* Chiamata metodo GetLblTyp
048200100512     C                   eval      wEsito1 = %char(UBLBLSPE_GetLblTyp(
048300100512     C                                                wAAS
048400100512     C                                               :wLNP
048500100512     C                                               :wNRS
048600100512     C                                               :wNSP
048700100512     C                                               :pOutLblTyp
048800100512     C                                               :pOutAnnoBO
048900100512     C                                               :pOutLineaParBO
049000100512     C                                               :pOutSerieBO
049100100512     C                                               :pOutNumSpedBO
049200100512     C                                               :pOutDcmBO
049300100512     C                                               :pOutCcaBO
049400100512     C                                               :pOutRblBO))
049500100512     C* Se trattasi d bolla nn originale reperisco la relativa bolla originale
049600100512     C                   if        pOutLblTyp <> 'O'
049700100512     C                   eval      wAAS = pOutAnnoBO
049800100512     C                   eval      wLNP = pOutLineaParBO
049900100512     C                   eval      wNRS = pOutSerieBO
050000100512     C                   eval      wNSP = pOutNumSpedBO
050100100512     C                   endif
050200100512     C*
050300100512     C* Chiamata metodo GetLastChild
050400100512     C                   eval      wEsito2 = %char(UBLBLSPE_GetLastChild(
050500100512     C                                                wAAS
050600100512     C                                               :wLNP
050700100512     C                                               :wNRS
050800100512     C                                               :wNSP
050900100512     C                                               :pOutAnnoFI
051000100512     C                                               :pOutLineaParFI
051100100512     C                                               :pOutSerieFI
051200100512     C                                               :pOutNumSpedFI
051300100512     C                                               :pOutDcmFI
051400100512     C                                               :pOutCcaFI))
051500100512     C*
051600100512     C* Considerazioni sullo stato corrente della bolla
051700100512     C                   if        (wEsito2 = '9' and pOutDcmBO > *zeros)  or
051800100512     C                             (wEsito2 = '0' and pOutDcmFI > *zeros)
051900161202     C                   seton                                        40        * bolla chiusa
052000100512     C                   endif
052100100512     C*
052200100512     C                   if        (wEsito2 = '9' and pOutDcmBO > *zeros
052300100512     C                                            and pOutCcaBO = *blanks) or
052400100512     C                             (wEsito2 = '0' and pOutDcmFI > *zeros
052500100512     C                                            and pOutCcaFI = *blanks)
052600100512     C                   seton                                        41        * bolla cons. OK
052700100512     C                   endif
052800100512     C*
052900100512     C                   ENDSR
053000100512     C*------------------------------------------------------------------------*
053100001218
053200001218
053300001218
053400001218     C*------------------------------------------------------------------------*
053500161205     C* VERIFICA - Routine di verifica validità record corrente
053600001218     C*------------------------------------------------------------------------*
053700001218     C     VERIFICA      BEGSR
053800001218     C*
053900001218     C                   movel     'S'           CHKREC            1
054000001218     C*
054100100512     C* Verifico le date spedizione
054200001218     C                   if        CHKREC = 'S'
054300100512     C                   if        DATADA > *zeros
054400001218     C                   movel     tasaas        wrkdata           8 0
054500001218     C                   move      tasmgs        wrkdata
054600001218     C                   if        wrkdata < DATADA or
054700001218     C                             wrkdata > DATAA
054800001218     C                   movel     'N'           CHKREC
054900100512     C                   endif
055000001218     C                   endif
055100001218     C                   endif
055200100512     C*
055300100512     C* Verifico le date spedizione
055400100512     C                   if        CHKREC = 'S'
055500100512     C                   if        DACMDA > *zeros
055600100512     C                   if        tasdcm  < DACMDA or
055700100512     C                             tasdcm  > DACMA
055800100512     C                   movel     'N'           CHKREC
055900100512     C                   endif
056000100512     C                   endif
056100100512     C                   endif
056200100521     C*
056300100521     C* Verifico il codice cliente mittente
056400100521     C                   if        CHKREC = 'S'
056500100521     C                   if        wChkCli  = 'C'      AND
056600100521     C                             tasccm  <> KUNI(I)
056700100521     C                   movel     'N'           CHKREC
056800100521     C                   endif
056900100521     C                   endif
057000070718     C*
057100100517     C* Verifico il codice cliente fatturazione (in caso d 'K')
057200070718     C                   if        CHKREC = 'S'
057300100521     C                   if        wChkCli  = 'K'      AND
057400100521     C                             tasksc  <> KUNI(I)
057500070718     C                   movel     'N'           CHKREC
057600070718     C                   endif
057700070718     C                   endif
057800100517     C*
057900100517     C* Verifico il codice cliente fatturazione (in caso d 'E')
058000100517     C                   if        CHKREC = 'S'
058100100521     C                   if        TIPCLI  = 'E'       AND
058200100521     C                             wChkCli = 'K'       AND
058300100521     C                             tasksc  = tasccm
058400100517     C                   movel     'N'           CHKREC
058500100517     C                   endif
058600100517     C                   endif
058700001218     C*
058800020923     C* Verifico che la bolla abbia la data consegna se trattasi di bolla PT
058900001218     C                   if        CHKREC = 'S'
059000020923     C                   if        tasTSP = 'P'
059100020923     C                   if        tasDCM > *zeros
059200001218     C                   else
059300001218     C                   movel     'N'           CHKREC
059400001218     C                   endif
059500020923     C                   endif
059600001218     C                   endif
059700020920     C*
059800040322     C* Verifico che il tipo bolla sia da considerare ai fini della rigenerazione dell'FNVAC
059900020920     C                   if        CHKREC = 'S'
060000040322     C     tasCBO        lookup    COD3A                                  55
060100081016     C                   if        %equal
060200040322     C                   else
060300020920     C                   movel     'N'           CHKREC
060400020920     C                   endif
060500020920     C                   endif
060600010301     C*
060700010301     C* Verifico che la bolla se POSTE non abbia il codice CA = 5
060800010301     C                   if        CHKREC = 'S'
060900010301     C                   if        tasTSP = 'P' and tasCCA = '5'
061000010301     C                   movel     'N'           CHKREC
061100010301     C                   endif
061200010301     C                   endif
061300100512     C*
061400040322     C* Verifico se richiesto nel lancio che la bolla sia consegnata
061500040322     C                   if        CHKREC = 'S'
061600040322     C                   if        FLGCONS = 'S'
061700100512     C                   exsr      CHKCONS
061800100512     C                   if        *in40 = *on
061900040322     C                   else
062000040322     C                   movel     'N'           CHKREC
062100040322     C                   endif
062200040322     C                   endif
062300040322     C                   endif
062400100512     C*
062500100512     C* Verifico se richiesto nel lancio che la bolla sia consegnata OK
062600100512     C                   if        CHKREC = 'S'
062700100512     C                   if        FLGCONS = 'O'
062800100512     C                   exsr      CHKCONS
062900100512     C                   if        *in41 = *on
063000100512     C                   else
063100100512     C                   movel     'N'           CHKREC
063200100512     C                   endif
063300100512     C                   endif
063400100512     C                   endif
063500100513     C*
063600100907     C* Verifico se richiesto nel lancio che la bolla NON sia consegnata OK
063700100513     C                   if        CHKREC = 'S'
063800100513     C                   if        FLGCONS = 'K'
063900100513     C                   exsr      CHKCONS
064000100513     C                   if        *in41 = *on
064100100513     C                   movel     'N'           CHKREC
064200100513     C                   endif
064300100513     C                   endif
064400100513     C                   endif
064500100907     C*
064600100907     C* Verifico se richiesto nel lancio che la bolla NON sia consegnata
064700100907     C                   if        CHKREC = 'S'
064800100907     C                   if        FLGCONS = 'P'
064900100907     C                   exsr      CHKCONS
065000100907     C                   if        *in40 = *on
065100100907     C                   movel     'N'           CHKREC
065200100907     C                   endif
065300100907     C                   endif
065400100907     C                   endif
065500040322     C*
065600040322     C* Verifico se richiesto nel lancio che la bolla provenga da scambio dati VAS cliente
065700040322     C                   if        CHKREC = 'S'
065800040322     C                   if        FLGVAS = 'S'
065900040322     C                   if        tasSOP = '§' OR
066000040322     C                             tasSOP = ':' OR
066100040322     C                             tasSOP = '%'
066200040322     C                   else
066300040322     C                   movel     'N'           CHKREC
066400040322     C                   endif
066500040322     C                   endif
066600040322     C                   endif
066700081016     C*
066800081016     C* Verifico se richiesto che la bolla corrente sia la singola bolla richiesta a video
066900081016     C                   if        CHKREC = 'S'
067000081016     C                   if        KAAS   <> *zeros OR
067100081016     C                             KLNP   <> *zeros OR
067200081016     C                             KNRS   <> *zeros OR
067300081016     C                             KNSP   <> *zeros
067400081016     C                   if        tasAAS = KAAS AND
067500081016     C                             tasLNP = KLNP AND
067600081016     C                             tasNRS = KNRS AND
067700081016     C                             tasNSP = KNSP
067800081016     C                   else
067900081016     C                   movel     'N'           CHKREC
068000081016     C                   endif
068100081016     C                   endif
068200081016     C                   endif
068300001218     C*
068400001218     C                   ENDSR
068500001218     C*------------------------------------------------------------------------*
068600001218
068700001218
068800001218
068900001218     C*------------------------------------------------------------------------*
069000001218     C* SCRIVIVAC - Routine di scrittura file esiti di consegna (FNVAC)
069100001218     C*------------------------------------------------------------------------*
069200001218     C     SCRIVIVAC     BEGSR
069300001218     C*
069400001218     C                   CLEAR                   fnvac000
069500001218     C*
069600001218     C                   MOVEL     tasAAS        VACAAS
069700001218     C                   MOVEL     tasLNP        VACLNP
069800001218     C                   MOVEL     tasNRS        VACNRS
069900001218     C                   MOVEL     tasNSP        VACNSP
070000001218     C                   MOVEL     tasMGS        VACMGS
070100001218     C                   MOVEL     tasCBO        VACCBO
070200001218     C                   MOVEL     tasLNA        VACLNA
070300010223     C                   MOVEL     tasRSD        VACRSD
070400010223     C                   MOVEL     tasPRD        VACPRD
070500010301     C*
070600001218     C* Reperisco dall'estensione anagrafica bolle di SEDE i dati del Destinatario
070700001218     C                   EXSR      REPDEST
070800151019     C*
070900151019     C* Eseguo eventuali "forzature"
071000151019     C                   EXSR      RTVAR5FAT
071100160208     C*
071200160208     C* Reprrisco NATURA MERCE
071300160208     C                   EXSR      RTVNAS
071400001218     C*
071500001218     C                   MOVEL     tasGC1        VACGC1
071600001218     C                   MOVEL     tasGC2        VACGC2
071700001218     C                   MOVEL     tasCTR        VACCTR
071800001218     C                   MOVEL     tasCTS        VACCTS
071900001218     C                   MOVEL     tasFTM        VACFTM
072000001218     C                   MOVEL     tasFIN        VACFIN
072100001218     C                   MOVEL     tasFAP        VACFAP
072200001218     C                   MOVEL     tasTSP        VACTSP
072300001218     C* Effettuo considerazioni particolare per bolla "POSTE"
072400001218     C     tasTSP        IFEQ      'P'                                          BOLLA POSTE
072500001218     C                   EXSR      REPPTIAS
072600001218     C                   EXSR      REPPTGIA
072700001218     C                   Z-ADD     tasNCD        VACDCR
072800001218     C                   ELSE
072900001218     C                   MOVEL     tasIAS        VACIAS
073000001218     C                   MOVEL     tasVAS        VACVAS
073100001218     C                   MOVEL     tasDCR        VACDCR
073200050310     C     KGCP          CHAIN     TIGCP51L
073300050310     C                   IF        %found(TIGCP51L)
073400001218     C                   MOVEL     GCPAGC        VACDAG
073500001218     C                   MOVE      GCPMGC        VACDAG
073600001218     C                   ENDIF
073700001218     C                   ENDIF
073800001218     C*
073900160208     C                   MOVEL     §TA4ANAS      VACNAS
074000001218     C                   MOVEL     tasNCL        VACNCL
074100001218     C                   MOVEL     tasPKF        VACPKB
074200001218     C                   MOVEL     tasVLF        VACVLB
074300001218     C                   MOVEL     tasQFT        VACQFT
074400001218     C*
074500001218     C                   EXSR      REPCSB
074600001218     C*
074700001218     C                   MOVEL     tasCCM        VACCCM
074800001218     C                   MOVEL     tasRMN        VACRMN
074900010301     C*
075000001218     C                   EXSR      REPRMA
075100010301     C*
075200001218     C                   MOVEL     tasFFD        VACFFD
075300001218     C                   MOVEL     tasTCR        VACTCR
075400001218     C                   MOVEL     tasHCR        VACHCR
075500001218     C                   MOVEL     tasFTC        VACTC1
075600001218     C                   MOVEL     tasTC2        VACTC2
075700001218     C                   MOVEL     tasDCM        VACDCM
075800001218     C                   MOVEL     tasHMC        VACHMC
075900001218     C*
076000010301     C                   IF        tasTSP = 'P' and tasCCA = '7'
076100010301     C                   MOVEL     *blanks       VACCCA
076200010301     C                   ELSE
076300100910     C                   MOVEL     tasCCA        VACCCA
076400100910     C*
076500100910     C* Se CCA ancora nn valorizzato => reperisco l'ultimo evento
076600100910     C                   IF        VACCCA = *blanks
076700100910     C                   EXSR      REPLASTEV
076800010301     C                   ENDIF
076900100910     C                   ENDIF
077000010301     C*
077100120427     C* Reperisco l'eventuale lasciato avviso
077200001218     C                   EXSR      REPLAV
077300120427     C*
077400120427     C* Effettuo particolari operazioni in caso d DIROTTAMENTO
077500120427     C***                IF        VACCCA = '1' AND
077600120427     C***                          VACDCM = *zeros
077700120427     C***                EXSR      REPDIR
077800120427     C***                ENDIF
077900001218     C*
078000020923     C* Verifico che in caso di bolla NON PT siano valorizzate o la data consegna...
078100020923     C* ... o la data lasciato avviso o la data giacenza o la consegna anomala
078200020923     C                   IF        tasTSP <> 'P'
078300071228     C*
078400071228     C* Se richiesto nel lancio anche bolle senza eventi => gestisco...
078500071228     C* ...altrimenti standard
078600071228     C                   IF        FLGNOE  = 'N'
078700020923     C                   IF        vacDCM  > *zeros OR
078800020923     C                             vacDLA  > *zeros OR
078900020923     C                             vacDAG  > *zeros OR
079000020923     C                             vacCCA <> *blanks
079100071228     C   25              WRITE     FNVAC000
079200161205     C   26              exsr      WRITIVGD
079300020923     C                   endif
079400071228     C                   else
079500071228     C   25              WRITE     FNVAC000
079600161205     C   26              exsr      WRITIVGD
079700071228     C                   endif
079800020923     C                   else
079900050823     C   25              WRITE     FNVAC000
080000161205     C   26              exsr      WRITIVGD
080100020923     C                   endif
080200001218     C*
080300001218     C                   ENDSR
080400120427
080500120427
080600120427
080700120427     C*------------------------------------------------------------------------*
080800161205     C* REPDIR - Reperimento data DIROTTAMENTO
080900120427     C*------------------------------------------------------------------------*
081000120427     C     REPDIR        BEGSR
081100120427     C*
081200120427     C                   movel     *blanks       wEsito1           1
081300120427     C*
081400120427     C* Chiamata metodo GetLblTyp
081500120427     C                   eval      wEsito1 = %char(UBLBLSPE_GetLblTyp(
081600120427     C                                                tasAAS
081700120427     C                                               :tasLNP
081800120427     C                                               :tasNRS
081900120427     C                                               :tasNSP
082000120427     C                                               :pOutLblTyp
082100120427     C                                               :pOutAnnoBO
082200120427     C                                               :pOutLineaParBO
082300120427     C                                               :pOutSerieBO
082400120427     C                                               :pOutNumSpedBO
082500120427     C                                               :pOutDcmBO
082600120427     C                                               :pOutCcaBO
082700120427     C                                               :pOutRblBO))
082800120427     C*
082900120427     C* Se reperimento effettuato e trattasi d dirottamento => recupero la data
083000120427     C                   if        wEsito1 = '0' and pOutCcaBO = '1'
083100120427     C                   eval      VACDCM = pOutDcmBO
083200120427     C                   endif
083300120427     C*
083400120427     C                   ENDSR
083500120427     C*------------------------------------------------------------------------*
083600100910
083700100910
083800100910
083900100910     C*------------------------------------------------------------------------*
084000161205     C* REPLASTEV - Routine di reperimento ultimo evento
084100100910     C*------------------------------------------------------------------------*
084200100910     C     REPLASTEV     BEGSR
084300100910     C*
084400100910     C     KEVB          SETGT     FNEVB01L
084500100910     C     KEVB          READPE    FNEVB01L                               34
084600100910     C*
084700100910     C* Devo cercare l'ultimo evento di tipo giacenza
084800100910     C                   IF        *IN34 = *OFF
084900100910     C*
085000100910     C* Evento 'MIC' considero solo se data evento >= data corrente
085100100910     C                   IF        EVBDEV >= datcor
085200100910     C* - MIC
085300100910     C                   IF        EVBCEV = 'MIC'
085400100910     C                   MOVEL     'C'           VACCCA
085500100910     C                   MOVEL     evbDEV        VACDCM
085600100910     C                   ENDIF
085700100910     C                   ENDIF
085800100910     C*
085900100910     C                   ENDIF
086000100910     C*
086100100910     C                   ENDSR
086200100910     C*------------------------------------------------------------------------*
086300050823
086400050823
086500050823
086600050823     C*------------------------------------------------------------------------*
086700161205     C* WRITIVGD - Routine di scrittura file TIVGD (file VAS generico download)
086800050823     C*------------------------------------------------------------------------*
086900161205     C     WRITIVGD      BEGSR
087000050823     C*
087100161205     C                   clear                   iDSVGD
087200080407     C                   eval      vgdDTA = %subst(FNVACDS:1:%size(FNVACDS))
087300100312     C                   eval      vgdTIP = TIPFILE
087400050823     C                   movel     *all'0'       vgdKSU
087500070525     C                   move      CODCLIVAS     vgdKSU
087600050823     C                   eval      vgdTSC = 'WW'
087700050823     C                   eval      vgdDAT = datcor
087800161205     C                   eval      vgdPRG = oPRG
087900161205     C                   eval      vgdPGM = procname
088000070627     C*
088100161205     C                   CALL      'TIS7VASR1'
088200161205     C                   PARM      '2'           iOPZ
088300161205     C                   PARM                    iDSVGD
088400161205     C                   PARM                    oPRG
088500161205     C                   PARM                    oOK
088600161205     C*
088700161205     C* Verifico esito chiamata
088800161205     C                   if        %error OR oOK = *off
088900161205     C                   exsr      EXEERR
089000161205     C                   endif
089100161205     C*
089200050823     C                   ENDSR
089300050823     C*------------------------------------------------------------------------*
089400001218
089500001218
089600161202
089700001218     C*------------------------------------------------------------------------*
089800001218     C* REPPTIAS - Routine di reperimento dati relativi all'importo/divisa da assicurare x POSTE
089900001218     C*------------------------------------------------------------------------*
090000001218     C     REPPTIAS      BEGSR
090100001218     C*
090200060215     C* Prendo importo da assicurare da TITA4 t.r.=J
090300001218     C                   movel     'J'           KTRC
090400001218     C     KTA4          chain     tita430c
090500001218     C                   if        %found(tita430c)
090600001218     C                   movel     ta4NOT        DSBL4J
090700001218     C                   z-add     §B4IAS        vacIAS
090800001218     C                   movel     §B4VAS        vacVAS
090900001218     C                   endif
091000001218     C*
091100001218     C                   ENDSR
091200001218     C*------------------------------------------------------------------------*
091300001218
091400001218
091500001218
091600001218     C*------------------------------------------------------------------------*
091700001218     C* REPRMA - Routine di reperimento dati relativi al riferimento mittente alfabetico
091800001218     C*------------------------------------------------------------------------*
091900001218     C     REPRMA        BEGSR
092000001218     C*
092100060215     C* Prendo importo da assicurare da TITA4 t.r.=A
092200001218     C                   movel     'A'           KTRC
092300001218     C     KTA4          chain     tita430c
092400001218     C                   if        %found(tita430c)
092500001218     C                   eval      vacRMA = %subst(ta4NOT:1:15)
092600001218     C                   endif
092700001218     C*
092800001218     C                   ENDSR
092900001218     C*------------------------------------------------------------------------*
093000001218
093100001218
093200001218
093300001218     C*------------------------------------------------------------------------*
093400001218     C* REPDEST - Routine di reperimento dati relativi al destinatario della bolla (TITAA)
093500001218     C*           ... e ai dati del mittente originale
093600001218     C*------------------------------------------------------------------------*
093700001218     C     REPDEST       BEGSR
093800001218     C*
093900010223     C* Prendo il tipo anagrafica = D
094000010223     C*                  movel     'D'           KTRC
094100010223     C*    KTAA          chain     titaa30c
094200010223     C*                  if        %found(titaa30c)
094300010223     C*                  movel     taaRSC        vacRSD
094400010223     C*                  movel     taaPRV        vacPRD
094500010223     C*                  endif
094600001218     C*
094700010223     C* Prendo il tipo anagrafica = O
094800001218     C                   movel     'O'           KTRC
094900001218     C     KTAA          chain     titaa30c
095000001218     C                   if        %found(titaa30c)
095100001218     C                   movel     taaRSC        vacRMO
095200001218     C                   endif
095300001218     C*
095400001218     C                   ENDSR
095500001218     C*------------------------------------------------------------------------*
095600001218
095700001218
095800001218
095900001218     C*------------------------------------------------------------------------*
096000001218     C* REPCSB - Routine di reperimento dati relativi al contrassegno
096100001218     C*------------------------------------------------------------------------*
096200001218     C     REPCSB        BEGSR
096300001218     C*
096400001219     C     KCSB          chain     tncsb03l
096500001219     C                   if        %found(tncsb03l)
096600001218     C                   if        csbSTA = 9 and tasTSP <> 'P'
096700001218     C                   else
096800001218     C                   movel     csbTPA        vactic
096900001218     C                   move      csbTPI        vactic
097000001218     C                   z-add     csbCAS        vacCAS
097100001218     C                   movel     csbVCA        vacVCA
097200001218     C                   endif
097300001218     C                   endif
097400001218     C*
097500001218     C                   ENDSR
097600001218     C*------------------------------------------------------------------------*
097700001218
097800001218
097900001218
098000001218     C*------------------------------------------------------------------------*
098100001218     C* REPPTGIA - Routine di reperimento dati relativi alla giacenza x POSTE
098200001218     C*------------------------------------------------------------------------*
098300001218     C     REPPTGIA      BEGSR
098400001218     C*
098500001218     C* Reperisco la data di giacenza dagli eventi poichè non vengono
098600001218     C* gestite le giacenze per le bolle poste
098700001218     C     KEVB          SETGT     FNEVB01L
098800001218     C     KEVB          READPE    FNEVB01L                               34
098900001218     C*
099000001218     C* Devo cercare l'ultimo evento di tipo giacenza
099100001218     C     *IN34         DOWEQ     *OFF
099200001218     C                   Z-ADD     1             W
099300001218     C     EVBCEV        LOOKUP    C2A(W)                                 42
099400001218     C     *IN42         IFEQ      '1'
099500001218     C     T2A(W)        ANDEQ     'G'
099600001218     C                   Z-ADD     EVBDEV        vacDAG
099700001218     C                   LEAVE
099800001218     C                   ELSE
099900001218     C     KEVB          READPE    FNEVB000                               34
100000001218     C                   ENDIF
100100001218     C                   ENDDO
100200001218     C*
100300001218     C                   ENDSR
100400001218     C*------------------------------------------------------------------------*
100500001218
100600001218
100700001218
100800001218
100900001218     C*------------------------------------------------------------------------*
101000001218     C* REPLAV - Routine di reperimento dati relativi al lasciato avviso
101100001218     C*------------------------------------------------------------------------*
101200001218     C     REPLAV        BEGSR
101300001218     C*
101400001218     C* Pererisco l'ultimo evento Lasciato Avviso
101500001218     C                   MOVEL     '2A'          COD
101600001218     C     KEVB          SETGT     FNEVB01L
101700001218     C     KEVB          READPE    FNEVB01L                               34
101800001218     C*
101900001218     C     *IN34         DOWEQ     *OFF
102000001218     C                   MOVEL(P)  EVBCEV        KEY
102100001218     C     KTAB2         CHAIN     TABEL                              30
102200001218     C     *IN30         IFEQ      *OFF
102300001218     C                   MOVEL     TBLUNI        DS2A
102400001218     C     §2AFTC        IFEQ      'A'
102500001218     C                   Z-ADD     EVBDEV        vacDLA
102600001218     C                   LEAVE
102700001218     C                   ENDIF
102800001218     C                   ENDIF
102900001218     C     KEVB          READPE    FNEVB000                               34
103000001218     C                   ENDDO
103100001218     C*
103200001218     C                   ENDSR
103300001218     C*------------------------------------------------------------------------*
103400001218
103500001218
103600001218
103700001218     C*------------------------------------------------------------------------*
103800001218     C* CARTBL - Routine di caricamento dati tabellati
103900001218     C*------------------------------------------------------------------------*
104000001218     C     CARTBL        BEGSR
104100001218     C*
104200001218     C                   Z-ADD     0             W                 4 0
104300001218     C                   MOVEA     *HIVAL        T2A
104400001218     C                   MOVEA     *HIVAL        C2A
104500001218     C                   MOVEL     '2A'          COD
104600001218     C     KTAB          CHAIN     TABEL                              31
104700001218     C     *IN31         DOWEQ     '0'
104800001218     C     TBLFLG        IFEQ      ' '
104900001218     C                   MOVEL     TBLUNI        DS2A
105000001218     C     §2AFTC        IFNE      *BLANKS
105100001218     C                   ADD       1             W
105200001218     C                   MOVEL     §2AFTC        T2A(W)
105300001218     C                   MOVEL     TBLKEY        C2A(W)
105400001218     C                   ENDIF
105500001218     C                   ENDIF
105600001218     C     KTAB          READE     TABEL                                  31
105700001218     C                   ENDDO
105800001218     C*
105900081016     C                   Z-ADD     1             I                 4 0
106000011017     C                   MOVEL     CODCLI        KUNI(I)
106100081016     C                   IF        FLGUNI = 'U'
106200001218     C                   MOVEL     '3K'          COD
106300001218     C     KTAB          CHAIN     TABEL                              31
106400001218     C     *IN31         DOWEQ     '0'
106500001218     C     TBLFLG        IFEQ      ' '
106600001218     C                   MOVEL     TBLUNI        DS3K
106700010301     C     §3KCKS        IFEQ      CODCLI
106800001218     C                   ADD       1             I
106900081016     C                   MOVEL     TBLKEY        wKUNI             7 0
107000081016     C     wKUNI         LOOKUP    KUNI                                   55
107100081016     C                   IF        %equal
107200081016     C                   ELSE
107300081016     C                   Z-ADD     wKUNI         KUNI(I)
107400081016     C                   ENDIF
107500001218     C                   ENDIF
107600001218     C                   ENDIF
107700001218     C     KTAB          READE     TABEL                                  31
107800001218     C                   ENDDO
107900010301     C                   ENDIF
108000040312     C*
108100040312     C                   Z-ADD     0             J                 4 0
108200040312     C                   MOVEL     '3A'          COD
108300040312     C     KTAB          CHAIN     TABEL                              31
108400040312     C     *IN31         DOWEQ     '0'
108500040312     C     TBLFLG        IFEQ      ' '
108600040312     C                   MOVEL     TBLUNI        DS3A
108700040312     C     §3AVAC        IFEQ      'N'
108800040312     C                   ELSE
108900040312     C                   ADD       1             J
109000040312     C                   MOVEL     TBLKEY        COD3A(J)
109100040312     C                   ENDIF
109200040312     C                   ENDIF
109300040312     C     KTAB          READE     TABEL                                  31
109400040312     C                   ENDDO
109500001218     C*
109600001218     C                   ENDSR
109700001218     C*---------------------------------------------------------------*
109800151019
109900151019
110000151019
110100151019     C     RTVAR5FAT     BEGSR
110200151019     C*
110300151019     C* Se, presente, considero il peso/volume DESUNTO al pari del VDL TOTALE
110400151019     C                   SETOFF                                       7071
110500151019     C                   EVAL      ar5TRD = 'FAT'
110600151019     C                   CLEAR                   DAR5FAT
110700151019     C     KEYar531_P    CHAIN     FIAR531C
110800151019     C                   IF        %found(FIAR531C)
110900151019     C                   MOVEL     AR5UNI        DAR5FAT
111000151019     C                   IF        §AR5PKTAS > *zeros
111100151019     C                   SETON                                        70
111200151019     C                   EVAL      tasPKF = §AR5PKTAS
111300151019     C                   ENDIF
111400151019     C                   IF        §AR5VLTAS > *zeros
111500151019     C                   SETON                                        71
111600160505     C                   EVAL      tasVLF = §AR5VLTAS
111700151019     C                   ENDIF
111800151019     C                   ENDIF
111900151019     C*
112000151019     C                   ENDSR
112100160208
112200160208
112300160208
112400160208     C     RTVNAS        BEGSR
112500160208     C*
112600160208     C* Inizializzo le DS relative ai tipi record del TITA4 da gestire
112700160208     C                   CLEAR                   DTA4A
112800160208     C*
112900160208     C* Reperisco dal tipo record 'A' del TITA4 la natura merce della bolla corrente
113000160208     C                   CALL      'UBTA400R'
113100160208     C                   PARM      *blanks       UBTA4IOPZ
113200160208     C                   PARM      *blanks       UBTA4ITLA
113300160208     C                   PARM      tasAAS        UBTA4IAAS
113400160208     C                   PARM      tasLNP        UBTA4ILNP
113500160208     C                   PARM      tasNRS        UBTA4INRS
113600160208     C                   PARM      tasNSP        UBTA4INSP
113700160208     C                   PARM      'A'           UBTA4ITRC
113800160208     C                   PARM                    UBTA4OERR
113900160208     C                   PARM                    UBTA4ODS
114000160208     C                   PARM                    UBTA4OLEN
114100160208     C                   PARM                    UBTA4ODATI
114200160208     C*
114300160208     C                   IF        UBTA4OERR = *zeros
114400160208     C                   SELECT
114500160208     C* Gestione output tipo record 'A'
114600160208     C                   WHEN      UBTA4ODS = 'DTA4A'
114700160208     C                   EVAL      DTA4A = %subst(UBTA4ODATI:1:UBTA4OLEN)
114800160208     C                   ENDSL
114900160208     C*
115000160208     C                   ENDIF
115100160208     C*
115200160208     C                   ENDSR
115300161205
115400161205
115500161205
115600161205     C*------------------------------------------------------------------------*
115700161205     C* ENDELA - Routine di lanci di chiusura finali
115800161205     C*------------------------------------------------------------------------*
115900161205     C     ENDELA        BEGSR
116000161205     C*
116100161205     C* Finita l'elaborazione chiamo driver reperimento TITA4 in chiusura
116200161205     C                   CALL      'UBTA400R'
116300161205     C                   PARM                    UBTA4IOPZ
116400161205     C                   PARM      'C'           UBTA4ITLA
116500161205     C                   PARM                    UBTA4IAAS
116600161205     C                   PARM                    UBTA4ILNP
116700161205     C                   PARM                    UBTA4INRS
116800161205     C                   PARM                    UBTA4INSP
116900161205     C                   PARM                    UBTA4ITRC
117000161205     C                   PARM                    UBTA4OERR
117100161205     C                   PARM                    UBTA4ODS
117200161205     C                   PARM                    UBTA4OLEN
117300161205     C                   PARM                    UBTA4ODATI
117400161205     C*
117500161205     C                   ENDSR
117600161205     C*------------------------------------------------------------------------*
117700161205
117800161205
117900161205
118000161205     C*------------------------------------------------------------------------*
118100161205     C* EXEERR - Routine di esecuzione azioni su Errore
118200161205     C*------------------------------------------------------------------------*
118300161205     C     EXEERR        BEGSR
118400161205     C*
118500161205     C                   dump(A)
118600161205     C                   rolbk
118700161205     C                   seton                                        lr
118800161205     C                   return
118900161205     C*
119000161205     C                   ENDSR
119100161205     C*------------------------------------------------------------------------*
119200001218
119300001218
119400001218
119500001218     C*------------------------------------------------------------------------*
119600001218     C* *INZSR - ROUTINE INIZIALE
119700001218     C*------------------------------------------------------------------------*
119800001218     C     *INZSR        BEGSR
119900001218     C*
120000001218     C     *ENTRY        PLIST
120100100512     C                   PARM                    PARAM            71
120200001218     C*
120300001218     C                   MOVEL     PARAM         DSINPUT
120400001218     C                   Z-ADD     1             CODUT
120500001218     C*
120600001218     C* Definizioni chiavi
120700001218     C     KTA4          KLIST
120800001218     C                   KFLD                    tasAAS
120900001218     C                   KFLD                    tasLNP
121000001218     C                   KFLD                    tasNRS
121100001218     C                   KFLD                    tasNSP
121200001218     C                   KFLD                    KTRC              1
121300001218     C*
121400001218     C     KTAA          KLIST
121500001218     C                   KFLD                    tasAAS
121600001218     C                   KFLD                    tasLNP
121700001218     C                   KFLD                    tasNRS
121800001218     C                   KFLD                    tasNSP
121900001218     C                   KFLD                    KTRC
122000001218     C*
122100001218     C     KCSB          KLIST
122200001218     C                   KFLD                    tasAAS
122300001218     C                   KFLD                    tasLNP
122400001218     C                   KFLD                    tasNRS
122500001218     C                   KFLD                    tasNSP
122600001218     C*
122700001218     C     KEVB          KLIST
122800001218     C                   KFLD                    tasAAS
122900001218     C                   KFLD                    tasLNP
123000001218     C                   KFLD                    tasNRS
123100001218     C                   KFLD                    tasNSP
123200001218     C*
123300001218     C     KGCP          KLIST
123400001218     C                   KFLD                    tasAAS
123500001218     C                   KFLD                    tasLNP
123600001218     C                   KFLD                    tasNRS
123700001218     C                   KFLD                    tasNSP
123800001218     C*
123900001218     C     KTAB          KLIST
124000001218     C                   KFLD                    CODUT             1 0
124100001218     C                   KFLD                    COD               2
124200001218     C*
124300001218     C     KTAB2         KLIST
124400001218     C                   KFLD                    CODUT             1 0
124500001218     C                   KFLD                    COD               2
124600001218     C                   KFLD                    KEY               8
124700151019     C*
124800151019     C* FIAR531C - Parziale
124900151019     C     KEYar531_P    KLIST
125000151019     C                   KFLD                    tasAAS
125100151019     C                   KFLD                    tasLNP
125200151019     C                   KFLD                    tasNRS
125300151019     C                   KFLD                    tasNSP
125400151019     C                   KFLD                    ar5TRD
125500001218     C*
125600001218     C* Determino la data corrente
125700100312     C                   Z-ADD     *zeros        DATCOR            8 0
125800100312     C                   EVAL      DATCOR = %dec(%date() : *ISO)
125900001218     C*
126000001218     C                   ENDSR

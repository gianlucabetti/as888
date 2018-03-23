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
001300161206     FTIVGD00F  O    E             DISK    USROPN COMMIT
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
002700161205     D psds           sds
002800161205     D  procname         *PROC
002900100512     D TITAS00F      E DS
003000900517     D KPJBA         E DS
003100921023     D DS3K          E DS
003200941228     D DS2A          E DS
003300040312     D DS3A          E DS
003400000526     D DSBL4J        E DS
003500151019     D DAR5FAT       E DS
003600050823     D FNVACDS       E DS                  EXTNAME(FNVAC00T)
003700160208     D DTA4A         E DS
003800170227     D TIS7VASDS     E DS
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
007800160208     D/COPY GAITRASRC/SRCPROTOPI,UBTA400R
007900001218
008000001218
008100920812     C*---------------------------------------------------------------*
008200001218     C* MAIN
008300001218     C*---------------------------------------------------------------*
008400100512     C*
008500100512     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
008600100512     C
008700100512     C/EXEC SQL
008800100512     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
008900100512     C/END-EXEC
009000100512     C
009100161202     C*
009200161206     C* Avvio il monitoring del processo
009300161206     C                   monitor
009400161206     C*
009500001218     C                   exsr      cartbl
009600001218     C                   exsr      chkpar
009700001218     C                   exsr      procedi
009800161205     C                   exsr      endela
009900161206     C*
010000170227     C* Sancisco il commit
010100170309     C                   commit(e)
010200161206     C*
010300161206     C* Su errore
010400161206     C                   on-error
010500161206     C                   exsr      exeerr
010600161206     C*
010700161206     C* Fine monitoring
010800161206     C                   endmon
010900001218     C*
011000001218     C                   seton                                        LR
011100001218
011200001218
011300001218
011400001218     C*------------------------------------------------------------------------*
011500001218     C* CHKPAR - Routine di controllo parametri ricevuti in input
011600001218     C*------------------------------------------------------------------------*
011700001218     C     CHKPAR        BEGSR
011800001218     C*
011900001218     C* Verifico le date
012000100512     C                   if        DATADA > *zeros AND
012100100512     C                             DATAA  = *zeros
012200001218     C                   movel     *all'9'       DATAA
012300001218     C                   endif
012400100512     C                   if        DACMDA > *zeros AND
012500100512     C                             DACMA  = *zeros
012600100512     C                   movel     *all'9'       DACMA
012700100512     C                   endif
012800050823     C*
012900050823     C* Verifico la modalità d scrittura output richiesta:
013000050823     C*   1 = crea membro "RIC" in file FNVAC00T
013100050823     C*   2 = scrivi record in file TIVGD00F
013200050823     C                   setoff                                       2526
013300050823     C                   if        FLGOPZ = '1'
013400050823     C                   seton                                        25
013500050823     C                   endif
013600050823     C                   if        FLGOPZ = '2'
013700050823     C                   seton                                        26
013800050823     C                   endif
013900001218     C*
014000001218     C                   ENDSR
014100001218     C*------------------------------------------------------------------------*
014200001218
014300001218
014400001218
014500001218     C*------------------------------------------------------------------------*
014600001218     C* PROCEDI - Routine principale
014700001218     C*------------------------------------------------------------------------*
014800001218     C     PROCEDI       BEGSR
014900050909     C*
015000050909     C* Inizializzo variabili d wrk
015100050909     C                   movel     'N'           wProcedi          1
015200050909     C*
015300050909     C* Apro il file di output
015400161202     C                   if        *in25
015500050909     C                   open      fnvac00t
015600050909     C                   movel     'S'           wProcedi
015700050909     C                   endif
015800161202     C*
015900161202     C* Inizializzo la transazione
016000161202     C                   if        *in26
016100161206     C                   open      tivgd00f
016200170227     C*
016300170227     C* Stacco progressivo univoco download
016400170227     C                   CLEAR                   TIS7VASDS
016500170227     C                   EVAL      i§7VASOPZ = 'PRG'
016600170227     C                   CALL(e)   'TIS7VASR1'
016700170227     C                   PARM                    TIS7VASDS
016800170227     C*
016900170227     C* Se OK => proseguo
017000170227     C                   if        not %error AND
017100170227     C                             o§7VASOK = *on AND o§7VASPRG <> *blanks
017200170227     C                   movel     'S'           wProcedi
017300170227     C                   endif
017400161205     C                   endif
017500050909     C*
017600050909     C* Se ok a procedere => elaboro
017700050909     C                   if        wProcedi = 'S'
017800001218     C*
017900001218     C* Ciclo sulla schiera dei clienti figli dell'unificante preso in input
018000001218     C                   z-add     1             I
018100090407     C                   sorta     KUNI
018200001218     C                   dow       KUNI(I) > *zeros
018300070718     C*
018400070718     C* Se richiesta elaborazione x codice cliente mittente...
018500100512     C                   select
018600100512     C                   when      TIPCLI = *blanks
018700100512     C                   exsr      EXECCM
018800070718     C*
018900070718     C* Se richiesta elaborazione x codice cliente fatturazione...
019000100512     C                   when      TIPCLI = 'K'
019100100512     C                   exsr      EXEKSC
019200100512     C*
019300100512     C* Se richiesta elaborazione x entrambi: codice cliente mittente + fatturazione...
019400100512     C                   when      TIPCLI = 'E'
019500100512     C                   exsr      EXECCM
019600100512     C                   exsr      EXEKSC
019700100512     C*
019800100512     C                   endsl
019900001218     C*
020000001218     C                   add       1             I
020100001218     C                   enddo
020200050909     C*
020300050909     C                   endif
020400001218     C*
020500001218     C* Chiudo il file di output
020600050823     C   25              close     fnvac00t
020700170227     C   26              close     tivgd00f
020800170227     C*
020900170227     C* Finalizzo la transazione
021000170227     C                   if        *in26
021100170227     C                   EVAL      i§7VASOPZ  = 'RLS'
021200170227     C                   EVAL      i§7VASTIP  = TIPFILE
021300170227     C                   EVAL      i§7VASKSU  = '0'+CODCLIVAS
021400170227     C                   EVAL      i§7VASTSC  = 'WW'
021500170227     C                   EVAL      i§7VASSTO  = '?'
021600170227     C                   EVAL      i§7VASSTTO = 'L'
021700170227     C                   EVAL      i§7VASPRG  = o§7VASPRG
021800170227     C                   CALL(e)   'TIS7VASR1'
021900170227     C                   PARM                    TIS7VASDS
022000170227     C*
022100170227     C* Verifico esito chiamata
022200170227     C                   if        %error OR o§7VASOK = *off
022300170227     C                   exsr      EXEERR
022400170227     C                   endif
022500170227     C                   endif
022600001218     C*
022700001218     C                   ENDSR
022800001218     C*------------------------------------------------------------------------*
022900100512
023000100512
023100100512
023200100512     C*------------------------------------------------------------------------*
023300161205     C* EXECCM   - Elaborazione x codice cliente mittente
023400100512     C*------------------------------------------------------------------------*
023500100512     C     EXECCM        BEGSR
023600100521     C*
023700100521     C                   movel     'C'           wChkCli           1
023800100512     C*
023900100512     C                   select
024000100512     C* ...se richiesta elaborazione x data spedizione
024100100512     C                   when      DATADA > *zeros
024200100512     C                   exsr      SQ_CCMDSP
024300100512     C* ...se richiesta elaborazione x data consegna merce
024400100512     C                   when      DACMDA > *zeros
024500100512     C                   exsr      SQ_CCMDCM
024600100512     C                   endsl
024700100512     C*
024800100512     C                   ENDSR
024900100512     C*------------------------------------------------------------------------*
025000100512
025100100512
025200100512
025300100512     C*------------------------------------------------------------------------*
025400161205     C* EXEKSC   - Elaborazione x codice cliente fatturazione
025500100512     C*------------------------------------------------------------------------*
025600100512     C     EXEKSC        BEGSR
025700100521     C*
025800100521     C                   movel     'K'           wChkCli           1
025900100512     C*
026000100512     C                   select
026100100512     C* ...se richiesta elaborazione x data spedizione
026200100512     C                   when      DATADA > *zeros
026300100512     C                   exsr      SQ_KSCDSP
026400100512     C* ...se richiesta elaborazione x data consegna merce
026500100512     C                   when      DACMDA > *zeros
026600100512     C                   exsr      SQ_KSCDCM
026700100512     C                   endsl
026800100512     C*
026900100512     C                   ENDSR
027000100512     C*------------------------------------------------------------------------*
027100100512
027200100512
027300100512
027400100512     C*------------------------------------------------------------------------*
027500161205     C* SQ_CCMDSP - Elaborazione x codice cliente mittente + data spedizione
027600100512     C*------------------------------------------------------------------------*
027700100512     C     SQ_CCMDSP     BEGSR
027800100512     C*
027900100512     C                   z-add     KUNI(I)       wParCCM           7 0
028000100512     C*
028100100512     C/EXEC SQL
028200100512     C+ declare C_CCMDSP cursor for
028300100512     C+ select * from titas00f
028400100512     C+ where tasccm = :wParCCM and
028500100512     C+ tasaas*10000+tasmgs between :DATADA and :DATAA
028600100512     C+ union
028700100512     C+ select * from titas10f
028800100512     C+ where tasccm = :wParCCM and
028900100512     C+ tasaas*10000+tasmgs between :DATADA and :DATAA
029000100512     C+ union
029100100512     C+ select * from titasP0f
029200100512     C+ where tasccm = :wParCCM and
029300100512     C+ tasaas*10000+tasmgs between :DATADA and :DATAA
029400100512     C+ for read only
029500100512     C/END-EXEC
029600100512     C
029700100512     C/EXEC SQL
029800100512     C+ open C_CCMDSP
029900100512     C/END-EXEC
030000100512     C
030100100512     C/EXEC SQL
030200100512     C+ Fetch C_CCMDSP into :TITAS00F
030300100512     C/END-EXEC
030400100512     C*
030500100512     C                   dow       sqlcod = *zeros
030600100512     C                   exsr      verifica
030700100512     C                   if        CHKREC = 'S'
030800100512     C                   exsr      scriviVAC
030900100512     C                   endif
031000100512     C
031100100512     C/EXEC SQL
031200100512     C+ Fetch C_CCMDSP into :TITAS00F
031300100512     C/END-EXEC
031400100512     C*
031500100512     C                   enddo
031600100512     C*
031700100512     C/EXEC SQL
031800100512     C+ close C_CCMDSP
031900100512     C/END-EXEC
032000100512     C*
032100100512     C*
032200100512     C                   ENDSR
032300100512     C*------------------------------------------------------------------------*
032400100512
032500100512
032600100512
032700100512     C*------------------------------------------------------------------------*
032800161205     C* SQ_CCMDCM - Elaborazione x codice cliente mittente + data consegna merce
032900100512     C*------------------------------------------------------------------------*
033000100512     C     SQ_CCMDCM     BEGSR
033100100512     C*
033200100512     C                   z-add     KUNI(I)       wParCCM           7 0
033300100512     C*
033400100512     C/EXEC SQL
033500100512     C+ declare C_CCMDCM cursor for
033600100512     C+ select * from titas00f
033700100512     C+ where tasccm = :wParCCM and
033800100512     C+ tasdcm between :DACMDA and :DACMA
033900100512     C+ union
034000100512     C+ select * from titas10f
034100100512     C+ where tasccm = :wParCCM and
034200100512     C+ tasdcm between :DACMDA and :DACMA
034300100512     C+ union
034400100512     C+ select * from titasP0f
034500100512     C+ where tasccm = :wParCCM and
034600100512     C+ tasdcm between :DACMDA and :DACMA
034700100512     C+ for read only
034800100512     C/END-EXEC
034900100512     C
035000100512     C/EXEC SQL
035100100512     C+ open C_CCMDCM
035200100512     C/END-EXEC
035300100512     C
035400100512     C/EXEC SQL
035500100512     C+ Fetch C_CCMDCM into :TITAS00F
035600100512     C/END-EXEC
035700100512     C*
035800100512     C                   dow       sqlcod = *zeros
035900100512     C                   exsr      verifica
036000100512     C                   if        CHKREC = 'S'
036100100512     C                   exsr      scriviVAC
036200100512     C                   endif
036300100512     C
036400100512     C/EXEC SQL
036500100512     C+ Fetch C_CCMDCM into :TITAS00F
036600100512     C/END-EXEC
036700100512     C*
036800100512     C                   enddo
036900100512     C*
037000100512     C/EXEC SQL
037100100512     C+ close C_CCMDCM
037200100512     C/END-EXEC
037300100512     C*
037400100512     C*
037500100512     C                   ENDSR
037600100512     C*------------------------------------------------------------------------*
037700100512
037800100512
037900100512
038000100512     C*------------------------------------------------------------------------*
038100161205     C* SQ_KSCDSP - Elaborazione x codice cliente fatturazione + data spedizione
038200100512     C*------------------------------------------------------------------------*
038300100512     C     SQ_KSCDSP     BEGSR
038400100512     C*
038500100512     C                   z-add     KUNI(I)       wParKSC           7 0
038600100512     C*
038700100512     C/EXEC SQL
038800100512     C+ declare C_KSCDSP cursor for
038900100512     C+ select * from titas00f
039000100512     C+ where tasksc = :wParKSC and
039100100512     C+ tasaas*10000+tasmgs between :DATADA and :DATAA
039200100512     C+ union
039300100512     C+ select * from titas10f
039400100512     C+ where tasksc = :wParKSC and
039500100512     C+ tasaas*10000+tasmgs between :DATADA and :DATAA
039600100512     C+ union
039700100512     C+ select * from titasP0f
039800100512     C+ where tasksc = :wParKSC and
039900100512     C+ tasaas*10000+tasmgs between :DATADA and :DATAA
040000100512     C+ for read only
040100100512     C/END-EXEC
040200100512     C
040300100512     C/EXEC SQL
040400100512     C+ open C_KSCDSP
040500100512     C/END-EXEC
040600100512     C
040700100512     C/EXEC SQL
040800100512     C+ Fetch C_KSCDSP into :TITAS00F
040900100512     C/END-EXEC
041000100512     C*
041100100512     C                   dow       sqlcod = *zeros
041200100512     C                   exsr      verifica
041300100512     C                   if        CHKREC = 'S'
041400100512     C                   exsr      scriviVAC
041500100512     C                   endif
041600100512     C
041700100512     C/EXEC SQL
041800100512     C+ Fetch C_KSCDSP into :TITAS00F
041900100512     C/END-EXEC
042000100512     C*
042100100512     C                   enddo
042200100512     C*
042300100512     C/EXEC SQL
042400100512     C+ close C_KSCDSP
042500100512     C/END-EXEC
042600100512     C*
042700100512     C*
042800100512     C                   ENDSR
042900100512     C*------------------------------------------------------------------------*
043000100512
043100100512
043200100512
043300100512     C*------------------------------------------------------------------------*
043400161205     C* SQ_KSCDCM - Elaborazione x codice cliente fatturazione + data consegna merce
043500100512     C*------------------------------------------------------------------------*
043600100512     C     SQ_KSCDCM     BEGSR
043700100512     C*
043800100512     C                   z-add     KUNI(I)       wParKSC           7 0
043900100512     C*
044000100512     C/EXEC SQL
044100100512     C+ declare C_KSCDCM cursor for
044200100512     C+ select * from titas00f
044300100512     C+ where tasksc = :wParKSC and
044400100512     C+ tasdcm between :DACMDA and :DACMA
044500100512     C+ union
044600100512     C+ select * from titas10f
044700100512     C+ where tasksc = :wParKSC and
044800100512     C+ tasdcm between :DACMDA and :DACMA
044900100512     C+ union
045000100512     C+ select * from titasP0f
045100100512     C+ where tasksc = :wParKSC and
045200100512     C+ tasdcm between :DACMDA and :DACMA
045300100512     C+ for read only
045400100512     C/END-EXEC
045500100512     C
045600100512     C/EXEC SQL
045700100512     C+ open C_KSCDCM
045800100512     C/END-EXEC
045900100512     C
046000100512     C/EXEC SQL
046100100512     C+ Fetch C_KSCDCM into :TITAS00F
046200100512     C/END-EXEC
046300100512     C*
046400100512     C                   dow       sqlcod = *zeros
046500100512     C                   exsr      verifica
046600100512     C                   if        CHKREC = 'S'
046700100512     C                   exsr      scriviVAC
046800100512     C                   endif
046900100512     C
047000100512     C/EXEC SQL
047100100512     C+ Fetch C_KSCDCM into :TITAS00F
047200100512     C/END-EXEC
047300100512     C*
047400100512     C                   enddo
047500100512     C*
047600100512     C/EXEC SQL
047700100512     C+ close C_KSCDCM
047800100512     C/END-EXEC
047900100512     C*
048000100512     C*
048100100512     C                   ENDSR
048200100512     C*------------------------------------------------------------------------*
048300100512
048400100512
048500100512
048600100512     C*------------------------------------------------------------------------*
048700161205     C* CHKCONS  - Verifica se bolla effettivamente consegnata
048800100512     C*------------------------------------------------------------------------*
048900100512     C     CHKCONS       BEGSR
049000100512     C*
049100100512     C                   setoff                                       4041
049200100512     C                   movel     *blanks       wEsito1           1
049300100512     C                   movel     *blanks       wEsito2           1
049400100512     C*
049500100512     C                   eval      wAAS = tasAAS
049600100512     C                   eval      wLNP = tasLNP
049700100512     C                   eval      wNRS = tasNRS
049800100512     C                   eval      wNSP = tasNSP
049900100512     C*
050000100512     C* Chiamata metodo GetLblTyp
050100100512     C                   eval      wEsito1 = %char(UBLBLSPE_GetLblTyp(
050200100512     C                                                wAAS
050300100512     C                                               :wLNP
050400100512     C                                               :wNRS
050500100512     C                                               :wNSP
050600100512     C                                               :pOutLblTyp
050700100512     C                                               :pOutAnnoBO
050800100512     C                                               :pOutLineaParBO
050900100512     C                                               :pOutSerieBO
051000100512     C                                               :pOutNumSpedBO
051100100512     C                                               :pOutDcmBO
051200100512     C                                               :pOutCcaBO
051300100512     C                                               :pOutRblBO))
051400100512     C* Se trattasi d bolla nn originale reperisco la relativa bolla originale
051500100512     C                   if        pOutLblTyp <> 'O'
051600100512     C                   eval      wAAS = pOutAnnoBO
051700100512     C                   eval      wLNP = pOutLineaParBO
051800100512     C                   eval      wNRS = pOutSerieBO
051900100512     C                   eval      wNSP = pOutNumSpedBO
052000100512     C                   endif
052100100512     C*
052200100512     C* Chiamata metodo GetLastChild
052300100512     C                   eval      wEsito2 = %char(UBLBLSPE_GetLastChild(
052400100512     C                                                wAAS
052500100512     C                                               :wLNP
052600100512     C                                               :wNRS
052700100512     C                                               :wNSP
052800100512     C                                               :pOutAnnoFI
052900100512     C                                               :pOutLineaParFI
053000100512     C                                               :pOutSerieFI
053100100512     C                                               :pOutNumSpedFI
053200100512     C                                               :pOutDcmFI
053300100512     C                                               :pOutCcaFI))
053400100512     C*
053500100512     C* Considerazioni sullo stato corrente della bolla
053600100512     C                   if        (wEsito2 = '9' and pOutDcmBO > *zeros)  or
053700100512     C                             (wEsito2 = '0' and pOutDcmFI > *zeros)
053800161202     C                   seton                                        40        * bolla chiusa
053900100512     C                   endif
054000100512     C*
054100100512     C                   if        (wEsito2 = '9' and pOutDcmBO > *zeros
054200100512     C                                            and pOutCcaBO = *blanks) or
054300100512     C                             (wEsito2 = '0' and pOutDcmFI > *zeros
054400100512     C                                            and pOutCcaFI = *blanks)
054500100512     C                   seton                                        41        * bolla cons. OK
054600100512     C                   endif
054700100512     C*
054800100512     C                   ENDSR
054900100512     C*------------------------------------------------------------------------*
055000001218
055100001218
055200001218
055300001218     C*------------------------------------------------------------------------*
055400161205     C* VERIFICA - Routine di verifica validità record corrente
055500001218     C*------------------------------------------------------------------------*
055600001218     C     VERIFICA      BEGSR
055700001218     C*
055800001218     C                   movel     'S'           CHKREC            1
055900001218     C*
056000100512     C* Verifico le date spedizione
056100001218     C                   if        CHKREC = 'S'
056200100512     C                   if        DATADA > *zeros
056300001218     C                   movel     tasaas        wrkdata           8 0
056400001218     C                   move      tasmgs        wrkdata
056500001218     C                   if        wrkdata < DATADA or
056600001218     C                             wrkdata > DATAA
056700001218     C                   movel     'N'           CHKREC
056800100512     C                   endif
056900001218     C                   endif
057000001218     C                   endif
057100100512     C*
057200100512     C* Verifico le date spedizione
057300100512     C                   if        CHKREC = 'S'
057400100512     C                   if        DACMDA > *zeros
057500100512     C                   if        tasdcm  < DACMDA or
057600100512     C                             tasdcm  > DACMA
057700100512     C                   movel     'N'           CHKREC
057800100512     C                   endif
057900100512     C                   endif
058000100512     C                   endif
058100100521     C*
058200100521     C* Verifico il codice cliente mittente
058300100521     C                   if        CHKREC = 'S'
058400100521     C                   if        wChkCli  = 'C'      AND
058500100521     C                             tasccm  <> KUNI(I)
058600100521     C                   movel     'N'           CHKREC
058700100521     C                   endif
058800100521     C                   endif
058900070718     C*
059000100517     C* Verifico il codice cliente fatturazione (in caso d 'K')
059100070718     C                   if        CHKREC = 'S'
059200100521     C                   if        wChkCli  = 'K'      AND
059300100521     C                             tasksc  <> KUNI(I)
059400070718     C                   movel     'N'           CHKREC
059500070718     C                   endif
059600070718     C                   endif
059700100517     C*
059800100517     C* Verifico il codice cliente fatturazione (in caso d 'E')
059900100517     C                   if        CHKREC = 'S'
060000100521     C                   if        TIPCLI  = 'E'       AND
060100100521     C                             wChkCli = 'K'       AND
060200100521     C                             tasksc  = tasccm
060300100517     C                   movel     'N'           CHKREC
060400100517     C                   endif
060500100517     C                   endif
060600001218     C*
060700020923     C* Verifico che la bolla abbia la data consegna se trattasi di bolla PT
060800001218     C                   if        CHKREC = 'S'
060900020923     C                   if        tasTSP = 'P'
061000020923     C                   if        tasDCM > *zeros
061100001218     C                   else
061200001218     C                   movel     'N'           CHKREC
061300001218     C                   endif
061400020923     C                   endif
061500001218     C                   endif
061600020920     C*
061700040322     C* Verifico che il tipo bolla sia da considerare ai fini della rigenerazione dell'FNVAC
061800020920     C                   if        CHKREC = 'S'
061900040322     C     tasCBO        lookup    COD3A                                  55
062000081016     C                   if        %equal
062100040322     C                   else
062200020920     C                   movel     'N'           CHKREC
062300020920     C                   endif
062400020920     C                   endif
062500010301     C*
062600010301     C* Verifico che la bolla se POSTE non abbia il codice CA = 5
062700010301     C                   if        CHKREC = 'S'
062800010301     C                   if        tasTSP = 'P' and tasCCA = '5'
062900010301     C                   movel     'N'           CHKREC
063000010301     C                   endif
063100010301     C                   endif
063200100512     C*
063300040322     C* Verifico se richiesto nel lancio che la bolla sia consegnata
063400040322     C                   if        CHKREC = 'S'
063500040322     C                   if        FLGCONS = 'S'
063600100512     C                   exsr      CHKCONS
063700100512     C                   if        *in40 = *on
063800040322     C                   else
063900040322     C                   movel     'N'           CHKREC
064000040322     C                   endif
064100040322     C                   endif
064200040322     C                   endif
064300100512     C*
064400100512     C* Verifico se richiesto nel lancio che la bolla sia consegnata OK
064500100512     C                   if        CHKREC = 'S'
064600100512     C                   if        FLGCONS = 'O'
064700100512     C                   exsr      CHKCONS
064800100512     C                   if        *in41 = *on
064900100512     C                   else
065000100512     C                   movel     'N'           CHKREC
065100100512     C                   endif
065200100512     C                   endif
065300100512     C                   endif
065400100513     C*
065500100907     C* Verifico se richiesto nel lancio che la bolla NON sia consegnata OK
065600100513     C                   if        CHKREC = 'S'
065700100513     C                   if        FLGCONS = 'K'
065800100513     C                   exsr      CHKCONS
065900100513     C                   if        *in41 = *on
066000100513     C                   movel     'N'           CHKREC
066100100513     C                   endif
066200100513     C                   endif
066300100513     C                   endif
066400100907     C*
066500100907     C* Verifico se richiesto nel lancio che la bolla NON sia consegnata
066600100907     C                   if        CHKREC = 'S'
066700100907     C                   if        FLGCONS = 'P'
066800100907     C                   exsr      CHKCONS
066900100907     C                   if        *in40 = *on
067000100907     C                   movel     'N'           CHKREC
067100100907     C                   endif
067200100907     C                   endif
067300100907     C                   endif
067400040322     C*
067500040322     C* Verifico se richiesto nel lancio che la bolla provenga da scambio dati VAS cliente
067600040322     C                   if        CHKREC = 'S'
067700040322     C                   if        FLGVAS = 'S'
067800040322     C                   if        tasSOP = '§' OR
067900040322     C                             tasSOP = ':' OR
068000040322     C                             tasSOP = '%'
068100040322     C                   else
068200040322     C                   movel     'N'           CHKREC
068300040322     C                   endif
068400040322     C                   endif
068500040322     C                   endif
068600081016     C*
068700081016     C* Verifico se richiesto che la bolla corrente sia la singola bolla richiesta a video
068800081016     C                   if        CHKREC = 'S'
068900081016     C                   if        KAAS   <> *zeros OR
069000081016     C                             KLNP   <> *zeros OR
069100081016     C                             KNRS   <> *zeros OR
069200081016     C                             KNSP   <> *zeros
069300081016     C                   if        tasAAS = KAAS AND
069400081016     C                             tasLNP = KLNP AND
069500081016     C                             tasNRS = KNRS AND
069600081016     C                             tasNSP = KNSP
069700081016     C                   else
069800081016     C                   movel     'N'           CHKREC
069900081016     C                   endif
070000081016     C                   endif
070100081016     C                   endif
070200001218     C*
070300001218     C                   ENDSR
070400001218     C*------------------------------------------------------------------------*
070500001218
070600001218
070700001218
070800001218     C*------------------------------------------------------------------------*
070900001218     C* SCRIVIVAC - Routine di scrittura file esiti di consegna (FNVAC)
071000001218     C*------------------------------------------------------------------------*
071100001218     C     SCRIVIVAC     BEGSR
071200001218     C*
071300001218     C                   CLEAR                   fnvac000
071400001218     C*
071500001218     C                   MOVEL     tasAAS        VACAAS
071600001218     C                   MOVEL     tasLNP        VACLNP
071700001218     C                   MOVEL     tasNRS        VACNRS
071800001218     C                   MOVEL     tasNSP        VACNSP
071900001218     C                   MOVEL     tasMGS        VACMGS
072000001218     C                   MOVEL     tasCBO        VACCBO
072100001218     C                   MOVEL     tasLNA        VACLNA
072200010223     C                   MOVEL     tasRSD        VACRSD
072300010223     C                   MOVEL     tasPRD        VACPRD
072400010301     C*
072500001218     C* Reperisco dall'estensione anagrafica bolle di SEDE i dati del Destinatario
072600001218     C                   EXSR      REPDEST
072700151019     C*
072800151019     C* Eseguo eventuali "forzature"
072900151019     C                   EXSR      RTVAR5FAT
073000160208     C*
073100170227     C* Reperisco NATURA MERCE
073200160208     C                   EXSR      RTVNAS
073300001218     C*
073400001218     C                   MOVEL     tasGC1        VACGC1
073500001218     C                   MOVEL     tasGC2        VACGC2
073600001218     C                   MOVEL     tasCTR        VACCTR
073700001218     C                   MOVEL     tasCTS        VACCTS
073800001218     C                   MOVEL     tasFTM        VACFTM
073900001218     C                   MOVEL     tasFIN        VACFIN
074000001218     C                   MOVEL     tasFAP        VACFAP
074100001218     C                   MOVEL     tasTSP        VACTSP
074200001218     C* Effettuo considerazioni particolare per bolla "POSTE"
074300001218     C     tasTSP        IFEQ      'P'                                          BOLLA POSTE
074400001218     C                   EXSR      REPPTIAS
074500001218     C                   EXSR      REPPTGIA
074600001218     C                   Z-ADD     tasNCD        VACDCR
074700001218     C                   ELSE
074800001218     C                   MOVEL     tasIAS        VACIAS
074900001218     C                   MOVEL     tasVAS        VACVAS
075000001218     C                   MOVEL     tasDCR        VACDCR
075100050310     C     KGCP          CHAIN     TIGCP51L
075200050310     C                   IF        %found(TIGCP51L)
075300001218     C                   MOVEL     GCPAGC        VACDAG
075400001218     C                   MOVE      GCPMGC        VACDAG
075500001218     C                   ENDIF
075600001218     C                   ENDIF
075700001218     C*
075800160208     C                   MOVEL     §TA4ANAS      VACNAS
075900001218     C                   MOVEL     tasNCL        VACNCL
076000001218     C                   MOVEL     tasPKF        VACPKB
076100001218     C                   MOVEL     tasVLF        VACVLB
076200001218     C                   MOVEL     tasQFT        VACQFT
076300001218     C*
076400001218     C                   EXSR      REPCSB
076500001218     C*
076600001218     C                   MOVEL     tasCCM        VACCCM
076700001218     C                   MOVEL     tasRMN        VACRMN
076800010301     C*
076900001218     C                   EXSR      REPRMA
077000010301     C*
077100001218     C                   MOVEL     tasFFD        VACFFD
077200001218     C                   MOVEL     tasTCR        VACTCR
077300001218     C                   MOVEL     tasHCR        VACHCR
077400001218     C                   MOVEL     tasFTC        VACTC1
077500001218     C                   MOVEL     tasTC2        VACTC2
077600001218     C                   MOVEL     tasDCM        VACDCM
077700001218     C                   MOVEL     tasHMC        VACHMC
077800001218     C*
077900010301     C                   IF        tasTSP = 'P' and tasCCA = '7'
078000010301     C                   MOVEL     *blanks       VACCCA
078100010301     C                   ELSE
078200100910     C                   MOVEL     tasCCA        VACCCA
078300100910     C*
078400100910     C* Se CCA ancora nn valorizzato => reperisco l'ultimo evento
078500100910     C                   IF        VACCCA = *blanks
078600100910     C                   EXSR      REPLASTEV
078700010301     C                   ENDIF
078800100910     C                   ENDIF
078900010301     C*
079000120427     C* Reperisco l'eventuale lasciato avviso
079100001218     C                   EXSR      REPLAV
079200120427     C*
079300120427     C* Effettuo particolari operazioni in caso d DIROTTAMENTO
079400120427     C***                IF        VACCCA = '1' AND
079500120427     C***                          VACDCM = *zeros
079600120427     C***                EXSR      REPDIR
079700120427     C***                ENDIF
079800001218     C*
079900020923     C* Verifico che in caso di bolla NON PT siano valorizzate o la data consegna...
080000020923     C* ... o la data lasciato avviso o la data giacenza o la consegna anomala
080100020923     C                   IF        tasTSP <> 'P'
080200071228     C*
080300071228     C* Se richiesto nel lancio anche bolle senza eventi => gestisco...
080400071228     C* ...altrimenti standard
080500071228     C                   IF        FLGNOE  = 'N'
080600020923     C                   IF        vacDCM  > *zeros OR
080700020923     C                             vacDLA  > *zeros OR
080800020923     C                             vacDAG  > *zeros OR
080900020923     C                             vacCCA <> *blanks
081000071228     C   25              WRITE     FNVAC000
081100161205     C   26              exsr      WRITIVGD
081200020923     C                   endif
081300071228     C                   else
081400071228     C   25              WRITE     FNVAC000
081500161205     C   26              exsr      WRITIVGD
081600071228     C                   endif
081700020923     C                   else
081800050823     C   25              WRITE     FNVAC000
081900161205     C   26              exsr      WRITIVGD
082000020923     C                   endif
082100001218     C*
082200001218     C                   ENDSR
082300120427
082400120427
082500120427
082600120427     C*------------------------------------------------------------------------*
082700161205     C* REPDIR - Reperimento data DIROTTAMENTO
082800120427     C*------------------------------------------------------------------------*
082900120427     C     REPDIR        BEGSR
083000120427     C*
083100120427     C                   movel     *blanks       wEsito1           1
083200120427     C*
083300120427     C* Chiamata metodo GetLblTyp
083400120427     C                   eval      wEsito1 = %char(UBLBLSPE_GetLblTyp(
083500120427     C                                                tasAAS
083600120427     C                                               :tasLNP
083700120427     C                                               :tasNRS
083800120427     C                                               :tasNSP
083900120427     C                                               :pOutLblTyp
084000120427     C                                               :pOutAnnoBO
084100120427     C                                               :pOutLineaParBO
084200120427     C                                               :pOutSerieBO
084300120427     C                                               :pOutNumSpedBO
084400120427     C                                               :pOutDcmBO
084500120427     C                                               :pOutCcaBO
084600120427     C                                               :pOutRblBO))
084700120427     C*
084800120427     C* Se reperimento effettuato e trattasi d dirottamento => recupero la data
084900120427     C                   if        wEsito1 = '0' and pOutCcaBO = '1'
085000120427     C                   eval      VACDCM = pOutDcmBO
085100120427     C                   endif
085200120427     C*
085300120427     C                   ENDSR
085400120427     C*------------------------------------------------------------------------*
085500100910
085600100910
085700100910
085800100910     C*------------------------------------------------------------------------*
085900161205     C* REPLASTEV - Routine di reperimento ultimo evento
086000100910     C*------------------------------------------------------------------------*
086100100910     C     REPLASTEV     BEGSR
086200100910     C*
086300100910     C     KEVB          SETGT     FNEVB01L
086400100910     C     KEVB          READPE    FNEVB01L                               34
086500100910     C*
086600100910     C* Devo cercare l'ultimo evento di tipo giacenza
086700100910     C                   IF        *IN34 = *OFF
086800100910     C*
086900100910     C* Evento 'MIC' considero solo se data evento >= data corrente
087000100910     C                   IF        EVBDEV >= datcor
087100100910     C* - MIC
087200100910     C                   IF        EVBCEV = 'MIC'
087300100910     C                   MOVEL     'C'           VACCCA
087400100910     C                   MOVEL     evbDEV        VACDCM
087500100910     C                   ENDIF
087600100910     C                   ENDIF
087700100910     C*
087800100910     C                   ENDIF
087900100910     C*
088000100910     C                   ENDSR
088100100910     C*------------------------------------------------------------------------*
088200050823
088300050823
088400050823
088500050823     C*------------------------------------------------------------------------*
088600161205     C* WRITIVGD - Routine di scrittura file TIVGD (file VAS generico download)
088700050823     C*------------------------------------------------------------------------*
088800161205     C     WRITIVGD      BEGSR
088900050823     C*
089000161206     C                   clear                   tivgd000
089100080407     C                   eval      vgdDTA = %subst(FNVACDS:1:%size(FNVACDS))
089200100312     C                   eval      vgdTIP = TIPFILE
089300170227     C                   eval      vgdKSU = '0'+CODCLIVAS
089400050823     C                   eval      vgdTSC = 'WW'
089500050823     C                   eval      vgdDAT = datcor
089600170227     C                   eval      vgdPRG = o§7VASPRG
089700161205     C                   eval      vgdPGM = procname
089800170227     C                   eval      vgdSTO = '?'
089900170227     C                   write     tivgd000
090000161205     C*
090100050823     C                   ENDSR
090200050823     C*------------------------------------------------------------------------*
090300001218
090400001218
090500161202
090600001218     C*------------------------------------------------------------------------*
090700001218     C* REPPTIAS - Routine di reperimento dati relativi all'importo/divisa da assicurare x POSTE
090800001218     C*------------------------------------------------------------------------*
090900001218     C     REPPTIAS      BEGSR
091000001218     C*
091100060215     C* Prendo importo da assicurare da TITA4 t.r.=J
091200001218     C                   movel     'J'           KTRC
091300001218     C     KTA4          chain     tita430c
091400001218     C                   if        %found(tita430c)
091500001218     C                   movel     ta4NOT        DSBL4J
091600001218     C                   z-add     §B4IAS        vacIAS
091700001218     C                   movel     §B4VAS        vacVAS
091800001218     C                   endif
091900001218     C*
092000001218     C                   ENDSR
092100001218     C*------------------------------------------------------------------------*
092200001218
092300001218
092400001218
092500001218     C*------------------------------------------------------------------------*
092600001218     C* REPRMA - Routine di reperimento dati relativi al riferimento mittente alfabetico
092700001218     C*------------------------------------------------------------------------*
092800001218     C     REPRMA        BEGSR
092900001218     C*
093000060215     C* Prendo importo da assicurare da TITA4 t.r.=A
093100001218     C                   movel     'A'           KTRC
093200001218     C     KTA4          chain     tita430c
093300001218     C                   if        %found(tita430c)
093400001218     C                   eval      vacRMA = %subst(ta4NOT:1:15)
093500001218     C                   endif
093600001218     C*
093700001218     C                   ENDSR
093800001218     C*------------------------------------------------------------------------*
093900001218
094000001218
094100001218
094200001218     C*------------------------------------------------------------------------*
094300001218     C* REPDEST - Routine di reperimento dati relativi al destinatario della bolla (TITAA)
094400001218     C*           ... e ai dati del mittente originale
094500001218     C*------------------------------------------------------------------------*
094600001218     C     REPDEST       BEGSR
094700001218     C*
094800010223     C* Prendo il tipo anagrafica = D
094900010223     C*                  movel     'D'           KTRC
095000010223     C*    KTAA          chain     titaa30c
095100010223     C*                  if        %found(titaa30c)
095200010223     C*                  movel     taaRSC        vacRSD
095300010223     C*                  movel     taaPRV        vacPRD
095400010223     C*                  endif
095500001218     C*
095600010223     C* Prendo il tipo anagrafica = O
095700001218     C                   movel     'O'           KTRC
095800001218     C     KTAA          chain     titaa30c
095900001218     C                   if        %found(titaa30c)
096000001218     C                   movel     taaRSC        vacRMO
096100001218     C                   endif
096200001218     C*
096300001218     C                   ENDSR
096400001218     C*------------------------------------------------------------------------*
096500001218
096600001218
096700001218
096800001218     C*------------------------------------------------------------------------*
096900001218     C* REPCSB - Routine di reperimento dati relativi al contrassegno
097000001218     C*------------------------------------------------------------------------*
097100001218     C     REPCSB        BEGSR
097200001218     C*
097300001219     C     KCSB          chain     tncsb03l
097400001219     C                   if        %found(tncsb03l)
097500001218     C                   if        csbSTA = 9 and tasTSP <> 'P'
097600001218     C                   else
097700001218     C                   movel     csbTPA        vactic
097800001218     C                   move      csbTPI        vactic
097900001218     C                   z-add     csbCAS        vacCAS
098000001218     C                   movel     csbVCA        vacVCA
098100001218     C                   endif
098200001218     C                   endif
098300001218     C*
098400001218     C                   ENDSR
098500001218     C*------------------------------------------------------------------------*
098600001218
098700001218
098800001218
098900001218     C*------------------------------------------------------------------------*
099000001218     C* REPPTGIA - Routine di reperimento dati relativi alla giacenza x POSTE
099100001218     C*------------------------------------------------------------------------*
099200001218     C     REPPTGIA      BEGSR
099300001218     C*
099400001218     C* Reperisco la data di giacenza dagli eventi poichè non vengono
099500001218     C* gestite le giacenze per le bolle poste
099600001218     C     KEVB          SETGT     FNEVB01L
099700001218     C     KEVB          READPE    FNEVB01L                               34
099800001218     C*
099900001218     C* Devo cercare l'ultimo evento di tipo giacenza
100000001218     C     *IN34         DOWEQ     *OFF
100100001218     C                   Z-ADD     1             W
100200001218     C     EVBCEV        LOOKUP    C2A(W)                                 42
100300001218     C     *IN42         IFEQ      '1'
100400001218     C     T2A(W)        ANDEQ     'G'
100500001218     C                   Z-ADD     EVBDEV        vacDAG
100600001218     C                   LEAVE
100700001218     C                   ELSE
100800001218     C     KEVB          READPE    FNEVB000                               34
100900001218     C                   ENDIF
101000001218     C                   ENDDO
101100001218     C*
101200001218     C                   ENDSR
101300001218     C*------------------------------------------------------------------------*
101400001218
101500001218
101600001218
101700001218
101800001218     C*------------------------------------------------------------------------*
101900001218     C* REPLAV - Routine di reperimento dati relativi al lasciato avviso
102000001218     C*------------------------------------------------------------------------*
102100001218     C     REPLAV        BEGSR
102200001218     C*
102300001218     C* Pererisco l'ultimo evento Lasciato Avviso
102400001218     C                   MOVEL     '2A'          COD
102500001218     C     KEVB          SETGT     FNEVB01L
102600001218     C     KEVB          READPE    FNEVB01L                               34
102700001218     C*
102800001218     C     *IN34         DOWEQ     *OFF
102900001218     C                   MOVEL(P)  EVBCEV        KEY
103000001218     C     KTAB2         CHAIN     TABEL                              30
103100001218     C     *IN30         IFEQ      *OFF
103200001218     C                   MOVEL     TBLUNI        DS2A
103300001218     C     §2AFTC        IFEQ      'A'
103400001218     C                   Z-ADD     EVBDEV        vacDLA
103500001218     C                   LEAVE
103600001218     C                   ENDIF
103700001218     C                   ENDIF
103800001218     C     KEVB          READPE    FNEVB000                               34
103900001218     C                   ENDDO
104000001218     C*
104100001218     C                   ENDSR
104200001218     C*------------------------------------------------------------------------*
104300001218
104400001218
104500001218
104600001218     C*------------------------------------------------------------------------*
104700001218     C* CARTBL - Routine di caricamento dati tabellati
104800001218     C*------------------------------------------------------------------------*
104900001218     C     CARTBL        BEGSR
105000001218     C*
105100001218     C                   Z-ADD     0             W                 4 0
105200001218     C                   MOVEA     *HIVAL        T2A
105300001218     C                   MOVEA     *HIVAL        C2A
105400001218     C                   MOVEL     '2A'          COD
105500001218     C     KTAB          CHAIN     TABEL                              31
105600001218     C     *IN31         DOWEQ     '0'
105700001218     C     TBLFLG        IFEQ      ' '
105800001218     C                   MOVEL     TBLUNI        DS2A
105900001218     C     §2AFTC        IFNE      *BLANKS
106000001218     C                   ADD       1             W
106100001218     C                   MOVEL     §2AFTC        T2A(W)
106200001218     C                   MOVEL     TBLKEY        C2A(W)
106300001218     C                   ENDIF
106400001218     C                   ENDIF
106500001218     C     KTAB          READE     TABEL                                  31
106600001218     C                   ENDDO
106700001218     C*
106800081016     C                   Z-ADD     1             I                 4 0
106900011017     C                   MOVEL     CODCLI        KUNI(I)
107000081016     C                   IF        FLGUNI = 'U'
107100001218     C                   MOVEL     '3K'          COD
107200001218     C     KTAB          CHAIN     TABEL                              31
107300001218     C     *IN31         DOWEQ     '0'
107400001218     C     TBLFLG        IFEQ      ' '
107500001218     C                   MOVEL     TBLUNI        DS3K
107600010301     C     §3KCKS        IFEQ      CODCLI
107700001218     C                   ADD       1             I
107800081016     C                   MOVEL     TBLKEY        wKUNI             7 0
107900081016     C     wKUNI         LOOKUP    KUNI                                   55
108000081016     C                   IF        %equal
108100081016     C                   ELSE
108200081016     C                   Z-ADD     wKUNI         KUNI(I)
108300081016     C                   ENDIF
108400001218     C                   ENDIF
108500001218     C                   ENDIF
108600001218     C     KTAB          READE     TABEL                                  31
108700001218     C                   ENDDO
108800010301     C                   ENDIF
108900040312     C*
109000040312     C                   Z-ADD     0             J                 4 0
109100040312     C                   MOVEL     '3A'          COD
109200040312     C     KTAB          CHAIN     TABEL                              31
109300040312     C     *IN31         DOWEQ     '0'
109400040312     C     TBLFLG        IFEQ      ' '
109500040312     C                   MOVEL     TBLUNI        DS3A
109600040312     C     §3AVAC        IFEQ      'N'
109700040312     C                   ELSE
109800040312     C                   ADD       1             J
109900040312     C                   MOVEL     TBLKEY        COD3A(J)
110000040312     C                   ENDIF
110100040312     C                   ENDIF
110200040312     C     KTAB          READE     TABEL                                  31
110300040312     C                   ENDDO
110400001218     C*
110500001218     C                   ENDSR
110600001218     C*---------------------------------------------------------------*
110700151019
110800151019
110900151019
111000151019     C     RTVAR5FAT     BEGSR
111100151019     C*
111200151019     C* Se, presente, considero il peso/volume DESUNTO al pari del VDL TOTALE
111300151019     C                   SETOFF                                       7071
111400151019     C                   EVAL      ar5TRD = 'FAT'
111500151019     C                   CLEAR                   DAR5FAT
111600151019     C     KEYar531_P    CHAIN     FIAR531C
111700151019     C                   IF        %found(FIAR531C)
111800151019     C                   MOVEL     AR5UNI        DAR5FAT
111900151019     C                   IF        §AR5PKTAS > *zeros
112000151019     C                   SETON                                        70
112100151019     C                   EVAL      tasPKF = §AR5PKTAS
112200151019     C                   ENDIF
112300151019     C                   IF        §AR5VLTAS > *zeros
112400151019     C                   SETON                                        71
112500160505     C                   EVAL      tasVLF = §AR5VLTAS
112600151019     C                   ENDIF
112700151019     C                   ENDIF
112800151019     C*
112900151019     C                   ENDSR
113000160208
113100160208
113200160208
113300160208     C     RTVNAS        BEGSR
113400160208     C*
113500160208     C* Inizializzo le DS relative ai tipi record del TITA4 da gestire
113600160208     C                   CLEAR                   DTA4A
113700160208     C*
113800160208     C* Reperisco dal tipo record 'A' del TITA4 la natura merce della bolla corrente
113900160208     C                   CALL      'UBTA400R'
114000160208     C                   PARM      *blanks       UBTA4IOPZ
114100160208     C                   PARM      *blanks       UBTA4ITLA
114200160208     C                   PARM      tasAAS        UBTA4IAAS
114300160208     C                   PARM      tasLNP        UBTA4ILNP
114400160208     C                   PARM      tasNRS        UBTA4INRS
114500160208     C                   PARM      tasNSP        UBTA4INSP
114600160208     C                   PARM      'A'           UBTA4ITRC
114700160208     C                   PARM                    UBTA4OERR
114800160208     C                   PARM                    UBTA4ODS
114900160208     C                   PARM                    UBTA4OLEN
115000160208     C                   PARM                    UBTA4ODATI
115100160208     C*
115200160208     C                   IF        UBTA4OERR = *zeros
115300160208     C                   SELECT
115400160208     C* Gestione output tipo record 'A'
115500160208     C                   WHEN      UBTA4ODS = 'DTA4A'
115600160208     C                   EVAL      DTA4A = %subst(UBTA4ODATI:1:UBTA4OLEN)
115700160208     C                   ENDSL
115800160208     C*
115900160208     C                   ENDIF
116000160208     C*
116100160208     C                   ENDSR
116200161205
116300161205
116400161205
116500161205     C*------------------------------------------------------------------------*
116600161205     C* ENDELA - Routine di lanci di chiusura finali
116700161205     C*------------------------------------------------------------------------*
116800161205     C     ENDELA        BEGSR
116900161205     C*
117000161205     C* Finita l'elaborazione chiamo driver reperimento TITA4 in chiusura
117100161205     C                   CALL      'UBTA400R'
117200161205     C                   PARM                    UBTA4IOPZ
117300161205     C                   PARM      'C'           UBTA4ITLA
117400161205     C                   PARM                    UBTA4IAAS
117500161205     C                   PARM                    UBTA4ILNP
117600161205     C                   PARM                    UBTA4INRS
117700161205     C                   PARM                    UBTA4INSP
117800161205     C                   PARM                    UBTA4ITRC
117900161205     C                   PARM                    UBTA4OERR
118000161205     C                   PARM                    UBTA4ODS
118100161205     C                   PARM                    UBTA4OLEN
118200161205     C                   PARM                    UBTA4ODATI
118300161205     C*
118400161205     C                   ENDSR
118500161205     C*------------------------------------------------------------------------*
118600161205
118700161205
118800161205
118900161205     C*------------------------------------------------------------------------*
119000170227     C* EXEERR - Routine di esecuzione azioni su Errore
119100161205     C*------------------------------------------------------------------------*
119200161205     C     EXEERR        BEGSR
119300161205     C*
119400161205     C                   dump(A)
119500170322     C                   rolbk(e)
119600161205     C                   seton                                        lr
119700161205     C                   return
119800161205     C*
119900161205     C                   ENDSR
120000161205     C*------------------------------------------------------------------------*
120100001218
120200001218
120300001218
120400001218     C*------------------------------------------------------------------------*
120500001218     C* *INZSR - ROUTINE INIZIALE
120600001218     C*------------------------------------------------------------------------*
120700001218     C     *INZSR        BEGSR
120800001218     C*
120900001218     C     *ENTRY        PLIST
121000100512     C                   PARM                    PARAM            71
121100001218     C*
121200001218     C                   MOVEL     PARAM         DSINPUT
121300001218     C                   Z-ADD     1             CODUT
121400001218     C*
121500001218     C* Definizioni chiavi
121600001218     C     KTA4          KLIST
121700001218     C                   KFLD                    tasAAS
121800001218     C                   KFLD                    tasLNP
121900001218     C                   KFLD                    tasNRS
122000001218     C                   KFLD                    tasNSP
122100001218     C                   KFLD                    KTRC              1
122200001218     C*
122300001218     C     KTAA          KLIST
122400001218     C                   KFLD                    tasAAS
122500001218     C                   KFLD                    tasLNP
122600001218     C                   KFLD                    tasNRS
122700001218     C                   KFLD                    tasNSP
122800001218     C                   KFLD                    KTRC
122900001218     C*
123000001218     C     KCSB          KLIST
123100001218     C                   KFLD                    tasAAS
123200001218     C                   KFLD                    tasLNP
123300001218     C                   KFLD                    tasNRS
123400001218     C                   KFLD                    tasNSP
123500001218     C*
123600001218     C     KEVB          KLIST
123700001218     C                   KFLD                    tasAAS
123800001218     C                   KFLD                    tasLNP
123900001218     C                   KFLD                    tasNRS
124000001218     C                   KFLD                    tasNSP
124100001218     C*
124200001218     C     KGCP          KLIST
124300001218     C                   KFLD                    tasAAS
124400001218     C                   KFLD                    tasLNP
124500001218     C                   KFLD                    tasNRS
124600001218     C                   KFLD                    tasNSP
124700001218     C*
124800001218     C     KTAB          KLIST
124900001218     C                   KFLD                    CODUT             1 0
125000001218     C                   KFLD                    COD               2
125100001218     C*
125200001218     C     KTAB2         KLIST
125300001218     C                   KFLD                    CODUT             1 0
125400001218     C                   KFLD                    COD               2
125500001218     C                   KFLD                    KEY               8
125600151019     C*
125700151019     C* FIAR531C - Parziale
125800151019     C     KEYar531_P    KLIST
125900151019     C                   KFLD                    tasAAS
126000151019     C                   KFLD                    tasLNP
126100151019     C                   KFLD                    tasNRS
126200151019     C                   KFLD                    tasNSP
126300151019     C                   KFLD                    ar5TRD
126400001218     C*
126500001218     C* Determino la data corrente
126600100312     C                   Z-ADD     *zeros        DATCOR            8 0
126700100312     C                   EVAL      DATCOR = %dec(%date() : *ISO)
126800001218     C*
126900001218     C                   ENDSR

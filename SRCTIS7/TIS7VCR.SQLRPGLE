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
003700170510     D FNVACOUT      E DS                  EXTNAME(FNVAC00T) PREFIX(O_)
003800160208     D DTA4A         E DS
003900170227     D TIS7VASDS     E DS
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
009200161202     C*
009300161206     C* Avvio il monitoring del processo
009400161206     C                   monitor
009500161206     C*
009600001218     C                   exsr      cartbl
009700001218     C                   exsr      chkpar
009800001218     C                   exsr      procedi
009900161205     C                   exsr      endela
010000161206     C*
010100170227     C* Sancisco il commit
010200170309     C                   commit(e)
010300161206     C*
010400161206     C* Su errore
010500161206     C                   on-error
010600161206     C                   exsr      exeerr
010700161206     C*
010800161206     C* Fine monitoring
010900161206     C                   endmon
011000001218     C*
011100001218     C                   seton                                        LR
011200001218
011300001218
011400001218
011500001218     C*------------------------------------------------------------------------*
011600001218     C* CHKPAR - Routine di controllo parametri ricevuti in input
011700001218     C*------------------------------------------------------------------------*
011800001218     C     CHKPAR        BEGSR
011900001218     C*
012000001218     C* Verifico le date
012100100512     C                   if        DATADA > *zeros AND
012200100512     C                             DATAA  = *zeros
012300001218     C                   movel     *all'9'       DATAA
012400001218     C                   endif
012500100512     C                   if        DACMDA > *zeros AND
012600100512     C                             DACMA  = *zeros
012700100512     C                   movel     *all'9'       DACMA
012800100512     C                   endif
012900050823     C*
013000050823     C* Verifico la modalità d scrittura output richiesta:
013100050823     C*   1 = crea membro "RIC" in file FNVAC00T
013200050823     C*   2 = scrivi record in file TIVGD00F
013300170510     C*   3 = output in passaggio parametri
013400170510     C                   setoff                                       252627
013500050823     C                   if        FLGOPZ = '1'
013600050823     C                   seton                                        25
013700050823     C                   endif
013800050823     C                   if        FLGOPZ = '2'
013900050823     C                   seton                                        26
014000050823     C                   endif
014100170510     C                   if        wFNVACOUT = '1'
014200170510     C                   seton                                        27
014300170510     C                   endif
014400001218     C*
014500001218     C                   ENDSR
014600001218     C*------------------------------------------------------------------------*
014700001218
014800001218
014900001218
015000001218     C*------------------------------------------------------------------------*
015100001218     C* PROCEDI - Routine principale
015200001218     C*------------------------------------------------------------------------*
015300001218     C     PROCEDI       BEGSR
015400050909     C*
015500050909     C* Inizializzo variabili d wrk
015600050909     C                   movel     'N'           wProcedi          1
015700170510     C*
015800170510     C* Output in passaggio parametri
015900170510     C                   if        *in27
016000170510     C                   movel     'S'           wProcedi
016100170510     C                   endif
016200050909     C*
016300050909     C* Apro il file di output
016400161202     C                   if        *in25
016500050909     C                   open      fnvac00t
016600050909     C                   movel     'S'           wProcedi
016700050909     C                   endif
016800161202     C*
016900161202     C* Inizializzo la transazione
017000161202     C                   if        *in26
017100161206     C                   open      tivgd00f
017200170227     C*
017300170227     C* Stacco progressivo univoco download
017400170227     C                   CLEAR                   TIS7VASDS
017500170227     C                   EVAL      i§7VASOPZ = 'PRG'
017600170227     C                   CALL(e)   'TIS7VASR1'
017700170227     C                   PARM                    TIS7VASDS
017800170227     C*
017900170227     C* Se OK => proseguo
018000170227     C                   if        not %error AND
018100170227     C                             o§7VASOK = *on AND o§7VASPRG <> *blanks
018200170227     C                   movel     'S'           wProcedi
018300170227     C                   endif
018400161205     C                   endif
018500050909     C*
018600050909     C* Se ok a procedere => elaboro
018700050909     C                   if        wProcedi = 'S'
018800001218     C*
018900001218     C* Ciclo sulla schiera dei clienti figli dell'unificante preso in input
019000001218     C                   z-add     1             I
019100170512     C                   movel     '0'           wEsci             1
019200090407     C                   sorta     KUNI
019300170512     C                   dow       (KUNI(I) > *zeros OR *IN27) AND wEsci = *off
019400170512     C*
019500170512     C* Se richiesta elaborazione x chiave spedizione ...
019600170512     C                   select
019700170512     C                   when      *IN27 OR
019800170512     C                             (KAAS   > *zeros AND
019900170512     C                              KLNP   > *zeros AND
020000170512     C                              KNSP   > *zeros)
020100170512     C                   exsr      EXESPED
020200070718     C*
020300070718     C* Se richiesta elaborazione x codice cliente mittente...
020400100512     C                   when      TIPCLI = *blanks
020500100512     C                   exsr      EXECCM
020600070718     C*
020700070718     C* Se richiesta elaborazione x codice cliente fatturazione...
020800100512     C                   when      TIPCLI = 'K'
020900100512     C                   exsr      EXEKSC
021000100512     C*
021100100512     C* Se richiesta elaborazione x entrambi: codice cliente mittente + fatturazione...
021200100512     C                   when      TIPCLI = 'E'
021300100512     C                   exsr      EXECCM
021400100512     C                   exsr      EXEKSC
021500100512     C*
021600100512     C                   endsl
021700001218     C*
021800001218     C                   add       1             I
021900001218     C                   enddo
022000050909     C*
022100050909     C                   endif
022200001218     C*
022300001218     C* Chiudo il file di output
022400050823     C   25              close     fnvac00t
022500170227     C   26              close     tivgd00f
022600170227     C*
022700170227     C* Finalizzo la transazione
022800170227     C                   if        *in26
022900170227     C                   EVAL      i§7VASOPZ  = 'RLS'
023000170227     C                   EVAL      i§7VASTIP  = TIPFILE
023100170227     C                   EVAL      i§7VASKSU  = '0'+CODCLIVAS
023200170227     C                   EVAL      i§7VASTSC  = 'WW'
023300170227     C                   EVAL      i§7VASSTO  = '?'
023400170227     C                   EVAL      i§7VASSTTO = 'L'
023500170227     C                   EVAL      i§7VASPRG  = o§7VASPRG
023600170227     C                   CALL(e)   'TIS7VASR1'
023700170227     C                   PARM                    TIS7VASDS
023800170227     C*
023900170227     C* Verifico esito chiamata
024000170227     C                   if        %error OR o§7VASOK = *off
024100170227     C                   exsr      EXEERR
024200170227     C                   endif
024300170227     C                   endif
024400001218     C*
024500001218     C                   ENDSR
024600001218     C*------------------------------------------------------------------------*
024700170512
024800170512
024900170512
025000170512     C*------------------------------------------------------------------------*
025100170512     C* EXESPED  - Elaborazione x chiave spedizione
025200170512     C*------------------------------------------------------------------------*
025300170512     C     EXESPED       BEGSR
025400170512     C*
025500170512     C                   movel     *blanks       wChkCli           1
025600170512     C*
025700170512     C                   exsr      SQ_SPED
025800170512     C*
025900170512     C                   ENDSR
026000170512     C*------------------------------------------------------------------------*
026100100512
026200100512
026300100512
026400100512     C*------------------------------------------------------------------------*
026500161205     C* EXECCM   - Elaborazione x codice cliente mittente
026600100512     C*------------------------------------------------------------------------*
026700100512     C     EXECCM        BEGSR
026800100521     C*
026900100521     C                   movel     'C'           wChkCli           1
027000100512     C*
027100100512     C                   select
027200100512     C* ...se richiesta elaborazione x data spedizione
027300100512     C                   when      DATADA > *zeros
027400100512     C                   exsr      SQ_CCMDSP
027500100512     C* ...se richiesta elaborazione x data consegna merce
027600100512     C                   when      DACMDA > *zeros
027700100512     C                   exsr      SQ_CCMDCM
027800100512     C                   endsl
027900100512     C*
028000100512     C                   ENDSR
028100100512     C*------------------------------------------------------------------------*
028200100512
028300100512
028400100512
028500100512     C*------------------------------------------------------------------------*
028600161205     C* EXEKSC   - Elaborazione x codice cliente fatturazione
028700100512     C*------------------------------------------------------------------------*
028800100512     C     EXEKSC        BEGSR
028900100521     C*
029000100521     C                   movel     'K'           wChkCli           1
029100100512     C*
029200100512     C                   select
029300100512     C* ...se richiesta elaborazione x data spedizione
029400100512     C                   when      DATADA > *zeros
029500100512     C                   exsr      SQ_KSCDSP
029600100512     C* ...se richiesta elaborazione x data consegna merce
029700100512     C                   when      DACMDA > *zeros
029800100512     C                   exsr      SQ_KSCDCM
029900100512     C                   endsl
030000100512     C*
030100100512     C                   ENDSR
030200100512     C*------------------------------------------------------------------------*
030300170512
030400170512
030500170512
030600170512     C*------------------------------------------------------------------------*
030700170512     C* SQ_SPED   - Elaborazione x chiave spedizione
030800170512     C*------------------------------------------------------------------------*
030900170512     C     SQ_SPED       BEGSR
031000170512     C*
031100170512     C/EXEC SQL
031200170512     C+ declare C_SPED   cursor for
031300170512     C+ select * from titas00f where
031400170512     C+ tasaas = :KAAS and taslnp = :KLNP and
031500170512     C+ tasnrs = :KNRS and tasnsp = :KNSP
031600170512     C+ union
031700170512     C+ select * from titas10f where
031800170512     C+ tasaas = :KAAS and taslnp = :KLNP and
031900170512     C+ tasnrs = :KNRS and tasnsp = :KNSP
032000170512     C+ union
032100170512     C+ select * from titasP0f where
032200170512     C+ tasaas = :KAAS and taslnp = :KLNP and
032300170512     C+ tasnrs = :KNRS and tasnsp = :KNSP
032400170512     C+ for read only
032500170512     C/END-EXEC
032600170512     C
032700170512     C/EXEC SQL
032800170512     C+ open C_SPED
032900170512     C/END-EXEC
033000170512     C
033100170512     C/EXEC SQL
033200170512     C+ Fetch C_SPED   into :TITAS00F
033300170512     C/END-EXEC
033400170512     C*
033500170512     C                   dow       sqlcod = *zeros
033600170512     C                   exsr      verifica
033700170512     C                   if        CHKREC = 'S'
033800170512     C                   exsr      scriviVAC
033900170512     C                   endif
034000170512     C
034100170512     C/EXEC SQL
034200170512     C+ Fetch C_SPED   into :TITAS00F
034300170512     C/END-EXEC
034400170512     C*
034500170512     C                   enddo
034600170512     C*
034700170512     C/EXEC SQL
034800170512     C+ close C_SPED
034900170512     C/END-EXEC
035000170512     C*
035100170512     C                   movel     '1'           wEsci
035200170512     C*
035300170512     C                   ENDSR
035400170512     C*------------------------------------------------------------------------*
035500100512
035600100512
035700100512
035800100512     C*------------------------------------------------------------------------*
035900161205     C* SQ_CCMDSP - Elaborazione x codice cliente mittente + data spedizione
036000100512     C*------------------------------------------------------------------------*
036100100512     C     SQ_CCMDSP     BEGSR
036200100512     C*
036300100512     C                   z-add     KUNI(I)       wParCCM           7 0
036400100512     C*
036500100512     C/EXEC SQL
036600100512     C+ declare C_CCMDSP cursor for
036700100512     C+ select * from titas00f
036800100512     C+ where tasccm = :wParCCM and
036900100512     C+ tasaas*10000+tasmgs between :DATADA and :DATAA
037000100512     C+ union
037100100512     C+ select * from titas10f
037200100512     C+ where tasccm = :wParCCM and
037300100512     C+ tasaas*10000+tasmgs between :DATADA and :DATAA
037400100512     C+ union
037500100512     C+ select * from titasP0f
037600100512     C+ where tasccm = :wParCCM and
037700100512     C+ tasaas*10000+tasmgs between :DATADA and :DATAA
037800100512     C+ for read only
037900100512     C/END-EXEC
038000100512     C
038100100512     C/EXEC SQL
038200100512     C+ open C_CCMDSP
038300100512     C/END-EXEC
038400100512     C
038500100512     C/EXEC SQL
038600100512     C+ Fetch C_CCMDSP into :TITAS00F
038700100512     C/END-EXEC
038800100512     C*
038900100512     C                   dow       sqlcod = *zeros
039000100512     C                   exsr      verifica
039100100512     C                   if        CHKREC = 'S'
039200100512     C                   exsr      scriviVAC
039300100512     C                   endif
039400100512     C
039500100512     C/EXEC SQL
039600100512     C+ Fetch C_CCMDSP into :TITAS00F
039700100512     C/END-EXEC
039800100512     C*
039900100512     C                   enddo
040000100512     C*
040100100512     C/EXEC SQL
040200100512     C+ close C_CCMDSP
040300100512     C/END-EXEC
040400100512     C*
040500100512     C*
040600100512     C                   ENDSR
040700100512     C*------------------------------------------------------------------------*
040800100512
040900100512
041000100512
041100100512     C*------------------------------------------------------------------------*
041200161205     C* SQ_CCMDCM - Elaborazione x codice cliente mittente + data consegna merce
041300100512     C*------------------------------------------------------------------------*
041400100512     C     SQ_CCMDCM     BEGSR
041500100512     C*
041600100512     C                   z-add     KUNI(I)       wParCCM           7 0
041700100512     C*
041800100512     C/EXEC SQL
041900100512     C+ declare C_CCMDCM cursor for
042000100512     C+ select * from titas00f
042100100512     C+ where tasccm = :wParCCM and
042200100512     C+ tasdcm between :DACMDA and :DACMA
042300100512     C+ union
042400100512     C+ select * from titas10f
042500100512     C+ where tasccm = :wParCCM and
042600100512     C+ tasdcm between :DACMDA and :DACMA
042700100512     C+ union
042800100512     C+ select * from titasP0f
042900100512     C+ where tasccm = :wParCCM and
043000100512     C+ tasdcm between :DACMDA and :DACMA
043100100512     C+ for read only
043200100512     C/END-EXEC
043300100512     C
043400100512     C/EXEC SQL
043500100512     C+ open C_CCMDCM
043600100512     C/END-EXEC
043700100512     C
043800100512     C/EXEC SQL
043900100512     C+ Fetch C_CCMDCM into :TITAS00F
044000100512     C/END-EXEC
044100100512     C*
044200100512     C                   dow       sqlcod = *zeros
044300100512     C                   exsr      verifica
044400100512     C                   if        CHKREC = 'S'
044500100512     C                   exsr      scriviVAC
044600100512     C                   endif
044700100512     C
044800100512     C/EXEC SQL
044900100512     C+ Fetch C_CCMDCM into :TITAS00F
045000100512     C/END-EXEC
045100100512     C*
045200100512     C                   enddo
045300100512     C*
045400100512     C/EXEC SQL
045500100512     C+ close C_CCMDCM
045600100512     C/END-EXEC
045700100512     C*
045800100512     C*
045900100512     C                   ENDSR
046000100512     C*------------------------------------------------------------------------*
046100100512
046200100512
046300100512
046400100512     C*------------------------------------------------------------------------*
046500161205     C* SQ_KSCDSP - Elaborazione x codice cliente fatturazione + data spedizione
046600100512     C*------------------------------------------------------------------------*
046700100512     C     SQ_KSCDSP     BEGSR
046800100512     C*
046900100512     C                   z-add     KUNI(I)       wParKSC           7 0
047000100512     C*
047100100512     C/EXEC SQL
047200100512     C+ declare C_KSCDSP cursor for
047300100512     C+ select * from titas00f
047400100512     C+ where tasksc = :wParKSC and
047500100512     C+ tasaas*10000+tasmgs between :DATADA and :DATAA
047600100512     C+ union
047700100512     C+ select * from titas10f
047800100512     C+ where tasksc = :wParKSC and
047900100512     C+ tasaas*10000+tasmgs between :DATADA and :DATAA
048000100512     C+ union
048100100512     C+ select * from titasP0f
048200100512     C+ where tasksc = :wParKSC and
048300100512     C+ tasaas*10000+tasmgs between :DATADA and :DATAA
048400100512     C+ for read only
048500100512     C/END-EXEC
048600100512     C
048700100512     C/EXEC SQL
048800100512     C+ open C_KSCDSP
048900100512     C/END-EXEC
049000100512     C
049100100512     C/EXEC SQL
049200100512     C+ Fetch C_KSCDSP into :TITAS00F
049300100512     C/END-EXEC
049400100512     C*
049500100512     C                   dow       sqlcod = *zeros
049600100512     C                   exsr      verifica
049700100512     C                   if        CHKREC = 'S'
049800100512     C                   exsr      scriviVAC
049900100512     C                   endif
050000100512     C
050100100512     C/EXEC SQL
050200100512     C+ Fetch C_KSCDSP into :TITAS00F
050300100512     C/END-EXEC
050400100512     C*
050500100512     C                   enddo
050600100512     C*
050700100512     C/EXEC SQL
050800100512     C+ close C_KSCDSP
050900100512     C/END-EXEC
051000100512     C*
051100100512     C*
051200100512     C                   ENDSR
051300100512     C*------------------------------------------------------------------------*
051400100512
051500100512
051600100512
051700100512     C*------------------------------------------------------------------------*
051800161205     C* SQ_KSCDCM - Elaborazione x codice cliente fatturazione + data consegna merce
051900100512     C*------------------------------------------------------------------------*
052000100512     C     SQ_KSCDCM     BEGSR
052100100512     C*
052200100512     C                   z-add     KUNI(I)       wParKSC           7 0
052300100512     C*
052400100512     C/EXEC SQL
052500100512     C+ declare C_KSCDCM cursor for
052600100512     C+ select * from titas00f
052700100512     C+ where tasksc = :wParKSC and
052800100512     C+ tasdcm between :DACMDA and :DACMA
052900100512     C+ union
053000100512     C+ select * from titas10f
053100100512     C+ where tasksc = :wParKSC and
053200100512     C+ tasdcm between :DACMDA and :DACMA
053300100512     C+ union
053400100512     C+ select * from titasP0f
053500100512     C+ where tasksc = :wParKSC and
053600100512     C+ tasdcm between :DACMDA and :DACMA
053700100512     C+ for read only
053800100512     C/END-EXEC
053900100512     C
054000100512     C/EXEC SQL
054100100512     C+ open C_KSCDCM
054200100512     C/END-EXEC
054300100512     C
054400100512     C/EXEC SQL
054500100512     C+ Fetch C_KSCDCM into :TITAS00F
054600100512     C/END-EXEC
054700100512     C*
054800100512     C                   dow       sqlcod = *zeros
054900100512     C                   exsr      verifica
055000100512     C                   if        CHKREC = 'S'
055100100512     C                   exsr      scriviVAC
055200100512     C                   endif
055300100512     C
055400100512     C/EXEC SQL
055500100512     C+ Fetch C_KSCDCM into :TITAS00F
055600100512     C/END-EXEC
055700100512     C*
055800100512     C                   enddo
055900100512     C*
056000100512     C/EXEC SQL
056100100512     C+ close C_KSCDCM
056200100512     C/END-EXEC
056300100512     C*
056400100512     C*
056500100512     C                   ENDSR
056600100512     C*------------------------------------------------------------------------*
056700100512
056800100512
056900100512
057000100512     C*------------------------------------------------------------------------*
057100161205     C* CHKCONS  - Verifica se bolla effettivamente consegnata
057200100512     C*------------------------------------------------------------------------*
057300100512     C     CHKCONS       BEGSR
057400100512     C*
057500100512     C                   setoff                                       4041
057600100512     C                   movel     *blanks       wEsito1           1
057700100512     C                   movel     *blanks       wEsito2           1
057800100512     C*
057900100512     C                   eval      wAAS = tasAAS
058000100512     C                   eval      wLNP = tasLNP
058100100512     C                   eval      wNRS = tasNRS
058200100512     C                   eval      wNSP = tasNSP
058300100512     C*
058400100512     C* Chiamata metodo GetLblTyp
058500100512     C                   eval      wEsito1 = %char(UBLBLSPE_GetLblTyp(
058600100512     C                                                wAAS
058700100512     C                                               :wLNP
058800100512     C                                               :wNRS
058900100512     C                                               :wNSP
059000100512     C                                               :pOutLblTyp
059100100512     C                                               :pOutAnnoBO
059200100512     C                                               :pOutLineaParBO
059300100512     C                                               :pOutSerieBO
059400100512     C                                               :pOutNumSpedBO
059500100512     C                                               :pOutDcmBO
059600100512     C                                               :pOutCcaBO
059700100512     C                                               :pOutRblBO))
059800100512     C* Se trattasi d bolla nn originale reperisco la relativa bolla originale
059900100512     C                   if        pOutLblTyp <> 'O'
060000100512     C                   eval      wAAS = pOutAnnoBO
060100100512     C                   eval      wLNP = pOutLineaParBO
060200100512     C                   eval      wNRS = pOutSerieBO
060300100512     C                   eval      wNSP = pOutNumSpedBO
060400100512     C                   endif
060500100512     C*
060600100512     C* Chiamata metodo GetLastChild
060700100512     C                   eval      wEsito2 = %char(UBLBLSPE_GetLastChild(
060800100512     C                                                wAAS
060900100512     C                                               :wLNP
061000100512     C                                               :wNRS
061100100512     C                                               :wNSP
061200100512     C                                               :pOutAnnoFI
061300100512     C                                               :pOutLineaParFI
061400100512     C                                               :pOutSerieFI
061500100512     C                                               :pOutNumSpedFI
061600100512     C                                               :pOutDcmFI
061700100512     C                                               :pOutCcaFI))
061800100512     C*
061900100512     C* Considerazioni sullo stato corrente della bolla
062000100512     C                   if        (wEsito2 = '9' and pOutDcmBO > *zeros)  or
062100100512     C                             (wEsito2 = '0' and pOutDcmFI > *zeros)
062200161202     C                   seton                                        40        * bolla chiusa
062300100512     C                   endif
062400100512     C*
062500100512     C                   if        (wEsito2 = '9' and pOutDcmBO > *zeros
062600100512     C                                            and pOutCcaBO = *blanks) or
062700100512     C                             (wEsito2 = '0' and pOutDcmFI > *zeros
062800100512     C                                            and pOutCcaFI = *blanks)
062900100512     C                   seton                                        41        * bolla cons. OK
063000100512     C                   endif
063100100512     C*
063200100512     C                   ENDSR
063300100512     C*------------------------------------------------------------------------*
063400001218
063500001218
063600001218
063700001218     C*------------------------------------------------------------------------*
063800161205     C* VERIFICA - Routine di verifica validità record corrente
063900001218     C*------------------------------------------------------------------------*
064000001218     C     VERIFICA      BEGSR
064100001218     C*
064200001218     C                   movel     'S'           CHKREC            1
064300001218     C*
064400100512     C* Verifico le date spedizione
064500001218     C                   if        CHKREC = 'S'
064600100512     C                   if        DATADA > *zeros
064700001218     C                   movel     tasaas        wrkdata           8 0
064800001218     C                   move      tasmgs        wrkdata
064900001218     C                   if        wrkdata < DATADA or
065000001218     C                             wrkdata > DATAA
065100001218     C                   movel     'N'           CHKREC
065200100512     C                   endif
065300001218     C                   endif
065400001218     C                   endif
065500100512     C*
065600100512     C* Verifico le date spedizione
065700100512     C                   if        CHKREC = 'S'
065800100512     C                   if        DACMDA > *zeros
065900100512     C                   if        tasdcm  < DACMDA or
066000100512     C                             tasdcm  > DACMA
066100100512     C                   movel     'N'           CHKREC
066200100512     C                   endif
066300100512     C                   endif
066400100512     C                   endif
066500100521     C*
066600100521     C* Verifico il codice cliente mittente
066700100521     C                   if        CHKREC = 'S'
066800100521     C                   if        wChkCli  = 'C'      AND
066900100521     C                             tasccm  <> KUNI(I)
067000100521     C                   movel     'N'           CHKREC
067100100521     C                   endif
067200100521     C                   endif
067300070718     C*
067400100517     C* Verifico il codice cliente fatturazione (in caso d 'K')
067500070718     C                   if        CHKREC = 'S'
067600100521     C                   if        wChkCli  = 'K'      AND
067700100521     C                             tasksc  <> KUNI(I)
067800070718     C                   movel     'N'           CHKREC
067900070718     C                   endif
068000070718     C                   endif
068100100517     C*
068200100517     C* Verifico il codice cliente fatturazione (in caso d 'E')
068300100517     C                   if        CHKREC = 'S'
068400100521     C                   if        TIPCLI  = 'E'       AND
068500100521     C                             wChkCli = 'K'       AND
068600100521     C                             tasksc  = tasccm
068700100517     C                   movel     'N'           CHKREC
068800100517     C                   endif
068900100517     C                   endif
069000001218     C*
069100020923     C* Verifico che la bolla abbia la data consegna se trattasi di bolla PT
069200001218     C                   if        CHKREC = 'S'
069300020923     C                   if        tasTSP = 'P'
069400020923     C                   if        tasDCM > *zeros
069500001218     C                   else
069600001218     C                   movel     'N'           CHKREC
069700001218     C                   endif
069800020923     C                   endif
069900001218     C                   endif
070000020920     C*
070100040322     C* Verifico che il tipo bolla sia da considerare ai fini della rigenerazione dell'FNVAC
070200020920     C                   if        CHKREC = 'S'
070300040322     C     tasCBO        lookup    COD3A                                  55
070400081016     C                   if        %equal
070500040322     C                   else
070600020920     C                   movel     'N'           CHKREC
070700020920     C                   endif
070800020920     C                   endif
070900010301     C*
071000010301     C* Verifico che la bolla se POSTE non abbia il codice CA = 5
071100010301     C                   if        CHKREC = 'S'
071200010301     C                   if        tasTSP = 'P' and tasCCA = '5'
071300010301     C                   movel     'N'           CHKREC
071400010301     C                   endif
071500010301     C                   endif
071600100512     C*
071700040322     C* Verifico se richiesto nel lancio che la bolla sia consegnata
071800040322     C                   if        CHKREC = 'S'
071900040322     C                   if        FLGCONS = 'S'
072000100512     C                   exsr      CHKCONS
072100100512     C                   if        *in40 = *on
072200040322     C                   else
072300040322     C                   movel     'N'           CHKREC
072400040322     C                   endif
072500040322     C                   endif
072600040322     C                   endif
072700100512     C*
072800100512     C* Verifico se richiesto nel lancio che la bolla sia consegnata OK
072900100512     C                   if        CHKREC = 'S'
073000100512     C                   if        FLGCONS = 'O'
073100100512     C                   exsr      CHKCONS
073200100512     C                   if        *in41 = *on
073300100512     C                   else
073400100512     C                   movel     'N'           CHKREC
073500100512     C                   endif
073600100512     C                   endif
073700100512     C                   endif
073800100513     C*
073900100907     C* Verifico se richiesto nel lancio che la bolla NON sia consegnata OK
074000100513     C                   if        CHKREC = 'S'
074100100513     C                   if        FLGCONS = 'K'
074200100513     C                   exsr      CHKCONS
074300100513     C                   if        *in41 = *on
074400100513     C                   movel     'N'           CHKREC
074500100513     C                   endif
074600100513     C                   endif
074700100513     C                   endif
074800100907     C*
074900100907     C* Verifico se richiesto nel lancio che la bolla NON sia consegnata
075000100907     C                   if        CHKREC = 'S'
075100100907     C                   if        FLGCONS = 'P'
075200100907     C                   exsr      CHKCONS
075300100907     C                   if        *in40 = *on
075400100907     C                   movel     'N'           CHKREC
075500100907     C                   endif
075600100907     C                   endif
075700100907     C                   endif
075800040322     C*
075900040322     C* Verifico se richiesto nel lancio che la bolla provenga da scambio dati VAS cliente
076000040322     C                   if        CHKREC = 'S'
076100040322     C                   if        FLGVAS = 'S'
076200040322     C                   if        tasSOP = '§' OR
076300040322     C                             tasSOP = ':' OR
076400040322     C                             tasSOP = '%'
076500040322     C                   else
076600040322     C                   movel     'N'           CHKREC
076700040322     C                   endif
076800040322     C                   endif
076900040322     C                   endif
077000081016     C*
077100081016     C* Verifico se richiesto che la bolla corrente sia la singola bolla richiesta a video
077200081016     C                   if        CHKREC = 'S'
077300081016     C                   if        KAAS   <> *zeros OR
077400081016     C                             KLNP   <> *zeros OR
077500081016     C                             KNRS   <> *zeros OR
077600081016     C                             KNSP   <> *zeros
077700081016     C                   if        tasAAS = KAAS AND
077800081016     C                             tasLNP = KLNP AND
077900081016     C                             tasNRS = KNRS AND
078000081016     C                             tasNSP = KNSP
078100081016     C                   else
078200081016     C                   movel     'N'           CHKREC
078300081016     C                   endif
078400081016     C                   endif
078500081016     C                   endif
078600001218     C*
078700001218     C                   ENDSR
078800001218     C*------------------------------------------------------------------------*
078900001218
079000001218
079100001218
079200001218     C*------------------------------------------------------------------------*
079300001218     C* SCRIVIVAC - Routine di scrittura file esiti di consegna (FNVAC)
079400001218     C*------------------------------------------------------------------------*
079500001218     C     SCRIVIVAC     BEGSR
079600001218     C*
079700001218     C                   CLEAR                   fnvac000
079800001218     C*
079900001218     C                   MOVEL     tasAAS        VACAAS
080000001218     C                   MOVEL     tasLNP        VACLNP
080100001218     C                   MOVEL     tasNRS        VACNRS
080200001218     C                   MOVEL     tasNSP        VACNSP
080300001218     C                   MOVEL     tasMGS        VACMGS
080400001218     C                   MOVEL     tasCBO        VACCBO
080500001218     C                   MOVEL     tasLNA        VACLNA
080600010223     C                   MOVEL     tasRSD        VACRSD
080700010223     C                   MOVEL     tasPRD        VACPRD
080800010301     C*
080900001218     C* Reperisco dall'estensione anagrafica bolle di SEDE i dati del Destinatario
081000001218     C                   EXSR      REPDEST
081100151019     C*
081200151019     C* Eseguo eventuali "forzature"
081300151019     C                   EXSR      RTVAR5FAT
081400160208     C*
081500170227     C* Reperisco NATURA MERCE
081600160208     C                   EXSR      RTVNAS
081700001218     C*
081800001218     C                   MOVEL     tasGC1        VACGC1
081900001218     C                   MOVEL     tasGC2        VACGC2
082000001218     C                   MOVEL     tasCTR        VACCTR
082100001218     C                   MOVEL     tasCTS        VACCTS
082200001218     C                   MOVEL     tasFTM        VACFTM
082300001218     C                   MOVEL     tasFIN        VACFIN
082400001218     C                   MOVEL     tasFAP        VACFAP
082500001218     C                   MOVEL     tasTSP        VACTSP
082600001218     C* Effettuo considerazioni particolare per bolla "POSTE"
082700001218     C     tasTSP        IFEQ      'P'                                          BOLLA POSTE
082800001218     C                   EXSR      REPPTIAS
082900001218     C                   EXSR      REPPTGIA
083000001218     C                   Z-ADD     tasNCD        VACDCR
083100001218     C                   ELSE
083200001218     C                   MOVEL     tasIAS        VACIAS
083300001218     C                   MOVEL     tasVAS        VACVAS
083400001218     C                   MOVEL     tasDCR        VACDCR
083500050310     C     KGCP          CHAIN     TIGCP51L
083600050310     C                   IF        %found(TIGCP51L)
083700001218     C                   MOVEL     GCPAGC        VACDAG
083800001218     C                   MOVE      GCPMGC        VACDAG
083900001218     C                   ENDIF
084000001218     C                   ENDIF
084100001218     C*
084200160208     C                   MOVEL     §TA4ANAS      VACNAS
084300001218     C                   MOVEL     tasNCL        VACNCL
084400001218     C                   MOVEL     tasPKF        VACPKB
084500001218     C                   MOVEL     tasVLF        VACVLB
084600001218     C                   MOVEL     tasQFT        VACQFT
084700001218     C*
084800001218     C                   EXSR      REPCSB
084900001218     C*
085000001218     C                   MOVEL     tasCCM        VACCCM
085100001218     C                   MOVEL     tasRMN        VACRMN
085200010301     C*
085300001218     C                   EXSR      REPRMA
085400010301     C*
085500001218     C                   MOVEL     tasFFD        VACFFD
085600001218     C                   MOVEL     tasTCR        VACTCR
085700001218     C                   MOVEL     tasHCR        VACHCR
085800001218     C                   MOVEL     tasFTC        VACTC1
085900001218     C                   MOVEL     tasTC2        VACTC2
086000001218     C                   MOVEL     tasDCM        VACDCM
086100001218     C                   MOVEL     tasHMC        VACHMC
086200001218     C*
086300010301     C                   IF        tasTSP = 'P' and tasCCA = '7'
086400010301     C                   MOVEL     *blanks       VACCCA
086500010301     C                   ELSE
086600100910     C                   MOVEL     tasCCA        VACCCA
086700100910     C*
086800100910     C* Se CCA ancora nn valorizzato => reperisco l'ultimo evento
086900100910     C                   IF        VACCCA = *blanks
087000100910     C                   EXSR      REPLASTEV
087100010301     C                   ENDIF
087200100910     C                   ENDIF
087300010301     C*
087400120427     C* Reperisco l'eventuale lasciato avviso
087500001218     C                   EXSR      REPLAV
087600120427     C*
087700120427     C* Effettuo particolari operazioni in caso d DIROTTAMENTO
087800120427     C***                IF        VACCCA = '1' AND
087900120427     C***                          VACDCM = *zeros
088000120427     C***                EXSR      REPDIR
088100120427     C***                ENDIF
088200001218     C*
088300020923     C* Verifico che in caso di bolla NON PT siano valorizzate o la data consegna...
088400020923     C* ... o la data lasciato avviso o la data giacenza o la consegna anomala
088500020923     C                   IF        tasTSP <> 'P'
088600071228     C*
088700071228     C* Se richiesto nel lancio anche bolle senza eventi => gestisco...
088800071228     C* ...altrimenti standard
088900071228     C                   IF        FLGNOE  = 'N'
089000020923     C                   IF        vacDCM  > *zeros OR
089100020923     C                             vacDLA  > *zeros OR
089200020923     C                             vacDAG  > *zeros OR
089300020923     C                             vacCCA <> *blanks
089400071228     C   25              WRITE     FNVAC000
089500161205     C   26              exsr      WRITIVGD
089600170510     C   27              eval      FNVACOUT = FNVACDS
089700020923     C                   endif
089800071228     C                   else
089900071228     C   25              WRITE     FNVAC000
090000161205     C   26              exsr      WRITIVGD
090100170510     C   27              eval      FNVACOUT = FNVACDS
090200071228     C                   endif
090300020923     C                   else
090400050823     C   25              WRITE     FNVAC000
090500161205     C   26              exsr      WRITIVGD
090600170510     C   27              eval      FNVACOUT = FNVACDS
090700020923     C                   endif
090800001218     C*
090900001218     C                   ENDSR
091000120427
091100120427
091200120427
091300120427     C*------------------------------------------------------------------------*
091400161205     C* REPDIR - Reperimento data DIROTTAMENTO
091500120427     C*------------------------------------------------------------------------*
091600120427     C     REPDIR        BEGSR
091700120427     C*
091800120427     C                   movel     *blanks       wEsito1           1
091900120427     C*
092000120427     C* Chiamata metodo GetLblTyp
092100120427     C                   eval      wEsito1 = %char(UBLBLSPE_GetLblTyp(
092200120427     C                                                tasAAS
092300120427     C                                               :tasLNP
092400120427     C                                               :tasNRS
092500120427     C                                               :tasNSP
092600120427     C                                               :pOutLblTyp
092700120427     C                                               :pOutAnnoBO
092800120427     C                                               :pOutLineaParBO
092900120427     C                                               :pOutSerieBO
093000120427     C                                               :pOutNumSpedBO
093100120427     C                                               :pOutDcmBO
093200120427     C                                               :pOutCcaBO
093300120427     C                                               :pOutRblBO))
093400120427     C*
093500120427     C* Se reperimento effettuato e trattasi d dirottamento => recupero la data
093600120427     C                   if        wEsito1 = '0' and pOutCcaBO = '1'
093700120427     C                   eval      VACDCM = pOutDcmBO
093800120427     C                   endif
093900120427     C*
094000120427     C                   ENDSR
094100120427     C*------------------------------------------------------------------------*
094200100910
094300100910
094400100910
094500100910     C*------------------------------------------------------------------------*
094600161205     C* REPLASTEV - Routine di reperimento ultimo evento
094700100910     C*------------------------------------------------------------------------*
094800100910     C     REPLASTEV     BEGSR
094900100910     C*
095000100910     C     KEVB          SETGT     FNEVB01L
095100100910     C     KEVB          READPE    FNEVB01L                               34
095200100910     C*
095300100910     C* Devo cercare l'ultimo evento di tipo giacenza
095400100910     C                   IF        *IN34 = *OFF
095500100910     C*
095600100910     C* Evento 'MIC' considero solo se data evento >= data corrente
095700100910     C                   IF        EVBDEV >= datcor
095800100910     C* - MIC
095900100910     C                   IF        EVBCEV = 'MIC'
096000100910     C                   MOVEL     'C'           VACCCA
096100100910     C                   MOVEL     evbDEV        VACDCM
096200100910     C                   ENDIF
096300100910     C                   ENDIF
096400100910     C*
096500100910     C                   ENDIF
096600100910     C*
096700100910     C                   ENDSR
096800100910     C*------------------------------------------------------------------------*
096900050823
097000050823
097100050823
097200050823     C*------------------------------------------------------------------------*
097300161205     C* WRITIVGD - Routine di scrittura file TIVGD (file VAS generico download)
097400050823     C*------------------------------------------------------------------------*
097500161205     C     WRITIVGD      BEGSR
097600050823     C*
097700161206     C                   clear                   tivgd000
097800080407     C                   eval      vgdDTA = %subst(FNVACDS:1:%size(FNVACDS))
097900100312     C                   eval      vgdTIP = TIPFILE
098000170227     C                   eval      vgdKSU = '0'+CODCLIVAS
098100050823     C                   eval      vgdTSC = 'WW'
098200050823     C                   eval      vgdDAT = datcor
098300170227     C                   eval      vgdPRG = o§7VASPRG
098400161205     C                   eval      vgdPGM = procname
098500170227     C                   eval      vgdSTO = '?'
098600170227     C                   write     tivgd000
098700161205     C*
098800050823     C                   ENDSR
098900050823     C*------------------------------------------------------------------------*
099000001218
099100001218
099200161202
099300001218     C*------------------------------------------------------------------------*
099400001218     C* REPPTIAS - Routine di reperimento dati relativi all'importo/divisa da assicurare x POSTE
099500001218     C*------------------------------------------------------------------------*
099600001218     C     REPPTIAS      BEGSR
099700001218     C*
099800060215     C* Prendo importo da assicurare da TITA4 t.r.=J
099900001218     C                   movel     'J'           KTRC
100000001218     C     KTA4          chain     tita430c
100100001218     C                   if        %found(tita430c)
100200001218     C                   movel     ta4NOT        DSBL4J
100300001218     C                   z-add     §B4IAS        vacIAS
100400001218     C                   movel     §B4VAS        vacVAS
100500001218     C                   endif
100600001218     C*
100700001218     C                   ENDSR
100800001218     C*------------------------------------------------------------------------*
100900001218
101000001218
101100001218
101200001218     C*------------------------------------------------------------------------*
101300001218     C* REPRMA - Routine di reperimento dati relativi al riferimento mittente alfabetico
101400001218     C*------------------------------------------------------------------------*
101500001218     C     REPRMA        BEGSR
101600001218     C*
101700060215     C* Prendo importo da assicurare da TITA4 t.r.=A
101800001218     C                   movel     'A'           KTRC
101900001218     C     KTA4          chain     tita430c
102000001218     C                   if        %found(tita430c)
102100001218     C                   eval      vacRMA = %subst(ta4NOT:1:15)
102200001218     C                   endif
102300001218     C*
102400001218     C                   ENDSR
102500001218     C*------------------------------------------------------------------------*
102600001218
102700001218
102800001218
102900001218     C*------------------------------------------------------------------------*
103000001218     C* REPDEST - Routine di reperimento dati relativi al destinatario della bolla (TITAA)
103100001218     C*           ... e ai dati del mittente originale
103200001218     C*------------------------------------------------------------------------*
103300001218     C     REPDEST       BEGSR
103400001218     C*
103500010223     C* Prendo il tipo anagrafica = D
103600010223     C*                  movel     'D'           KTRC
103700010223     C*    KTAA          chain     titaa30c
103800010223     C*                  if        %found(titaa30c)
103900010223     C*                  movel     taaRSC        vacRSD
104000010223     C*                  movel     taaPRV        vacPRD
104100010223     C*                  endif
104200001218     C*
104300010223     C* Prendo il tipo anagrafica = O
104400001218     C                   movel     'O'           KTRC
104500001218     C     KTAA          chain     titaa30c
104600001218     C                   if        %found(titaa30c)
104700001218     C                   movel     taaRSC        vacRMO
104800001218     C                   endif
104900001218     C*
105000001218     C                   ENDSR
105100001218     C*------------------------------------------------------------------------*
105200001218
105300001218
105400001218
105500001218     C*------------------------------------------------------------------------*
105600001218     C* REPCSB - Routine di reperimento dati relativi al contrassegno
105700001218     C*------------------------------------------------------------------------*
105800001218     C     REPCSB        BEGSR
105900001218     C*
106000001219     C     KCSB          chain     tncsb03l
106100001219     C                   if        %found(tncsb03l)
106200001218     C                   if        csbSTA = 9 and tasTSP <> 'P'
106300001218     C                   else
106400001218     C                   movel     csbTPA        vactic
106500001218     C                   move      csbTPI        vactic
106600001218     C                   z-add     csbCAS        vacCAS
106700001218     C                   movel     csbVCA        vacVCA
106800001218     C                   endif
106900001218     C                   endif
107000001218     C*
107100001218     C                   ENDSR
107200001218     C*------------------------------------------------------------------------*
107300001218
107400001218
107500001218
107600001218     C*------------------------------------------------------------------------*
107700001218     C* REPPTGIA - Routine di reperimento dati relativi alla giacenza x POSTE
107800001218     C*------------------------------------------------------------------------*
107900001218     C     REPPTGIA      BEGSR
108000001218     C*
108100001218     C* Reperisco la data di giacenza dagli eventi poichè non vengono
108200001218     C* gestite le giacenze per le bolle poste
108300001218     C     KEVB          SETGT     FNEVB01L
108400001218     C     KEVB          READPE    FNEVB01L                               34
108500001218     C*
108600001218     C* Devo cercare l'ultimo evento di tipo giacenza
108700001218     C     *IN34         DOWEQ     *OFF
108800001218     C                   Z-ADD     1             W
108900001218     C     EVBCEV        LOOKUP    C2A(W)                                 42
109000001218     C     *IN42         IFEQ      '1'
109100001218     C     T2A(W)        ANDEQ     'G'
109200001218     C                   Z-ADD     EVBDEV        vacDAG
109300001218     C                   LEAVE
109400001218     C                   ELSE
109500001218     C     KEVB          READPE    FNEVB000                               34
109600001218     C                   ENDIF
109700001218     C                   ENDDO
109800001218     C*
109900001218     C                   ENDSR
110000001218     C*------------------------------------------------------------------------*
110100001218
110200001218
110300001218
110400001218
110500001218     C*------------------------------------------------------------------------*
110600001218     C* REPLAV - Routine di reperimento dati relativi al lasciato avviso
110700001218     C*------------------------------------------------------------------------*
110800001218     C     REPLAV        BEGSR
110900001218     C*
111000001218     C* Pererisco l'ultimo evento Lasciato Avviso
111100001218     C                   MOVEL     '2A'          COD
111200001218     C     KEVB          SETGT     FNEVB01L
111300001218     C     KEVB          READPE    FNEVB01L                               34
111400001218     C*
111500001218     C     *IN34         DOWEQ     *OFF
111600001218     C                   MOVEL(P)  EVBCEV        KEY
111700001218     C     KTAB2         CHAIN     TABEL                              30
111800001218     C     *IN30         IFEQ      *OFF
111900001218     C                   MOVEL     TBLUNI        DS2A
112000001218     C     §2AFTC        IFEQ      'A'
112100001218     C                   Z-ADD     EVBDEV        vacDLA
112200001218     C                   LEAVE
112300001218     C                   ENDIF
112400001218     C                   ENDIF
112500001218     C     KEVB          READPE    FNEVB000                               34
112600001218     C                   ENDDO
112700001218     C*
112800001218     C                   ENDSR
112900001218     C*------------------------------------------------------------------------*
113000001218
113100001218
113200001218
113300001218     C*------------------------------------------------------------------------*
113400001218     C* CARTBL - Routine di caricamento dati tabellati
113500001218     C*------------------------------------------------------------------------*
113600001218     C     CARTBL        BEGSR
113700001218     C*
113800001218     C                   Z-ADD     0             W                 4 0
113900001218     C                   MOVEA     *HIVAL        T2A
114000001218     C                   MOVEA     *HIVAL        C2A
114100001218     C                   MOVEL     '2A'          COD
114200001218     C     KTAB          CHAIN     TABEL                              31
114300001218     C     *IN31         DOWEQ     '0'
114400001218     C     TBLFLG        IFEQ      ' '
114500001218     C                   MOVEL     TBLUNI        DS2A
114600001218     C     §2AFTC        IFNE      *BLANKS
114700001218     C                   ADD       1             W
114800001218     C                   MOVEL     §2AFTC        T2A(W)
114900001218     C                   MOVEL     TBLKEY        C2A(W)
115000001218     C                   ENDIF
115100001218     C                   ENDIF
115200001218     C     KTAB          READE     TABEL                                  31
115300001218     C                   ENDDO
115400001218     C*
115500081016     C                   Z-ADD     1             I                 4 0
115600011017     C                   MOVEL     CODCLI        KUNI(I)
115700081016     C                   IF        FLGUNI = 'U'
115800001218     C                   MOVEL     '3K'          COD
115900001218     C     KTAB          CHAIN     TABEL                              31
116000001218     C     *IN31         DOWEQ     '0'
116100001218     C     TBLFLG        IFEQ      ' '
116200001218     C                   MOVEL     TBLUNI        DS3K
116300010301     C     §3KCKS        IFEQ      CODCLI
116400001218     C                   ADD       1             I
116500081016     C                   MOVEL     TBLKEY        wKUNI             7 0
116600081016     C     wKUNI         LOOKUP    KUNI                                   55
116700081016     C                   IF        %equal
116800081016     C                   ELSE
116900081016     C                   Z-ADD     wKUNI         KUNI(I)
117000081016     C                   ENDIF
117100001218     C                   ENDIF
117200001218     C                   ENDIF
117300001218     C     KTAB          READE     TABEL                                  31
117400001218     C                   ENDDO
117500010301     C                   ENDIF
117600040312     C*
117700040312     C                   Z-ADD     0             J                 4 0
117800040312     C                   MOVEL     '3A'          COD
117900040312     C     KTAB          CHAIN     TABEL                              31
118000040312     C     *IN31         DOWEQ     '0'
118100040312     C     TBLFLG        IFEQ      ' '
118200040312     C                   MOVEL     TBLUNI        DS3A
118300040312     C     §3AVAC        IFEQ      'N'
118400040312     C                   ELSE
118500040312     C                   ADD       1             J
118600040312     C                   MOVEL     TBLKEY        COD3A(J)
118700040312     C                   ENDIF
118800040312     C                   ENDIF
118900040312     C     KTAB          READE     TABEL                                  31
119000040312     C                   ENDDO
119100001218     C*
119200001218     C                   ENDSR
119300001218     C*---------------------------------------------------------------*
119400151019
119500151019
119600151019
119700151019     C     RTVAR5FAT     BEGSR
119800151019     C*
119900151019     C* Se, presente, considero il peso/volume DESUNTO al pari del VDL TOTALE
120000151019     C                   SETOFF                                       7071
120100151019     C                   EVAL      ar5TRD = 'FAT'
120200151019     C                   CLEAR                   DAR5FAT
120300151019     C     KEYar531_P    CHAIN     FIAR531C
120400151019     C                   IF        %found(FIAR531C)
120500151019     C                   MOVEL     AR5UNI        DAR5FAT
120600151019     C                   IF        §AR5PKTAS > *zeros
120700151019     C                   SETON                                        70
120800151019     C                   EVAL      tasPKF = §AR5PKTAS
120900151019     C                   ENDIF
121000151019     C                   IF        §AR5VLTAS > *zeros
121100151019     C                   SETON                                        71
121200160505     C                   EVAL      tasVLF = §AR5VLTAS
121300151019     C                   ENDIF
121400151019     C                   ENDIF
121500151019     C*
121600151019     C                   ENDSR
121700160208
121800160208
121900160208
122000160208     C     RTVNAS        BEGSR
122100160208     C*
122200160208     C* Inizializzo le DS relative ai tipi record del TITA4 da gestire
122300160208     C                   CLEAR                   DTA4A
122400160208     C*
122500160208     C* Reperisco dal tipo record 'A' del TITA4 la natura merce della bolla corrente
122600160208     C                   CALL      'UBTA400R'
122700160208     C                   PARM      *blanks       UBTA4IOPZ
122800160208     C                   PARM      *blanks       UBTA4ITLA
122900160208     C                   PARM      tasAAS        UBTA4IAAS
123000160208     C                   PARM      tasLNP        UBTA4ILNP
123100160208     C                   PARM      tasNRS        UBTA4INRS
123200160208     C                   PARM      tasNSP        UBTA4INSP
123300160208     C                   PARM      'A'           UBTA4ITRC
123400160208     C                   PARM                    UBTA4OERR
123500160208     C                   PARM                    UBTA4ODS
123600160208     C                   PARM                    UBTA4OLEN
123700160208     C                   PARM                    UBTA4ODATI
123800160208     C*
123900160208     C                   IF        UBTA4OERR = *zeros
124000160208     C                   SELECT
124100160208     C* Gestione output tipo record 'A'
124200160208     C                   WHEN      UBTA4ODS = 'DTA4A'
124300160208     C                   EVAL      DTA4A = %subst(UBTA4ODATI:1:UBTA4OLEN)
124400160208     C                   ENDSL
124500160208     C*
124600160208     C                   ENDIF
124700160208     C*
124800160208     C                   ENDSR
124900161205
125000161205
125100161205
125200161205     C*------------------------------------------------------------------------*
125300161205     C* ENDELA - Routine di lanci di chiusura finali
125400161205     C*------------------------------------------------------------------------*
125500161205     C     ENDELA        BEGSR
125600161205     C*
125700161205     C* Finita l'elaborazione chiamo driver reperimento TITA4 in chiusura
125800161205     C                   CALL      'UBTA400R'
125900161205     C                   PARM                    UBTA4IOPZ
126000161205     C                   PARM      'C'           UBTA4ITLA
126100161205     C                   PARM                    UBTA4IAAS
126200161205     C                   PARM                    UBTA4ILNP
126300161205     C                   PARM                    UBTA4INRS
126400161205     C                   PARM                    UBTA4INSP
126500161205     C                   PARM                    UBTA4ITRC
126600161205     C                   PARM                    UBTA4OERR
126700161205     C                   PARM                    UBTA4ODS
126800161205     C                   PARM                    UBTA4OLEN
126900161205     C                   PARM                    UBTA4ODATI
127000161205     C*
127100161205     C                   ENDSR
127200161205     C*------------------------------------------------------------------------*
127300161205
127400161205
127500161205
127600161205     C*------------------------------------------------------------------------*
127700170227     C* EXEERR - Routine di esecuzione azioni su Errore
127800161205     C*------------------------------------------------------------------------*
127900161205     C     EXEERR        BEGSR
128000161205     C*
128100161205     C                   dump(A)
128200170322     C                   rolbk(e)
128300161205     C                   seton                                        lr
128400161205     C                   return
128500161205     C*
128600161205     C                   ENDSR
128700161205     C*------------------------------------------------------------------------*
128800001218
128900001218
129000001218
129100001218     C*------------------------------------------------------------------------*
129200001218     C* *INZSR - ROUTINE INIZIALE
129300001218     C*------------------------------------------------------------------------*
129400001218     C     *INZSR        BEGSR
129500001218     C*
129600001218     C     *ENTRY        PLIST
129700100512     C                   PARM                    PARAM            71
129800170510     C                   PARM                    FNVACOUT
129900170510     C*
130000170510     C                   if        %parms = 2
130100170510     C                   movel     '1'           wFNVACOUT         1
130200170510     C                   clear                   FNVACOUT
130300170510     C                   else
130400170510     C                   movel     '0'           wFNVACOUT
130500170510     C                   endif
130600001218     C*
130700001218     C                   MOVEL     PARAM         DSINPUT
130800001218     C                   Z-ADD     1             CODUT
130900001218     C*
131000001218     C* Definizioni chiavi
131100001218     C     KTA4          KLIST
131200001218     C                   KFLD                    tasAAS
131300001218     C                   KFLD                    tasLNP
131400001218     C                   KFLD                    tasNRS
131500001218     C                   KFLD                    tasNSP
131600001218     C                   KFLD                    KTRC              1
131700001218     C*
131800001218     C     KTAA          KLIST
131900001218     C                   KFLD                    tasAAS
132000001218     C                   KFLD                    tasLNP
132100001218     C                   KFLD                    tasNRS
132200001218     C                   KFLD                    tasNSP
132300001218     C                   KFLD                    KTRC
132400001218     C*
132500001218     C     KCSB          KLIST
132600001218     C                   KFLD                    tasAAS
132700001218     C                   KFLD                    tasLNP
132800001218     C                   KFLD                    tasNRS
132900001218     C                   KFLD                    tasNSP
133000001218     C*
133100001218     C     KEVB          KLIST
133200001218     C                   KFLD                    tasAAS
133300001218     C                   KFLD                    tasLNP
133400001218     C                   KFLD                    tasNRS
133500001218     C                   KFLD                    tasNSP
133600001218     C*
133700001218     C     KGCP          KLIST
133800001218     C                   KFLD                    tasAAS
133900001218     C                   KFLD                    tasLNP
134000001218     C                   KFLD                    tasNRS
134100001218     C                   KFLD                    tasNSP
134200001218     C*
134300001218     C     KTAB          KLIST
134400001218     C                   KFLD                    CODUT             1 0
134500001218     C                   KFLD                    COD               2
134600001218     C*
134700001218     C     KTAB2         KLIST
134800001218     C                   KFLD                    CODUT             1 0
134900001218     C                   KFLD                    COD               2
135000001218     C                   KFLD                    KEY               8
135100151019     C*
135200151019     C* FIAR531C - Parziale
135300151019     C     KEYar531_P    KLIST
135400151019     C                   KFLD                    tasAAS
135500151019     C                   KFLD                    tasLNP
135600151019     C                   KFLD                    tasNRS
135700151019     C                   KFLD                    tasNSP
135800151019     C                   KFLD                    ar5TRD
135900001218     C*
136000001218     C* Determino la data corrente
136100100312     C                   Z-ADD     *zeros        DATCOR            8 0
136200100312     C                   EVAL      DATCOR = %dec(%date() : *ISO)
136300001218     C*
136400001218     C                   ENDSR

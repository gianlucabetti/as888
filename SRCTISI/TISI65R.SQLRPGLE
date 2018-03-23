000100010925      *PARMS DYNUSRPRF(*OWNER)
000200051117      *PARMS TGTRLS(*PRV)
000300010522
000400010502      * NOTE: Compilare il relativo PRTF con le seguenti caratteristiche =>
000500010502      *       - PAGESIZE 66/132
000600010502      *       - OVRFLW   60
000700010502      *       - LPI      8
000800010502      *       - CPI      15
000900010522      *                                                                        *
001000960702     H*------------------------------------------------------------------------*
001100010329     H* STAMPA PICKUP   TIME - ESEGUE                                          *
001200960702     H*------------------------------------------------------------------------*
001300010522     H DECEDIT('0,') DATEDIT(*DMY.)
001400960702     F*------------------------------------------------------------------------*
001500960702     F* DATA BASE
001600960702     F*------------------------------------------------------------------------*
001700980216     FTABEL00F  IF   E           K DISK
001800980216     FAZORG01L  IF   E           K DISK
001900010504     FTISI65P   O    E             PRINTER OFLIND(*IN05) USROPN
002000960702     D*------------------------------------------------------------------------*
002100960702     D* SCHIERE
002200960702     D*------------------------------------------------------------------------*
002300960704     D*-------------------
002400960704     D* COMANDI CLP
002500960704     D*-------------------
002600960704     D CMD             S              1    DIM(80) CTDATA PERRCD(80)
002700010518     D*-------------------
002800010518     D* REGOLE IN STAMPA
002900010518     D*-------------------
003000010518     D REG             S             90    DIM(3) CTDATA PERRCD(1)
003100980217     D*-------------------
003200980217     D* REGIONI
003300980217     D*-------------------
003400980217     D RAP             S              2    DIM(50)                              CODICE
003500010522     D RDES            S             27    DIM(50)                              DESCRIZIONE
003600980217     D*-------------------
003700980217     D* PROVINCIE
003800980217     D*-------------------
003900980217     D PRO             S              2    DIM(300)                             CODICE
004000010522     D PDES            S             20    DIM(300)                             DESCRIZIONE
004100980218     D*-------------------
004200980218     D* NOTE
004300980218     D*-------------------
004400980218     D NOTE            S             75    DIM(5)
004500980217     D*-------------------
004600980217     D* DS CODICI TASSAZIONE
004700980217     D*-------------------
004800980217     D DSCT          E DS
004900980217     D*-------------------
005000980217     D* DS PROVINCIE
005100980217     D*-------------------
005200980217     D DSPR          E DS
005300980219     D*------------------
005400980219     D* DS CONTROLLA DATA (8)
005500980219     D*------------------
005600980219     D WLBDA8          DS
005700980219     D  G08DAT                 1      8  0
005800980219     D  G08INV                 9     16  0
005900980219     D  G08ERR                17     17
006000980219     D  G08TGI                18     22  0
006100960702     D*-------------------
006200960702     D* PARAMETRI DI LANCIO
006300960702     D*-------------------
006400960702     D PARAM           DS
006500980216     D  PARLNP                 1      3
006600980216     D  PARTEM                 4      4
006700980216     D  PARTSP                 5      5
006800980216     D  PARISO                 6      6
006900980216     D  PARDIS                 7      7
007000980216     D  PARSSA                 8      8
007100980216     D  PARND1                 9     83
007200980216     D  PARND2                84    158
007300980216     D  PARND3               159    233
007400010509     D  PARORE               234    235  0
007500010509     D  PARRD1               236    310
007600010509     D  PARRD2               311    385
007700010509     D  PARRD3               386    460
007800011016     D  PARPRI               461    461
007900980216     D*-------------------
008000980216     D* ARCHITETTURA
008100980216     D*-------------------
008200980216     D KPJBA         E DS
008300010404     D*-------------------
008400010404     D* DS DI LETTURA RECORDSET DEL FILE DI WRK
008500010404     D*-------------------
008600010404     D DSPKU           DS
008700010426     D  §COR                   1      5
008800010426     D  §PRV                   6      7
008900010426     D  §FCR                   8      8
009000010426     D  §ISO                   9      9
009100010521     D  §MAT                  10     11P 0
009200010521     D  §POM                  12     13P 0
009300010521     D  §REG                  14     15
009400010521     D  §LOC                  16     50
009500010518     D*-------------------
009600010518     D* DS DI RIDEFINIZIONE REGOLE IN STAMPA
009700010518     D*-------------------
009800010518     D DSREG           DS
009900010518     D ORE                            2  0
010000010518     D DES                           88
010100010430     D*-------------------
010200010430     D* VARIABILI DI WORK
010300010430     D*-------------------
010400010430     D  depREG         S                   like(§REG)
010500010430     D  depPRV         S                   like(§PRV)
010600010430     D  depMAT         S                   like(§MAT)
010700010430     D  depPOM         S                   like(§POM)
010800010502     D  depCOR         S                   like(§COR)
010900010502     D  depISO         S                   like(§ISO)
011000010430     D  depsqlcd1      S              3  0
011100010430     D  depsqlcd2      S              3  0
011200010504     D  contaRighe     S              5i 0 inz(20)
011300010522     D  cntList        S              5i 0 inz(*zeros)
011400010522     D  righeNote      S              5i 0 inz(*zeros)
011500010523     D  overflow       S              5i 0 inz(88)
011600010430     D  i              S              5i 0
011700010430     D*-----------------
011800010430     D* PARAMETRI DI PASSAGGIO AL PROGRAMMA DI "GIUSTIFICAZIONE" TESTO
011900010430     D*-----------------
012000010430     D TxtInput        S            200    varying
012100010504     D Lungh           S              5i 0 inz(87)
012200010430     D TxtGiust        S            200    varying
012300010430     D TxtResto        S            200    varying
012400010430
012500010430
012600960702     C*------------------------------------------------------------------------*
012700960702     C* MAIN LINE
012800960702     C*------------------------------------------------------------------------*
012900010404     C*
013000010404     C* ESEGUO COMANDI SUL PRINTER FILE
013100010404     C                   Z-ADD     80            LUNG             15 5
013200010404     C                   MOVEL     *BLANKS       COMMAN           80
013300010404     C                   MOVEA     CMD           COMMAN
013400010404     C                   CALL(e)   'QCMDEXC'
013500010404     C                   PARM                    COMMAN
013600010404     C                   PARM                    LUNG
013700010404     C*
013800010404     C* ...DOPODICHE' LO APRO
013900010404     C                   OPEN      TISI65P
014000010404     C*
014100010404     C* STAMPO LA TESTATA DEL PROSPETTO
014200010404     C                   EXSR      PRTTES
014300010404     C*
014400010404     C* ESEGUO SQL PER PREPARAZIOINE SNAPSHOT SUL FILE DI WRK
014500051117     ***C                   IF        PARTEM = 'I'
014600051117     ***C                   EXSR      EXEPKU1
014700051117     ***C                   ELSE
014800010427     C                   EXSR      EXEPKU2
014900051117     ***C                   ENDIF
015000010404     C*
015100010404     C* STAMPO IL PIE' DI PAGINA DEL PROSPETTO
015200010404     C                   EXSR      PRTPIE
015300010404     C*
015400010404     C* ...AL TERMINE LO CHIUDO
015500010404     C                   CLOSE     TISI65P
015600010404     C*
015700051117     C                   SETON                                        LR
015800010404     C*------------------------------------------------------------------------*
015900010404
016000010404
016100051117     ***C*------------------------------------------------------------------------*
016200051117     ***C* EXEPKU1 - Routine di preparazione dati x tempi INTERNI
016300051117     ***C*------------------------------------------------------------------------*
016400051117     ***C     EXEPKU1       BEGSR
016500051117     ***C*
016600051117     ***C/EXEC SQL
016700051117     ***C+ declare C1 cursor for
016800051117     ***C+ SELECT pkucor, pkuprv, pkufcr, pkuiso,
016900051117     ***C+ pkurim, pkurip, pkurap, pkuloc
017000051117     ***C+ FROM wapku00f
017100051117     ***C+ WHERE pkutsp = :PARTSP and (pkurim > :PARORE or pkurip > :PARORE)
017200051117     ***C+ and pkuloc <> ' ' and pkudlv <> :PARPRI
017300051117     ***C+ ORDER BY pkurim, pkurip, pkucor, pkufcr desc,
017400051117     ***C+ pkuiso, pkuprv, pkuloc
017500051117     ***C+ for read only
017600051117     ***C/END-EXEC
017700051117     ***C
017800051117     ***C/EXEC SQL
017900051117     ***C+ open C1
018000051117     ***C/END-EXEC
018100051117     ***C
018200051117     ***C/EXEC SQL
018300051117     ***C+ Fetch C1 into :dspku
018400051117     ***C/END-EXEC
018500051117     ***C*
018600051117     ***C                   eval      depsqlcd1 = sqlcod
018700051117     ***C                   dow       depsqlcd1 <> 100
018800051117     ***C                   exsr      EXEMEM1
018900051117     ***C                   enddo
019000051117     ***C*
019100051117     ***C/EXEC SQL
019200051117     ***C+ close C1
019300051117     ***C/END-EXEC
019400051117     ***C
019500051117     ***C*
019600051117     ***C                   ENDSR
019700051117     ***C*------------------------------------------------------------------------*
019800010404
019900010426
020000010426
020100010426     C*------------------------------------------------------------------------*
020200010427     C* EXEPKU2 - Routine di preparazione dati x tempi CLIENTE
020300010426     C*------------------------------------------------------------------------*
020400010427     C     EXEPKU2       BEGSR
020500010426     C*
020600010426     C/EXEC SQL
020700010426     C+ declare C2 cursor for
020800051117     C+ SELECT  DISTINCT
020900051117     C+ pkucor, pkuprv, pkufcr, pkuiso, pkurcm, pkurcp, pkurap, pkuloc
021000050318     C+ FROM wapku00f
021100010521     C+ WHERE pkutsp = :PARTSP and (pkurcm > :PARORE or pkurcp > :PARORE)
021200051003     C+ and pkuloc <> ' ' and pkudlv <> :PARPRI
021300051117     C+ ORDER   BY
021400051117     C+ pkurcm, pkurcp, pkucor, pkufcr, pkuiso, pkuprv, pkuloc
021500051117     C+ FOR READ ONLY
021600010426     C/END-EXEC
021700010426     C
021800010426     C/EXEC SQL
021900010426     C+ open C2
022000010426     C/END-EXEC
022100010426     C
022200010426     C/EXEC SQL
022300010426     C+ Fetch C2 into :dspku
022400010426     C/END-EXEC
022500010426     C*
022600010427     C                   eval      depsqlcd1 = sqlcod
022700010427     C                   dow       depsqlcd1 <> 100
022800010427     C                   exsr      EXEMEM1
022900010426     C                   enddo
023000010426     C*
023100010426     C/EXEC SQL
023200010426     C+ close C2
023300010426     C/END-EXEC
023400010426     C
023500010426     C*
023600010426     C                   ENDSR
023700010426     C*------------------------------------------------------------------------*
023800010426
023900010521
024000010521
024100010427
024200010404     C*------------------------------------------------------------------------*
024300010427     C* EXEMEM1 - Routine di considerazioni e memorizzazione
024400010404     C*------------------------------------------------------------------------*
024500010427     C     EXEMEM1       BEGSR
024600010522     C*
024700010525     C* Ciclo per tempi eccezioni
024800010522     C                   eval      depMAT = §MAT
024900010525     C* ... stampo descrizione comportameto tempo di eccezione
025000010522     C                   exsr      REPECC
025100010522     C*
025200010522     C                   dow       (§MAT = depMAT) and
025300010522     C                             depsqlcd1 <> 100
025400010521     C*
025500010427     C* Ciclo per regioni
025600010522     C                   exsr      STALIST
025700010522     C                   eval      depREG = §REG
025800010521     C* ... reperisco la descrizione della Regione
025900010404     C                   exsr      REPREG
026000010522     C                   z-add     *zeros        cntList
026100010522     C* Stampo la riga x descrizione Regione
026200010522     C                   if        contaRighe > overflow
026300010522     C                   write     SI65TE
026400010522     C                   reset                   contaRighe
026500010522     C                   endif
026600010522     C                   write     SI65RI
026700010522     C                   write     SI65DE
026800010522     C                   clear                   SI65DE
026900010522     C                   add       2             contaRighe
027000010522     C*
027100010521     C                   dow       (§REG = depREG) and
027200010427     C                             depsqlcd1 <> 100
027300010521     C*
027400010521     C* Ciclo x provincia
027500010522     C                   exsr      STALIST
027600010521     C                   eval      depPRV = §PRV
027700010521     C* ... reperisco la descrizione della Provincia
027800010521     C                   exsr      REPPRV
027900010522     C                   z-add     *zeros        cntList
028000010521     C*
028100010521     C                   eval      depCOR = §COR
028200010404     C                   dow       (§REG = depREG) and
028300010503     C                             (§COR = depCOR) and
028400010427     C                             depsqlcd1 <> 100
028500010521     C*
028600010521     C                   if        %len(%trim(STPLIST)) + 1 +
028700010521     C                             %len(%trim(§LOC)) + 1 <= Lungh
028800010521     C                   exsr      CRTLIST
028900010521     C                   else
029000010522     C                   add       1             cntList
029100010521     C                   exsr      STALIST
029200010521     C                   exsr      CRTLIST
029300010521     C                   endif
029400010521     C*
029500010427     C* Eseguo lettura successiva
029600051117     ***C                   if        PARTEM = 'I'
029700051117     ***C*
029800051117     ***C/EXEC SQL
029900051117     ***C+ Fetch C1 into :dspku
030000051117     ***C/END-EXEC
030100051117     ***C*
030200051117     ***C                   eval      depsqlcd1 = sqlcod
030300051117     ***C                   else
030400010427     C*
030500010427     C/EXEC SQL
030600010427     C+ Fetch C2 into :dspku
030700010427     C/END-EXEC
030800010427     C*
030900010427     C                   eval      depsqlcd1 = sqlcod
031000051117     ***C                   endif
031100010427     C*
031200010404     C                   enddo
031300010404     C                   enddo
031400010522     C                   enddo
031500010521     C*
031600010521     C* A fine ciclo scarico il buffer (se pieno)
031700010522     C* ... (Testata Regione/Provincia)
031800010521     C                   if        STPDETT <> *blanks
031900010521     C                   if        contaRighe > overflow
032000010521     C                   write     SI65TE
032100010521     C                   reset                   contaRighe
032200010521     C                   endif
032300010522     C                   write     SI65RI
032400010521     C                   write     SI65DE
032500010521     C                   clear                   SI65DE
032600010522     C                   add       2             contaRighe
032700010521     C                   endif
032800010522     C* ... (Elenco località)
032900010521     C                   if        STPLIST <> *blanks
033000010521     C                   exsr      STALIST
033100010521     C                   endif
033200010427     C*
033300010427     C                   ENDSR
033400010404     C*------------------------------------------------------------------------*
033500010404
033600010521
033700010521
033800010521     C*------------------------------------------------------------------------*
033900010521     C* CRTLIST - Routine di creazione lista località che fanno eccezione
034000010521     C*------------------------------------------------------------------------*
034100010521     C     CRTLIST       BEGSR
034200010521     C*
034300010521     C                   if        STPLIST = *blanks
034400051117     C                   eval      STPLIST = §LOC
034500010521     C                   else
034600010521     C                   eval      STPLIST = %trim(STPLIST)+','+%trim(§LOC)
034700010521     C                   endif
034800010521     C*
034900010521     C                   ENDSR
035000010521     C*------------------------------------------------------------------------*
035100010521
035200010404
035300010521
035400010521
035500010521     C*------------------------------------------------------------------------*
035600010521     C* STALIST - Routine di stampa lista località che fanno eccezione
035700010521     C*------------------------------------------------------------------------*
035800010521     C     STALIST       BEGSR
035900010521     C*
036000010522     C                   if        cntList > 1
036100010522     C                   eval      STPDETT = *blanks
036200010522     C                   endif
036300010522     C*
036400010521     C                   if        STPLIST <> *blanks
036500010521     C                   eval      TxtInput = %trim(STPLIST)
036600010522     C                   if        (cntlist > 0) and
036700010522     C                             (%len(%trim(TxtInput)) > 70)
036800010521     C                   exsr      exegiust
036900010521     C                   if        not %error
037000010521     C                   eval      STPLIST = TxtGiust
037100010521     C                   else
037200010521     C                   eval      STPLIST = TxtInput
037300010521     C                   endif
037400010522     C                   else
037500010522     C                   eval      STPLIST = TxtInput
037600010522     C                   endif
037700010521     C                   if        contaRighe > overflow
037800010521     C                   write     SI65TE
037900010521     C                   reset                   contaRighe
038000010521     C                   endif
038100010521     C                   write     SI65LO
038200010521     C                   clear                   SI65LO
038300010521     C                   add       1             contaRighe
038400010521     C                   endif
038500010521     C*
038600010521     C                   ENDSR
038700010521     C*------------------------------------------------------------------------*
038800010521
038900010521
039000010430
039100010430     C*------------------------------------------------------------------------*
039200010430     C* EXEGIUST - Routine di "GIUSTIFICAZIONE" testo
039300010430     C*------------------------------------------------------------------------*
039400010430     C     EXEGIUST      BEGSR
039500010430     C*
039600010430     C                   call(e)   'XGIUST'
039700010430     C                   parm                    TxtInput
039800010430     C                   parm                    Lungh
039900010430     C                   parm                    TxtGiust
040000010430     C                   parm                    TxtResto
040100010430     C*
040200010430     C                   ENDSR
040300010430     C*------------------------------------------------------------------------*
040400010430
040500010430
040600010518
040700010518     C*------------------------------------------------------------------------*
040800010525     C* REPECC - Routine di reperimento giusta regola di eccezione "Tempi PICKUP"
040900010518     C*------------------------------------------------------------------------*
041000010518     C     REPECC        BEGSR
041100010518     C*
041200040628      * salto questo passaggio tanto le ore sono fisse '12' dal lancio
041300040628     c                   goto      salta
041400010518     C                   z-add     1             x                 1 0
041500010518     C                   dow       x <= 3
041600010518     C                   eval      DSREG = REG(x)
041700010521     C                   if        ORE = depMAT
041800010518     C                   eval      STPECC = DES
041900010521     C                   leave
042000010518     C                   else
042100010518     C                   movel     *all'*'       STPECC
042200010518     C                   endif
042300010518     C                   add       1             x
042400010518     C                   enddo
042500040628     c     salta         tag
042600040628      * imposto io fissa la scritta in stampa
042700040628     c                   Eval      stpecc = 'Per le seguenti località la -
042800040628     c                             partenza della merce potrà anche avvenire -
042900040628     c                             il giorno successivo al ritiro.'
043000010521     C*
043100010521     C                   write     SI65RI
043200010521     C                   write     SI65EC
043300010521     C                   add       2             contaRighe
043400010521     C                   clear                   SI65EC
043500010518     C*
043600010518     C                   ENDSR
043700010518     C*------------------------------------------------------------------------*
043800010518
043900010518
044000010518
044100010404
044200010404     C*------------------------------------------------------------------------*
044300010404     C* REPREG - Routine di reperimento descrizione delle Regioni
044400010404     C*------------------------------------------------------------------------*
044500010404     C     REPREG        BEGSR
044600010404     C*
044700010404     C* Decodifica la Regione
044800010404     C                   Z-ADD     1             J                 4 0
044900010404     C                   SETOFF                                           98
045000010404     C     depREG        LOOKUP    RAP(J)                                 98
045100010404     C     *IN98         IFEQ      *ON                                          *TROVATO
045200010404     C                   MOVEL(P)  RDES(J)       STPREG
045300010404     C                   ELSE                                                   *NON TROVATO
045400010404     C                   MOVEL(P)  *ALL'*'       STPREG
045500010404     C                   ENDIF
045600010404     C*
045700010404     C                   ENDSR
045800010404     C*------------------------------------------------------------------------*
045900010404
046000010404
046100010404
046200960702     C*------------------------------------------------------------------------*
046300010404     C* REPPRV - Reperisce la descrizione della Provincia
046400960702     C*------------------------------------------------------------------------*
046500010404     C     REPPRV        BEGSR
046600980217     C*
046700010404     C* Decodifica la Provincia
046800980217     C                   Z-ADD     1             J                 4 0
046900980217     C                   SETOFF                                           98
047000010521     C     depPRV        LOOKUP    PRO(J)                                 98
047100010404     C     *IN98         IFEQ      *ON                                          *TROVATO
047200010404     C                   MOVEL(P)  PDES(J)       desPRV           35
047300010404     C                   ELSE                                                   *NON TROVATO
047400010404     C                   MOVEL(P)  *ALL'*'       desPRV
047500010404     C                   ENDIF
047600010404     C*
047700010705     C* Effettuo operazioni x allineamento a DX (non uso evalr xchè codesto PGM va in filiale)
047800010705     C                   movel     *all' '       WrkBlanks        20
047900110222     c                   if        %len( %trim( desPRV ) ) <
048000110222     c                                                   %size( WrkBlanks ) - 1
048100010705     C                   eval      STPDETT = %subst(WrkBlanks:1:(19-(%len(%trim(
048200010705     C                                       desPRV))))) + %trim(desPRV) + ':'
048300110222     c                   else
048400110222     c                   eval      STPDETT = desPRV
048500110222     c                   move      ':'           STPDETT
048600110222     c                   endif
048700010521     C*
048800010404     C                   ENDSR
048900010404     C*------------------------------------------------------------------------*
049000010404
049100010404
049200010427
049300010427     C*------------------------------------------------------------------------*
049400010521     C* PRTTES - Routine di stampa testata prospetto
049500010427     C*------------------------------------------------------------------------*
049600010427     C     PRTTES        BEGSR
049700010427     C*
049800051117     ***C                   eval      STPIOC = PARTEM
049900010427     C                   write     SI65TE
050000010521     C*
050100010521     C                   eval      STPREG1 = %trim(PARRD1) + ' ' +
050200010521     C                                       %trim(PARRD2) + ' ' +
050300010521     C                                       %trim(PARRD3)
050400010521     C                   write     SI65RE
050500010521     C                   add       1             contaRighe
050600010427     C*
050700010427     C                   ENDSR
050800010427     C*------------------------------------------------------------------------*
050900010427
051000010427
051100010427
051200010427     C*------------------------------------------------------------------------*
051300010427     C* PRTPIE - Routine di stampa piè di pagina prospetto
051400010427     C*------------------------------------------------------------------------*
051500010427     C     PRTPIE        BEGSR
051600010522     C*
051700010522     C                   eval      righeNote = 2
051800010427     C*
051900010522     C* Se peni valorizzo i campi note e stampo la chiusura prospetto
052000010522     C                   if        PARND1 <> *blanks
052100010522     C                   eval      STPNOT1 = PARND1
052200010522     C                   eval      righeNote = righeNote + 1
052300010522     C                   endif
052400010522     C*
052500010522     C                   if        PARND2 <> *blanks
052600010522     C                   eval      STPNOT2 = PARND2
052700010522     C                   eval      righeNote = righeNote + 1
052800010522     C                   endif
052900010522     C*
053000010522     C                   if        PARND3 <> *blanks
053100010522     C                   eval      STPNOT3 = PARND3
053200010522     C                   eval      righeNote = righeNote + 1
053300010522     C                   endif
053400010504     C*
053500010522     C                   if        contaRighe > overflow - righeNote
053600010504     C                   write     SI65TE
053700010504     C                   reset                   contaRighe
053800010504     C                   endif
053900010522     C*
054000010522     C                   if        righeNote > 2
054100010427     C                   write     SI65RI
054200010504     C                   write     SI65NO
054300010522     C                   endif
054400010522     C*
054500010504     C                   write     SI65RI
054600010427     C                   write     SI65FI
054700010427     C*
054800010427     C                   ENDSR
054900010427     C*------------------------------------------------------------------------*
055000010427
055100010404
055200010404
055300980217     C*------------------------------------------------------------------------*
055400980217     C* CARTAB - CARICAMENTO TABELLE OCCORRENTI
055500980217     C*------------------------------------------------------------------------*
055600980217     C     CARTAB        BEGSR
055700980217     C*---
055800980217     C* REGIONI
055900980217     C*---
056000980217     C                   Z-ADD     *ZEROS        I
056100020906     C                   Z-ADD     1             KTBKUT
056200980217     C                   MOVEL     'CT'          KTBCOD
056300980217     C     KEYTAB        SETLL     TABEL00F
056400980217     C     KEYTAB        READE     TABEL00F                               99
056500010427     C     *IN99         DOWEQ     *OFF
056600010427     C     TBLFLG        IFEQ      *BLANKS
056700980217     C                   MOVEL     TBLUNI        DSCT
056800010502     C*
056900010502     C     §CTNAR        IFEQ      *BLANKS                                      *ITALIA
057000010502     C                   MOVE      §CTCOR        A1                1
057100010502     C     A1            IFEQ      '9'                                          *REGIONE
057200980217     C                   ADD       1             I
057300010502     C                   MOVEL     §CTRAP        RAP(I)                         *CODICE
057400980217     C                   MOVEL     §CTDES        RDES(I)                        *DESCRIZIONE
057500010427     C                   ENDIF
057600010502     C                   ENDIF
057700010427     C                   ENDIF
057800980217     C     KEYTAB        READE     TABEL00F                               99
057900010427     C                   ENDDO
058000980217     C*---
058100980217     C* PROVINCIE
058200980217     C*---
058300980217     C                   Z-ADD     *ZEROS        I
058400020906     C                   Z-ADD     1             KTBKUT
058500980217     C                   MOVEL     'PR'          KTBCOD
058600980217     C     KEYTAB        SETLL     TABEL00F
058700980217     C     KEYTAB        READE     TABEL00F                               99
058800010427     C     *IN99         DOWEQ     *OFF
058900010427     C     TBLFLG        IFEQ      *BLANKS
059000980217     C                   MOVEL     TBLUNI        DSPR
059100980217     C                   ADD       1             I
059200980217     C                   MOVEL     TBLKEY        PRO(I)                         *CODICE
059300980217     C                   MOVEL     §PRDES        PDES(I)                        *DESCRIZIONE
059400010427     C                   ENDIF
059500980217     C     KEYTAB        READE     TABEL00F                               99
059600010427     C                   ENDDO
059700980218     C*---
059800980218     C* NOTE
059900980218     C*---
060000980218     C                   Z-ADD     *ZEROS        I
060100020906     C                   Z-ADD     1             KTBKUT
060200980218     C                   MOVEL     '9C'          KTBCOD
060300980218     C     KEYTAB        SETLL     TABEL00F
060400980218     C     KEYTAB        READE     TABEL00F                               99
060500010427     C     *IN99         DOWEQ     *OFF
060600010427     C     TBLFLG        IFEQ      *BLANKS
060700980218     C                   ADD       1             I
060800980218     C                   MOVEL     TBLUNI        NOTE(I)
060900010427     C                   ENDIF
061000980218     C     KEYTAB        READE     TABEL00F                               99
061100010427     C                   ENDDO
061200980217     C*
061300960702     C                   ENDSR
061400960702     C*------------------------------------------------------------------------*
061500010427     C* *INZSR - IMPOSTAZIONI INIZIALI
061600960702     C*------------------------------------------------------------------------*
061700960702     C     *INZSR        BEGSR
061800980216     C*---
061900960702     C* RICEVIMENTO PARAMETRI
062000980216     C*---
062100960702     C     *ENTRY        PLIST
062200960702     C                   PARM                    KPJBA
062300010509     C*---
062400010509     C* DEFINISCO LA *LDA
062500010509     C*---
062600010509     C     *DTAARA       DEFINE    *LDA          PARAM
062700010509     C                   IN        PARAM
062800051003     C*---
062900051003     C* CONSIDERAZOINI "MOLTO" PARTICOLARI SUI PARAMETRI RICEVUTI
063000051003     C* Le istruzioni seguenti sono state fatte solo xchè altrimenti bisognava
063100051003     C* praticamente sventrare il programma.
063200051003     C* Le selezioi SQL prima ragionavano x PKUDLY = PARPRI ma siccome
063300051003     C* prima o stampava SOLO le principali o stampava SOLO le NN principali
063400051003     C* si è modificata la selezione SQL ragionando x PKUDLY = PARPRI ed
063500051003     C* apportando le forzature d cui sotto al parametro stesso (PARPRI).
063600051003     C*---
063700051003     C                   IF        PARPRI = 'S'
063800051003     C                   EVAL      PARPRI = 'N'
063900051003     C                   ELSE
064000051003     C                   IF        PARPRI = 'N'
064100051003     C                   EVAL      PARPRI = *blanks
064200051003     C                   ENDIF
064300051003     C                   ENDIF
064400051117     ***C*---
064500051117     ***C* PRIME CONSIDERAZOINI SUI PARAMETRI RICEVUTI
064600051117     ***C*---
064700051117     ***C                   SETOFF                                       20
064800051117     ***C                   SETOFF                                       212223
064900051117     ***C*                  IF        PARDIS = 'S'
065000051117     ***C*                  SETON                                        21
065100051117     ***C*                  ENDIF
065200051117     ***C*                  IF        PARISO = 'S'
065300051117     ***C*                  SETON                                        22
065400051117     ***C*                  ENDIF
065500980216     C*---
065600960702     C* VARIABILI RIFERITE AL DATABASE
065700980216     C*---
065800960702     C     *LIKE         DEFINE    TBLKUT        KTBKUT                         *TABEL00F
065900960702     C     *LIKE         DEFINE    TBLCOD        KTBCOD
066000051117     ***C     *LIKE         DEFINE    ORGFIL        KORFIL                         *AZORG00F
066100980216     C*---
066200960702     C* CHIAVI DI LETTURA
066300980216     C*---
066400980216     C     KEYTAB        KLIST                                                  *TABEL00F
066500980216     C                   KFLD                    KTBKUT                          -UTENTE
066600980216     C                   KFLD                    KTBCOD                          -CODICE TABELLA
066700051117     ***C     KEYORG        KLIST                                                  *AZORG00F
066800051117     ***C                   KFLD                    KORFIL                          -FILIALE
066900980219     C*
067000980219     C* CALCOLA LA DATA CORRENTE
067100980219     C                   TIME                    N14              14 0          *ORA (6)+ DATA (8)
067200980219     C                   MOVE      N14           N8                8 0          *DATA (8) IN G/M/AA
067300980219     C                   Z-ADD     N8            G08DAT
067400980219     C                   Z-ADD     *ZEROS        G08INV
067500980219     C                   MOVEL     '0'           G08ERR
067600980219     C                   CALL      'XSRDA8'
067700980219     C                   PARM                    WLBDA8
067800980219     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
067900980219     C*
068000980217     C* CARICAMENTO TABELLE OCCORRENTI
068100980217     C                   EXSR      CARTAB
068200960702     C*
068300960702     C                   ENDSR
068400960702     O*------------------------------------------------------------------------*
068500980225** CMD -80-
068600010404OVRPRTF FILE(TISI65P) FORMTYPE(LET1) USRDTA('Pick-Up')
068700010518** REG -75-
06880001051812Per chiamate entro le ore 12:00 si effettuerà il ritiro in giornata.
06890001051824Per le seguenti località si effettuerà il ritiro 1 giorno dopo quello di chiamata.
06900001051848Per le seguenti località si effettuerà il ritiro 2 giorni dopo quello di chiamata.

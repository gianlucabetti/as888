000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200040305     H* FNLRP5R *----------------------------------------------------*
000400040305     H*         - Batch elenco spedizioni di valore in distinta      *
000500940221      *--------------------------------------------------------------*
000600940221      *--------------------------------------------------------------*
000700040305     FFNARB70L  IF   E           K DISK
000800090309     FFiARt01L  IF   E           K DISK
000900040305     Ffiar501l  IF   E           K DISK
001000040305     Ffiapd01L  IF   E           K DISK
001100040308     Ffnlrp5p   o    E             printer
001200941230      *--------------------------------------------------------------*
001300040305     D kfgs            S                   LIKE(arbifp)
001400040308     D wtrd            S                   LIKE(ar5trd)
001500040308     D sav_arbndc      S                   LIKE(arbndc)
001600040305     d data_eur        s               d   datfmt(*eur)
001700040305      *--------------------------------------------------------------*
001800040305     D FNLRP5DS      E DS
001900040305     D dar5gen       E DS
002000040305     D CNCR80        E DS
002100040305     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
002200040305     D KPJBA         E DS
002300040305     D  LIBSYS                92    100
002400040309     d                 ds
002500040309     d  stps01                 1      7  0
002600040309     d  stps02                 8     14  0
002700040309     d  stps03                15     21  0
002800040309     d  stps04                22     28  0
002900040309     d  stps05                29     35  0
003000040309     d  stps06                36     42  0
003100040309     d  stps07                43     49  0
003200040309     d  stps08                50     56  0
003300040309     d  stps09                57     63  0
003400040309     d  stps10                64     70  0
003500040309     d  stps11                71     77  0
003600040309     d  stps12                78     84  0
003700040309     d* stps13                85     91  0
003800040309     d* stps14                92     98  0
003900040309     d* stps15                99    105  0
004000040309     d  stp                    1     84  0 dim(12)
004100040309     d                 ds
004200040309     d  bar01                  1      1
004300040309     d  bar02                  2      2
004400040309     d  bar03                  3      3
004500040309     d  bar04                  4      4
004600040309     d  bar05                  5      5
004700040309     d  bar06                  6      6
004800040309     d  bar07                  7      7
004900040309     d  bar08                  8      8
005000040309     d  bar09                  9      9
005100040309     d  bar10                 10     10
005200040309     d  bar11                 11     11
005300040309     d  bar12                 12     12
005400040309     d* bar13                 13     13
005500040309     d* bar14                 14     14
005600040309     d* bar15                 15     15
005700040309     d  bar                    1     12    dim(12)
005800040305      *--------------------------------------------------------------*
005900000000     I/SPACE 3
006000900702     C*****************************************************************
006100000000     C     *ENTRY        PLIST
006200000000     C                   PARM                    KPJBA
006300000000     C                   Z-ADD     1             CODUT
006400920909     C                   CALL      'X§PARUT'
006500920909     C                   PARM                    UT§DSE
006600000000     C                   MOVEL     RAGUT         RSUT             20
006700920909     C                   MOVEL     REC80         CNCR80
006800040305     c                   movel     kpjbu         fnlrp5ds
006900040305     c* inverto la data distinta
007000040305     c     *iso          move      ip5dat        data_eur
007100040305     c     karb          klist
007200040305     c                   kfld                    kfgs
007300040305     c                   kfld                    ip5nrd
007400040305     c     kar5          klist
007500040305     c                   kfld                    arbaas
007600040305     c                   kfld                    arblnp
007700040305     c                   kfld                    arbnrs
007800040305     c                   kfld                    arbnsp
007900040305     c                   kfld                    wtrd
008000040305     c                   movel(p)  'GEN'         wtrd
008100040309     c     kart          klist
008200040309     c                   kfld                    arbaas
008300040309     c                   kfld                    arblnp
008400040309     c                   kfld                    arbnrs
008500040309     c                   kfld                    arbnsp
008600040308     C     Kapd          KLIST
008700040308     C                   KFLD                    apdtip
008800040308     C                   KFLD                    arbpdc
008900040308     c                   movel     'A'           apdtip
009000900511     C*---------------------------------------------------------------*
009100900517     C*
009200040305     c                   z-add     ip5fil        kfgs
009300040305     c                   clear                   sav_arbndc
009400040305     c                   movel     ragut         rsut
009500040305     c* stampo testata
009600040305     c                   write     tes01
009700040309     c                   z-add     6             $righe            2 0
009800040305     c*
009900040305     c     karb          setll     fnarb70l
010000040305do  2c                   Do        *Hival
010100040305     c     kfgs          reade     fnarb70l
010200040308     c                   If        %Eof(Fnarb70l)
010300040305     c                   Leave
010400040305     c                   EndIf
010500040305     c* se numero distinta > "numero distinta al" esco dal ciclo
010600040305     c     arbndc        ifgt      ip5nra
010700040305     c                   leave
010800040305     c                   endif
010900040305     c* se data distinta diversa da quella passata ignoro il record
011000040305     c     arbddc        ifne      ip5dat
011100040305     c                   iter
011200040305     c                   endif
011300040305     c* Se spedizione non di valore ignoro il record
011400040305     c     kar5          chain     fiar501l
011500040305     c                   if        %found(fiar501l)
011600040305     c                   movel     ar5uni        dar5gen
011700040305     c                   else
011800040305     c                   clear                   dar5gen
011900040305     c                   endif
012000040305     c     §ar5bva       ifeq      *blanks
012100040305     c                   iter
012200040305     c                   endif
012300040305     c* A cambio di distinta stampo totali
012400040305     c     arbndc        ifne      sav_arbndc
012500040305     c     sav_arbndc    ifgt      *zeros
012600040305     c* ..stampo totali
012700040305     c                   write     tot01
012800040308     c                   add       2             $righe
012900040305     c                   clear                   sttotb
013000040305     c                   clear                   sttotc
013100040305     c                   endif
013200040305     c                   z-add     arbndc        sav_arbndc
013300040305     c* ..stampo intestazione nuova distinta
013400040701     c     $righe        ifgt      57
013500040308     c                   write     tes01
013600040309     c                   z-add     6             $righe            2 0
013700040308     c                   endif
013800040305     c                   exsr      rie02
013900040305     c                   write     tes02
014000040309     c                   add       3             $righe
014100040305     c                   endif
014200040305     c* Stampo riga di dettaglio
014300040305     c                   exsr      riedet
014400040701     c     $righe        ifgt      60
014500040308     c                   write     tes01
014600040309     c                   z-add     6             $righe            2 0
014700040308     c                   write     tes02
014800040309     c                   add       3             $righe
014900040308     c                   endif
015000040305     c                   write     det01
015100040308     c                   add       1             $righe
015200040305     c                   add       arbncl        sttotc
015300040305     c                   add       1             sttotb
015400040309     c* richiamo routine per stampare i segnacolli
015500040309     c                   exsr      gesscn
015600040305     c                   enddo
015700040305     c* se totali > 0 stampo ultima totalizzazione
015800040305     c
015900040305     c     sttotb        ifgt      *zeros
016000040305     c                   write     tot01
016100040305     c                   clear                   sttotb
016200040305     c                   clear                   sttotc
016300040305     c                   endif
016400040305     c* stampo dicitura "fine stampa"
016500040305     c                   write     fines
016600040305     c
016700910320     C*
016800000000     C     FINE          TAG
016900000000     C                   SETON                                        LR
017000040305     C*****************************************************************
017100040305     C* RIE02 - Riempimento campi file printer - intestazione distinta
017200040305     C*****************************************************************
017300040305     C     rie02         BEGSR
017400920909     C*
017500040305     c                   z-add     ip5fil        stpfgs
017600040305     c                   z-add     arbndc        stpnfv
017700040305     c                   move      data_eur      stpdts
017800040308     C                   MOVEL     *BLANKS       APDRSC
017900040308     c                   z-add     arbpdc        stppdc
018000040308     C     kapd          CHAIN     fiapd01L                           31
018100040308     C                   MOVEL     APDRSC        STDPDR
018200040305     c                   endsr
018300040308     C*****************************************************************
018400040308     C* RIEDET - Riempimento campi file printer - riga di dettaglio
018500040308     C*****************************************************************
018600040308     C     riedet        BEGSR
018700040308     C*
018800040308     c                   z-add     arblnp        stplnp
018900040309     c                   clear                   tra01
019000040308     c                   z-add     arbnrs        stpnrs
019100040309     c                   if        arbnrs > 0
019200040309     c                   eval      tra01='-'
019300040309     c                   endif
019400040308     c                   z-add     arbnsp        stpnsp
019500040308     c                   movel     arbmgs        wmm               2 0
019600040308     c                   move      arbmgs        wgg               2 0
019700040308     c                   movel     wgg           stpmgs
019800040308     c                   move      wmm           stpmgs
019900040308     c                   z-add     arbaas        stpaas
020000040308     c                   movel(p)  arbrsm        stprsm
020100040308     c                   movel(p)  arbrsd        stprsd
020200040308     c                   movel(p)  arbind        stpind
020300040308     c                   movel(p)  arblod        stplod
020400040308     c                   movel(p)  arbprd        stpprd
020500040308     c                   z-add     arbncl        stpncl
020600040309     c                   z-add     arbznc        stpznc
020700040308     c                   endsr
020800040309     C*****************************************************************
020900040309     C* GESSCN - Riempimento campi file printer - Numeri segnacollo
021000040309     C*****************************************************************
021100040309     C     gesscn        BEGSR
021200040309     C*
021300040309     c                   z-add     0             ii                4 0
021400040309     c                   seton                                        10
021500040309     c                   clear                   stp
021600040309     c                   clear                   bar
021700090309     c     kart          setll     fiart01l
021800040309     c                   do        *hival
021900090309     c     kart          reade     fiart01l
022000090309     c                   If        %Eof(Fiart01l)
022100040309     c                   Leave
022200040309     c                   EndIf
022300040309     c     ii            ifge      12
022400040309     c* stampo riga segnacolli
022500040701     c     $righe        ifgt      61
022600040309     c                   write     tes01
022700040309     c                   z-add     6             $righe            2 0
022800040309     c                   write     tes02
022900040309     c                   add       3             $righe
023000040309     c                   endif
023100040309     c                   clear                   bar12
023200040309     c                   write     detscn
023300040309     c                   add       1             $righe
023400040309     c                   setoff                                       10
023500040309     c                   z-add     0             ii
023600040309     c                   clear                   stp
023700040309     c                   clear                   bar
023800040309     c                   endif
023900040309     c                   add       1             ii
024000040309     c                   z-add     artnsc        stp(ii)
024100040309     c                   movel     '/'           bar(ii)
024200040309     c                   Enddo
024300040309     c* stampo riga segnacolli
024400040309     c     ii            ifgt      0
024500040309     c                   clear                   bar(ii)
024600040701     c     $righe        ifgt      61
024700040309     c                   write     tes01
024800040309     c                   z-add     6             $righe            2 0
024900040309     c                   write     tes02
025000040309     c                   add       3             $righe
025100040309     c                   endif
025200040309     c                   write     detscn
025300040309     c                   add       1             $righe
025400040309     c                   endif
025500040309     c
025600040309     c                   endsr

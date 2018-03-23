000100080327     H DECEDIT('0,') DATEDIT(*DMY.)
000200080327     H*         - Rientri autotrasportatore anomalie
000300000000     H*--------------------------------------------------------------*
000400041117     Ffnlrr6d   cf   e             workstn
000500041025     Ffiapd01l  if   e           k disk
000600041025     Ftntbe01l  if   e           k disk
000700070828     Fazorg01l  if   e           k disk
000800930707     F*
000900041025     D kpjba         e ds
001000950125     D*
001100041025     D* PASSAGGIO DATI A PGM DI STAMPA RIEPILOGO
001200041025     D param           DS
001300030723     d  data1                         8  0
001400030723     d  data2                         8  0
001500030723     d  aut1                          7  0
001600030723     d  aut2                          7  0
001700041124     d  dett                          1
001800041124     d  valo                          1
001900070828     d  parmes                        1
002000071031     d  sede                          1
002100910423     D WLBDAT          DS
002200950125     D  G02DAT                 1      8  0
002300950125     D  G02INV                 9     16  0
002400950125     D  G02ERR                17     17
002500950125     D  G02TGI                18     22  0
002600931015     D                 DS
002700041026     D  vi1pf1                 1      3  0
002800041025     D  vi1pd1                 4      7
002900041025     D  vi1p1                  1      7  0
003000931126     D                 DS
003100041025     D  vi1pf2                 1      3  0
003200041025     D  vi1pd2                 4      7
003300041025     D  vi1p2                  1      7  0
003400041025     D Dcra          e ds
003500070828     D og150         e ds
003600041025     D fnlv24ds      e ds
003700041025     D cncr80        e ds
003800041025     D ut§dse        e ds                  extname(ut§dse0f)
003900131008     D tibs42ds      E DS
004000131008     D uteautds      E DS
004100050505      *
004200030723     d w01dd1          s                   like(data1)
004300030723     d w01dd2          s                   like(data2)
004400000000     I/SPACE 3
004500041025     C*--------------------------------------------------------------
004600900521     C*  RIEPILOGO INDICATORI
004700041025     C*--------------------------------------------------------------
004800931116     C* 30    - DI COMODO
004900010906     C* 40/56 - ERRORI
005000950125     C* 89    - SONO UN "REMXXX"
005100900521     C* 90    - ON INDICA CHE C'E' UN ERRORE NEI CAMPI
005200900521     C*****************************************************************
005300041025     C     *entry        plist
005400041025     C                   parm                    kpjba
005500041025     C                   z-add     1             codut
005600900615     C                   CALL      'X§PARUT'
005700041025     C                   parm                    UT§DSE
005800041025     C                   movel     ragut         rsut             20
005900041025     C                   movel     rec80         cncr80
006000041025      *
006100041025     C     kapdv         klist
006200041025     C                   kfld                    apdtip
006300041025     C                   kfld                    vi1p1
006400041025     C                   movel     'A'           apdtip
006500041026      *
006600041025     c     ktbe          klist
006700041025     c                   kfld                    tbecod
006800041025     c                   kfld                    tbeke1
006900041025     c                   kfld                    tbeke2
007000041025     c                   move      'CRA'         tbecod
007100041025     c                   movel     'DFT'         tbeke1
007200061129     c                   clear                   dcra
007300041025      *
007400041025     c     ktbe          chain     tntbe01l                           93
007500041025     c                   if        not *in93 and tbeatb = *blank
007600041025     c                   movel     tbeuni        dcra
007700041025     c                   endif
007800071031     C* profilo di sede abilito riga totali per pgm stampa
007900080327      * abilitata per tutti dal 27/03/2008
008000080327     c*                  if        simpou = 46
008100071031     c                   move      '1'           sede
008200080327     c*                  endif
008300100118     c*********
008400030723     c* se  profilo edp sproteggo i campi filiale
008500100118     c*********   NON PIU' UTILIZZATO da tempo  .......
008600100118     c*********          if        %subst(knmus: 1: 3) <> 'EDP' and
008700100118     c*********                    %subst(knmus: 1: 3) <> 'QUA'
008800100118     c*********          seton                                        46
008900100118     c*********          end
009000931116     C*---------------------------------------------------------------*
009100910419     C*
009200950411     C* OPERAZIONI INIZIALI
009300041025     C                   exsr      pul01
009400070828      * verifico se la filiale ha gestione parcel/messaggerie
009500070828     C     simpou        chain     azorg01l
009600070828    1C                   if        %found(azorg01l)
009700070828     C                   movel     orgdf0        og150
009800070828     c                   endif
009900070828     c     §ogpkm        comp      'S'                                    20
010000070828     c  n20              clear                   vi1pm
010100950411     C*
010200041025     C     inizio        tag
010300041025     C                   movel     '0000'        vi1pd1
010400041025     C                   movel     '9999'        vi1pd2
010500041025     C                   clear                   vi1dd1
010600041025     C                   clear                   vi1dd2
010700041025     C                   movel     'N'           vi1det
010800061129      * non effettuo controllo se profilo qualità x lanci da sede su tutto
010900061129      * il territorio
011000930426     C*
011100041025     C     for01         tag
011200041117     C                   exfmt     lrr6d01
011300050512     c   66              goto      fine
011400931116     C*
011500920323     C** CMD3 - FINE LAVORO
011600041025     C   kc              goto      fine
011700071203      * verifica se utente autorizzato all'utilizzo del pgm
011800100118      *  da inizio gennaio 2010 utilizzando esclusivamente il programma che interroga
011900100118     C*  la tabella "AUT"  e non più chiodato:
012000071203     c                   setoff                                       66
012100100118     c*********          if        %subst(knmus:1:3) <> 'QUA'
012200071203     c                   exsr      autorizza
012300081106     c   66              goto      for01
012400100118     c*********          endif
012500931015     C* CONTROLLI FORMATO1
012600041025     C                   exsr      contr1
012700041025     C  nkf
012800041025     COR 90              goto      for01
012900041025     C*
013000041025     C     vi1pd1        ifeq      *blanks
013100041025     C                   movel     *zeros        vi1pd1
013200041025     C                   endif
013300041025     C     vi1pd2        ifeq      *blanks
013400041025     C                   movel     *zeros        vi1pd2
013500041025     C                   endif
013600041026     C                   movel     vi1p1         aut1
013700041025     C                   movel     vi1p2         aut2
013800030723     c                   move      w01dd1        data1
013900030723     c                   move      w01dd2        data2
014000041112     c                   move      vi1det        dett
014100041124     c                   move      'S'           valo
014200070828     c                   move      vi1pm         parmes
014300041025     C                   movel     param         kpjbu
014400931202     C*
014500041026     c                   if        *inkf
014600041117     C                   call      'FNLRR7R'
014700011115     C                   parm                    Kpjba
014800011115     c                   end
014900900611     C*
015000041025     C                   goto      inizio
015100041025     C     fine          tag
015200041025     C                   seton                                        lr
015300931111     C*
015400041025     C* --------------------------------------------------------------*
015500041025     C     pul01         begsr
015600041025     C* --------------------------------------------------------------*
015700041025     C* PULIZIA CAMPI FORMATO 1
015800950125     C***
015900041025     C* Vedo se sono SIMFEL O REMOTO
016000950125     C***
016100041026     C                   eval      Vi1PF1 = simpou
016200041026     C                   eval      Vi1PF2 = simpou
016300041025     C                   seton                                        89
016400041025     C                   time                    w0140            14 0
016500000203     C* UDATE IN GGMMAAAA
016600041025     C                   move      W0140         WDTGIO            8 0
016700041025     C                   move      WDTGIO        annoCO            4 0
016800041025     C     ANNOCO        ADD       1             annoPI            4 0
016900041025     C     ANNOCO        SUB       1             annoME            4 0
017000950125     C*
017100931111     C                   ENDSR
017200041025     C*----------------------------------------------------------------
017300041025     C     contr1        begsr
017400041025     C*----------------------------------------------------------------
017500041025     C*--- CONTROLLI FORMATO1
017600041104     C                   setoff                                       90
017700041104     C*
017800041104     C* D A T A   VIAGGI: DAL
017900041104     C                   move      vi1dd1        g02dat
018000041104     C                   movel     *blank        g02err
018100041104     C                   call      'XSRDA8'
018200041104     C                   parm                    wlbdat
018300041104    1C     g02err        ifeq      '1'
018400041104     C                   seton                                        43  90
018500041104     C                   goto      endct1
018600041025    1C                   end
018700041104     C                   z-add     g02inv        W01dd1
018800041104     C                   z-add     G02dat        VI1dd1
018900041104     C                   z-add     vi1dd1        an1               4 0
019000041104     C* CONTROLLO L'ANNO
019100041104     C                   z-add     g02dat        W0040             4 0
019200041025     C     W0040         ifgt      annopi
019300041104     C     W0040         orlt      annome
019400041104     C                   seton                                        43  90
019500041104     C                   GOTO      ENDCT1
019600041104    1C                   END
019700041104     C* DATA dal < DATA ultimapulizia archivi
019800041104    2C     W01DD1        IFLT      d§cradp
019900041104     C                   SETON                                        55  90
020000041104     C                   GOTO      ENDCT1
020100041104    2C                   ENDIF
020200931126     C*
020300041104     C* comunque imposta il 2 campo data
020400041104     C                   Z-ADD     w01dd1        w01dd2
020500041104    1C     vi1dd2        ifgt      0
020600041104     C                   move      vi1dd2        G02DAT
020700041104     C                   movel     *blank        G02ERR
020800041104     C                   CALL      'XSRDA8'
020900041104     C                   parm                    WLBDAT
021000041104    2C     G02ERR        ifeq      '1'
021100041104     C                   seton                                        44  90
021200041104     C                   goto      endct1
021300041104    2C                   END
021400931126     C                   Z-ADD     G02INV        W01DD2
021500041104     C                   Z-ADD     G02DAT        VI1DD2
021600041104     C                   z-add     vi1dd2        an2               4 0
021700041104     C* CONTROLLO L'ANNO
021800041104     C                   Z-ADD     G02DAT        W0040             4 0
021900041104     c     an1           ifne      an2
022000041104     C                   SETON                                        56  90
022100041104     C                   GOTO      ENDCT1
022200041104     c                   end
022300000203     C     W0040         IFGT      ANNOPI
022400041104     C     W0040         ORLT      ANNOME
022500041104     C                   SETON                                        44  90
022600041104     C                   GOTO      ENDCT1
022700041104    1C                   END
022800041104     C* DATA AL NO < DATA DAL
022900041104    2C     W01DD2        IFLT      W01DD1
023000041104     C                   SETON                                        45  90
023100041104     C                   GOTO      ENDCT1
023200041104    2C                   ENDIF
023300041104     C**
023400041104    1C                   ENDIF
023500041104     C*
023600100118     C******** specifiche asteriscate da tempo....
023700050505      * verifiche se cambiata la filiale
023800071203     c*                  setoff                                       5051
023900071203     c*                  if        vi1pf1 <> simpou and *in46
024000071203     c*    vi1pf1        lookup    lin                                    50
024100071203     c* n50              seton                                        51
024200071203     c*  51              goto      endct1
024300071203     c*                  endif
024400100118     C******** specifiche asteriscate
024500050505      *
024600050505     c                   movel     vi1pf1        vi1pf2
024700070828      * verifico se la filiale ha gestione parcel/messaggerie
024800070828     C     Vi1pf1        chain     azorg01l
024900070828    1C                   if        %found(azorg01l)
025000070828     C                   movel     orgdf0        og150
025100070828     c                   endif
025200070828     c     §ogpkm        comp      'S'                                    20
025300070828     c  n20              clear                   vi1pm
025400050505     c
025500041104     C* P A D R O N C I N O
025600041104     C* RICERCA
025700041104     C     '?'           SCAN      VI1PD1                                 90
025800041104     C*
025900041104    1C     *IN90         IFEQ      *ON
026000041104     C                   Z-ADD     VI1PF1        D24FIL
026100041104     C                   EXSR      RICPAD
026200041104     C*
026300041104     C     D24PDR        IFNE      0
026400041104     C                   MOVEL     D24PDR        VI1PF1
026500041104     C                   MOVEL     *ZEROS        VI1PD1
026600041104     C                   MOVE      D24PDR        VI1PD1
026700041104     C                   MOVEL     D24RSC        VI1DP1
026800041104     C                   ENDIF
026900041104     C                   GOTO      ENDCT1
027000041104    1C                   ENDIF
027100041104     C* PADRONCINO AL
027200041104     C* RICERCA
027300041104     C     '?'           SCAN      VI1PD2                                 90
027400041104     C*
027500041104    1C     *IN90         IFEQ      *ON
027600041104     C                   Z-ADD     VI1PF2        D24FIL
027700931126     C                   EXSR      RICPAD
027800931126     C*
027900021203     C     D24PDR        IFNE      0
028000021203     C                   MOVEL     D24PDR        VI1PF2
028100931126     C                   MOVEL     *ZEROS        VI1PD2
028200021203     C                   MOVE      D24PDR        VI1PD2
028300950125     C                   ENDIF
028400931126     C                   GOTO      ENDCT1
028500931126    1C                   ENDIF
028600931126     C*
028700931126     C*
028800931126    1C     VI1PD2        IFNE      *BLANKS
028900931126     C     VI1PD2        ANDNE     *ZEROS
029000931126     C                   TESTN                   VI1PD2               30
029100931126     C  N30              SETON                                        4790
029200931126     C                   ENDIF
029300931116     C*
029400041104    1C     VI1PD1        IFNE      *BLANKS
029500041104     C     VI1PD1        ANDNE     *ZEROS
029600041104     C                   TESTN                   VI1PD1               30
029700931126     C  N30              SETON                                        4190
029800041104     C*
029900041104     C* SE NE HO IMMESSO SOLTANTO UNO CONTROLLO L'ESISTENZ
030000041104    2C     VI1PD2        IFEQ      *BLANKS
030100041104     C     VI1PD2        OREQ      *ZEROS
030200041104     C*
030300041104     C     KAPDV         CHAIN     FIAPD000                           30
030400041104     C*
030500041104    3C     *IN30         IFEQ      *ON
030600041104     C* Tolto il controllo sul padroncino annullato
030700041104     C                   SETON                                        4190
030800041104     C                   GOTO      ENDCT1
030900931126    3C                   ENDIF
031000931129     C*
031100950125     C                   MOVEL     APDRSC        VI1DP1
031200931123     C*
031300931123   X2C                   ELSE
031400931126     C*
031500931123     C* PADRONCINO AL  <  MINORE PADRONCINO DAL
031600041104    3C     VI1P2         IFLT      VI1P1
031700041104     C                   SETON                                        4290
031800041104     C                   GOTO      ENDCT1
031900041104    3C                   ENDIF
032000041104    2C                   ENDIF
032100041104    1C                   ENDIF
032200041104     C*
032300010906     C* PADRONCINO AL E PADRONCINO DAL A ZERO
032400041104     C     VI1PD1        IFEQ      *ZEROS
032500041104     C     VI1PD2        ANDEQ     *ZEROS
032600041104     C                   SETON                                        4090
032700041104     C                   GOTO      ENDCT1
032800041104     C                   ENDIF
032900910320     C*
033000900614     C     ENDCT1        ENDSR
033100931126     C*
033200041025     C* --------------------------------------------------------------*
033300041025     C     ricpad        begsr
033400041025     C* --------------------------------------------------------------*
033500041025     C* RICERCA CODICE PADRONCINO
033600041025     C                   movel     *blanks       d24rsc
033700041025     C                   z-add     0             d24pdr
033800041025     C                   movel     'R'           d24flg
033900041025     C                   movel     'A'           d24tip
034000041025     C                   movel(P)  fnlv24ds      kpjbu
034100041025     C                   call      'FNLV24R'
034200041025     C                   parm                    kpjba
034300041025     C                   movel     kpjbu         fnlv24ds
034400041025     C                   endsr
034500050512     C* --------------------------------------------------------------*
034600050512     C     autorizza     begsr
034700050512     C* --------------------------------------------------------------*
034800131008     c                   clear                   tibs42ds
034900131008     c                   movel     vi1pf1        i42pge
035100131008     c                   call      'TIBS42R'
035200131008     c                   parm                    tibs42ds
035300131008     c                   movel     o42uni        uteautds
035400050512      * se non abilitato con *in66 segnalo la mancata autorizzazione
035500050512     c                   if        §rieabi <> 'S'
035600050512     c                   seton                                        6690
035700050512     c                   endif
035800050512     c                   endsr

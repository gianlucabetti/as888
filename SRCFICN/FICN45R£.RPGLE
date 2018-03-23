000100021111     H DFTACTGRP(*NO) BNDDIR('PJXBND')
000200000000     H DECEDIT('0,') DATEDIT(*DMY.)
000300021108     H* ficn45R *----------------------------------------------------*
000400000000     H*-----------------*                                            *
000500021108     H*       Rettifiche Conteggi Autotrasportatori in Sede
000600000000     H*--------------------------------------------------------------*
000700021108     Fficn45D   CF   E             WORKSTN
000800021108      *
000900021113     F**AnFrn01l  if   E           K DISK
001000021113     F**AnRco01l  if   E           K DISK
001100021113     F**AnSog01l  if   E           K DISK
001200021113      *
001300021111     FFiFtt06l  uf   E           K DISK
001400021111     FFiFre00F  o    E             DISK
001500021111     FTnTbe01l  if   E           K DISK
001600021108      *
001700021108     D soc001        E DS                  EXTNAME(xsoc001ds)
001800021108     D xsocds          DS          1000
001900021111     D dblc          E DS
002000021108      *
002100021112     D KPJBA         E DS
002200021112     D RET_SEDE        C                   CONST('Rett. di sede su Forn.:')
002300930811     C*---------------------------------------------------------------*
002400021108     c     inizio        tag
002500021108     C                   setoff                                       9966
002600021108     c                   clear                   vsoc
002700021111     c                   clear                   vfrn
002800021108     c                   clear                   vimp
002900021113     c                   z-add     datmes        vmes
003000021113     c                   z-add     datann        vann
003100021111     c                   z-add     §blcmax       maxret
003200930907     C*
003300021108     C     for01         TAG
003400021108     C                   EXFMT     CN45D01
003500021108     C*
003600021108     C** CMD3  - FINE LAVORO
003700021108     C   KC              goto      fine
003800021108     C*
003900021108     C* su doppia conferma esegue la rettifica
004000021108     c     *inkf         ifeq      *on
004100021108     c     *in66         andeq     *on
004200021113     c                   if        vimp <> 0
004300021108     c                   exsr      esegui
004400021113     c                   end
004500021108     c                   goto      inizio
004600021108     c                   end
004700021108     C*
004800021108     C                   setoff                                       9966
004900021108     C*
005000021108     C* controlli sui campi a video
005100021108     C                   EXSR      Contr1
005200021108     c  nkf
005300021108     Cor 99              GOTO      for01
005400930811     C*
005500021111     c   kf*in66         cabeq     *off          for01                    66
005600021108     c
005700021108     C*
005800021108     c     fine          tag
005900021108     C                   SETON                                        LR
006000021108     C*---------------------------------------------------------------*
006100021108      *    CONTROLLI
006200021108     C*---------------------------------------------------------------*
006300021108     C     Contr1        BEGSR
006400021108     C*
006500021108     C* Controlla Società
006600021111     C     Vsoc          Ifeq      *blank
006700021111     C                   SETON                                        0199
006800950113     C                   GOTO      ENDCT1
006900021111     C                   Else
007000021111     c     vsoc          chain(n)  fiftt06l                           21
007100021111     c     *in21         ifeq      *on
007200021111     C                   SETON                                        0199
007300021111     C                   GOTO      ENDCT1
007400021111     C                   Endif
007500021111     C                   Endif
007600021108     C*
007700021108     C* Controlla Fornitore
007800021111     c                   clear                   vfrnd
007900021111     C     Vfrn          Ifeq      *blank
008000021111     C                   SETON                                        0299
008100021111     C                   GOTO      ENDCT1
008200021111     C                   Else
008300021113     C****               move      vfrn          kksc
008400021113     c****               movel     *blank        sogdes
008500021113     C**** Kfrn          CHAIN     anfrn01l                           21
008600021113     C**N21Krco          CHAIN     anrco01l                           21
008700021113     C**N21rcosogg       CHAIN     ansog01l                           21
008800021113     C****               MOVEL(p)  sogdes        vfrnd
008900021113     C**** *IN21         IFEQ      '1'
009000021113     C****               SETON                                        0299
009100021113     C****               GOTO      ENDCT1
009200021113     C****               END
009300021113     C****               if        rcoctgan02 <> 'N   ' and
009400021113     C****                         rcoctgan02 <> 'I   ' and
009500021113     C****                         rcoctgan02 <> 'M   '
009600021113     C****               SETON                                        0299
009700021113     C****               GOTO      ENDCT1
009800021113     c****               end
009900021113      ****
010000021111     c     kftt6_2       chain(n)  fiftt06l                           21
010100021111     c     *in21         ifeq      *on
010200021113     C                   SETON                                        0299
010300021111     C                   GOTO      ENDCT1
010400021111     C                   Endif
010500021111     C                   Endif
010600021111     C*
010700021111     C* Controlla  Anno / Mese
010800021111     c     vann          ifgt      datann
010900021111     C                   SETON                                        0399
011000021111     C                   GOTO      ENDCT1
011100021111     C                   Endif
011200021111     c     vmes          ifgt      datmes
011300021111     C                   SETON                                        0499
011400021111     C                   GOTO      ENDCT1
011500021111     C                   Endif
011600021111     C*
011700021108     C* Calcola VIMP
011800021111     c                   clear                   vimp
011900021112     c                   clear                   savddc
012000021112     c                   clear                   savpdr
012100021111     c     kftt6         chain(n)  fiftt06l                           21
012200021111     c     *in21         doweq     *off
012300021111     C*
012400021111     C* imposta l'anno e il mese
012500021111     C                   movel     fttddc        fld6              6 0
012600021111     C     fld6          div       100           fttann            4 0
012700021111     c                   z-add     fld6          fttmes            2 0
012800021111     C*
012901030217     c     Fttnff        ifeq      0
013000021111     c     vann          andeq     fttann
013100021111     c     vmes          andeq     fttmes
013200021112     c                   add       fttitt        vimp
013300021112     c                   add       fttita        vimp
013400021111     c                   add       ftttpe        vimp
013500021111     c                   add       ftttbn        vimp
013600021111     c                   add       ftttim        vimp
013700021111     c                   add       fttmnt        vimp
013800021111      * salva l'ultima data valida del mese
013900021111     c                   if        fttddc > savddc
014000021111     c                   z-add     fttddc        savddc
014100021111     c                   z-add     fttpdr        savpdr
014200021111     c                   end
014300021111     c                   end
014400021111     C*
014500021111     c     kftt6         reade(n)  fiftt06l                               21
014600021111     c                   enddo
014700021111     C*
014800021111     C*  se l'importo da rettificare supera la soglia impostata in tabella BLC
014900021111     c   kf              if        vimp > §BLCMAX
015000021111     C                   SETON                                        0699
015100021111     C                   GOTO      ENDCT1
015200021111     c                   end
015300930908     C*
015400930811     C     ENDCT1        ENDSR
015500021108     C*---------------------------------------------------------------*
015600021108     C*  esegue la rettifica dopo doppia convalida F6
015700021108     C*---------------------------------------------------------------*
015800021108     c     esegui        begsr
015900021108     C*
016000021108     C*  rettifica un padroncino come fornitore
016100021112     c                   move      savpdr        frepdr
016200021108     c                   z-add     savddc        freddc
016300021108     c                   movel     savcau        frecre
016400021108     c                   z-add     1             frepno
016500021108     c                   movel     RET_SEDE      frenot
016600021112     c                   move      vfrn          frenot
016700021112     c                   z-sub     vimp          fretim
016800021108     c                   z-add     0             fredds
016801030123     c                   move      'A'           fretip
016900021108     c                   write     fifre000
017000021108     C*
017100021111     C*  quindi modificare l'ultimo record valido del periodo
017200021111     c     kftt6_Upd     chain     fiftt06l
017300021111     c                   if        %found(fiftt06l)
017400021112     c                   sub       vimp          ftttim
017500021111     c                   update    fiftt000
017600021111     C                   End
017700021111     C*
017800021108     C                   Endsr
017900021108     C*---------------------------------------------------------------*
018000021108     c     *inzsr        begsr
018100021108     C*---------------------------------------------------------------*
018200020125     C     *ENTRY        PLIST
018300020125     C                   PARM                    KPJBA
018400021111      *
018500021111     c     *like         define    fttddc        savddc
018600021111     c     *like         define    fttpdr        savpdr
018700021108      *
018800021108     C*---------- RICERCA DITTA :
018900021111     C                   MOVEL     'SOC001'      TIPXSC
019000021111     C                   MOVEL     *BLANK        SOCXSC
019100021111     C                   EXSR      REPSOC
019200021111     C     RTNXSC        IFNE      '1'
019300021111     C                   MOVEL     XSOCDS        SOC001
019400021111     C                   MOVEL     xscrgs        RSUT             20
019500021111     c                   else
019600021111     c                   goto      fine
019700021111     c                   end
019800021111     C* Richiamo XCAPCLIFOR per il reperimento del capoconto fornitori
019900021111     C                   movel     'F     '      $kcc              6
020000021111     C                   movel     *blanks       $ksc              8
020100021111     C                   callb     'XCAPCLIFOR'
020200021111     C                   parm                    xscsoc
020300021111     C                   parm                    $kcc
020400021111     C                   parm                    $ksc
020500021111     C                   movel     $kcc          forita            6
020600021111      *  lettura tabella BLC
020700021111     c                   clear                   dblc
020800021111     c                   movel     'BLC'         tbecod
020900021111     c                   movel(p)  '1'           tbeke1
021000021111     c                   clear                   tbeke2
021100021111     c     ktbe          chain     tntbe01l
021200021111     c                   if        %found(tntbe01l)
021300021111     c                   movel     tbeuni        dblc
021400021111     c                   movel     §blcCAU       savcau            3
021500021111      * data protocollo
021600021111     C                   movel     §blcDTP       fld6              6 0
021700021111     C     fld6          div       100           datann            4 0
021800021111     c                   z-add     fld6          datmes            2 0
021900021111     c                   end
022000020125     C*
022100021111     c     kftt6         klist
022200021111     c                   kfld                    vsoc
022300021111     c                   kfld                    vfrn
022400021111     c                   kfld                    ktsr              1
022500021111     c                   kfld                    kfvl              1
022600021111     c                   move      *blank        ktsr
022700021111     c                   move      'C'           kfvl
022800021111     C*
022900021111     c     kftt6_2       klist
023000021111     c                   kfld                    vsoc
023100021111     c                   kfld                    vfrn
023200021111     C*
023300021111     c     kftt6_Upd     klist
023400021111     c                   kfld                    vsoc
023500021111     c                   kfld                    vfrn
023600021111     c                   kfld                    ktsr
023700021111     c                   kfld                    kfvl
023800021111     c                   kfld                    savpdr
023900021111     c                   kfld                    savddc
024000021111     C*
024100021111     C     Kfrn          KLIST
024200021111     C                   KFLD                    xscsoc
024300021111     C                   KFLD                    kksc
024400021111     C*
024500021111     C     Krco          KLIST
024600021111     C                   KFLD                    xscsoc
024700021111     C                   KFLD                    forita
024800021111     C                   KFLD                    KKSC              8
024900021111     C*
025000021111     C     Ktbe          KLIST
025100021111     C                   KFLD                    tbecod
025200021111     C                   KFLD                    tbeke1
025300021111     C                   KFLD                    tbeke2
025400021111     C*
025500020125     c                   endsr
025600021108     C*---------------------------------------------------------------*
025700021108     C* Reperimento dati società
025800021108     C*---------------------------------------------------------------*
025900021108     C     REPSOC        BEGSR
026000021108     C*
026100021111     C                   CALLb     'XSOC'
026200021108     C                   PARM                    TIPXSC            6
026300021108     C                   PARM                    SOCXSC            3
026400021108     C                   PARM                    CDSXSC            9 0
026500021108     C                   PARM                    MODXSC            3
026600021108     C                   PARM      *blanks       RTNXSC            1
026700021108     C                   PARM                    XSOCDS
026800021108     C                   PARM                    KPJBA
026900021108     C*
027000021108     C                   ENDSR

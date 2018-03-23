000100990614     h DECEDIT('0,') DATEDIT(*DMY.)
000200980323     H*------------------------------------------------------------------------*
000300031105     H*  Stampa tabulato delta clienti                                         *
000400980323     H*------------------------------------------------------------------------*
000500980323     F*
000600041126     FWfaut01l  if   e           k DISK    rename(wfaut000:wfaut01) prefix(C_)
000700031105     FWfaut02l  if   e           k DISK
000800031113     Ftavr25p   o    e             printer oflind(*in90) usropn infds(prttab)
000900990614      *
001000071102     D tabul1          C                   CONST('CLIENTI CON DELTA POSITIVO O-
001100071102     D                                      NEGATIVO FINO A -2% E ISTAT NO')
001200071102     D tabul2          C                   CONST('CLIENTI CON DELTA DA -2% A --
001300071102     D                                     5% ISTAT NO')
001400071102     D tabul3          C                   CONST('CLIENTI CON DELTA POSITIVO O-
001500071102     D                                      NEGATIVO FINO A -5% ISTAT SI E NO')
001600051103     D tabul4          C                   CONST('CLIENTI CON DELTA INFERIORE -
001700071030     D                                     A  -5%')
001800031105      *
001900031105     D tibs34ds      e ds                  inz
002000031105     D ddatiute      e ds
002100031105     D azuteds       e ds                  extname(AZUTE00F)
002200031105     D kpjba         e ds
002300031105      * Parametri di selezione tabulati
002400031105     D Partab          ds
002500031105     D tab1                           2
002600031105     D tab2                           2
002700041115     D tab3                           2
002800041117     D tab4                           2
002900031113      *
003000031113     D PRTTAB          DS
003100031113     D  curline              367    368I 0
003200031113      *
003300031103      *------------------------------------------------------------------------*
003400041126     D Kaut            s                   like(wfaftb)
003500031110     D area            s                   like(wfaare)
003600031110     D distretto       s                   like(wfadiv)
003700031110     D filiale         s                   like(wfafil)
003800041126     D tipo            s                   like(wfatcl) inz('C')
003900031103
004000031103      *------------------------------------------------------------------------*
004100031105      * tabulato numero 1
004200041126      *------------------------------------------------------------------------*
004300031105     c                   if        tab1 = 'SI'
004400071102     c                   eval      kaut = '2'
004500041126      * preparo la testata
004600041126     c                   movel(p)  tabul1        testa1
004601071102      * VALORIZZO IL NUMERO DELL'ELENCO IN BASE ALLA DV DELL'AUMENTO
004602071102     c                   eval      numero = 2
004700041126      * gestione stampa
004800041126     c                   exsr      STP_TABULA
004900031105     c                   endif
005000041126      *------------------------------------------------------------------------*
005100031105      * tabulato numero 2
005200041126      *------------------------------------------------------------------------*
005300031105     c                   if        tab2 = 'SI'
005400071102     c                   eval      kaut = '4'
005500031105      * preparo la testata
005600041126     c                   movel(p)  tabul2        testa1
005601071102      * VALORIZZO IL NUMERO DELL'ELENCO IN BASE ALLA DV DELL'AUMENTO
005602071102     c                   eval      numero = 4
005700041126      * gestione stampa
005800041126     c                   exsr      STP_TABULA
005900031105     c                   endif
006000031105     c
006100041126      *------------------------------------------------------------------------*
006200041115      * tabulato numero 3
006300041126      *------------------------------------------------------------------------*
006400041115     c                   if        tab3 = 'SI'
006500071102     c                   eval      kaut = '5'
006600041115      * preparo la testata
006700051103     c                   movel(p)  tabul3        testa1
006701071102      * VALORIZZO IL NUMERO DELL'ELENCO IN BASE ALLA DV DELL'AUMENTO
006702071102     c                   eval      numero = 5
006800041126      * gestione stampa
006900041126     c                   exsr      STP_TABULA
007000041115     c                   endif
007100041126      *------------------------------------------------------------------------*
007200041117      * tabulato numero 4
007300041126      *------------------------------------------------------------------------*
007400041117     c                   if        tab4 = 'SI'
007500071102     c                   eval      kaut = '6'
007600041117      * preparo la testata
007700051103     c                   movel(p)  tabul4        testa1
007701071102      * VALORIZZO IL NUMERO DELL'ELENCO IN BASE ALLA DV DELL'AUMENTO
007702071102     c                   eval      numero = 6
007800041126      * gestione stampa
007900041126     c                   exsr      STP_TABULA
008000041117     c                   endif
008100041126      *
008200031103     c                   seton                                        lr
008300041126      *
008400041126      *****************************************************************
008500041126      * ROUTINE stampa TABUALTI
008600041126      *****************************************************************
008700041126     C     STP_TABULA    BEGSR
008800041126
008900041126      * pulisco alcuni campi
009000041126     c                   clear                   agente
009100041126     c                   clear                   area
009200041126     c                   clear                   distretto
009300041126     c                   clear                   filiale
009400041126     c                   clear                   desage
009500041126
009600041126     c                   open      tavr25p
009700041126     c     kaut          setll     WFaut02l
009800041126      * leggo
009900041126     c                   do        *hival
010000041126      *
010100041126     c     kaut          reade     WFaut02l
010200041126      *
010300041126     c                   if        %eof(WFaut02l)
010400041126     c                   write     ta25e2
010500041126     c                   leave
010600041126     c                   endif
010700041126      *
010800041126      * vedo se rottura p.o. /distretto/area/agente
010900041126      *
011000041126     c                   if        area <> wfaare or distretto <> wfadiv or
011100041126     c                             filiale <> wfafil or agente <> wfaage
011200041126     c                   eval      agente =  wfaage
011300041126     c                   eval      desage =  wfaagd
011400041126     c                   eval      area   =  wfaare
011500041126     c                   eval      distretto =  wfadiv
011600041126     c                   eval      filiale =  wfafil
011700041126     c                   write     ta25t1
011800041126     c                   setoff                                       90
011900041126     c                   endif
012000041126      *
012100041126      * dettaglio
012200041126      *
012300041126      * ragione sociale  cliente
012400041126     c                   movel     wfarag        prtrag
012500041126      * controllo se of
012600041126     c                   If        *in90
012700041126     c                   write     ta25t1
012800041126     c                   eval      *in90=*off
012900041126     c                   endif
013000041126      *
013100041126     c                   write     ta25d1
013200041126      * elaboro stampa dei figli
013300041126     c                   exsr      stp_figli
013400041126      *
013500041126     c                   enddo
013600041126      *
013700041126     c                   close     tavr25p
013800041126      *
013900041126     c                   ENDSR
014000031103      *
014100041126      *****************************************************************
014200041126      * ROUTINE stampa figli
014300041126      *****************************************************************
014400041126     C     STP_figli     BEGSR
014500041126      *
014600041126     c     Kaut01        setll     wfaut01l
014700041126      *
014800041126     c                   do        *hival
014900041126      *
015000041126     c     kaut01        reade     wfaut01l
015100041126      *
015200041126     c                   if        %eof(wfaut01l)
015300041126     c                   leave
015400041126     c                   endif
015500041126      * se figlio uguale ad unificante non stmpo
015600041126     c                   If        C_wfaclf = C_wfaclu
015700041126     c                   iter
015800041126     c                   endif
015900041126      * se lo stampo vewrifico se bloccato
016000041126     c                   If        C_wfafbl = 'S'
016100041126     c                   seton                                        10
016200041126     c                   else
016300041126     c                   setoff                                       10
016400041126     c                   endif
016500051103      * verifico ISTAT solo per i tabulati 3 e 4
016600051103     c                   clear                   C_ISTAT
016700051103
016800051103     c                   If        Kaut = '3' or Kaut = '4'
016900051103     c                   If        C_wfafis = 'N'
017000051103     c                   eval      C_ISTAT = 'ISTAT=NO'
017100051103     c                   endif
017200051103
017300051103     c                   If        C_wfafis = 'S'
017400051103     c                   eval      C_ISTAT = 'ISTAT=SI'
017500051103     c                   endif
017600051103      * misto
017700051103     c                   If        C_wfafis = 'M'
017800051103     c                   eval      C_ISTAT = 'ISTAT=SI-NO'
017900051103     c                   endif
018000051103
018100051103     c                   endif
018200051103
018300051103      * imposto il campo della ragione sociale
018400051103     c                   movel(p)  C_wfarag      S_wfarag
018500041126      * controllo se of
018600041126     c                   If        *in90
018700041126     c                   write     ta25t1
018800041126     c                   eval      *in90=*off
018900041126     c                   endif
019000041126      * stampo dettaglio figli
019100041126     c                   write     ta25d2
019200041126      *
019300041126     c                   enddo
019400041126      *
019500041126     c                   ENDSR
019600041126      *
019700031103      *****************************************************************
019800031103      * ROUTINE INIZIALE
019900031103      *****************************************************************
020000031103     C     *INZSR        BEGSR
020100031105      *
020200031105     C     *ENTRY        PLIST
020300031105     C                   PARM                    KPJBA
020400031105     c                   movel     kpjbu         partab
020500031105      *
020600031105     c     *dtaara       define    §azute        azuteds
020700031105     c     *dtaara       define    §datiute      ddatiute
020800031105     C                   in(E)     *dtaara
020900031105     C                   IF        %error  or RSUT = *blanks
021000031105     C                   CLEAR                   tibs34ds
021100031105     C                   CALL      'TIBS34R'
021200031105     C                   PARM                    tibs34ds
021300031105     C                   in        *dtaara
021400031105     c                   endif
021500031103      *
021600031110     C                   TIME                    WTIME            14 0
021700031110     C                   MOVE      WTIME         WDATE             8 0
021800031110     C                   MOVEL     WTIME         UTIME             6 0
021900041126      *
022000041126     c     Kaut01        Klist
022100041126     c                   Kfld                    agente
022200041126     c                   Kfld                    tipo
022300041126     c                   kfld                    WFAclu
022400031103
022500031103     C                   endsr

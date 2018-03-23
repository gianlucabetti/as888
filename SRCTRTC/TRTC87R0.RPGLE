000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200100513     H* FICND1R0 *---------------------------------------------------*
000300030409     H*   Controllo trasmissione dati in sede Conteggi Autotrasportatori
000400000000     H*--------------------------------------------------------------*
000500100621     FTRTC87D0  CF   E             WORKSTN usropn
000600100513     Ffiwalc2L  IF   E           K DISK
000700030418      *--------------------------------------------------------------*
000800030418     D SndMg01         S             10                                         Message type
000900030418     D                                     INZ('*INFO')
001000030418     D SndMg02         S             10                                         Deluvery mode
001100030418     D                                     INZ('*BREAK')
001200030418     D SndMg03         S            256                                         Message text
001300030418     D SndMg04         S             10I 0                                      Length of text
001400030418     D                                     INZ(%SIZE(SndMg03))
001500030418     D SndMg05         S             10                                         User profile
001600030418     D                                     DIM(299)
001700030418     D SndMg06         S             10I 0                                      Number of user
001800030418     D SndMg07         S             10I 0                                      Message sent indic.
001900030418     D SndMg08         S             10I 0                                      Function requested
002000030418     D SndMg10         S              1                                         Show display
002100030418     D                                     INZ('N')
002200030418     D SndMg11         S             20                                         Qualified MSGQ name
002300030418     D SndMg12         S              4                                         Name type indicator
002400030418     D                                     INZ('*USR')
002500030418      * indice di scihera
002600030418     D X               S              5I 0
002700030418      *
002800100513     D savFLD          S                   like(walFLD)
002900100513      *
003000030418     D/COPY QSYSINC/QRPGLESRC,QUSEC
003100030418      *
003200100513     D* PASSAGGIO parametri di CONTROLLO
003300100621     D trtc87ds0     e DS
003400100513     d  COD_TEStare           11     30
003500931129     D*
003600931129     D KPJBA         E DS
003700900615     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
003800030409     D CNCR80        E DS
003900030409     D*
004000030409     D WLBDAT          DS
004100030409     D  G02DAT                 1      8  0
004200030409     D  G02INV                 9     16  0
004300030409     D  G02ERR                17     17
004400030409     D  G02TGI                18     22  0
004500030409     D WGIDAT          DS
004600030409     D  GIODAT                 1      8  0
004700030409     D  GIOINV                 9     16  0
004800030409     D  GIOTGI                17     21  0
004900030528      *
005000030528     D  DSMSG          DS
005100030528     D  MSusr                 10     19
005200030528      *
005300030528     D  DSMST          DS
005400030528     D  MTusr                 50     59
005500030528      *
005600100513     D Avvisa          C                   CONST('UN altro utente sta utilizzan-
005700100513     D                                     do la PROCEDURA. NON è ammesso PROCE-
005800100513     D                                     DERE oltre.')
005900100513     D Avvisa2         C                   CONST('SOLLECITARE la chiusura -
006000100513     D                                     della Procedura già in uso.')
006100900521     C****************************************************************
006200900521     C*  RIEPILOGO INDICATORI
006300900521     C*****************************************************************
006400000000     C     *ENTRY        PLIST
006500000000     C                   PARM                    KPJBA
006600100621     c                   movel     kpjbu         Trtc87DS0
006700030428      *
006800030428     C                   TIME                    W0140            14 0
006900030428     C                   movel     W0140         Uore              2 0
007000030409      *
007100030409      *  quindi richiamato interattivamente
007200100621     c                   if        FN0PGM = 'TRTC87R'
007300100621     c                   open      TRTC87D0
007400030409     c                   endIf
007500030409      *
007600000000     C                   Z-ADD     1             CODUT
007700900615     C                   CALL      'X§PARUT'
007800900615     C                   PARM                    UT§DSE
007900000000     C                   MOVEL     RAGUT         RSUT             20
008000900615     C                   MOVEL     REC80         CNCR80
008100900511     C*---------------------------------------------------------------*
008200100513     c     kALC          klist
008300100513     c                   kfld                    FN0pgm
008400100513     c                   kfld                    savFLD
008500100513      *
008600100513     c                   eval      savFLD = COD_TEStare
008700100513      *
008800030418      * pulisce il flag di ritorno
008900100513     c                   clear                   FN0FLG
009000030418      *
009100030418     C*  Legge i records presenti
009200100513     c     kalc          setll     fiwalc2l
009300100513     c     kalc          reade     fiwalc2l
009400100513     c                   dow       not %eof(fiwalc2l)
009500030930     C*
009600030930      * chiama API per avere conferma se il lavoro è attivo altrimenti
009700030930      * deve andare oltre e non controllare ulteriormente
009800030930     c                   movel     walnoj        nomeJ            10
009900030930     c                   movel     walnus        userJ            10
010000030930     c                   move      walnrj        numeJ             6
010100030930     c                   clear                   StatoJ           10
010200030930     c                   call      'FICNB8C'
010300030930     c                   parm                    nomeJ
010400030930     c                   parm                    userJ
010500030930     c                   parm                    numeJ
010600030930     c                   parm                    StatoJ
010700030930     C*
010800030930     C* Va oltre se lo stato del lavoro non è attivo:
010900030930     c                   if        StatoJ =  '*ACTIVE   '
011000100513     c                             and
011100100513     c                               ( KNMUS <> walNUS or (
011200100513     c                                 KNMUS  = walNUS and KNRJO <> walNRJ) )
011300030418     C*
011400030418     C* in atto - segnala a video che non è possibile accedere al programma
011500030418     C* e esce dal ciclo.
011600030418     c                   eval      daOre  = WalORC
011700030428     C                   Z-ADD     WalDAC        G02inv
011800030428     C                   MOVE      *ZEROS        G02DAT
011900030428     C                   MOVE      '3'           G02ERR
012000030428     C                   CALL      'XSRDA8'
012100030428     C                   PARM                    WLBDAT
012200030428     c                   eval      inData = g02dat
012300030428      * controlla se da troppo tempo è in trasmissione
012400030428      *  potrebbe essere successo qualcosa
012500030428     c                   movel     daOre         ORE               2 0
012600030428     c                   clear                   msgvid
012700030428     c                   clear                   msgvd2
012800100513     c                   if        Uore < ore or (Uore - ore) > 1
012900030428     c                   movel     Avvisa        msgvid
013000030428     c                   movel     Avvisa2       msgvd2
013100030428     c                   end
013200100513     c                   eval      vUSER = walNUS
013300100513     C*
013400030418     C* Sta trasmettendo quindi Messaggio a Video e chiusura pgm
013500100513     C                   EXFMT     FNB0D01
013600030418     C*
013700030418     c*  Se qualcuno è dentro alla Conferma durante l'orario di trasmissione:
013800030418     C*   Invia un messaggio al lavoro per far chiudere la Conferma conteggi
013900030418     C*    e poi ritenta di eseguire la trasmissione dopo tot.secondi
014000030418     c                   z-add     1             x
014100030418     C                   exsr      CalQEZSNDMG
014200100513     c                   eval      FN0FLG = 'N'
014300030418     C*
014400100513     c                   leave
014500060119     c                   else
014600060119      * ?Se non è attivo :
014700030930     c                   endif
014800030930     C*
014900100513     c     kalc          reade     fiwalc2l
015000030409     c                   enddo
015100940610     C*
015200000000     C     FINE          TAG
015300100621     C                   MOVEL     trtc87DS0     KPJBU
015400030409     C*
015500931027     C                   SETON                                        LR
015600030418     ***********************************************************************
015700030418     **
015800030418     ** Send Message (QEZSNDMG) API
015900030418     **
016000030418     ***********************************************************************
016100030418     C     CalQEZSNDMG   BEGSR
016200030418      *
016300030418     ** Invio messaggio all'utente.
016400100513     C                   EVAL      SndMg03 = 'ATTENZIONE:  NON è possibile util-
016500100513     C                             izzare contemporaneamente da più utenti la s-
016600100621     C                             tessa procedura di Invio Date ATR. Non è pos-
016700100621     C                             sibile andare oltre. ATTENZIONE'
016800100513     C                   EVAL      SndMg05(x) = KNMUS
016900030418     C                   EVAL      SndMg06 = x
017000030418     C                   CLEAR                   QUSEC
017100030418     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
017200030418
017300030418     C                   CALL      'QEZSNDMG'
017400030418     C                   PARM                    SndMg01
017500030418     C                   PARM                    SndMg02
017600030418     C                   PARM                    SndMg03
017700030418     C                   PARM                    SndMg04
017800030418     C                   PARM                    SndMg05
017900030418     C                   PARM                    SndMg06
018000030418     C                   PARM                    SndMg07
018100030418     C                   PARM                    SndMg08
018200030418     C                   PARM                    Qusec
018300030418     C                   PARM                    SndMg10
018400030418     C                   PARM                    SndMg11
018500030418     C                   PARM                    SndMg12
018600030528      *
018700030418     C                   ENDSR
018800030528      **********************************************************************
018900030418
019000030418     ***********************************************************************

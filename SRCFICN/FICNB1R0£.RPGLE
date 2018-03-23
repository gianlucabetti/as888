000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200030409     H* FICND8R *----------------------------------------------------*
000300030409     H*   Controllo trasmissione dati in sede Conteggi Autotrasportatori
000400000000     H*--------------------------------------------------------------*
000500030409     FFICNB8D   CF   E             WORKSTN usropn
000600030409     Ffiwalc1L  IF   E           K DISK
000700030418      *--------------------------------------------------------------*
000800030528     D MSGT            S             70    DIM(1) CTDATA PERRCD(1)              MSG PER SEDE
000900030528     D MSG             S             66    DIM(3) CTDATA PERRCD(1)              MSG PER SEDE
001000030528      *
001100030418     D SndMg01         S             10                                         Message type
001200030418     D                                     INZ('*INFO')
001300030418     D SndMg02         S             10                                         Deluvery mode
001400030418     D                                     INZ('*BREAK')
001500030418     D SndMg03         S            256                                         Message text
001600030418     D SndMg04         S             10I 0                                      Length of text
001700030418     D                                     INZ(%SIZE(SndMg03))
001800030418     D SndMg05         S             10                                         User profile
001900030418     D                                     DIM(299)
002000030418     D SndMg06         S             10I 0                                      Number of user
002100030418     D SndMg07         S             10I 0                                      Message sent indic.
002200030418     D SndMg08         S             10I 0                                      Function requested
002300030418     D SndMg10         S              1                                         Show display
002400030418     D                                     INZ('N')
002500030418     D SndMg11         S             20                                         Qualified MSGQ name
002600030418     D SndMg12         S              4                                         Name type indicator
002700030418     D                                     INZ('*USR')
002800030418      * indice di scihera
002900030418     D X               S              5I 0
003000030418      *
003100030418     D/COPY QSYSINC/QRPGLESRC,QUSEC
003200030418      *
003300030409     D* PASSAGGIO DATI
003400030428     D FICNB8ds      e DS
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
005600030428     D Avvisa          C                   CONST('Attenzione: avvisare il CED d-
005700030428     D                                     i Sede. Tempo di trasmissione Dati -
005800030428     D                                     eccessivo.')
005900030428     D Avvisa2         C                   CONST('Probabili problemi di trasmis-
006000030428     D                                     sione o di allocazione archivi.')
006100900521     C****************************************************************
006200900521     C*  RIEPILOGO INDICATORI
006300900521     C*****************************************************************
006400000000     C     *ENTRY        PLIST
006500000000     C                   PARM                    KPJBA
006600030428     c                   movel     kpjbu         Ficnb8DS
006700030428      *
006800030428     C                   TIME                    W0140            14 0
006900030428     C                   movel     W0140         Uore              2 0
007000030409      *
007100030409      *  quindi richiamato interattivamente
007200030428     c                   if        FN8PGM <> 'FICN43R'
007300030409     c                   open      FICNB8D
007400030409     c                   endIf
007500030409      *
007600000000     C                   Z-ADD     1             CODUT
007700900615     C                   CALL      'X§PARUT'
007800900615     C                   PARM                    UT§DSE
007900000000     C                   MOVEL     RAGUT         RSUT             20
008000900615     C                   MOVEL     REC80         CNCR80
008100900511     C*---------------------------------------------------------------*
008200030418      * pulisce il flag di ritorno
008300030428     c                   clear                   FN8FLG
008400030418      *
008500030418     C*  Legge i records presenti
008600030418     c                   read      fiwalc1l
008700030418     c                   dow       not %eof(fiwalc1l)
008800030930     C*
008900030930      * chiama API per avere conferma se il lavoro è attivo altrimenti
009000030930      * deve andare oltre e non controllare ulteriormente
009100030930     c                   movel     walnoj        nomeJ            10
009200030930     c                   movel     walnus        userJ            10
009300030930     c                   move      walnrj        numeJ             6
009400030930     c                   clear                   StatoJ           10
009500030930     c                   call      'FICNB8C'
009600030930     c                   parm                    nomeJ
009700030930     c                   parm                    userJ
009800030930     c                   parm                    numeJ
009900030930     c                   parm                    StatoJ
010000030930     C*
010100030930     C* Va oltre se lo stato del lavoro non è attivo:
010200030930     c                   if        StatoJ =  '*ACTIVE   '
010300030418     C*
010400030418     C* Se chiamato dalla Conferma conteggi e c'è la trasmissione
010500030418     C* in atto - segnala a video che non è possibile accedere al programma
010600030418     C* e esce dal ciclo.
010700030428     c                   if        WALPGM = 'FICN43R   '
010800030428     c                   if        FN8PGM = 'FICNB3R   ' or
010900030428     c                             FN8PGM = 'FICN40R   '
011000030418     c                   eval      daOre  = WalORC
011100030428     C                   Z-ADD     WalDAC        G02inv
011200030428     C                   MOVE      *ZEROS        G02DAT
011300030428     C                   MOVE      '3'           G02ERR
011400030428     C                   CALL      'XSRDA8'
011500030428     C                   PARM                    WLBDAT
011600030428     c                   eval      inData = g02dat
011700030428      * controlla se da troppo tempo è in trasmissione
011800030428      *  potrebbe essere successo qualcosa
011900030428     c                   movel     daOre         ORE               2 0
012000030428     c                   clear                   msgvid
012100030428     c                   clear                   msgvd2
012200030428     c                   if        Uore < ore or (Uore - ore) > 2
012300030428     c                   movel     Avvisa        msgvid
012400030428     c                   movel     Avvisa2       msgvd2
012500030428     c                   end
012600030418     C* Sta trasmettendo quindi Messaggio a Video e chiusura pgm
012700030418     C                   EXFMT     LRD3D01
012800030428     c                   eval      FN8FLG = 'T'
012900030409     c                   leave
013000030409     c                   end
013100030428     c                   end
013200030418     C*
013300030418     C* Se chiamato dalla Trasmissione Conteggi Batch
013400030428     c                   if        FN8PGM = 'FICN43R'
013500030418     C*
013600030418     C*   Se Conferma o Rettifiche Conteggi
013700030418     c                   if        WALPGM = 'FICNB3R   ' or
013800030418     c                             WALPGM = 'FICN40R   '
013900030418     C*
014000030418     c*  Se qualcuno è dentro alla Conferma durante l'orario di trasmissione:
014100030418     C*   Invia un messaggio al lavoro per far chiudere la Conferma conteggi
014200030418     C*    e poi ritenta di eseguire la trasmissione dopo tot.secondi
014300030418     c                   z-add     1             x
014400030418     C                   exsr      CalQEZSNDMG
014500030429     c                   eval      FN8FLG = 'T'
014600030418     c                   end
014700030418     C*
014800030418     c                   end
014900030418     C*
014901060119     c                   else
014902060119      * ?Se non è attivo :
014904060119     C*
015000030930     c                   endif
015100030930     C*
015200030418     c                   read      fiwalc1l
015300030409     c                   enddo
015400030409     C*
015500940610     C*
015600000000     C     FINE          TAG
015700030428     C                   MOVEL     Ficnb8DS      KPJBU
015800030409     C*
015900931027     C                   SETON                                        LR
016000030418     ***********************************************************************
016100030418     **
016200030418     ** Send Message (QEZSNDMG) API
016300030418     **
016400030418     ***********************************************************************
016500030418     C     CalQEZSNDMG   BEGSR
016600030418      *
016700030418     ** Invio messaggio all'utente.
016800030418     C                   EVAL      SndMg03 = 'ATTENZIONE:  Chiudere immediatame-
016900030418     C                             nte il pgm di Conferma Conteggi o Rettifiche-
017000030418     C                              ai Corrispettivi. Stai creando problemi all-
017100030418     C                             e Trasmissioni dei Conteggi in sede.'
017200030418     C                   EVAL      SndMg05(x) = WalNUS
017300030418     C                   EVAL      SndMg06 = x
017400030418     C                   CLEAR                   QUSEC
017500030418     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
017600030418
017700030418     C                   CALL      'QEZSNDMG'
017800030418     C                   PARM                    SndMg01
017900030418     C                   PARM                    SndMg02
018000030418     C                   PARM                    SndMg03
018100030418     C                   PARM                    SndMg04
018200030418     C                   PARM                    SndMg05
018300030418     C                   PARM                    SndMg06
018400030418     C                   PARM                    SndMg07
018500030418     C                   PARM                    SndMg08
018600030418     C                   PARM                    Qusec
018700030418     C                   PARM                    SndMg10
018800030418     C                   PARM                    SndMg11
018900030418     C                   PARM                    SndMg12
019000030528      *
019100030528      * Manda un Msg in posta in sede a EDP*
019200030528     c                   exsr      send_sede
019300030528      *
019400030418     C                   ENDSR
019500030528      **********************************************************************
019600030528     c     SEND_sede     begsr
019700030528      *
019800030528     C                   MOVEL     MSG(1)        DSMSG
019900030528     C                   MOVE      WalNUS        MSusr
020000030528     C                   MOVEL     DSMSG         MSG(1)
020100030528      *
020200030528     C                   MOVEL     MSGT(1)       DSMST
020300030528     C                   MOVE      WalNUS        MTusr
020400030528     C                   MOVEL     DSMST         MSGT(1)
020500030528      * Invio Messaggio
020600030528     C                   CALL      'TNSYM4C'
020700030528     C                   PARM                    MSGT
020800030528     C                   PARM                    MSG
020900030528     C                   PARM      046           FIL               3 0
021000030528     C                   PARM      'EDP*'        UTEN             10
021100030528      *
021200030528     c                   endsr
021300030528      **********************************************************************
021400030418
021500030418     ***********************************************************************
021600030528** MSGT
021700030528ATTENZIONE: TRASMISSIONI IN ATTESA contattare :
021800030528** MSG
021900030528L'utente            non permette la trasmissione a sede dei dati
022000030528    per i saldi di C/E pomeridiano. Avvisare prima possibile la
022100030528filiale  < Sollecitare il P.O. a Chiudere i pgm dell'utente >

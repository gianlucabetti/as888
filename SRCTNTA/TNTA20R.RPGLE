000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200001124      *****************************************************************
000300001124      *                                                               *
000400001124      *       STAMPA TARIFFE CON ADDIZIONALE DI GESTIONE              *
000500001124      *                                                               *
000600001124      *****************************************************************
000700001124
000800001124     FAZORG02L  IP   E           K DISK
000900001124     FWADDT01L  IF   E           K DISK
001000001124     FTNTAM01L  IF   E           K DISK
001100001124     FTABEL00F  IF   E           K DISK
001200001124     FTNTA20P   O    E             PRINTER oflind(*IN01)
001300001124
001400001124      *****************************************************************
001500001124
001600001124      *   D S   I N T E R N E / E S T E R N E
001700001124
001800001124     D Parm01          DS
001900001124     D  pstp                          2
002000001124
002100001124     D CNCR80        E DS
002200001124     D UT§DSE0F      E DS
002300001124     D KPJBA         E DS
002400011122     D TIBS69DS      E DS
002500011122     D ds_cnaco      E DS                  extname(CNACO00F)
002600011122     D ds_cnind      E DS                  extname(CNIND00F)
002700011122     D ds_cnclp      E DS                  extname(CNCLP00F)
002800011122     D ds_fncls      E DS                  extname(FNCLS00F)
002900001124
003000001124      *   D E F I N I Z I O N E   C A M P I
003100001124
003200001124     D savorgdes       S                   like(ORGdes)
003300001124     D savorgfil       S                   like(ORGfil)
003400001124     D Kcod            S                   like(TBLcod)
003500001124     D Kkey            S                   like(TBLkey)
003600001124     D Kksc            S                   like(WADksc)
003700001124     D Kctr            S                   like(WADctr)
003800001124     D Kprg            S                   like(WADprg)
003900001124     D kwaddt          S                   like(WADksc)
004000001124     D ksc3            S                   like(ORGfil)
004100001124     D kkcc            S                   like(ACOkcc)
004200001124
004300001124     D W0140           S             14  0
004400001124
004500001124      *---------------------------------------------------------------*
004600001124
004700001124      * Solo i validi
004800001124     C                   if        ORGfva = *blanks
004900001124
005000001124     C                   if        ORGfil <> savorgfil
005100001124
005200001124      * Legge File pilota
005300001124     C                   movel     ORGfil        kwaddt
005400001124     C     kwaddt        setll     waddt01l
005500001124     C                   do        *hival
005600001124     C                   read      waddt01l
005700001124     C                   if        %eof(waddt01l)
005800001124     C                   leave
005900001124     C                   endif
006000001124     C                   movel     WADksc        ksc3
006100001124      * Clienti della filiale
006200001124     C                   if        ksc3 <> ORGfil
006300001124     C                   iter
006400001124     C                   endif
006500001124
006600001124      * Legge File testata Tariffe
006700001124     C                   eval      kksc = WADksc
006800001124     C                   eval      kctr = WADctr
006900001124     C                   eval      kprg = WADprg
007000001124     C     ktntam        chain     tntam01l
007100001124     C                   if        %found(tntam01l)
007200001124     C                   eval      *in11 = (TAMatb = 'A')
007300001124     C                   else
007400001124     C                   eval      *in11 = *on
007500001124     C                   endif
007600001124
007700001124      * Ragione sociale cliente
007800001124     C                   clear                   cliente
007900020118     C                   clear                   blocco
008000011122     C                   clear                   TIBS69DS
008100011122     C                   z-add     kksc          I69kac
008200011122     C                   call      'TIBS69R'
008300011122     C                   parm                    tibs69DS
008400011122     C                   parm                    ds_cnaco
008500011122     C                   parm                    ds_cnind
008600011122     C                   parm                    ds_cnclp
008700011122     C                   parm                    ds_fncls
008800011122     C                   if        o69err <> '1'
008900011122     C                   movel     acorag        cliente
009000020117     C                   movel     acoabl        blocco
009100011122     C                   endif
009200130321      * Se blocco "7" stampo blocco "8"
009300130321     c                   IF        blocco = '7'
009400130321     c                   eval      blocco = '8'
009500130321     c                   ENDIF
009600001124
009700001124      * Stampa solo se modificate o se annullate
009800001124     C                   if        pstp = 'SI'
009900041129     C                   if        WADpag <> TAMpag or *in11 or
010000041129     C                             WADppa <> TAMppa
010100041129     C                   if        WADpag <> TAMpag
010200001124     C  n11              eval      *in10 = *on
010300041129     c                   endif
010400041129     C                   if        WADppa <> TAMppa
010500041129     C  n11              eval      *in12 = *on
010600041129     c                   endif
010700061201     c                   endif
010800061201
010900061201      * se c'è modifica stampo
011000061201     c                   If            *in10
011100061201     c                             or  *in11
011200061201     c                             or  *in12
011300061201     c                             or  *in22
011400061201     c                             or  *in23
011500001124      * Testata
011600001124     C                   if        ORGdes <> savorgdes
011700001124     C                             or *in01 = *on
011800001124     C                   exsr      Sr_Decodifica
011900001124     C                   write     testa
012000001124     C                   eval      *in01 = *off
012100001124     C                   eval      savorgdes = ORGdes
012200001124     C                   endif
012300001124      * Dettaglio
012400001124     C                   write     detta                                01
012500041129     C                   setoff                                       101112
012600061201     C                   setoff                                       212223
012700061205     C                   setoff                                       24
012800001124     C                   endif
012900001124     C                   endif
013000001124
013100001124      * Stampa tutto
013200001124     C                   if        pstp = 'NO'
013300001124      * Testata
013400001124     C                   if        ORGdes <> savorgdes
013500001124     C                             or *in01 = *on
013600001124     C                   exsr      Sr_Decodifica
013700001124     C                   write     testa
013800001124     C                   eval      *in01 = *off
013900001124     C                   eval      savorgdes = ORGdes
014000001124     C                   endif
014100001124      * Dettaglio
014200001124     C  n11              eval      *in10 =(WADpag <> TAMpag)
014300041129     C  n11              eval      *in12 =(WADppa <> TAMppa)
014400061201
014500001124     C                   write     detta                                01
014600041129     C                   setoff                                       101112
014700061201     C                   setoff                                       212223
014800061205     C                   setoff                                       24
014900001124     C                   endif
015000001124
015100001124     C                   enddo
015200001124
015300001124     C                   endif
015400001124
015500001124     C                   eval      savorgfil = ORGfil
015600001124     C                   endif
015700001124
015800001124      *---------------------------------------------------------------*
015900001124      * DECODIFICA
016000001124      *---------------------------------------------------------------*
016100001124     C     Sr_Decodifica BEGSR
016200001124
016300001124     C                   clear                   distretto
016400001124     C                   clear                   area
016500001124
016600001124     C                   movel     '17'          kcod
016700001124     C                   movel(P)  ORGfl3        kkey
016800001124     C     ktab          chain     tabel00f
016900001124     C                   if        %found(tabel00f)
017000001124     C                   movel     TBLuni        distretto
017100001124     C                   endif
017200001124
017300001124     C                   movel     '05'          kcod
017400001124     C                   movel(P)  ORGcar        kkey
017500001124     C     ktab          chain     tabel00f
017600001124     C                   if        %found(tabel00f)
017700001124     C                   movel     TBLuni        area
017800001124     C                   endif
017900001124
018000001124     C                   endsr
018100001124      *---------------------------------------------------------------*
018200001124      * ROUTINE INIZIALE
018300001124      *---------------------------------------------------------------*
018400001124     C     *INZSR        BEGSR
018500001124
018600001124     C     *ENTRY        PLIST
018700001124     C                   PARM                    KPJBA
018800001124     C                   movel     kpjbu         parm01
018900001124      *
019000001124     C                   Z-ADD     1             CODUT
019100001124     C                   CALL      'X§PARUT'
019200001124     C                   PARM                    UT§DSE0F
019300001124     C                   MOVEL     RAGUT         RSUT
019400001124     C                   MOVEL     REC80         CNCR80
019500001124
019600001124      * reperisco data e ora
019700001124     C                   TIME                    W0140
019800001124      * UDATE IN GGMMAAAA
019900001124     C                   MOVE      W0140         WDATE
020000001124
020100001124     C     ktab          KLIST
020200001124     C                   KFLD                    codut
020300001124     C                   KFLD                    kcod
020400001124     C                   KFLD                    kkey
020500001124
020600001124     C     ktntam        KLIST
020700001124     C                   KFLD                    kksc
020800001124     C                   KFLD                    kctr
020900001124     C                   KFLD                    kprg
021000061201
021100001124     C                   endsr

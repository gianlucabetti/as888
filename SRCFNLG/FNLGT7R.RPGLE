000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200050225     H* FNLGT7R *----------------------------------------------------*
000300000000     H*-----------------*                                            *
000400050218     H*  aggiorna titas e tncsb da giacenze (pgm schedulato)
000500000000     H*--------------------------------------------------------------*
000600050218     FFIGIC00f  UF   E             DISK
000700941014     FTNCSB03L  UF   E           K DISK
000800990729     FTITAS30C  UF   E           K DISK
000900941014     FTNCSV00F  O  A E             DISK
001000941122     FTABEL00F  IF   E           K DISK    USROPN
001100940518     FRICCASS   O    F  132        PRINTER OFLIND(*INOF)
001200941122     D TTB             S              2    DIM(40)
001300941122     D DTB             S             89    DIM(40)
001400941122     D DSTB          E DS
001500941014     D CSBDS         E DS                  EXTNAME(TNCSB00F)
001600941014     D               E DS                  EXTNAME(TNCSV00F)
001700941014     D  CSVDS                 16    229
001800000000     D KPJBA         E DS
001900000000     D UTEDSE        E DS                  EXTNAME(UTEDSE0F)
002000941014     D WLBDA8          DS
002100941014     D  G02DAT                 1      8  0
002200941014     D  G02INV                 9     16  0
002300941014     D  G02ERR                17     17
002400941014     D  G02TGI                18     22  0
002500990729     ITITASP00      10
002600990729     ITITAS000      11
002700990729     ITITAS010      12
002800941014     C*--------------------------------------------------------------*
002900050218     C                   do        *hival
003000990720     C                   READ      FIGIC000                               01
003100050218     C   01              leave
003200920403     C* METTO SEGNALAZ. SULLA BOLLA CHE C'E' STATA GIACENZA
003400050218     C     KBOL          setll     TITAS30C
003500050218     c                   do        *hival
003501050221     C                   SETOFF                                       101112
003600050218     C     KBOL          reade     TITAS30C
003700050218     c                   if        %eof(titas30c)
003800050218     c                   leave
003900050218     c                   end
003901050418     c* esclude AP xchè considerati come c/servizio ma in realtà sono dei
003902050418     c* recuperi
003903050418     c                   if        tastbl = 'AP'
003904050418     c                   iter
003905050418     c                   end
004000941122      * ESAMINO LA BOLLA LETTA PER VEDERE SE è UNA BOLLA DI RECUPERO
004100941122      * ---->   NON DEVO METTERE IL FLAG SU UN RECUPERO
004200941122     C                   CLEAR                   DSTB
004300941122     C                   Z-ADD     1             X
004400941122     C     TASTBL        LOOKUP    TTB(X)                                 26
004500050218     C                   MOVEL     DTB(X)        DSTB
004600050218     C     §TBRBL        IFeq      'R'
004700050218     c                   iter
004800050218     c                   else
004900050218     c                   leave
005000050218     c                   end
005100050218     c                   enddo
005200990901     C* SE HO RICEVUTO L'APERTURA DELLA GIACENZA AGGIORNO BOLLA
005300990901     C* CON RELATIVO FLAG
005400990901     C     GICCAV        IFEQ      'APGC'
005500941014     C                   MOVEL     'S'           TASFGC
005600941102     C   11              EXCEPT    AGTAS0
005700941102     C   12              EXCEPT    AGTAS1
005800941102     C   10              EXCEPT    AGTASP
005801050408     c                   if        not *in10 and not *in11 and not *in12
005802050408     c                   iter
005900050218     C                   End
005901050408     C                   End
006000050218     C* .. Se ho ricevuto la comunicazione di chiusura procedo al
006100050218     C*    calcolo dei giorni di giacenza
006200990901     C     GICCAV        IFEQ      'CHGC'
006300990901     C* Richiamo routine X tradurre in gg. la data apertura giac.
006400990901     C                   MOVE      GICDGC        G02INV
006500990901     C                   MOVEL     '3'           G02ERR
006600990901     C                   CALL      'XSRDA8'
006700990901     C                   PARM                    WLBDA8
006800990901     C                   Z-ADD     G02TGI        WGGINI            5 0
006900990901     C                   MOVE      GICDCH        G02INV
007000990901     C                   MOVEL     '3'           G02ERR
007100990901     C                   CALL      'XSRDA8'
007200990901     C                   PARM                    WLBDA8
007300990901     C                   Z-ADD     G02TGI        WGGFIN            5 0
007400990903     C     WGGFIN        SUB       WGGINI        WGGGIA            5 0
007500990901     C                   Z-ADD     WGGGIA        TASGGS
007600990901     C   11              EXCEPT    AGTAG0
007700990901     C   12              EXCEPT    AGTAG1
007800990901     C   10              EXCEPT    AGTAGP
007900050218     c                   if        *in10 or *in11 or *in12
007901080128     c* se ricevo la chiusura della giacenza metto lo stato del contrassegno
007902080128     c* a 0 a prescindere se ho incassato.
007903080128     C     KCON          CHAIN     TNCSB000
007904080128     c                   if        %found(tncsb03l) and csbsta = 2
007905080128     c                   eval      csbsta = 0
007906080128     C                   UPDATE    TNCSB000
007907080128     c                   end
008000050218     C                   DELETE    FIGIC000
008100050218     c                   iter
008200050218     c                   end
008201050408     c                   if        not *in10 and not *in11 and not *in12
008202050408     c                   iter
008203050408     C                   End
008300990901     C                   END
008400941014     C*
008500941014     C     KCON          CHAIN     TNCSB000                           02
008601050408     c                   if        *in02
008700990720     C                   DELETE    FIGIC000
008800050218     C                   iter
008901050408     c                   END
009000050408     C** CONTRASSEGNO NON PRESENTE
009100911008     C** CONTRASSEGNO GIA INCASSATO O GIA PAGATO - NO MOVIM. GIAC.
009200941014     C     CSBSTA        IFNE      9
009300941014     C     CSBDDC        IFNE      *ZEROS
009400941014     C     CSBBNA        ORNE      *ZEROS
009500941014     C                   EXCEPT    DET
009600941014     C                   EXCEPT    ERR6
009700941014     C   OF              EXCEPT    TEST
009800941014     C                   END
009900941122     C** CONTRASSEGNO GIACENTE
010000910913     C* CREO STORICO VARIAZIONI C/A
010100910913     C                   EXSR      CSVWTR
010200950207     C* METTO LO STATO GIAC. SOLO SE NON INC. O PAGATO
010300950207     C     CSBDDC        IFEQ      *ZEROS
010400950207     C     CSBBNA        ANDEQ     *ZEROS
010500050218     C                   MOVEL     '2'           CSBSTA
010600950207     C                   ENDIF
010700920403     C                   MOVE      GICNGC        CSBNGC
010800920403     C                   MOVE      GICFGC        CSBFGC
010900920403     C                   MOVE      GICDGC        CSBDGC
011000941014     C                   UPDATE    TNCSB000
011100000000     C                   EXCEPT    DET
011200900615     C   OF              EXCEPT    TEST
011300950310     C                   ELSE
011400950310     C                   EXCEPT    DET
011500950310     C                   EXCEPT    ERR7
011600950310     C   OF              EXCEPT    TEST
011700950310     C                   END
011800990720     C                   DELETE    FIGIC000
011900050218     C                   ENDdo
012000000000     C                   SETON                                        LR
012100910913     C**---------------------------------------------------------
012200941014     C** ESEGUE WRITE TNCSV00F                                  -
012300910913     C**---------------------------------------------------------
012400910913     C     CSVWTR        BEGSR
012500941122     C                   MOVEL     GICCAV        CSVCAV
012600941122     C                   MOVE      GICDTV        CSVDTV
012700941122     C                   MOVE      GICORV        CSVORV
012800941122     C                   MOVEL     GICFEV        CSVFEV
012900910913     C                   MOVEL     CSBDS         CSVDS
013000941014     C                   WRITE     TNCSV000
013100910913     C                   ENDSR
013200941014     C**---------------------------------------------------------
013300941014     C** OPERAZIONI INIZIALI
013400941014     C**---------------------------------------------------------
013500941014     C     *INZSR        BEGSR
013600941014     C     *ENTRY        PLIST
013700941014     C                   PARM                    KPJBA
013800041001     C*
013900941014     C                   Z-ADD     1             CODUT
014000941014     C                   CALL      'XPARUT'
014100941014     C                   PARM                    UTEDSE
014200050218     C     CODUT         ifeq      -1
014300050218     c                   seton                                        lr
014400050218     c                   return
014500050218     c                   end
014600000128     C                   MOVEL     RAGUT         RSUT             20
014700941014     C** UDATE IN AMG
014800941014     C                   TIME                    WHHDAT           14 0
014900941014     C                   CLEAR                   WLBDA8
015000941014     C                   MOVE      WHHDAT        G02DAT
015100941014     C                   Z-ADD     0             G02INV
015200941014     C                   CALL      'XSRDA8'
015300941014     C                   PARM                    WLBDA8
015400941014     C                   Z-ADD     G02INV        RUDATE            8 0
015500941014     C                   Z-ADD     G02DAT        OGGI              8 0
015600941014     C* KLIST
015700941014     C     KCON          KLIST
015800941014     C                   KFLD                    GICAAS
015900941025     C                   KFLD                    GICLNP
016000941014     C                   KFLD                    GICNRS
016100941014     C                   KFLD                    GICNSP
016200941014     C* KLIST
016300941014     C     KBOL          KLIST
016400941014     C                   KFLD                    GICAAS
016500941014     C                   KFLD                    GICLNP
016600941014     C                   KFLD                    GICNRS
016700941014     C                   KFLD                    GICNSP
016800941014     C*
016900941122      * TABEL00F
017000941122     C     KCOD          KLIST
017100941122     C                   KFLD                    CODUT
017200941122     C                   KFLD                    CODTAB            2
017300941122      *
017400941122     C                   OPEN      TABEL00F
017500941122      * CARICO TUTTI I TIPI BOLLA    TAB.TB
017600941122     C                   MOVEL     'TB'          CODTAB
017700941122     C                   MOVE      *ZEROS        X                 2 0
017800941122     C     KCOD          SETLL     TABEL00F
017900941122     C     *IN21         DOUEQ     '1'
018000941122     C     KCOD          READE     TABEL00F                               21
018100941122     C  N21              ADD       1             X
018200941122     C  N21              MOVEL     TBLKEY        TTB(X)
018300941122     C  N21              MOVEL     TBLUNI        DTB(X)
018400941122     C                   END
018500941122     C                   CLOSE     TABEL00F
018600941014     C* LETTURA
018700941014     C                   EXCEPT    TEST
018800941014     C*
018900941014     C                   ENDSR
019000000128     ORICCASS   E    LRNLR   TEST           2 02
019100000000     O                       RSUT
019200941014     O                                         + 15 'C/ASSEGNI GIACENTI DEL'
019300941014     O                       OGGI              +  1 '  /  /    '
019400000128     O                                         +  1 'FNLGT7R'
019500000000     O                                         +  5 'PAG.'
019600000000     O                       PAGE          Z   +  1
019700000128     O          E    LRNLR   TEST           2
019800900615     O                                              'L.P.'
019900920403     O                                         +  1 'SER.'
020000900615     O                                         +  1 'SPEDIZ.'
020100941014     O                                         +  1 'ANNO'
020200900615     O                                         +  1 'L.A.'
020300900615     O                                         + 10 'NOTE'
020400000128     O          E    LRNLR   DET         1
020500900604     O                       GICLNP
020600920403     O                       GICNRS        Z   +  1
020700900604     O                       GICNSP        Z   +  1
020800941122     O                       GICAAS            +  4
020900900604     O                       GICLNA            +  1
021000000128     O          E    LRNLR   ERR6        0
021100911008     O                                         + 50 'C/ASSEGNO GIA INCASSATO'
021200911008     O                                         +  1 '- PAGATO'
021300000128     O          E    LRNLR   ERR7        0
021400950310     O                                         + 50 'C/ASSEGNO GIA ANNULLATO'
021500990729     OTITAS000  E            AGTAS0
021600920403     O                       TASFGC
021700990729     OTITAS010  E            AGTAS1
021800920403     O                       TASFGC
021900990729     OTITASP00  E            AGTASP
022000930225     O                       TASFGC
022100990901     OTITAS000  E            AGTAG0
022200990903     O                       TASGGS
022300990901     OTITAS010  E            AGTAG1
022400990903     O                       TASGGS
022500990901     OTITASP00  E            AGTAGP
022600990903     O                       TASGGS

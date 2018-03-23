000100990127      *----------------------------------------------------*
000200990209      *  Ricalcolo Responsabilità Danno per CA con flag DCTCAR = 'R'
000300990127      *----------------------------------------------------*
000400990127
000500990127      ****************************************************************
000600990127      *  RIEPILOGO INDICATORI
000700990127      ***************************************************************
000800990127      * 30 - COMODO
000900990209      * 31 - COMODO
001000990209      * 90 - prima volta che stampo
001100990127      ***************************************************************
001200990127
001300990127     H DECEDIT('0,') DATEDIT(*DMY.)
001400990127
001500990308     FTABEL00F  IF   E           K DISK
001600020912     FFNDCT14L  IF   E           K DISK
001700040804     FFNDCK01L  IF   E           K DISK
001800990209     FFNARB01L  IF   E           K DISK
001900050715     FFNBLP01L  IF   E           K DISK
002000050715     f                                     prefix(ARB:3)
002100990209     FQPRINT    O    F  132        PRINTER oflind(*INOF)
002200990127
002300990127      *------------------------------------------------------------------------*
002400990202     D dateu           s              8  0
002500990202     D wdtgio          S              8  0
002600990202     D w0140           S             14  0
002700990209     D ggspe           S              2  0
002800990209     D mmspe           S              2  0
002900990209     D TRAT            S            132    INZ(*all'-')
003000990308     D Kcod            S                   LIKE(tblcod)
003100990308     D Kkey            S                   LIKE(tblkey)
003200990202      *
003300990209     D fidn50ds      E DS
003400990920     D TIBS02DS      E DS
003500990202      *
003600990209     D CNCR80        E DS
003700990209     D UT§DSE0F      E DS
003800990202     D KPJBA         E DS
003900990308     D DS3A          E DS
004000990920     D DCCH          E DS
004100040804     D DDCT01        E DS
004200990208      *
004300990208     D WLBDAT          DS                  INZ
004400990208     D  G02DAT                 1      8  0
004500990208     D  G02INV                 9     16  0
004600990208     D  G02ERR                17     17
004700990208     D  G02TGI                18     22  0
004800990209
004900990209     D TES             S            100    DIM(3) CTDATA PERRCD(1)              TESTATA STAMPA
005000990209
005100990202      *****************************************************************
005200990202      *
005300990202     c* Lettura di tutte le CA
005400990202      *
005500990920    1c                   do        *hival
005600990202      *
005700020912     c                   read      fndct000                               30
005800990920     C*
005900040804      *
006000020912    2c                   if        not *in30
006100040804      * se CA con legami recupero il numero di  spedizione da FNDCK
006200040804     c                   movel     dctflo        ddct01
006300050920     c****               If        §dctlega = 'S' and dctfil<>dctlnp
006400050920     c                   If        §dctlega = 'S'
006500040804     c     Kdck          chain     Fndck01l
006600040804     c                   If        %found(fndck01l)
006700040804     c                   eval      dctaas = dckaas
006800040804     c                   eval      dctlnp = dcklnp
006900040804     c                   eval      dctnrs = dcknrs
007000040804     c                   eval      dctnsp = dcknsp
007100040804     c                   endif
007200040804     c                   endif
007300990920     c
007400990209      * aggancio file bolle arrivi per recuperare i dati da passare al PGM calcolo responsabilità
007500990209     c     karb          chain     fnarb01l                           31
007600050715     c
007700050715     c                   if        not %found(fnarb01l)
007800050715     c     karb          chain     fnblp01l                           31
007900050715     c                   endif
008000050715     c
008100990209      * se CA non regolare leggo la successiva
008200990920    3c                   if        *in31
008300990202     c                   iter
008400990202      *
008500990202     c                   else
008600990218      * se CA regolare calcolo la responsabilità
008700990209     c                   exsr      SUB_RESP
008800990202      *
008900990920    3c                   endif
009000990202      *
009100990920    2c                   endif
009200990202      *
009300990920    1c  N30              enddo
009400990202      *
009500990209     c* se stampata almeno una riga scrivo la riga finale di mandare i dati alla sede
009600990209     c   90              except    fine
009700990209     c*
009800990202     c                   eval      *inlr = *on
009900990202      *---------------------------------------------------------------*
010000990209      *   SUB_RESP  Routine richiamo PGM Calcolo
010100990202      *---------------------------------------------------------------*
010200990209     C     SUB_RESP      BEGSR
010300990202      *
010400990202      * preparo la ds
010500990209     c                   clear                   fidn50ds
010600990202     c*
010700990209     c                   movel     'C'           I50mod
010800990209     c                   z-add     DCTaac        I50aac
010900990209     c                   z-add     DCTfil        I50fil
011000990209     c                   z-add     DCTnca        I50nca
011100990209     c                   z-add     DCTaas        I50aas
011200990209     c                   z-add     DCTlnp        I50lnp
011300990209     c                   z-add     DCTnrs        I50nrs
011400990209     c                   z-add     DCTnsp        I50nsp
011500990209     c                   z-add     ARBlna        I50lna
011600990209     c                   z-add     ARBncl        I50ncl
011700990209     c                   z-add     ARBtfp        I50tfp
011800990209     c                   z-add     ARBtfa        I50tfa
011900050915     c* non più usato
012000050915     c********           movel     'A'           I50tpb
012100050914     c                   movel     §dctlnpc      I50lnpmam
012200990308     c* aggancio il tipo bolla
012300990308     C                   movel     '3A'          kCOD
012400990308     C                   movel(P)  ARBCBO        kKEY
012500990308     C     ktab          CHAIN     TABEL                              31
012600990308     C  n31              movel     TBLUNI        DS3A
012700990308     C  n31              movel     §3ATB1        WPORTO            1
012800990308     c   31              movel     'F'           WPORTO
012900990308     c                   if        wporto='F'
013000990308     c                   z-add     arbksc        i50ksm
013100990308     c                   else
013200990308     c                   z-add     arbksc        i50ksd
013300990308     c                   z-add     arbccm        i50ksm
013400990308     c                   endif
013500990308     c
013600990209     c                   movel(P)  FIDN50DS      KPJBU
013700990209      *
013800990209     c                   Call      'FIDN50R'
013900990209     c                   Parm                    KPJBA
014000990202      *
014100990209      * se mi ritorna un errore stampo l'errore e la CA / Spedizione legata
014200990209      *
014300990209     c                   if        o50err = 'E'
014400990209     c*
014500990209     c                   if        not *in90 or *inof
014600990209     c*
014700990209     c                   except    testa
014800990209     c                   eval      *in90 = *on
014900990209     c                   eval      *inof = *off
015000990209     c                   endif
015100990209     c*
015200990209      *
015300990209     c                   clear                   mmspe
015400990209     c                   clear                   ggspe
015500990209      *
015600990209     c                   movel     arbmgs        mmspe
015700990209     c                   move      arbmgs        ggspe
015800990209      *
015900990209     c                   except    riga
016000990209     c*
016100990209     c                   endif
016200990209     c*
016300990202     c                   endsr
016400990127      *****************************************************************
016500990127      *   ROUTINE INIZIALE
016600990127      *****************************************************************
016700990127     C     *INZSR        BEGSR
016800990127      *
016900990127     C     *ENTRY        PLIST
017000990127     C                   PARM                    KPJBA
017100990209      *
017200990209     C                   Z-ADD     1             CODUT
017300990209     C                   CALL      'X§PARUT'
017400990209     C                   PARM                    UT§DSE0F
017500990209     C                   MOVEL     RAGUT         RSUT             20
017600990209     C                   MOVEL     REC80         CNCR80
017700990208      * ORA
017800990208     C                   TIME                    W0140
017900990208      * UDATE IN GGMMAAAA
018000990208     C                   MOVE      W0140         WDTGIO
018100990209      * accesso Bolle Arrivi
018200990209     C     karb          KLIST
018300990209     C                   KFLD                    dctaas
018400990209     C                   KFLD                    dctlnp
018500990209     C                   KFLD                    dctnrs
018600990209     C                   KFLD                    dctnsp
018700990308     C     ktab          KLIST                                                  *
018800990308     C                   KFLD                    codut                          *
018900990308     C                   KFLD                    kcod                           *
019000990308     C                   KFLD                    kkey                           *
019100040804      * accesso FNDCK01L
019200040804     C     kdck          KLIST
019300040804     C                   KFLD                    dctaac
019400040804     C                   KFLD                    dctfil
019500040804     C                   KFLD                    dctnca
019600990208      *
019700990127     C                   ENDSR
019800990209     OQPRINT    E            TESTA
019900990209     O                       RSUT              +  0
020000990209     O                                         +  0 '/'
020100990209     O                       SIMFEL            +  0
020200990209     O                                           56 'ELENCO CA / SPEDIZIONI CON'
020300990209     O                                         +  1 'ERRORI NEL CALCOLO RESPONS'
020400990209     O                                              'ABILITA'''
020500990209     O                                          100 'FIDN51R'
020600990209     O                       WDTGIO        Y    115
020700990209     O                                          125 'PAG.'
020800990209     O                       PAGE          Z    130
020900990209     O          E            TESTA             3
021000990209     O                       KNSIF             +  0
021100990209     O                       KNMUS             +  1
021200990209     O          E            TESTA             4
021300990209     O                       TRAT               132
021400990209     O          E            TESTA             5
021500990209     O                       tes(1)
021600990217     O          E            TESTA             6  1
021700990209     O                       TRAT               132
021800990217     O          E            riga              1
021900990209     O                       dctaac
022000990209     O                       dctfil           +   1
022100990209     O                       dctnca        Z  +   1
022200990209     O                       arbaas           +   1
022300990209     O                       arblnp           +   1
022400990209     O                       arbnrs        Z  +   1
022500990209     O                       arbnsp        Z  +   1
022600990209     O                       ggspe            +   1
022700990209     O                                              '/'
022800990209     O                       mmspe
022900990209     o                       o50err           +   5
023000990217     O          E            fine              1
023100990209     o                       Tes(3)           +  30
023200990217     O          E            fine              1
023300990209     o                       Tes(2)           +  30
023400990217     O          E            fine              1
023500990209     o                       Tes(3)           +  30
023600990209** TES  (Lungh. 100)   -Disegno stampa-
023700990209Anno P.O.  N°C.A.  Anno LNP Ser Numero  Data          E R R O R E
023800990209MANDARE LA LISTA IN SEDE ALL'UFF. CED
023900990209-------------------------------------

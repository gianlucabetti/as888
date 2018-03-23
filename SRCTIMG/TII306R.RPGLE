000100990127     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PRNPGM) ACTGRP(QILE)
000200990127     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000300000000     H* OCO303R *-OC/38-*--------------------------------------------*
000400000000     H*-----------------*                                            *
000500000000     H*        - LISTA DI CONTROLLO SIMULAZIONE ASSEGNAZIONE -       *
000600000000     H*          -------------------------------------------         *
000700000000     H*                                                      AZ.O303 *
000800990127     H*         FILE : tmmoc01L - CNACO00F -                         *
000900870601     H*                                                              *
001000000000     H*                                                              *
001100000000     H*--------------------------------------------------------------*
001200990127     Ftmmoc01L  IF   E           K DISK
001300990127     Fansog01l  IF   E           K DISK
001400000000     FOCPRTSTD  O    F  132        PRINTER
001500870527     D*--------------------------------------------------------------*
001600870527     D SK1             S             66    DIM(2) CTDATA PERRCD(1)              *TESTATA 1
001700870527     D SK2             S             66    DIM(2) CTDATA PERRCD(1)              *TESTATA 2
001800870527     D SK3             S             66    DIM(2) CTDATA PERRCD(1)              *TESTATA 3
001900870527     D SK4             S             66    DIM(2) CTDATA PERRCD(1)              *TESTATA 4
002000890926     D SK5             S             66    DIM(2) CTDATA PERRCD(1)              *TESTATA 5
002100870601     D T1              S             66    DIM(2) CTDATA PERRCD(1)
002200870601     D T2              S             66    DIM(2) CTDATA PERRCD(1)
002300870601     D T3              S             66    DIM(2) CTDATA PERRCD(1)
002400870601     D TC1             S             66    DIM(2) CTDATA PERRCD(1)
002500870601     D TC2             S             66    DIM(2) CTDATA PERRCD(1)
002600870601     D TC3             S             66    DIM(2) CTDATA PERRCD(1)
002700870527     D*--------------------------------------------------------------*
002800000000     D RCDS            DS
002900990127     D  MOCCLI                 1      8
003000990127     D  MOCNOR                 9     13  0
003100000000     D RCPRE           DS
003200990127     D  PRECLI                 1      8
003300990127     D  PRENOR                 9     13  0
003400990127     d dataiso         s               d   datfmt(*iso)
003500990127     d dataeur         s               d   datfmt(*eur)
003600000000     D KPJBA         E DS
003700000000     I*--- PASSAGGIO PARAMETRI
003800000000     C*----------------------------------------------------*
003900000000     C     *ENTRY        PLIST                                                  *
004000000000     C                   PARM                    KPJBA                          *
004100000000     C******************************************************
004200000000     C*--- FILE LISTA DI CONTROLLO CLIENTE/N.ORDINE        *
004300990127     C* 'tmmoc01L'                                         *
004400000000     C     KEYMOC        KLIST                                                  *
004500000000     C                   KFLD                    MOCCLI                         *
004600000000     C                   KFLD                    MOCNOR                         *
004700000000     C*----------------------------------------------------*
004800870602     C                   Z-ADD     0             PAGE              3 0          *
004900870610     C                   Z-ADD     0             PAGE1             3 0          *
005000870610     C                   Z-ADD     60            CONRIG            3 0
005100000000     C*PULIZIA DEI CAMPI DELLA DS
005200000000     C                   MOVEL     *BLANKS       PRECLI
005300000000     C                   Z-ADD     0             PRENOR
005400000000     C     SU            TAG
005500990127     C                   READ      tmmoc000                               02
005600870610     C   02              DO
005700870916     C                   Z-ADD     0             MOCNOR
005800870916     C     FLAG          IFEQ      1
005900870916     C                   EXSR      SCELTA
006000870610     C     CONRIG        IFGE      60
006100870610     C                   EXCEPT    TESTA
006200870610     C                   Z-ADD     3             CONRIG
006300870610     C                   END
006400870610     C                   EXCEPT    TOTCLI
006500880215     C                   END
006600870610     C                   GOTO      FINE
006700870610     C                   END
006800870630     C*ESCLUSIONE DELLE RIGHE DIVERSE DA 'OR' E DA TIPO RIGA 1
006900870922     C     MOCTRG        CABNE     '1'           SU
007000000000     C     MOCFAS        CABNE     'OR'          SU
007100000000     C  N99              MOVEL     RCDS          RCPRE
007200000000     C  N99              SETON                                            99
007300000000     C     MOCNOR        IFNE      PRENOR
007400870609     C     FLAG          IFEQ      1
007500870609     C                   EXSR      SCELTA
007600870610     C     CONRIG        IFGE      60
007700870610     C                   EXCEPT    TESTA
007800870610     C                   Z-ADD     3             CONRIG
007900870610     C                   END
008000870602     C                   EXCEPT    TOTALE
008100870610     C                   ADD       4             CONRIG
008200870602     C                   END
008300870602     C                   Z-ADD     0             TOTQTA
008400870602     C                   Z-ADD     0             TOTASS
008500870602     C                   Z-ADD     0             VALASS
008600870602     C                   Z-ADD     0             VALORD
008700870609     C                   Z-ADD     0             FLAG
008800870605     C                   MOVEL     MOCNOR        PRENOR
008900000000     C                   END
009000880215     C     MOCCLI        IFNE      PRECLI
009100880215     C     FLAG          IFEQ      1
009200880215     C     CONRIG        IFGE      60
009300880215     C                   EXCEPT    TESTA
009400880215     C                   Z-ADD     3             CONRIG
009500880215     C                   END
009600880215     C                   EXCEPT    TOTCLI
009700880215     C                   ADD       4             CONRIG
009800880215     C                   Z-ADD     0             CLIQTA
009900880215     C                   Z-ADD     0             CLIASS
010000880215     C                   Z-ADD     0             CLITAS
010100880215     C                   Z-ADD     0             CLITOR
010200880215     C                   END
010300880215     C                   MOVEL     MOCCLI        PRECLI
010400880215     C                   END
010500880215     C     MOCASS        IFGT      0
010600880215     C                   MOVEL     1             FLAG              1 0
010700880215     C                   END
010800000000     C                   GOTO      SU
010900000000     C     FINE          TAG
011000000000     C                   SETON                                            LR
011100000000     C***********************************************************
011200000000     C*HO TROVATO ALMENO 1 RIGA ASSEGNATA QUINDI MI RIPOSIZIONO
011300000000     C*SU L'ORDINE PER ELENCARE TUTTE LE RIGHE ANCHE QUELLE NON
011400000000     C*ASSEGNATE
011500000000     C     SCELTA        BEGSR
011600870602     C                   MOVEL     MOCNOR        COM1              5 0
011700000000     C                   MOVEL     RCPRE         RCDS
011800990127     C     KEYMOC        SETLL     tmmoc000
011900000000     C     STAR          TAG
012000990127     C                   READ      tmmoc000                               10
012100870916     C   10              GOTO      END1
012200870922     C     MOCTRG        CABNE     '1'           STAR
012300870922     C     MOCFAS        CABNE     'OR'          STAR
012400000000     C     MOCNOR        IFNE      PRENOR
012500000000     C                   GOTO      END1
012600000000     C                   END
012700000000     C                   EXSR      ASSORD
012800870603     C                   MOVEL     MOCDMT        DESMAT           20
012900990127     C     moccli        CHAIN     ansog01l                           20
013000990127     C  N20              MOVEL     sogdes        RAGSOC           30
013100870602     C   20              MOVEL     *BLANKS       RAGSOC
013200990127     c                   if        mocdcp > 0
013300990127     c                   move      mocdcp        dataiso
013400990127     c                   move      dataiso       dataeur
013500990127     c                   move      dataeur       dcp               8 0
013600990127     c                   end
013700990127     c                   if        mocdor > 0
013800990127     c                   move      mocdor        dataiso
013900990127     c                   move      dataiso       dataeur
014000990127     c                   move      dataeur       dor               8 0
014100990127     c                   end
014200870609     C                   EXSR      RUTSTA
014300870610     C                   MOVEL     MOCNOR        COM1                           *
014400000000     C                   GOTO      STAR
014500000000     C     END1          ENDSR
014600000000     C******************************************************
014700000000     C*CALCOLO E MEMORIZZO  IL  VALORE RIGA ASSEGNATO
014800000000     C*E IL  VALORE RIGA ORDINATO
014900000000     C     ASSORD        BEGSR
015000870602     C                   ADD       MOCQTA        TOTQTA            9 2
015100870602     C                   ADD       MOCQTA        CLIQTA            9 2
015200870602     C                   ADD       MOCASS        TOTASS           11 2
015300870602     C                   ADD       MOCASS        CLIASS           11 2
015400000000     C                   Z-ADD     MOCPRZ        PRENET           11 2
015500000000     C     PRENET        MULT      MOCSC1        SCONTO           11 2
015600000000     C     SCONTO        DIV       100           SCONTO
015700000000     C     PRENET        SUB       SCONTO        PRENET
015800000000     C     PRENET        MULT      MOCSC2        SCONTO
015900000000     C     SCONTO        DIV       100           SCONTO
016000000000     C     PRENET        SUB       SCONTO        PRENET
016100000000     C     PRENET        MULT      MOCSC3        SCONTO
016200000000     C     SCONTO        DIV       100           SCONTO
016300000000     C     PRENET        SUB       SCONTO        PRENET
016400000000     C     MOCASS        MULT(H)   PRENET        RIGASS           11 2
016500000000     C     MOCQTA        MULT(H)   PRENET        RIGORD           11 2
016600000000     C                   ADD       RIGASS        VALASS           11 2
016700870602     C                   ADD       RIGASS        CLITAS           11 2
016800000000     C                   ADD       RIGORD        VALORD           11 2
016900870602     C                   ADD       RIGORD        CLITOR           11 2
017000000000     C                   ENDSR
017100000000     C**--------- RUTINE EMISSIONE STAMPA ---------------***
017200000000     C***------------------------------------------------***
017300000000     CSR   RUTSTA        BEGSR                                                  *
017400870610     C     CONRIG        IFLT      60                                           *
017500870605     C     COM1          IFNE      MOCNOR                                       *
017600870605     C                   EXCEPT    TESTA1                                       *
017700870610     C                   ADD       4             CONRIG                         *
017800870605     C                   END                                                    *
017900870605     C                   END                                                    *
018000870610     C     CONRIG        IFGE      60                                           *
018100870605     C                   EXCEPT    TESTA                                        *
018200870605     C                   EXCEPT    TESTA1                                       *
018300870610     C                   Z-ADD     7             CONRIG                         *
018400870605     C                   END                                                    *
018500000000     C                   EXCEPT    DETAIL                                       *
018600870610     C                   ADD       1             CONRIG                         *
018700000000     C                   ENDSR                                                  *
018800870527     O*--------------------------------------------------------------*
018900870527     O*     TESTATA                                                  *
019000870527     O*--------------------------------------------------------------*
019100870602     OOCPRTSTD  E            TESTA            01
019200870527     O                       SK1                132
019300870527     O                       UDATE              118 '  /  /  '
019400870527     O                       PAGE          Z    132
019500870527     O          E            TESTA       1
019600870527     O                       SK2                132
019700870527     O          E            TESTA       1
019800870527     O                       SK3                132
019900870527     O                       KNMUS              132
020000870602     O          E            TESTA1      2
020100870527     O                                           16 'Cliente :'
020200870527     O                       MOCCLI              24
020300870527     O                                           26 '-'
020400870602     O                       RAGSOC              57
020500870527     O                                           73 'N. ORDINE :'
020600870918     O                       MOCNOR        Z     79
020700870602     O                                           88 'DEL'
020800990127     O                       DOR                102 '  /  /    '
020900870602     O          E            TESTA1      0  0
021000870527     O                                           16 'Cliente :'
021100870527     O                       MOCCLI              24
021200870527     O                                           26 '-'
021300870602     O                       RAGSOC              57
021400870527     O                                           73 'N. ORDINE :'
021500870918     O                       MOCNOR        Z     79
021600870602     O                                           88 'DEL'
021700990127     O                       DOR                102 '  /  /    '
021800870602     O          E            TESTA1      2
021900870527     O                       SK4                132
022000870601     O*--------------------------------------------------------------*
022100870601     O*     DETTAGLIO                                                *
022200870601     O*--------------------------------------------------------------*
022300870527     O          E            DETAIL      1
022400870527     O                       PAGE1              132
022500870527     O                       SK5                132
022600870929     O                       MOCMAT              13
022700870929     O                       DESMAT              34
022800870929     O                       MOCQTA        2     48
022900870929     O                       MOCASS        2     63
023000870929     O                       MOCFLS              65
023100870918     O                       MOCLIS              69
023200870918     O                       MOCCLP              71
023300870602     O                       MOCPRZ        2     86
023400870602     O                       MOCSC1        2     95
023500870602     O                       MOCSC2        2    104
023600870601     O                       MOCSC3        2    113
023700990127     O                       DCP                127 '  /  /    '
023800870601     O*--------------------------------------------------------------*
023900870601     O*     TOTALE  ORDINE                                           *
024000870601     O*--------------------------------------------------------------*
024100870601     O          E            TOTALE      1
024200870601     O                       T1                 132
024300870601     O          E            TOTALE      1
024400870601     O                       T2                 132
024500870605     O          E            TOTALE      0
024600870605     O                       T2                 132
024700870602     O          E            TOTALE      1
024800870602     O                       T3                 132
024900870605     O                       TOTQTA        2     46
025000870605     O                       TOTASS        2     63
025100870605     O                       VALORD        2     80
025200870605     O                       VALASS        2     98
025300870602     O          E            TOTALE      1
025400870602     O                       T1                 132
025500870601     O*--------------------------------------------------------------*
025600870601     O*     TOTALE  CLIENTE                                          *
025700870601     O*--------------------------------------------------------------*
025800870601     O          E            TOTCLI      1
025900870601     O                       TC1                132
026000870601     O          E            TOTCLI      1
026100870601     O                       TC2                132
026200870605     O          E            TOTCLI      0
026300870605     O                       TC2                132
026400870601     O          E            TOTCLI      1
026500870601     O                       TC3                132
026600870605     O                       CLIQTA        2     46
026700870605     O                       CLIASS        2     63
026800870605     O                       CLITOR        2     80
026900870605     O                       CLITAS        2     98
027000870602     O          E            TOTCLI      1
027100870601     O                       TC1                132
027200870915**   SK1
027300870527*-DESDIT-----------****-------------------------------------------SK01
027400870527-------------------------------------*** il XX/XX/XX      Pag. XXXSK01
027500870527**   SK2
027600870527                            ** Gest. Ordini Clienti : LISTA DI CONSK02
027700990317TROLLO SIMULAZIONE ASSEGNAZIONE **              PGM.:   TII306R   SK02
027800870527**   SK3
027900870527                    ***-------------------------------------------SK03
028000870527-------------------------------------***        Utente  XXXXXXXXXXSK03
028100870527**   SK4
028200870929*------ARTICOLO-------------------*Q.TA'ORDINATA*QTA' ASSEGNATA* *SK04
028300870918LIST.*-----PREZZO----*--------SCONTI------------* CONS.PREV.*     SK04
028400870602**   SK5
028500870915*
028600870915                                                            *
028700870601**   T1
028800870601*-----------------------------------------------------------------T1
028900870602------------------------------------------------------------*     T1
029000870601**   T2
029100870915                 T O T A L E     Q.TA' ORDINATA  Q.TA'ASSEGNATA  VT2
029200870915ALORE ORDINATO   VALORE ASSEGNATO                                 T2
029300870601**   T3
029400870915*
029500870915                                                            *
029600870601**   TC1
029700870601*-----------------------------------------------------------------
029800870602------------------------------------------------------------*
029900870601**   TC2
030000870915                TOTALE CLIENTE   Q.TA' ORDINATA  Q.TA'ASSEGNATA  VTC02
030100870915ALORE ORDINATO   VALORE ASSEGNATO
030200870601**   TC3
030300870915
030400870915

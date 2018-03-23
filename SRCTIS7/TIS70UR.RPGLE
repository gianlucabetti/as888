000100990908     H dftactgrp(*yes)
000200990908
000300990920     Ftivtf01l  if   e           k disk
000400011129     Ftabel00f  IF   E           K DISK                                         *Tabelle
000500020321     Ftntbe01l  IF   E           K DISK                                         *Tabelle
000600990908
000700011129     D*------------
000800011129     D* ARCHITETTURA
000900011129     D*------------
001000011129     D KPJBA         E DS
001100011129     D*------------
001200011129     D* RELAZIONE TABELLE <=> *PGM ASSOCIATI
001300011129     D*------------
001400020321     D PGMTAB          S             16    DIM(10) CTDATA PERRCD(1)
001500011129     D*------------
001600011129     D* RIDEFINIONE TABELLA "PGMTAB"
001700011129     D*------------
001800011129     D                 DS
001900020321     D COD                     1      2
002000011129     D PGM                     3     12
002100020321     D TTT                    13     13
002200020321     D TAB                    14     16
002300020321     D DSPGMTAB                1     16
002400011129     D*------------
002500011129     D* DS DI RIDEFINIZINE TABELLE VARIE
002600011129     D*------------
002700011129     D DS3K          E DS
002800011129     D DS3Q          E DS
002900011129     D DS4C          E DS
003000011129     D*------------
003100011129     D* VARIABILI DI WRK
003200011129     D*------------
003300011129     D ktbkut          S                   LIKE(tblkut)                         *lettura tabel00f
003400011129     D ktbcod          S                   LIKE(tblcod)
003500011129     D ktbkey          S                   LIKE(tblkey)
003600000626
003700000626
003800011129     C* Leggo il record dell'anagrafica tipi-file
003900011129     C                   EXSR      readtivtf
004000011129     C*
004100011129     C* Controllo la tabella associata
004200060130     C                   IF        vtffi1 <> *blanks
004300011129     C*
004400011129     C* Se richiesto solo controllo esistenza
004500011129     C                   IF        flgope = '0'
004600060925     C*
004700060925     C* Il controllo lo effettuo solo x i tipi file con abilitazioni specifiche x cliente
004800060925     C* (ad esempio il tipo file VX prevede 1 abilitazione nn x cliente bensì x logo)
004900060925     C                   IF        vtftip <> 'VX'
005000020321     C*
005100020321     C* Cerco il codice passato nella schiera delle associazioni tabelle/PGM
005200020321     C                   Z-ADD     1             i                 3 0
005300020321     C                   SETOFF                                       55
005400020321     C                   DOW       i<=%elem(PGMTAB)
005500020321     C                   IF        PGMTAB(i) > *blanks
005600020321     C                   EVAL      DSPGMTAB = PGMTAB(i)
005700060130     C                   IF        COD = vtffi1
005800020321     C                   SETON                                        55
005900020321     C                   LEAVE
006000020321     C                   ENDIF
006100020321     C                   ELSE
006200020321     C                   LEAVE
006300020321     C                   ENDIF
006400020321     C                   EVAL      i = i +1
006500020321     C                   ENDDO
006600020321     C                   IF        *IN55 = *ON
006700020321     C                   MOVEL     PGMTAB(i)     DSPGMTAB
006800020321     C* Se richiesta tabella "vecchia" leggo su TABEL00F
006900020321     C                   IF        TTT = '1'
007000011129     C                   Z-ADD     1             ktbkut
007100020321     C                   MOVEL(p)  TAB           ktbcod
007200011129     C                   MOVEL(p)  wrkkscnum     ktbkey
007300011129     C     keytab        CHAIN     tabel00f
007400011129     C                   IF        %found(tabel00f)
007500011129     C                   IF        tblflg = *blanks
007600060130     C                   IF        vtffi1 = '3K'
007700011129     C                   MOVEL     tbluni        ds3k
007800011129     C                   IF        vtftip = 'VC' AND §3KCSD <> 'WW'
007900011129     C                   EVAL      flgope = '2'
008000011129     C                   ENDIF
008100011129     C                   IF        vtftip = 'VG' AND §3KGSD <> 'WW'
008200011129     C                   EVAL      flgope = '2'
008300011129     C                   ENDIF
008400011129     C                   ENDIF
008500060130     C                   IF        vtffi1 = '3Q'
008600011129     C                   MOVEL     tbluni        ds3q
008700011129     C                   IF        §3QCSD <> 'WW'
008800011129     C                   EVAL      flgope = '2'
008900011129     C                   ENDIF
009000011129     C                   ENDIF
009100060130     C                   IF        vtffi1 = '4C'
009200011129     C                   MOVEL     tbluni        ds4c
009300011129     C                   IF        vtftip = 'VS' AND §4CTSD <> 'WW'
009400011129     C                   EVAL      flgope = '2'
009500011129     C                   ENDIF
009600060203     C                   IF        vtftip = 'VF' AND
009700060206     C                             §4CFSD <> 'WW' AND §4CFSD <> 'W2'
009800011129     C                   EVAL      flgope = '2'
009900011129     C                   ENDIF
010000011129     C                   ENDIF
010100011129     C                   ELSE
010200011129     C                   EVAL      flgope = '2'
010300011129     C                   ENDIF
010400011129     C                   ELSE
010500011129     C                   EVAL      flgope = '2'
010600011129     C                   ENDIF
010700060206     C* Altrimenti leggo sulle "nuove" tabelle (TNTBE01L) con codice cliente d 7 byte
010800020321     C                   ELSE
010900060206     C                   IF        TTT = '2'
011000020321     C                   MOVEL     TAB           tbecod
011100020321     C                   MOVEL(p)  wrkkscnum     tbeke1
011200020321     C     keytbe        CHAIN     tntbe01l
011300020321     C                   IF        not %found(tntbe01l)
011400020321     C                   EVAL      flgope = '2'
011500060206     C                   ENDIF
011600060206     C                   ELSE
011700060410     C* Altrimenti leggo sulle "nuove" tabelle (TNTBE01L) con codice cliente d 8 byte
011800060206     C                   IF        TTT = '3'
011900060206     C                   MOVEL     TAB           tbecod
012000060206     C                   MOVEL(p)  wrkksc        tbeke1
012100060206     C     keytbe        CHAIN     tntbe01l
012200060206     C                   IF        not %found(tntbe01l)
012300060206     C                   EVAL      flgope = '2'
012400060206     C                   ENDIF
012500060206     C                   ENDIF
012600020321     C                   ENDIF
012700020321     C                   ENDIF
012800020321     C                   ENDIF
012900060925     C*
013000060925     C                   ENDIF
013100011129     C*
013200011129     C                   ELSE
013300011129     C*
013400011129     C                   Z-ADD     1             i                 3 0
013500011129     C                   DOW       i<=%elem(PGMTAB)
013600011129     C                   IF        PGMTAB(i) > *blanks
013700011129     C                   EVAL      DSPGMTAB = PGMTAB(i)
013800060130     C                   IF        COD = vtffi1
013900060206     C                   IF        TTT = '3'
014000060206     C                   MOVEL     wrkksc        kpjbu
014100060206     C                   ELSE
014200060206     C                   MOVEL     wrkkscnum     kpjbu
014300060206     C                   ENDIF
014400011129     C                   CALL      PGM
014500011129     C                   PARM                    kpjba
014600011129     C                   LEAVE
014700011129     C                   ENDIF
014800011129     C                   ENDIF
014900011129     C                   EVAL      i = i +1
015000011129     C                   ENDDO
015100011129     C                   ENDIF
015200010615     C*
015300011129     C                   ENDIF
015400000626     C*
015500011129     C                   SETON                                        LR
015600000626
015700000623
015800011129      /TITLE Lettura TIVTF00F.
015900011129     C     readtivtf     BEGSR
016000000623     C*
016100011129     C     vtftip        CHAIN     tivtf01l
016200000623     C*
016300011129     C                   IF        %found(tivtf01l)
016400000623     C*
016500011129     C                   ENDIF
016600000630     C*
016700011129     C                   ENDSR
016800000711
016900000711
017000000626
017100990908      /TITLE Operazioni iniziali.
017200011129     C     *inzsr        BEGSR
017300990908     C*
017400011129     C     *entry        PLIST
017500011129     C                   PARM                    kpjba
017600011129     C                   PARM                    vtftip
017700011129     C                   PARM                    wrkksc            8
017800011129     C                   PARM                    flgope            1
017900011129     C*
018000020321     C* Rendo il codice cliente numerico lungo 7,0
018100011129     C                   MOVE      wrkksc        wrkkscnum         7 0
018200011129     C*
018300011129     C* chiavi di lettura
018400011129     C     keytab        KLIST                                                  *tabel00f - parziale
018500011129     C                   KFLD                    ktbkut                          -utente
018600011129     C                   KFLD                    ktbcod                          -tabella
018700011129     C                   KFLD                    ktbkey                          -codice
018800020321     C*
018900020321     C     keytbe        KLIST
019000020321     C                   KFLD                    tbecod
019100020321     C                   KFLD                    tbeke1
019200990908     C*
019300011129     C                   ENDSR
019400011129** PGMTAB **
0195000203213CTRTB28R   13C
0196000203213KTRTB40R   13K
0197000203213QTRTB83R   13Q
0198000203214CTRTB44R   14C
019900020321DKTNTB10R   2DKC
020000060206VPTNTB52R   3VAP
020100060410DCTNTB55R   2DSC
020200060925VXTNTB35R   2AR8
020300011129
020400011129

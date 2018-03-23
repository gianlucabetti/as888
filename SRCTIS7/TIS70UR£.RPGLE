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
004600020321     C*
004700020321     C* Cerco il codice passato nella schiera delle associazioni tabelle/PGM
004800020321     C                   Z-ADD     1             i                 3 0
004900020321     C                   SETOFF                                       55
005000020321     C                   DOW       i<=%elem(PGMTAB)
005100020321     C                   IF        PGMTAB(i) > *blanks
005200020321     C                   EVAL      DSPGMTAB = PGMTAB(i)
005300060130     C                   IF        COD = vtffi1
005400020321     C                   SETON                                        55
005500020321     C                   LEAVE
005600020321     C                   ENDIF
005700020321     C                   ELSE
005800020321     C                   LEAVE
005900020321     C                   ENDIF
006000020321     C                   EVAL      i = i +1
006100020321     C                   ENDDO
006200020321     C                   IF        *IN55 = *ON
006300020321     C                   MOVEL     PGMTAB(i)     DSPGMTAB
006400020321     C* Se richiesta tabella "vecchia" leggo su TABEL00F
006500020321     C                   IF        TTT = '1'
006600011129     C                   Z-ADD     1             ktbkut
006700020321     C                   MOVEL(p)  TAB           ktbcod
006800011129     C                   MOVEL(p)  wrkkscnum     ktbkey
006900011129     C     keytab        CHAIN     tabel00f
007000011129     C                   IF        %found(tabel00f)
007100011129     C                   IF        tblflg = *blanks
007200060130     C                   IF        vtffi1 = '3K'
007300011129     C                   MOVEL     tbluni        ds3k
007400011129     C                   IF        vtftip = 'VC' AND §3KCSD <> 'WW'
007500011129     C                   EVAL      flgope = '2'
007600011129     C                   ENDIF
007700011129     C                   IF        vtftip = 'VG' AND §3KGSD <> 'WW'
007800011129     C                   EVAL      flgope = '2'
007900011129     C                   ENDIF
008000011129     C                   ENDIF
008100060130     C                   IF        vtffi1 = '3Q'
008200011129     C                   MOVEL     tbluni        ds3q
008300011129     C                   IF        §3QCSD <> 'WW'
008400011129     C                   EVAL      flgope = '2'
008500011129     C                   ENDIF
008600011129     C                   ENDIF
008700060130     C                   IF        vtffi1 = '4C'
008800011129     C                   MOVEL     tbluni        ds4c
008900011129     C                   IF        vtftip = 'VS' AND §4CTSD <> 'WW'
009000011129     C                   EVAL      flgope = '2'
009100011129     C                   ENDIF
009200060203     C                   IF        vtftip = 'VF' AND
009300060206     C                             §4CFSD <> 'WW' AND §4CFSD <> 'W2'
009400011129     C                   EVAL      flgope = '2'
009500011129     C                   ENDIF
009600011129     C                   ENDIF
009700011129     C                   ELSE
009800011129     C                   EVAL      flgope = '2'
009900011129     C                   ENDIF
010000011129     C                   ELSE
010100011129     C                   EVAL      flgope = '2'
010200011129     C                   ENDIF
010300060206     C* Altrimenti leggo sulle "nuove" tabelle (TNTBE01L) con codice cliente d 7 byte
010400020321     C                   ELSE
010500060206     C                   IF        TTT = '2'
010600020321     C                   MOVEL     TAB           tbecod
010700020321     C                   MOVEL(p)  wrkkscnum     tbeke1
010800020321     C     keytbe        CHAIN     tntbe01l
010900020321     C                   IF        not %found(tntbe01l)
011000020321     C                   EVAL      flgope = '2'
011100060206     C                   ENDIF
011200060206     C                   ELSE
011300060410     C* Altrimenti leggo sulle "nuove" tabelle (TNTBE01L) con codice cliente d 8 byte
011400060206     C                   IF        TTT = '3'
011500060206     C                   MOVEL     TAB           tbecod
011600060206     C                   MOVEL(p)  wrkksc        tbeke1
011700060206     C     keytbe        CHAIN     tntbe01l
011800060206     C                   IF        not %found(tntbe01l)
011900060206     C                   EVAL      flgope = '2'
012000060206     C                   ENDIF
012100060206     C                   ENDIF
012200020321     C                   ENDIF
012300020321     C                   ENDIF
012400020321     C                   ENDIF
012500011129     C*
012600011129     C                   ELSE
012700011129     C*
012800011129     C                   Z-ADD     1             i                 3 0
012900011129     C                   DOW       i<=%elem(PGMTAB)
013000011129     C                   IF        PGMTAB(i) > *blanks
013100011129     C                   EVAL      DSPGMTAB = PGMTAB(i)
013200060130     C                   IF        COD = vtffi1
013300060206     C                   IF        TTT = '3'
013400060206     C                   MOVEL     wrkksc        kpjbu
013500060206     C                   ELSE
013600060206     C                   MOVEL     wrkkscnum     kpjbu
013700060206     C                   ENDIF
013800011129     C                   CALL      PGM
013900011129     C                   PARM                    kpjba
014000011129     C                   LEAVE
014100011129     C                   ENDIF
014200011129     C                   ENDIF
014300011129     C                   EVAL      i = i +1
014400011129     C                   ENDDO
014500011129     C                   ENDIF
014600010615     C*
014700011129     C                   ENDIF
014800000626     C*
014900011129     C                   SETON                                        LR
015000000626
015100000623
015200011129      /TITLE Lettura TIVTF00F.
015300011129     C     readtivtf     BEGSR
015400000623     C*
015500011129     C     vtftip        CHAIN     tivtf01l
015600000623     C*
015700011129     C                   IF        %found(tivtf01l)
015800000623     C*
015900011129     C                   ENDIF
016000000630     C*
016100011129     C                   ENDSR
016200000711
016300000711
016400000626
016500990908      /TITLE Operazioni iniziali.
016600011129     C     *inzsr        BEGSR
016700990908     C*
016800011129     C     *entry        PLIST
016900011129     C                   PARM                    kpjba
017000011129     C                   PARM                    vtftip
017100011129     C                   PARM                    wrkksc            8
017200011129     C                   PARM                    flgope            1
017300011129     C*
017400020321     C* Rendo il codice cliente numerico lungo 7,0
017500011129     C                   MOVE      wrkksc        wrkkscnum         7 0
017600011129     C*
017700011129     C* chiavi di lettura
017800011129     C     keytab        KLIST                                                  *tabel00f - parziale
017900011129     C                   KFLD                    ktbkut                          -utente
018000011129     C                   KFLD                    ktbcod                          -tabella
018100011129     C                   KFLD                    ktbkey                          -codice
018200020321     C*
018300020321     C     keytbe        KLIST
018400020321     C                   KFLD                    tbecod
018500020321     C                   KFLD                    tbeke1
018600990908     C*
018700011129     C                   ENDSR
018800011129** PGMTAB **
0189000203213CTRTB28R   13C
0190000203213KTRTB40R   13K
0191000203213QTRTB83R   13Q
0192000203214CTRTB44R   14C
019300020321DKTNTB10R   2DKC
019400060206VPTNTB52R   3VAP
019500060410DCTNTB55R   2DSC
019600011129
019700011129
019800011129

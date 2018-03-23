000100990910     h DECEDIT('0,') DATEDIT(*DMY.)
000200990910
000300990914     fFNBLPA0T  IP   E             DISK
000400990914     fFIBLPA0T  O    E             DISK
000500990914     fTABEL00F  IF   E           K DISK
000600990910     fprtf198   O    F  198        PRINTER  oflind(*INOF)
000700990910
000800990914     d DS3A          E DS
000900990914
000901991126     c                   eval      BLPPKF = BLPPKB
001000990914     c                   WRITE     FIBLPa00
001100990914
001200990914      * Stampo se ci sono dati di BL2
001300990914     c                   IF        BL2SV1 <> *blanks  OR  BL2cei <> *blanks
001400990914     c                   except    stampa
001500990910     c                   ENDIF
001600990914
001700990914      * Stampo se cliente non codificato
001800990914     c                   move      BLPksc        W0040             4 0
001900990914     c                   IF        W0040 = 8888
002000990914     c                   except    stampa1
002100990914     c                   ENDIF
002200990914
002300990914      * Stampo se ASSEGNATO
002400990914     C                   movel     '1'           TBLKUT
002500990914     C                   movel     '3A'          tblCOD
002600990914     C                   movel(P)  BLPcbo        tblKEY
002700990914     C     ktab          chain(N)  TABEL                              31
002800990914     c                   IF        %Found
002900990914     C                   movel     TBLUNI        DS3A
003000990916     C                   IF        %subst(§3ATB1:1:1) = 'A'
003100990914     c                   except    stampa2
003200990914     c                   ENDIF
003300990914     c                   ENDIF
003400990910      *--------------------------------------------------------------------------------------------*
003500990914     C     KTAB          KLIST
003600990914     C                   KFLD                    TBLKUT
003700990914     C                   KFLD                    TBLCOD
003800990914     C                   KFLD                    TBLKEY
003900990914      *--------------------------------------------------------------------------------------------*
004000990914     OPRTF198   E            STAMPA      1
004100990914     o                                        +   2 'FNBLPA0T  con campi BL2'
004200990914     o                       BLPaas        Z  +   3
004300990914     o                       BLPlnp        Z  +   1
004400990914     o                       BLPnrs        Z  +   1
004500990914     o                       BLPnsp        Z  +   1
004600990914     o                       BL2sv1           +   3
004700990914     o                       BL2va1        2  +   1
004800990914     o                       BL2cei           +   1
004900990914     O          E            STAMPA1     1
005000990914     o                                        +   2 'FNBLPA0T con cliente non c-
005100990914     o                                              odificato'
005200990914     o                       BLPaas        Z  +   3
005300990914     o                       BLPlnp        Z  +   1
005400990914     o                       BLPnrs        Z  +   1
005500990914     o                       BLPnsp        Z  +   1
005600990914     o                       BLPksc        Z  +   3
005700990914     O          E            STAMPA2     1
005800990914     o                                        +   2 'FNBLPA0T sped. assegnato'
005900990914     o                       BLPaas        Z  +   3
006000990914     o                       BLPlnp        Z  +   1
006100990914     o                       BLPnrs        Z  +   1
006200990914     o                       BLPnsp        Z  +   1
006300990914     o                       BLPcbo           +   3

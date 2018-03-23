000100000000      *------------------------------------------------------------------------*
000200110419      *  Accoda tariffe da EXL (file GAITRAAZM/ECEVD) e le inserisce nel
000300110405      *  nel file ECEVD00F della libreria passata come parametro di INPUT
000400110406      *  NO UNITRAGRU
000500000000      *------------------------------------------------------------------------*
000600000000     H DECEDIT('0,') DATEDIT(*DMY.)
000700000000     F*------------------------------------------------------------------------*
000800000000     F* DATA BASE
000900000000     F*------------------------------------------------------------------------*
001000110418     Fprtf198   o    f  198        printer oflind(*inof)
001100000000     D*-------------------
001200000000     D* ARCHITETTURA
001300000000     D*-------------------
001400000000     D KPJBA         E DS
001500110506     D WLBDAT          DS                  INZ
001600110506     D  G02DAT                 1      8  0
001700110506     D  G02INV                 9     16  0
001800110506     D  G02ERR                17     17
001900110506     D  G02TGI                18     22  0
002000000000     C*------------------------------------------------------------------------*
002100000000     C* MAIN LINE
002200000000     C*------------------------------------------------------------------------*
002300110404     C/EXEC SQL
002400131128     C+ DELETE FROM ECEVD06L
002500110404     C/END-EXEC
002600110413     C                   IF        sqlcod < 0
002700110418     C                   seton                                        lr88
002800110413     c                   exsr      fine
002900110413     c                   return
003000110413     C                   ENDIF
003100110405     C/EXEC SQL
003200131128     C+ INSERT INTO ECEVD06L SELECT * FROM gaitraazm/ecevd
003300121126     C+ WHERE F1 <> 0
003400110405     C/END-EXEC
003500110413     C                   IF        sqlcod < 0
003600110418     C                   seton                                        lr89
003700110413     c                   exsr      fine
003800110413     c                   return
003900110413     C                   ENDIF
004000110506     c                   seton                                        lr
004100110506     C*------------------------------------------------------------------------*
004200110506     C* fine - stampa
004300110506     C*------------------------------------------------------------------------*
004400110506     C     fine          BEGSR
004500110506     C*
004600110506     c                   if        *inof
004700110506     c                   except    testa
004800110506     c                   setoff                                       of
004900110506     c                   end
005000110506     c   89              except    errkey
005100110506     c   88              except    errlck
005200110506     C*
005300110506     C                   ENDSR
005400000000     C*------------------------------------------------------------------------*
005500000000     C*  *INZSR - ROUTINE INIZIALE
005600000000     C*------------------------------------------------------------------------*
005700000000     C     *INZSR        BEGSR
005800000000     C*
005900000000     C* RICEVIMENTO PARAMETRI
006000000000     C     *ENTRY        PLIST
006100000000     C                   PARM                    KPJBA
006200110412     C                   MOVEL     KPJBU         lib              10
006300110405     c* questo per sicurezza, in buona è meglio farlo a mano dopo che
006400110405     c* abbiamo già ricevuto senza errori
006500110506     c                   if        lib = 'UNITRAGRU' or
006600110506     c                             lib = ' '
006700110405     c                   return
006800110405     c                   end
006900110412     c                   seton                                        of
007000110506     C* REPERISCE LA DATA CORRENTE
007100110506     C                   TIME                    N14              14 0
007200110506     C                   MOVE      N14           N8                8 0
007300110506     C                   MOVEl     N14           N6                6 0
007400110506     C                   Z-ADD     N8            G02DAT
007500110506     C                   Z-ADD     *ZEROS        G02INV
007600110506     C                   MOVEL     '0'           G02ERR
007700110506     C                   CALL      'XSRDA8'
007800110506     C                   PARM                    WLBDAT
007900110506     C                   Z-ADD     G02INV        DATCOR            8 0
008000110506     c                   endsr
008100110419     oprtf198   e            testa             1
008200110419     o                                          100 'Importa tariffe da EXL -
008300110419     o                                              nella libreria'
008400110412     o                       lib                + 1
008500110419     o                       n8                 +10 '  /  /    '
008600110419     o                       n6                 + 2 '  :  :  '
008700110419     o                       page               198
008800110418     o          e            errkey      1
008900110413     o                                          + 0 'Probabili chiavi duplicate'
009000110418     o          e            errlck      1
009100110413     o                                          + 0 'Probabile file allocato'

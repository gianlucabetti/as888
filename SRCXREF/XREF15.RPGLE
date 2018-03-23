000100000505      /TITLE Ricerca direttiva /COPY
000200000505
000300000505     H DFTACTGRP(*YES)
000400000505
000500000505     D PrmSrcDta       S            100
000600000505     D PrmLib          S             10
000700000505     D PrmFil          S             10
000800000505     D PrmMbr          S             10
000900000505     D PrmEsito        S              1
001000000505     D PosB            S              5U 0
001100000505     D PosV            S              5U 0
001200000505     D WrkLibFilMbr    S             37
001300000505     D                                     VARYING
001400000505
001500000505     C     *ENTRY        PLIST
001600000505     C                   PARM                    PrmSrcDta
001700000505     C                   PARM                    PrmLib
001800000505     C                   PARM                    PrmFil
001900000505     C                   PARM                    PrmMbr
002000000505     C                   PARM                    PrmEsito
002100000505      * Pulisco i parametri da restituire.
002200000505     C                   CLEAR                   PrmLib
002300000505     C                   CLEAR                   PrmFil
002400000505     C                   CLEAR                   PrmMbr
002500000505     C                   EVAL      PrmEsito = '1'
002600000505      *
002700000505     C                   IF        %SUBST(PrmSrcDta:7:5) = '/COPY'
002800000505      * I riferimenti del membro da copiare sono da posizione 13 a 49.
002900000505      * Il formato è "Libreria/File,Membro".
003000000505     C                   EVAL      WrkLibFilMbr = %TRIM(%SUBST(PrmSrcDta:13:37))
003100000505      * Cerco la libreria.
003200000505     C                   EVAL      PosB = %SCAN('/':WrkLibFilMbr)
003300000505     C                   IF        PosB > 1
003400000505     C                   EVAL      PrmLib =
003500000505     C                             %SUBST(WrkLibFilMbr:1:(PosB-1))
003600000505     C                   ELSE
003700000505     C                   EVAL      PrmLib = '*LIBL'
003800000505     C                   ENDIF
003900000505      * Cerco il file.
004000000505     C                   EVAL      PosV = %SCAN(',':WrkLibFilMbr)
004100000505     C                   IF        PosV > 1
004200000505     C                   EVAL      PrmFil =
004300000505     C                             %SUBST(WrkLibFilMbr:(PosB+1):(PosV-1-PosB))
004400000505     C                   ELSE
004500000505     C                   ENDIF
004600000505      * Cerco il membro.
004700000505     C                   EVAL      PrmMbr =
004800000505     C                             %SUBST(WrkLibFilMbr:(PosV+1)
004900000505     C                             :(%LEN(WrkLibFilMbr)-PosV))
005000000505      *
005100000505     C                   EVAL      PrmEsito = '0'
005200000505      *
005300000505     C                   ENDIF
005400000505      *
005500000505     C                   EXSR      Uscita
005600000505
005700000505      * Operazioni finali
005800000505     C     Uscita        BEGSR
005900000505      *
006000000505     C                   RETURN
006100000505      *
006200000505     C                   ENDSR

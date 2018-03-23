000100000000     H*------------------------------------------------------------------------*
000200040309     H* STATISTICA STAGIONALITA' GENERA WRK FILE
000300000000     H*------------------------------------------------------------------------*
000400040309     H DECEDIT('0,') DATEDIT(*YMD.)
000500040311      *----
000600040415     FCNCLP00F  IF   E           K DISK
000700040415     D*-------------------
000800040415     d tibs10ds      E DS
000900000000     C*------------------------------------------------------------------------*
001000000000     C* RICEVIMENTO PARAMETRI
001100040415     C*------------------------------------------------------------------------*
001200000000     C     *ENTRY        PLIST
001300040415     C                   PARM                    CodKUT
001400040415     C                   PARM                    CodKCC
001500040415     C                   PARM                    CodKSC
001600040415     C                   PARM                    CODUNI            7 0
001700040415     C                   PARM                    Tipo_Unif         1
001800040415      *
001900040415     C     *like         define    clpkut        CodKUT
002000040415     C     *like         define    clpkcc        CodKCC
002100040415     C     *like         define    clpksc        CodKSC
002200000000     C*
002300040415     C     keyCLP        KLIST
002400040415     C                   KFLD                    codKUT
002500040415     C                   KFLD                    codKCC
002600040415     C                   KFLD                    CODUNI
002700040415     C*
002800040415     C* Ricerca codice unificante
002900040415     c                   clear                   tibs10ds
003000040415     C*
003100040415     c                   clear                   CODUNI
003200040415     c                   clear                   Tipo_Unif
003300040415      *
003400040415     c                   movel     'ST'          d10TLE
003500040415     c                   z-add     codKSC        d10COD
003600040415     c                   movel     'P'           d10PAF
003700040415     c                   z-add     *date         d10DRF
003800040415     c                   CALL      'TIBS10R'
003900040415     c                   PARM                    tibs10ds
004000040415     c                   if        d10ERR = *blank
004100040415     c                   z-add     d10COP        CODUNI
004200040415     c                   else
004300040415     c                   z-add     codKSC        CODUNI
004400040415     c                   end
004500040415     C*
004600040415     c     keyCLP        chain     CnCLP00F
004700040415     c                   if        %Found(CnCLP00F)
004800040415     c                   move      clpclv        Tipo_Unif
004900040415     c                   end
005000040312     C*
005100040415     c                   seton                                        RT
005200040415     C*-----------------------------------------------------------------------------

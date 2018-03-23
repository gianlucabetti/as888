000100121004      *PARMS OPTION(*NOXREF) TGTRLS(*CURRENT)
000200121004     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000300121004
000400121004      *---------------------------------------------------------------*
000500121004
000600121004     FWFSER00F  IF   E             DISK
000700121004     FTABEL00F  IF   E           K DISK
000800121004     fazorg01l  if   e           k disk
000900121004     FWFSER10F  O    E             DISK
001000121004
001100121004
001200121004     d kpjba         e ds
001300121004
001400121004     d ds17          e ds
001500121004     d ds05          e ds
001600121004     d codut           s              1  0 Inz(1)
001700121004     D kcod            s                   like(tblcod)
001800121004     D kkey            s                   like(tblkey)
001900121004
002000121004     C     *entry        plist
002100121004     C                   parm                    kpjba
002200121004     c*
002300121004     C     KTAB          KLIST
002400121004     C                   KFLD                    CODUT
002500121004     C                   KFLD                    KCOD
002600121004     C                   KFLD                    KKEY
002700121004      *
002800121004
002900121004Do  1c                   Do        *hival
003000121004      *
003100121004     c                   clear                   wfser100
003200121004      *
003300121004     c                   read      wfser00f
003400121004      *
003500121004     c                   if        %eof(wfser00f)
003600121004     c                   leave
003700121004     c                   endif
003800121004      * recupero la filiale del cliente intestazione fattura
003900121004     c                   movel     sercfa        serfil
004000121004     c* reperisco cod. distretto e area
004100121004     c     serfil        chain     azorg01l
004200121004     C* Decodifico distretto
004300121004     C                   EXSR      DECDST
004400121004     C* Decodifico area
004500121004     C                   EXSR      DECCAR
004600121004     c* decodifica p.o.
004700121004     c                   movel     orgdes        serfild
004800121004     c                   movel     orgfl3        serdit
004900121004     c                   movel     §17des        serditd
005000121004     c                   z-add     orgcar        serare
005100121004     c                   movel     §05des        serared
005200121004
005300121004     c                   write     wfser100
005400121004
005500121004     c                   enddo
005600121004
005700121004     c                   seton                                        lr
005800121004     C*****************************************************************
005900121004     C* DECODIFICA CODICE DISTRETTO
006000121004     C*****************************************************************
006100121004     C     DECDST        BEGSR
006200121004     C* Chaino tabella 17 per decodifica
006300121004     C                   MOVEL     '17'          KCOD
006400121004     C                   MOVEL(P)  ORGFL3        KKEY
006500121004     c                   clear                   ds17
006600121004     C     KTAB          CHAIN     TABEL00F                           30
006700121004    2C     *IN30         IFEQ      *OFF
006800121004     C                   MOVEL     TBLUNI        DS17
006900121004     c                   endif
007000121004     C                   ENDSR
007100121004     C*****************************************************************
007200121004     C* DECODIFICA CODICE AREA
007300121004     C*****************************************************************
007400121004     C     DECCAR        BEGSR
007500121004     C* Chaino tabella 05 per decodifica
007600121004     C                   MOVEL     '05'          KCOD
007700121004     C                   MOVEL(P)  ORGCAR        KKEY
007800121004     c                   clear                   ds05
007900121004     C     KTAB          CHAIN     TABEL00F                           30
008000121004    2C     *IN30         IFEQ      *OFF
008100121004     C                   MOVEL     TBLUNI        DS05
008200121004     c                   endif
008300121004     C                   ENDSR

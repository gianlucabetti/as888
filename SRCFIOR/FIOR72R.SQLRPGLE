000100070731     H DECEDIT('0,') DATEDIT(*DMY.) option(*nodebugio)
000200070731     h dftactgrp(*no) actgrp(*caller)
000300070731     H* FIOR72R *----------------------------------------------------*
000400070731     H*         Aggiorno valori assegnato su FIDST
000500070731     H*--------------------------------------------------------------*
000600070913     FFior63p   O    E             PRINTER
000700070731      *--------------------------------------------------------------*
000800070731      *   DS
000900070731      *--------------------------------------------------------------*
001000070731     ddtaiso           s               d
001100070731     ddtaeur           s               d   datfmt(*eur)
001200070731     D ddatiute      e ds
001300070731     D fidstds       e ds                  extname(fidst00F)
001400070731     D azuteds       e ds                  extname(AZUTE00F)
001500070731     D tibs34ds      E DS                  inz
001600070731     D KPJBA         E DS
001700070731     D Fior68ds      E DS
001800070731     D Fior67ds      E DS
001900070731     C*--------------------------------------------------------------*
002000070731     C*  Ciclo principale
002100070731     C*--------------------------------------------------------------*
002200070731      * leggo tutte le distinte
002300070731     C/EXEC SQL
002400070731     C+ DECLARE A1 CURSOR FOR SELECT fidst09l.*  FROM
002500070731     C+ fidst09l WHERE
002600070731     C+ dstdfv=:or68dfvi and dstfgs = :or68fgsi and dstnpg = 4
002700070731     C+ and dstatb = ' '
002800070731     C/END-EXEC
002900070731
003000070731     C/EXEC SQL
003100070731     C+ OPEN a1
003200070731     C/END-EXEC
003300070731
003400070731     C                   do        *hival
003500070731
003600070731     C/EXEC SQL
003700070731     C+ FETCH NEXT FROM a1 INTO :fidstds
003800070731     C/END-EXEC
003900070731
004000070731     C                   select
004100070731     C                   when      sqlcod = 100
004200070731     c                   leave
004300070731     C                   WHEN      SqlCod < 0
004400070731     c* record allocato
004500070731     c                   if        sqlcod <> -913
004600070731     C                   seton                                        H1
004700070731     c                   end
004800070731     c                   exsr      srstampa
004900070731     c                   exsr      srfine
005000070731     c                   other
005100070731     C* prestazione mattina/pomeriggio
005200070731     c                   if        dstfpp <> or68fppi and or68fppi <> ' '
005300070731     c                   iter
005400070731     c                   end
005500070731     C* parcel/messaggerie
005600070731     c                   if        (or68tpmi= 'P' and dsttpm <>' ') or
005700070731     c                             (or68tpmi= 'M' and dsttpm <>'M')
005800070731     c                   iter
005900070731     c                   end
006000070731
006100070731     c                   clear                   fior67ds
006200070731     c                   eval      or67FGSI = dstfgs
006300070731     c                   eval      or67DFVI = dstdfv
006400070731     c                   eval      or67NFVI = dstnfv
006500070731     c                   eval      or67cmtI = 'N'
006600070731     c                   eval      kpjbu = FIOR67ds
006700070731     c                   call      'FIOR67R'
006800070731     c                   parm                    kpjba
006900070731     c                   eval      FIOR67ds = kpjbu
007000070731     c                   if        or67erro <>' '
007100070731     c                   seton                                        81
007200070731     c                   exsr      srstampa
007300070731     c                   iter
007400070731     c                   end
007500070731     C                   endsl
007600070731     C*
007700070731     C                   enddo
007800070731
007900070731     C/EXEC SQL
008000070731     C+ CLOSE a1
008100070731     C/END-EXEC
008200070731
008300070731     c                   if        or68cmti= 'S'
008400070731     c                   commit
008500070731     c                   end
008600070731     c                   eval      or68pag = pages
008700070731     c                   eval      or68riga= riga
008800070731     c                   eval      kpjbu = fior68ds
008900070731
009000070731     c                   seton                                        LR
009100070731      *------------------------------------------------------------------------*
009200070731      * stampa
009300070731      *------------------------------------------------------------------------*
009400070731     c     srstampa      BEGSR
009500070731     c                   move      or68dfvi      dtaiso
009600070731     c                   move      dtaiso        dtaeur
009700070731     c                   move      dtaeur        prtdfv
009800070731      * PAGINA IN STAMPA
009900070731     c                   IF        *IN80 OR RIGA >=60
010000070731     c                   SETON                                        80
010100070731     c                   ADD       1             PAGES
010200070731     c                   Z-ADD     3             RIGA              3 0
010300070731     c                   END
010400070731
010500070731     c   80              write     testa
010600070731     c                   setoff                                       80
010700070731     c   81              write     riga3
010800070731     c   81              add       1             riga
010900070731     c   82              write     riga4
011000070731     c   82              add       1             riga
011100070731     c                   setoff                                       8182
011200070731
011300070731     c                   endsr
011400070731      **************************************************************************
011500070731      * esco con codice d'errore
011600070731      **************************************************************************
011700070731     c     srfine        BEGSR
011800070731
011900070731     c                   if        or68cmti= 'S'
012000070731     c                   rolbk
012100070731     c                   end
012200070731
012300070731     c                   eval      or68erro= '1'
012400070731     c                   eval      kpjbu = fior68ds
012500070731     c                   seton                                        lr
012600070731     c                   return
012700070731
012800070731     c                   endsr
012900070731      *--------------------------------------------------------------*
013000070731      *  Operazioni iniziali
013100070731      *--------------------------------------------------------------*
013200070731     c     *INZSR        BEGSR
013300070731
013400070731     C     *ENTRY        PLIST
013500070731     C                   PARM                    KPJBA
013600070731     C                   MOVEL(P)  KPJBU         Fior68ds
013700070731     c                   eval      pages = or68pag
013800070731     c                   eval      riga = or68riga
013900070731     c                   clear                   or68erro
014000070731
014100070731     c     *dtaara       define    §azute        azuteds
014200070731     c     *dtaara       define    §datiute      ddatiute
014300070731     c                   in(E)     *dtaara
014400070731     c                   IF        %Error  or  RSUT = *blanks
014500070731     c                   call      'TIBS34R'
014600070731     c                   parm                    Tibs34Ds
014700070731     c                   in        *dtaara
014800070731     c                   ENDIF
014900070731     c                   ENDSR

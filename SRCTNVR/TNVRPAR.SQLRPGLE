000100100729     H DEBUG DECEDIT('0,') DATEDIT(*ymd/)
000200100802     ftabel00f  uf   e           K disk
000300100730     fqsysprt   o    f  132        printer oflind(*inof)
000400100802     Ddtaiso           s               d
000500100802     DSAVKSC           s                         LIKE(UPAKSC)
000600100802     doggi             s               d         datfmt(*eur)
000700100802     ddtaeur           s               d         datfmt(*eur)
000800100802     ddtadtr           s              8
000900100802     d data            ds
001000100802     d  aa                     1      2
001100100802     d  aacom                  1      2  0
001200100802     d  mm                     3      4
001300100802     d  mmcom                  3      4  0
001400100802     d  gg                     5      6
001500100730     D KPJBA         E DS
001600100802     D WFUPADS       E DS                  EXTNAME(WFUPA00F)
001700100324     c*
001800100730     C     *ENTRY        PLIST
001900100730     C                   PARM                    KPJBA
002000100802     C*
002100100802     C     KTAB          KLIST
002200100802     C                   KFLD                    TBLKUT
002300100802     C                   KFLD                    TBLCOD
002400100802     C                   KFLD                    TBLKEY
002500100802     C     KTAB1         KLIST
002600100802     C                   KFLD                    TBLKUT
002700100802     C                   KFLD                    TBLCOD
002800100802     C                   EVAL      TBLKUT = 1
002900100802     C                   EVAL      TBLCOD = '7U'
003000100802     C*
003100100730     c                   seton                                        of
003200100730     c*
003300100802     c                   move      *date         dtaiso
003400100802     c                   move      dtaiso        oggi
003500100324     C/EXEC SQL
003600100802     C+ DECLARE E1 CURSOR FOR SELECT UPAKSC, UPATAR, SUM(UPATSPED) FROM
003700100802     C+ WFUPA00f GROUP BY UPAKSC, UPATAR HAVING SUM(UPATSPED)=0 ORDER BY
003800100802     C+ UPAKSC, UPATAR DESC
003900100802     C/END-EXEC
004000100324     c*
004100100324     C/EXEC SQL
004200100324     C+ OPEN e1
004300100324     C/END-EXEC
004400100324
004500100324     C                   DO        *HIVAL
004600100324
004700100324     C/EXEC SQL
004800100802     C+ FETCH NEXT FROM e1 INTO :UPAKSC, :UPATAR, :UPATSPED
004900100324     C/END-EXEC
005000100324
005100100324     C                   IF        SQLCod = 100 or sqlcod < 0
005200100324     C                   LEAVE
005300100324     C                   ENDIF
005400100802     C*
005500100802     C                   IF        UPAKSC = SAVKSC
005600100802     C                   ITER
005700100802     C                   END
005800100802     C                   IF        UPATAR = 'S'
005900100802     C                   MOVE      UPAKSC        SAVKSC
006000100802     C                   ITER
006100100802     C                   END
006200100802     C                   MOVEL(P)  UPAKSC        TBLKEY
006300100324     c*
006400100802     c     KTAB          SETLL     tabel00f
006500100802     C                   DO        *HIVAL
006600100802     c     KTAB1         READE     tabel00f
006700100802     C                   IF        %EOF(TABEL00F)
006800100802     C                   LEAVE
006900100802     C                   END
007000100802     c                   movel     tbldtr        data
007100100802     c                   if        aacom = uyear
007200100802     c                   iter
007300100802     c                   end
007400100802     c                   eval      aacom = uyear - 1
007500100802     c                   if        aacom = 9  and
007600100802     c                             (mmcom > umonth)
007700100802     C                   ITER
007800100802     C                   END
007900100802     c*
008000100802     C                   MOVE      UPAKSC        UPACOM            7
008100100802     c                   if        %SUBST(TBLKEY: 1: 7)= UPACOM
008200100730     c                   if        %subst(kpjbu: 1: 1) = 'S'
008300100730     c                   eval      tblflg = '*'
008400100730     c                   update    tabel
008500100730     c                   end
008600100730     c                   exsr      stampa
008700100729     c                   end
008800100324     C                   ENDDO
008900100802     C                   ENDDO
009000100324
009100100324     C/EXEC SQL
009200100324     C+ CLOSE e1
009300100324     C/END-EXEC
009400100324     c*
009500100324     c                   seton                                        lr
009600100730     c**********************************************************************
009700100730     c     stampa        begsr
009800100730     c**********************************************************************
009900100730     c                   if        *inof
010000100730     c                   except    testa
010100100730     c                   setoff                                       of
010200100730     c                   end
010300100802     c                   move      tbldtr        data
010400100802     c                   eval      dtadtr = gg + '/' + mm + '/' + aa
010500100730     c                   except    riga
010600100730     c                   endsr
010700100730     oqsysprt   e            testa          2 02
010800100802     o                                         +  0 'Elenco abilitazioni Clien'
010900100802     o                                         +  0 'ti (7U) cancellabili perch'
011000100802     o                                         +  0 'è al'
011100100802     o                       oggi              +  1
011200100802     o                                         +  1 'non hanno tariffe e non sp'
011300100802     o                                         +  0 'ediscono da un anno'
011400100802     o                                         +  3 'TNVRPAR'
011500100730     o                       page          Z    132
011600100730     o                                          128 'Pag.'
011700100802     o          e            testa       2  1
011800100802     o                                         +  0 'Codice   Particolarità'
011900100802     o                                          110 'Data trasm.'
012000100802     o          e            riga        1
012100100802     o                       tblkey            +  0
012200100802     o                       tbluni            +  1
012300100802     o                       dtadtr            +  1

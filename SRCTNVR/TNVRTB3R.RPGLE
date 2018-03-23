000100931029     H DECEDIT('0,') DATEDIT(*DMY.)
000200020529      *--------------------------------------------------------------*
000300020807      * TNVRTB3R *                                                   *
000400020807      *         Spostato TABEL00F in GRU.                            *
000500020807      *         Accodamento della SDI101 alla GAI201                 *
000600020529      *--------------------------------------------------------------*
000700020807     Ftabel101  IF   E           K DISK    rename(tabel:tabel01) prefix(£)
000800020807     Ftabel00f  if a E           K DISK
000900020807     fqsysprt   o    f  132        printer oflind(*inof)
001000020807     D ds            E DS
001100020807     D ds101         E DS                  extname(tabel101) prefix(£)
001200020807     D ds201         E DS                  extname(tabel00f)
001300020807     d cod             s              2    dim(1000)
001400020807     d comme           s             10    inz
001500931102      *--------------------------------------------------------------*
001600931102      *  CICLO PRINCIPALE:                                           *
001700931102      *--------------------------------------------------------------*
001800020807     c                   do        x             y                 4 0
001900020807     c                   movel     cod(y)        tblcod
002000020807     C     ktab          setll     tabel101
002100020529     c                   do        *hival
002200020807     c     ktab          reade     tabel101
002300020807     c                   if        %eof(tabel101)
002400020529     c                   leave
002500020529     c                   end
002600020807     c     ktab1         chain     tabel00f
002700020807     c                   if        not %found(tabel00f)
002800020807     c                   eval      ds201 = ds101
002900020807     c                   write     tabel
003000020807     c                   else
003100020807     c                   if        £tblflg = tblflg and £tbluni = tbluni
003200020807     c                             and £tblftt = tblftt and £tblflt = tblflt
003300021106     c*                  eval      comme = 'UGUALE'
003400020807     c                   else
003500020807     c                   eval      comme = 'DIFFERENZE'
003700020807     c                   except    error
003701021106     c                   end
003800020807     c                   end
003900020722     c                   enddo
004000020807     c                   enddo
004100020529     c*
004200020510     C                   SETON                                        LR
004300020515      *--------------------------------------------------------------*
004400020515      *  *INZSR: Operazioni iniziali                                 *
004500020515      *--------------------------------------------------------------*
004600020515     C     *INZSR        BEGSR
004700931104      *--------------------------------------------------------------*
004800020722     C                   z-add     1             tblkut
004900020722     C     Ktab          KLIST
005000020807     C                   KFLD                    tblkut
005100020807     C                   KFLD                    tblcod
005200020807     c                   clear                   tblcod
005300020807     C     Ktab1         KLIST
005400020807     C                   KFLD                    £tblkut
005500020807     C                   KFLD                    £tblcod
005600020807     C                   KFLD                    £tblkey
005700020807     c* memorizza le tabelle da analizzare (no quelle
005800020807     c* di gruppo e no bartolini)
005900020807     c     ktab          setll     tabel101
006000020807     c                   do        *hival
006100020807     c     ktab          reade     tabel101
006200020807     c                   if        %eof(tabel101)
006300020807     c                   leave
006400020807     c                   end
006500021106     c*                  movel     £tbluni       ds
006600021106     c*                  if        §tip = ' '
006700020807     c                   add       1             x                 4 0
006800020807     c                   move      £tblkey       cod(x)
006900021106     c*                  end
007000020807     c                   enddo
007100931108     C                   ENDSR
007200020807     OQSYSPRT   E            error       1
007300020807     O                                        +   0 'Esiste già il record'
007400020807     o                       £tblkey          +   1
007500020807     O                                        +   1 'della tabella'
007600020807     O                       £tblcod          +   1
007700020807     O                       comme            +   1

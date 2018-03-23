000100931029     H DECEDIT('0,') DATEDIT(*DMY.)
000200020529      *--------------------------------------------------------------*
000300020722      * TNVRTB1R *                                                   *
000400020722      *         trascodifica di una tabella di TABEL00F              *
000500020722      *         ad una di TNTBE00f mettendo in KE1 la società e in   *
000600020722      *         KE2 il codice tabella di TABEL00F                    *
000700020529      *--------------------------------------------------------------*
000800020722     Ftabel00f  IF   E           K DISK
000900020722     Ftntbe00f  O    E           K DISK
001000020725     D ds4o          E DS
001100020725     D dy4o          E DS                  prefix(£)
001200020725     D KPJBA         E DS
001300020723     d prova                  98    100
001400020722     D TNVRTB1DS     E DS
001500931102      *--------------------------------------------------------------*
001600931102      *  CICLO PRINCIPALE:                                           *
001700931102      *--------------------------------------------------------------*
001800020722     C     ktab          setll     tabel00f
001900020529     c                   do        *hival
002000020722     c     ktab          reade     tabel00f
002100020722     c                   if        %eof(tabel00f)
002200020529     c                   leave
002300020529     c                   end
002400020726     c                   if        tbluni = ' '
002500020726     c                   iter
002600020726     c                   end
002700020722     c                   movel(p)  tblkey        tbeke2
002800020725     c                   movel(p)  tbluni        ds4o
002900020725     c                   movel     *all'0'       dy4o
003000020725     c                   move      §4occ1        £§4occ1
003100020725     c                   move      §4osc1        £§4osc1
003200020725     c                   movel     §4oco1        £§4oco1
003300020725     c                   move      §4occ2        £§4occ2
003400020725     c                   move      §4osc2        £§4osc2
003500020725     c                   movel     §4oco2        £§4oco2
003600020725     c                   move      §4occ3        £§4occ3
003700020725     c                   move      §4osc3        £§4osc3
003800020725     c                   movel     §4oco3        £§4oco3
003900020725     c                   move      §4occ4        £§4occ4
004000020725     c                   move      §4osc4        £§4osc4
004100020725     c                   movel     §4oco4        £§4oco4
004200020725     c                   movel     dy4o          tbeuni
004300020725     c* trsmetto in filiale solo se società 201, non in ambiente di prova
004400020725     c* e se su tabel era già ammessa la trasmissione
004500020725     c                   z-add     0             tbedtr
004600020725     c                   movel     ' '           tbeftr
004700020725     c                   if        tnvsoc = '201' and
004800020725     c                             prova <> 'PFP' and prova <> 'PSP'  and
004900020725     c                             (tblftt = '1' or tblftt = '2')
005000020725     c                   move      tblflt        tbeflt
005100020725     c                   movel     'S'           tbeftt
005200020725     c                   else
005300020725     c                   z-add     0             tbeflt
005400020725     c                   movel     *blanks       tbeftt
005500020725     c                   end
005600020722     c                   write     tntbe000
005700020722     c                   enddo
005800020529     c*
005900020510     C                   SETON                                        LR
006000020515      *--------------------------------------------------------------*
006100020515      *  *INZSR: Operazioni iniziali                                 *
006200020515      *--------------------------------------------------------------*
006300020515     C     *INZSR        BEGSR
006400931104      *--------------------------------------------------------------*
006500931104     C     *ENTRY        PLIST
006600931104     C                   PARM                    KPJBA
006700020722     c                   movel(p)  kpjbu         tnvrtb1ds
006800020722     c                   movel(p)  tnvda         tblcod
006900020722     c                   movel(p)  tnva          tBEcod
007000020722     c                   movel(p)  tnvsoc        tBEke1
007100020722     C                   z-add     1             tblkut
007200020722     C     Ktab          KLIST
007300020722     C                   KFLD                    tblkut
007400020722     C                   KFLD                    tblcod
007500931108     C                   ENDSR

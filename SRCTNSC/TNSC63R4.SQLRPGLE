000100990504     H DEBUG DECEDIT('0,') DATEDIT(*DMY/)
000200020409     h*****************************************************************
000300031023     h* Il pgm pulisce tutti i contrassegni già archiviati (CSBFIN=S) e
000400031023     h* non ancora cancellati perchè presenta la bolla
000500020409     h*****************************************************************
000600031023     ftitas30c  if   e           k disk
000700031023     ftncsb03l  uf   e           k disk
000800120214     ftncsm03l  uf   e           k disk
000900031023     ftncsa01l  uf   e           k disk
001000031023     ftncsg01l  uf   e           k disk
001100031023     ftncsv01l  uf   e           k disk
001200000607     D kpjba         e ds
001300031023     D tnsc60ds      e ds
001400031023     D tncsb         e ds                  extname(tncsb00f) inz
001500020702     c*
001600031023     c* leggo tutti i record di tncsb00f che hanno csbfin = 'S' e cioè
001700031023     c* che sono già stati archiviati ma non ancora puliti xchè presente
001800031023     c* la bolla e che hanno data spedizione minore o uguale alla data
001900031023     c* richiesta
002000020702     C/EXEC SQL
002100031023     C+ DECLARE A1 CURSOR FOR SELECT * FROM tncsb00f WHERE csbfin = 'S'
002200031023     C+ and decimal(concat(digits(csbaas),digits(csbmgs))) <= :d60sda
002300020702     C/END-EXEC
002400020702
002500020702     C/EXEC SQL
002600020702     C+ OPEN A1
002700020702     C/END-EXEC
002800020702
002900020702     C                   dou       *inlr
003000020702
003100020702     C/EXEC SQL
003200031023     C+ FETCH NEXT FROM A1 INTO :tncsb
003300020702     C/END-EXEC
003400020702
003500020702     C                   select
003600020702     C                   when      sqlcod = 100
003700020702     C                   seton                                        lr
003800020702     C                   when      sqlcod = 0
003900020702     C                   exsr      srdel
004000020702     C                   other
004100020702     C                   exsr      sqlerror
004200020702     C                   endsl
004300020702     C*
004400020702     C                   enddo
004500020702
004600020702     C/EXEC SQL
004700020702     C+ CLOSE A1
004800020702     C/END-EXEC
004900020702      *-----------------------------------------------
005000020702     c     srdel         begsr
005100020702      *-----------------------------------------------
005200031023     c* controllo che ora non esisti più la bolla
005300031023     c                   setoff                                       40
005400031023     c     kspe          setll     titas30c                               40
005500031023     c* se non esiste la bolla procedo con la cancellazione dei contrassegni
005600031023     c* e dei suoi file
005700031023     c                   if        not *in40
005800031023     c     kspe          chain     tncsb03l
005900031023     c                   if        %found(tncsb03l)
006000031023     C* pulisco anagrafico
006100031023     c     kspe          chain     tncsa01l
006200031023     c                   if        %found(tncsa01l)
006300031023     c                   delete    tncsa000
006400031023     c                   end
006500031023     C* pulisco variazioni
006600031023     c     kspe          setll     tncsv01l
006700020603     c                   do        *hival
006800031023     c     kspe          reade     tncsv01l
006900031023     c                   if        %eof
007000020603     c                   leave
007100020603     c                   end
007200031023     c                   delete    tncsv000
007300031023     c                   enddo
007400120214     C* pulisco multiassegni
007500120220     c                   if        %subst(csbnra: 10: 1) <> ' ' and
007600120220     c                             csbtpi = 'M' and csbabi = 0
007700120214     c     csbnra        setll     tncsm03l
007800120214     c                   do        *hival
007900120214     c     csbnra        reade     tncsm03l
008000120214     c                   if        %eof
008100120214     c                   leave
008200120214     c                   end
008300120214     c                   delete    tncsm000
008400120214     c                   enddo
008500120214     c                   end
008600031023     C* pulisco rif.
008700031023     c     kspe          setll     tncsg01l
008800020328     c                   do        *hival
008900031023     c     kspe          reade     tncsg01l
009000020328     c                   if        %eof
009100020328     c                   leave
009200020328     c                   end
009300031023     c                   delete    tncsg000
009400020328     c                   enddo
009500031023     C* pulisco contrassegni
009600031023     c                   delete    tncsb000
009700031023     c                   end
009800031023     c                   end
009900020702     c                   endsr
010000020702      *-----------------------------------------------
010100020702     C     sqlerror      begsr
010200020702      *-----------------------------------------------
010300020702     C*
010400020702     C                   call      'X66CHGJOB'
010500020702     C                   seton                                        lr
010600020702     C*
010700020702     C                   endsr
010800000607      *-----------------------------------------------
010900000607     c     *inzsr        begsr
011000000607      *-----------------------------------------------
011100000607     c
011200000607     c     *entry        plist
011300020205     c                   parm                    kpjba
011400031023     c                   movel     kpjbu         tnsc60ds
011500020702     c*
011600031023     C     Kspe          KLIST
011700031023     C                   KFLD                    csbaas
011800031023     C                   KFLD                    csblnp
011900031023     C                   KFLD                    csbnrs
012000031023     C                   KFLD                    csbnsp
012100020402     c*
012200020402     c                   endsr

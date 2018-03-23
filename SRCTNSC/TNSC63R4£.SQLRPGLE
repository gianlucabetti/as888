000100990504     H DEBUG DECEDIT('0,') DATEDIT(*DMY/)
000200020409     h*****************************************************************
000300031023     h* Il pgm pulisce tutti i contrassegni già archiviati (CSBFIN=S) e
000400031023     h* non ancora cancellati perchè presenta la bolla
000500020409     h*****************************************************************
000600031023     ftitas30c  if   e           k disk
000700031023     ftncsb03l  uf   e           k disk
000800031023     ftncsa01l  uf   e           k disk
000900031023     ftncsg01l  uf   e           k disk
001000031023     ftncsv01l  uf   e           k disk
001100000607     D kpjba         e ds
001200031023     D tnsc60ds      e ds
001300031023     D tncsb         e ds                  extname(tncsb00f) inz
001400020702     c*
001500031023     c* leggo tutti i record di tncsb00f che hanno csbfin = 'S' e cioè
001600031023     c* che sono già stati archiviati ma non ancora puliti xchè presente
001700031023     c* la bolla e che hanno data spedizione minore o uguale alla data
001800031023     c* richiesta
001900020702     C/EXEC SQL
002000031023     C+ DECLARE A1 CURSOR FOR SELECT * FROM tncsb00f WHERE csbfin = 'S'
002100031023     C+ and decimal(concat(digits(csbaas),digits(csbmgs))) <= :d60sda
002200020702     C/END-EXEC
002300020702
002400020702     C/EXEC SQL
002500020702     C+ OPEN A1
002600020702     C/END-EXEC
002700020702
002800020702     C                   dou       *inlr
002900020702
003000020702     C/EXEC SQL
003100031023     C+ FETCH NEXT FROM A1 INTO :tncsb
003200020702     C/END-EXEC
003300020702
003400020702     C                   select
003500020702     C                   when      sqlcod = 100
003600020702     C                   seton                                        lr
003700020702     C                   when      sqlcod = 0
003800020702     C                   exsr      srdel
003900020702     C                   other
004000020702     C                   exsr      sqlerror
004100020702     C                   endsl
004200020702     C*
004300020702     C                   enddo
004400020702
004500020702     C/EXEC SQL
004600020702     C+ CLOSE A1
004700020702     C/END-EXEC
004800020702      *-----------------------------------------------
004900020702     c     srdel         begsr
005000020702      *-----------------------------------------------
005100031023     c* controllo che ora non esisti più la bolla
005200031023     c                   setoff                                       40
005300031023     c     kspe          setll     titas30c                               40
005400031023     c* se non esiste la bolla procedo con la cancellazione dei contrassegni
005500031023     c* e dei suoi file
005600031023     c                   if        not *in40
005700031023     c     kspe          chain     tncsb03l
005800031023     c                   if        %found(tncsb03l)
005900031023     C* pulisco anagrafico
006000031023     c     kspe          chain     tncsa01l
006100031023     c                   if        %found(tncsa01l)
006200031023     c                   delete    tncsa000
006300031023     c                   end
006400031023     C* pulisco variazioni
006500031023     c     kspe          setll     tncsv01l
006600020603     c                   do        *hival
006700031023     c     kspe          reade     tncsv01l
006800031023     c                   if        %eof
006900020603     c                   leave
007000020603     c                   end
007100031023     c                   delete    tncsv000
007200031023     c                   enddo
007300031023     C* pulisco rif.
007400031023     c     kspe          setll     tncsg01l
007500020328     c                   do        *hival
007600031023     c     kspe          reade     tncsg01l
007700020328     c                   if        %eof
007800020328     c                   leave
007900020328     c                   end
008000031023     c                   delete    tncsg000
008100020328     c                   enddo
008200031023     C* pulisco contrassegni
008300031023     c                   delete    tncsb000
008400031023     c                   end
008500031023     c                   end
008600020702     c                   endsr
008700020702      *-----------------------------------------------
008800020702     C     sqlerror      begsr
008900020702      *-----------------------------------------------
009000020702     C*
009100020702     C                   call      'X66CHGJOB'
009200020702     C                   seton                                        lr
009300020702     C*
009400020702     C                   endsr
009500000607      *-----------------------------------------------
009600000607     c     *inzsr        begsr
009700000607      *-----------------------------------------------
009800000607     c
009900000607     c     *entry        plist
010000020205     c                   parm                    kpjba
010100031023     c                   movel     kpjbu         tnsc60ds
010200020702     c*
010300031023     C     Kspe          KLIST
010400031023     C                   KFLD                    csbaas
010500031023     C                   KFLD                    csblnp
010600031023     C                   KFLD                    csbnrs
010700031023     C                   KFLD                    csbnsp
010800020402     c*
010900020402     c                   endsr

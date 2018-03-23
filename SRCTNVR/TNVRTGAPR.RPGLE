000100990504     H* ACTGRP=*CALLER
000200990504     H DEBUG DECEDIT('0,') DATEDIT(*DMY/)
000300050131     ffigap01l  if   e           k disk    rename(figap000:figap) prefix(p)
000400050131     ffigapx1l  if   e           k disk    rename(figap000:figapx) prefix(s)
000500050131     ftigap01l  uf a e           k disk
000600050203     ftigcp51l  if   e           k disk
000700050211     Fprtf198   O    F  198        PRINTER OFLIND(*INOF)
000800050118
000900050118      *ds comodo per tracciati da trascodificare
001000050131     D salvap        e ds                  extname(figap00f) prefix(p)
001100050131     D salvas        e ds                  extname(figap00f) prefix(s)
001200050131     D salvac        e ds                  extname(figap00f) prefix(c)
001300050131     D salvat        e ds                  extname(tigap00f)
001400050128     D ok              S              1
001500050202     d contagcp        s             10  0
001600050202     d contanotp       s             10  0
001700050202     d contagcps       s             10  0
001800050202     d contanots       s             10  0
001801050211     d contadups       s             10  0
001900050202     d contanew        s             10  0
001901050211     d totpar          s             10  0
001902050211     d totsede         s             10  0
002000050118
002100050131      * ANAGRAFICA PARTENZA
002200050131     c     *loval        setll     figap01l
002300050117     c                   do        *hival
002400050131     c                   read      figap01l
002500050131     c                   if        %eof(figap01l)
002600050117     c                   leave
002700050117     c                   end
002800050202     c                   add       1             contagcp
002801050211     c                   eval      salvac = salvap
003000050128     c* controllo giacenza
003100050203     c     ktigcp        setll     tigcp51l                               01
003200050128     c* se non trovo la giacenza sul file trascodificato salto a quella dopo
003300050131     c                   if        not *in01
003400050202     c                   add       1             contanotp
003500050131     C* Segnalo l'anomalia solo se la giacenza è del 2004 o 2005
003600050131     C                   IF        CGAPAAS >= 2004
003700050131     c                   seton                                        21
003800050131     c                   exsr      stampa
003900050131     C                   END
004000050128     c                   iter
004100050128     c                   end
004200050128      *valorizza la ds del file tignp00f per la scrittura del rekord
004300050128     c                   movel     salvap        salvat
004400050128      * scrivo il rekord nel nuovo archivio
004500050202     c                   add       1             contanew
004600050131     c                   write     tigap000
004700050117     c                   enddo
004800050128      * NOTE SEDE
004900050131     c     *loval        setll     figapx1l
005000050128     c                   do        *hival
005100050131     c                   read      figapx1l
005200050131     c                   if        %eof(figapx1l)
005300050128     c                   leave
005400050128     c                   end
005500050202     c                   add       1             contagcps
005501050211     c                   eval      salvac = salvas
005601050211      * prima di scrivere il record verifico che non sia un record già
005602050211      * trascodificato
005603050211     c     ktigcp        setll     tigap01l                               01
005604050211     c                   if        *in01
005605050211     c                   add       1             contadups
005606050211     c                   iter
005607050211     c                   end
005700050128     c* controllo giacenza
005800050203     c     ktigcp        setll     tigcp51l                               01
005900050128     c* se non trovo la giacenza sul file trascodificato salto a quella dopo
006000050131     c                   if        not *in01
006100050202     c                   add       1             contanots
006200050131     C* Segnalo l'anomalia solo se la giacenza è del 2004 o 2005
006300050131     C                   IF        CGAPAAS >= 2004
006400050131     c                   seton                                        22
006500050131     c                   exsr      stampa
006600050131     c                   end
006700050128     c                   iter
006800050128     c                   end
006900050128      *valorizza la ds del file tignp00f per la scrittura del rekord
007000050128     c                   movel     salvas        salvat
007100050128      * scrivo il rekord nel nuovo archivio
007200050202     c                   add       1             contanew
007300050131     c                   write     tigap000
007400050128     c                   enddo
007401050211     c*
007402050211     c                   eval      totpar = contagcp - contanotp
007403050211     c                   eval      totsede= contagcps- contanots-contadups
007404050211     c                   except    totali
007500050131     c                   seton                                        lr
007700050128      *-------------------------------------------------
007800050131     c     stampa        begsr
007900050128      *-------------------------------------------------
008000050131     c   of              except    testa
008100050131     c                   eval      *inof = '0'
008200050203     c                   if        *in21 or *in22
008300050131     c                   except    riga
008400050131     c                   end
008500050203     c                   setoff                                       2122
008600050131     c                   endsr
008700050131      *-------------------------------------------------
008800050131     c     *inzsr        begsr
008900050131      *-------------------------------------------------
009100050131     c     ktigcp        klist
009200050131     c                   kfld                    cgapaas
009300050131     c                   kfld                    cgaplnp
009400050131     c                   kfld                    cgapnrs
009500050131     c                   kfld                    cgapnsp
009501050211     c                   except    testa
009600050114     c                   endsr
009700050211     Oprtf198   E            TESTa            01
009800050131     O                                           65 'ANOMALIE TRASCODIFICA ANAG'
009900050211     O                                           +0 'RAFICA GIACENZE'
010000050131     O                       PAGE1         Z    132
010100050131     O          E            RIGA       01
010200050131     O                                           29 'Spedizione non trovata nel'
010300050131     O                                           +1 'file'
010400050211     O               21                          38 'TIGCP00F'
010500050211     O               22                          43 'TIGCP00F SEDE'
010600050131     O                       cgapaas             48
010700050131     O                       cgaplnp             52
010800050131     O                       cgapnrs       z     55
010900050131     O                       cgapnsp       z     63
011000050131     O                                           +1 'Anagrafica non trascodific'
011100050131     O                                           +1 'ata'
011200050211     O          E            totali     03
011300050202     o                                           +0 'Anagrafica PART. '
011400050203     O                       contagcp      z     +1
011500050202     o                                           +1 'Ana. PART.  non trasc.'
011600050202     o                                           +0 ' manca giacenza nel TI'
011700050202     o                                           +0 'GCP00F'
011800050203     O                       contanotp     z     +1
011801050211     o                                           +1 'TOTOTALE PARTENZA ('
011802050211     O                       contagcp      z     +0
011803050211     o                                           +0 '-'
011804050211     O                       contanotp     z     +0
011805050211     o                                           +0 ')'
011806050211     O                       totpar        z     +1
011900050202     O          E            totali     01
012000050202     o                                           +0 'Anagrafica SEDE  '
012100050203     O                       contagcps     z     +1
012200050202     o                                           +1 'Ana. SEDE   non trasc.'
012300050202     o                                           +0 ' manca giacenza nel TI'
012400050202     o                                           +0 'GCP00F'
012500050203     O                       contanots     z     +1
012501050211     o                                           +1 'Ana. SEDE   non trasc.'
012502050211     o                                           +0 ' record doppio'
012503050211     O                       contadups     z     +1
012504050211     o                                           +1 'TOTOTALE SEDE ('
012505050211     O                       contagcps     z     +0
012506050211     o                                           +0 '-'
012507050211     O                       contanots     z     +0
012508050211     o                                           +0 '-'
012509050211     O                       contadups     z     +0
012510050211     o                                           +0 ')'
012511050211     O                       totsede       z     +1
012600050202     O          E            totali     01
012700050202     o                                           +0 'Anagrafica TRASCODIFICATE'
012701050211     o                                           +1 '('
012704050211     O                       totpar        z     +0
012705050211     o                                           +0 '+'
012706050211     O                       totsede       z     +0
012707050211     o                                           +0 ')'
012800050203     O                       contanew      z     +1

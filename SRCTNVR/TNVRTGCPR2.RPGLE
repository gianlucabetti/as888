000100990504     H* ACTGRP=*CALLER
000200990504     H DEBUG DECEDIT('0,') DATEDIT(*DMY/)
000300050201     ffigcp02l  if   e           k disk
000400050203     ftigcp52l  uf a e           k disk
000500050201     ftitas30c  if   e           k disk
000600050211     Fprtf198   O    F  198        PRINTER OFLIND(*INOF)
000700050124      *
000800050124     d wgcpagc         s                   like(gcpagc)
000900050124     d wgcpfgc         s                   like(gcpfgc)
001000050124     d wgcpngc         s                   like(gcpngc)
001100050124     d
001200050202     d contagcp        s             10  0
001300050202     d contagia        s             10  0
001400050202     d contafil        s             10  0
001500050202     d contanob        s             10  0
001600050202     d contanew        s             10  0
001700050202     d contanom        s             10  0
001800000607      *--------------------------------------------------------------------
001900050114
002000050114      * reperimento dati pilota da file giacenze in sede
002100050114      *
002101050211     c     0000          setll     figcp02l
002300000607     c                   do        *hival
002400050124     c                   read      figcp02l
002500050124     c                   if        %eof(figcp02l)
002600000607     c                   leave
002700000607     c                   end
002800050202     c                   add       1             contagcp
002900050114
003000050124     c                   if        gcpagc <> wgcpagc or
003100050124     c                             gcpfgc <> wgcpfgc or
003200050124     c                             gcpngc <> wgcpngc
003300050202     c                   add       1             contagia
003400050124     c
003500050124      * cerca giacenza in nuovo archivio
003600050203     c     kgia          chain     tigcp52l
003700050124      * se trovo il record vuol dire che è ancora presente sui file della
003800050124      * filiale quindi tengo quelli e non faccio nessun aggiornamento
003900050114
004000050203     c                   if        %found(tigcp52l)
004100050202     c                   add       1             contafil
004200050124     c                   iter
004300050124     c                   else
004400050201     c* se non esiste la bolla non trascodifico la giacenza
004500050201     c     ktitas        setll     titas30c                               10
004600050201     c                   if        not *in10
004700050202     c                   add       1             contanob
004800050201     c     kgia          setgt     figcp02l
004900050201     c                   iter
005000050201     c                   end
005100050124      * se non lo trovo scrivo il record e mi salvo la chiave di test
005200050124      * per controllare la rottura
005300050202     c                   add       1             contanew
005400050124     c                   move      gcpagc        wgcpagc
005500050124     c                   move      gcpfgc        wgcpfgc
005600050124     c                   move      gcpngc        wgcpngc
005700050124     c                   exsr      newfld
005800050124     c                   write     tigcp000
005900040507     c                   endif
006000050124     c                   else
006100050124      * se sto leggendo rekord aggiuntivi della giacenza che ho appena
006200050124      * scritto li aggiungo
006300050202     c                   add       1             contanew
006400050124     c                   exsr      newfld
006500050124     c                   write     tigcp000
006600050124     c                   endif
006700050124     c                   enddo
006800050202     c                   except    totali
006900000607
007000000607     c                   seton                                        lr
007100040510     c
007200050114      *-------------------------------------------------
007300050114     c     newfld        begsr
007400050114      *-------------------------------------------------
007500050117     c
007600050117      * questi campi sono relativi ai dati in arrivo e si riferiscono
007700050124      * a nuovi campi del tracciato unico
007800050117     c                   z-add     GCPAAS        GCPAASA
007900050117     c                   z-add     GCPLNP        GCPLNPA
008000050117     c                   z-add     GCPNRS        GCPNRSA
008100050117     c                   z-add     GCPNSP        GCPNSPA
008200050117     c                   z-add     GCPLNA        GCPLNAA
008300050203     c                   eval      gcpsede = 'S'
008301050210     C                   if        gcpdur = 0
008302050210     C                   Z-ADD     GCPMGC        gcpdur
008303050210     C                   MOVEL     GCPAGC        gcpdur
008304050210     c                   clear                   gcpriap
008305050210     C                   ELSE
008306050210     C                   Z-ADD     GCPDUR        gcpdur
008307050210     c                   eval      gcpriap = 'R'
008308050210     C                   END
008400050126     c* se la giacenza presente solo in sede è in una fase < 40 (non chiusa)
008500050202     c* stampo anomalia
008600050128     c                   if        gcpfas < 40 AND GCPFRG = 0
008700050202     c                   add       1             contanom
008800050201     c                   seton                                        20
008900050201     c                   exsr      stampa
009000050126     c                   end
009100050114     c                   endsr
009200050114      *-------------------------------------------------
009300050114     c     *inzsr        begsr
009400050114      *-------------------------------------------------
009500050117
009600050117     c     kgia          klist
009700050124     c                   kfld                    gcpagc
009800050124     c                   kfld                    gcpfgc
009900050124     c                   kfld                    gcpngc
010000050124
010100050201     c     ktitas        klist
010200050201     c                   kfld                    gcpaas
010300050201     c                   kfld                    gcplnp
010400050201     c                   kfld                    gcpnrs
010500050201     c                   kfld                    gcpnsp
010601050211     c                   except    testa
010700050114     c                   endsr
010800050201      *-------------------------------------------------
010900050201     c     stampa        begsr
011000050201      *-------------------------------------------------
011100050201     c   of              except    testa
011200050201     c                   eval      *inof = '0'
011300050201     c                   if        *in20
011400050201     c                   except    riga
011500050201     c                   end
011600050201     c                   setoff                                       20
011700050201     c                   endsr
011800050211     Oprtf198   E            TESTa            01
011900050201     o                                           10 'TNVRTGCPR2'
012000050201     O                                           65 'ANOMALIE TRASCODIFICA'
012100050203     O                                           +1 'GIACENZE SEDE'
012200050201     O                       PAGE1         Z    132
012300050201     O          E            RIGA       01
012400050203     O                                           +0 'Giacenza presente solo in '
012500050203     O                                           +0 'sede ancora APERTA'
012700050201     O                       gcpagc              +1
012800050201     O                       gcpfgc              +1
012900050201     O                       gcpngc              +1
013000050201     O                                           +1 'Spedizione'
013100050201     O                       gcpaas              +1
013200050201     O                       gcplnp              +1
013300050201     O                       gcpnrs        z     +1
013400050201     O                       gcpnsp        z     +1
013500050211     O          E            totali     03
013600050202     o                                           +0 'Nr. rec. Giacenze in SEDE'
013700050202     O                       contagcp      z     +1
013800050203     o                                           +1 'Nr. Giacenze in SEDE'
013900050202     O                       contagia      z     +1
014000050202     O          E            totali     01
014100050202     o                                           +0 'Nr. Giacenze NON trascodif'
014200050202     o                                           +0 'icate già prese in filiale'
014300050202     O                       contafil      z     +1
014400050202     O          E            totali     01
014500050202     o                                           +0 'Nr. Giacenze NON trascodif'
014600050202     o                                           +0 'icate manca TITAS'
014700050202     O                       contanob      z     +1
014800050202     O          E            totali     01
014900050202     o                                           +0 'Nr. Giacenze trascodif'
015000050203     o                                           +0 'icate ma ancora aperte'
015100050202     O                       contanom      z     +1
015200050202     O          E            totali     01
015300050202     o                                           +0 'Nr. record TRSCODIFICATI'
015400050202     O                       contanew      z     +1
015500050211     o                                           +1 '('
015600050211     O                       contagia      z     +0
015700050211     o                                           +0 '-'
015800050211     O                       contafil      z     +0
015900050211     o                                           +0 '-'
016000050211     O                       contanob      z     +0
016100050211     o                                           +0 '+'
016200050211     o                                           +1 '('
016300050211     O                       contagcp      z     +0
016400050211     o                                           +0 '-'
016500050211     O                       contagia      z     +0
016600050211     o                                           +0 ')'
016700050211     o                                           +0 ')'

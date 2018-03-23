000100991207     H DECEDIT('0,') DATEDIT(*DMY.)
000200991201     Ffnblp34l  if   e           k disk
000300021203     F*fnapd01l  if   e           k disk
000400021203     Ffiapd01l  if   e           k disk
000500041008     Ftabel00f  if   e           k disk
000600991202     Fqsysprt   o    f  132        printer oflind(*inof)
000700991215     f                                     formlen(66)
000800991215     f                                     formofl(58)
000900991202     f*
001000991214     D STA             s             66    dim(17) ctdata perrcd(1)
001100991202     d*
001200991202     D despdr          s             20
001300991202     D prersm          s                   like(blprsm)
001400021203      *
001500021203     d kpdpdr          S                   LIKE(APDpdr)
001600021203     d kpdtip          S                   LIKE(APDtip)
001700041008     d cod             S                   LIKE(tblcod)
001800041008     d key             S                   LIKE(tblkey)
001900991202     d*
002000991201     Dkpjba          e ds
002100991201     Dfnls86ds       e ds
002200991202     dcncr80         e ds
002300991202     dut§dse0f       e ds
002400041008     dDS3A           e ds
002500991201     c*
002600991201     c* stampo intestazione
002700991201     c                   except    testa
002800991201     c*
002900991201     c                   clear                   totvlb            6 3
003000991201     c                   clear                   tgnvlb            6 3
003100991214     c                   clear                   tgnspe            5 0
003200991214     c                   clear                   totspe            5 0
003300991214     c                   clear                   tgnncl            6 0
003400991214     c                   clear                   totncl            6 0
003500991214     c                   clear                   tgnpkb            8 1
003600991214     c                   clear                   totpkb            8 1
003700991201     c     kblp          setll     fnblp34l
003800991201     c*
003900991201     c  n01kblp          reade     fnblp34l                               30
004000991201     c   01kblp1         reade     fnblp34l                               30
004100991201     c     *in30         doweq     *off
004200041008     c* Escludo le bolle di recupero
004300041008     C                   MOVEL     '3A'          COD
004400041008     C                   MOVEL(P)  blpcbo        KEY
004500041008     c     ktab          chain     tabel
004600041008     c                   if        %found(tabel00f)
004700041008     c                   movel     tbluni        ds3a
004800041008     c                   else
004900041008     c                   clear                   ds3a
005000041008     c                   endif
005100041008     c                   if        §3arbl<>'R'
005200041008     c
005300991201     c* controllo rottura mittente (ragione sociale/codice)
005400991201     c     blprsm        ifne      prersm
005500991201     c     prersm        ifne      *blanks
005600991214     c     totspe        andgt     *zeros
005700991202     c   of              except    testa
005800991202     c                   except    riga
005900991201     c                   add       totvlb        tgnvlb
006000991201     c                   move      *zeros        totvlb
006100991214     c                   add       totpkb        tgnpkb
006200991214     c                   move      *zeros        totpkb
006300991214     c                   add       totncl        tgnncl
006400991214     c                   move      *zeros        totncl
006500991214     c                   add       totspe        tgnspe
006600991214     c                   move      *zeros        totspe
006700991201     c                   endif
006800991201     c                   move      blprsm        prersm
006900991201     c                   endif
007000991201     c*
007100991201     c     blpatb        ifeq      *blanks
007200991203     c* se richiesto mattino o pomeriggio scarto se diverso
007300991203     c* scarto anche se diverso numero ritiro
007400991203     c     d86fpp        ifne      *blanks
007500991203     c     d86fpp        andne     blpfpp
007600991203     c     d86fpp        orne      *blanks
007700991203     c     blpnrt        andne     d86nrt
007800991203     c                   goto      leggi
007900991203     c                   endif
008000991201     c*
008100991201     c* se richiesto codice mittente specifico scarto se diverso
008200991201     c                   select
008300991201     c     d86ksc        whengt    *zeros
008400991201     c     blpccm        andgt     *zeros
008500991201     c     blpccm        andne     d86ksc
008600991201     c                   goto      leggi
008700991201     c     d86ksc        whengt    *zeros
008800991201     c     blpccm        andeq     *zeros
008900991201     c     blpksc        andne     d86ksc
009000991201     c                   goto      leggi
009100991201     c                   endsl
009200991201     c                   add       blpvlb        totvlb
009300991214     c                   add       1             totspe
009400991214     c                   add       blpncl        totncl
009500991214     c                   add       blppkb        totpkb
009600991201     c*
009700991201     c                   endif
009800041008     c                   endif
009900991201     c*
010000991201     c     leggi         tag
010100991201     c*
010200991201     c  n01kblp          reade     fnblp34l                               30
010300991201     c   01kblp1         reade     fnblp34l                               30
010400991201     c                   enddo
010500991230     c     totspe        ifgt      *zeros
010600991209     c* operazioni di rottura a fine file
010700991209     c   of              except    testa
010800991209     c                   except    riga
010900991209     c                   add       totvlb        tgnvlb
011000991209     c                   move      *zeros        totvlb
011100991214     c                   add       totpkb        tgnpkb
011200991214     c                   move      *zeros        totpkb
011300991214     c                   add       totncl        tgnncl
011400991214     c                   move      *zeros        totncl
011500991214     c                   add       totspe        tgnspe
011600991214     c                   move      *zeros        totspe
011700991209     c* a fine lettura stampo totale generale con relativa % di scostamento
011800991201     c     d86vlr        sub       tgnvlb        vscost            6 3
011900991201     c     vscost        mult      100           w0061             6 1
012000991201     c     w0061         div       d86vlr        percs             7 2
012100991201     c                   except    totgen
012200991201     c                   else
012300991201     c                   except    nodati
012400991201     c                   endif
012500991201     c                   except    fines
012600991201     c*
012700991201     c                   seton                                        lr
012800991201     c**********************************************************************
012900991201     c     *inzsr        begsr
013000991201     c     *entry        plist
013100991201     c                   parm                    kpjba
013200991201     c                   movel     kpjbu         fnls86ds
013300991202     c*
013400991202     C                   Z-ADD     1             codut
013500991202     C                   CALL      'X§PARUT'
013600991202     C                   PARM                    ut§dse0f
013700991202     c                   MOVEL     ragut         rsut             20
013800991202     C                   movel     rec80         cncr80
013900991202     C                   time                    wtime            14 0
014000991202     C                   move      wtime         wdate             8 0
014100991202     C                   movel     wtime         utime             6 0
014200991201     c     d86rsc        comp      *blanks                                01
014300991202     c     d86fpp        comp      'M'                                    02
014400991207     c     d86fpp        comp      'P'                                    03
014500991202     c* prendo ragione sociale autotrasportatore per la stampa
014600021203     c*    d86pdr        chain     fnapd01l                           31
014700021205     C                   move      d86pdr        kpdpdr
014800021203     C                   move      'A'           kpdtip
014900021203     C     kfapd         chain     fiapd01L                           31
015000991202     c     *in31         ifeq      *off
015100991202     c                   movel     apdrsc        despdr
015200991202     c                   else
015300991202     c                   clear                   despdr
015400991202     c                   endif
015500991201     c*
015600991201     c     kblp          klist
015700991201     c                   kfld                    d86pdr
015800991201     c                   kfld                    d86drt
015900991201     c                   kfld                    d86rsc
016000991201     c     kblp1         klist
016100991201     c                   kfld                    d86pdr
016200991201     c                   kfld                    d86drt
016300021203
016400021203     C     Kfapd         klist
016500021203     C                   kfld                    kpdtip
016600021203     C                   kfld                    kpdpdr
016700041008     C     Ktab          klist
016800041008     C                   kfld                    codut
016900041008     C                   kfld                    cod
017000041008     C                   kfld                    key
017100041008     c                   endsr
017200991202     Oqsysprt   e            testa          1  2
017300991202     o                       STA(1)              66
017400991202     o                       STA(2)             132
017500991202     O                       rsut                20
017600991202     O                       simfel              24
017700991202     O                       wdate              120 '  /  /    '
017800991202     O                       page          z    132
017900991202     O          e            testa        001
018000991202     o                       knsif               10
018100991202     o                       knmus               21
018200991202     o                       utime              119 ' 0:  :  '
018300991202     O          e            testa        001
018400991202     o                       STA(3)              66
018500991202     o                       STA(4)             132
018600991202     o                       d86pdr              25
018700991202     o                       despdr              46
018800991202     o                       d86grt        8     69
018900991207     o               03                          80 'Pomeriggio'
019000991207     o               02                          80 'Mattino   '
019100991209     o               03                          84 'nr.'
019200991209     o               02                          84 'nr.'
019300991207     o               03      d86nrt              85
019400991207     o               02      d86nrt              85
019500991215     o                       d86vlr        4    108
019600991202     O          e            testa        002
019700991202     o                       STA(5)              66
019800991207     o                       d86ksc        z     25
019900991202     o                       d86rsc              61
020000991202     O          e            testa        001
020100991202     o                       STA(6)              66
020200991214     o                       STA(16)            132
020300991202     O          e            testa        001
020400991202     o                       STA(7)              66
020500991214     o                       STA(17)            132
020600991209     O          e            testa          2
020700991207     o                       STA(9)              66
020800991214     o                       sta(15)            132
020900991202     o          e            riga         001
021000991202     o                       prersm              35
021100991202     o                       totvlb        4     49
021200991214     o                       totspe        4     61
021300991214     o                       totncl        4     70
021400991214     o                       totpkb        4     81
021500991209     o          e            totgen      1001
021600991202     o                       STA(9)              66
021700991214     o                       STA(15)            132
021800991209     o          e            totgen         2
021900991207     o                       STA(10)             66
022000991207     o                       tgnvlb        4     49
022100991214     o                       tgnspe        4     61
022200991214     o                       tgnncl        4     70
022300991214     o                       tgnpkb        4     81
022400991209     o          e            totgen       001
022500991207     o                       STA(11)             66
022600991207     o                       d86vlr        4     49
022700991207     o                       percs         Q     56
022800991207     o                                           57 '%'
022900991215     o          e            fines       2
023000991202     o                       STA(12)             66
023100991202     o          e            nodati       002
023200991202     o                       STA(13)             66
023300991201**
023400991202xxxxxxxxxxxxxxxxxxxx/xxx              ** LISTA METRI CUBI BOLLETTA  01
023500991202TI PER AUTOTRASPORTATORE **      FNLS86R    XX/XX/XXXX    PAG.XXXX  02
023600991202Autotrasportatore:xxxxxxx 12345678901234567890  Ritiri del XX/XX/X  03
023700991215XXX                  Mc dichiarati xxx,xxx                          04
023800991202Mittente.........:xxxxxxx 12345678901234567890123456789012345       05
023900991214Ragione Sociale Mittente                  Tot.Vol.      Tot.     T  06
024000991214                                          Bollett.      Spediz   C  07
02410099121412345678901234567890123456789012345       xxx,xxx       xxxxx   xx  08
024200991214------------------------------------------------------------------  09
024300991214Totale generale                           xxx,xxx       xxxxx   xx  10
024400991215Mc dichiarati autotrasp. e % scostamento  xxx,xxx  xx,xx%           11
024500991215                                                 ** Fine stampa **  12
024600991202Non trovati dati da stampare per la selezione richiesta             13
024700991214xxxx  xxxxxxx,x                                                     14
024800991214----------------                                                    15
024900991214ot.   Tot.peso                                                      16
025000991214olli  Bollettato                                                    17

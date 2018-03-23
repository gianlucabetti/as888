000100990504     H DEBUG DECEDIT('0,') DATEDIT(*DMY/)
000200030516     fsicop01l  if   e           k disk
000300030113     fazorg01l  if   e           k disk
000400031023     ffiatt01l  if   e           k disk
000500030124     ftntbe01l  if   e           k disk
000600030519     ftabel00f  if   e           k disk
000700030519     ffifsa01l  if a e           k disk
000800020626     d kpjba         e ds
000900030113
001000030519     d ds8a          e ds
001100030205     d dccg          e ds
001200030113     D param           DS
001300030113     D  datadal                       8s 0
001400030113     D  dataal                        8s 0
001500030113
001600030113     d dataiso         s               d   datfmt(*iso)
001700030113     d dataeur         s               d   datfmt(*eur)
001800030113
001900031023     d exddc           s                   like(attddc)
002000031023     d pdr             s                   like(attpdr)
002100031023     d tsr             s                   like(fsatsr)
002200030519     d com03           s              3    inz
002300030516     d x               s              3  0 inz
002400030516     d xx              s              3  0 inz
002500030516     c* voci delle competenze
002600030516     d voc             s              3    inz dim(500)
002700030516     c* competenze
002800030516     d com             s              3    inz dim(500)
002900030113
003000030113     D WLBDAT          DS
003100030113     D  G02DAT                 1      8  0
003200030113     D  G02INV                 9     16  0
003300030113     D  G02ERR                17     17
003400030113     D  G02TGI                18     22  0
003500030113     d
003600000607      *--------------------------------------------------------------------
003700030113      * eliminazione record esistenti nel periodo da elaborare
003800030516     c                   call      'FICN46R1'
003900030113     c                   parm                    param
004000030113      *loop sui record di azorg per reperire tutti i po
004100030113     c     *loval        setll     azorg01l
004200030113     c                   do        *hival
004300030113     c                   read      azorg01l                               99
004400030113     c   99              leave
004500030516     c                   clear                   pdr
004600030516     c                   movel     orgfil        pdr
004700030516     c                   exsr      elabora
004800030516     c* controllo se sono rimasti indietro dei costi senza competenze
004900030516     c                   exsr      srnocom
005000030113     c                   enddo
005100000607     c
005200020521     c                   seton                                        lr
005300030516      *------------------------------------------------------
005400030516     c     elabora       begsr
005500030516      *------------------------------------------------------
005600030516     c                   clear                   exddc
005700030516      * legge i record di dettaglio per totalizzare nel rek di saldo
005800030516     c     ksic          setll     sicop01l
005900030516     c                   do        *hival
006000030516     c     orgfil        reade     sicop01l
006100030516     c                   if        %eof(sicop01l)
006200030516     c                   leave
006300030516     c                   end
006400030516      * se data maggiore al limite massimo richiesto esco dal loop
006500030516     c                   if        copdat > dataal
006600030516     c                   leave
006700030516     c                   endif
006800030526     c* controllo che la voce mi vada bene
006900030516     c                   z-add     1             x
007000030519     c                   move      copvoc        com03
007100030519     c     com03         lookup    voc(x)                                 99
007200030516     c                   if        not *in99
007300030516     c                   iter
007400030516     c                   end
007500030519     c* a rottura di data
007600030519     c                   if        copdat <> exddc and exddc <> 0
007700031023      * leggo i costi su fiatt
007800030519     c                   exsr      costi
007900030519      * scrivo FIFSA00F
008000030516     c                   exsr      writefsa
008100030519     c                   endif
008200030519     c                   z-add     copdat        exddc
008300030516     c* imbuca le competenze
008400030516     c                   exsr      imbcom
008500030516     c                   enddo
008600030516      * ultimo record
008700030526     c                   if        exddc <> 0
008800031023      * leggo i costi su fiatt
008900030519     c                   exsr      costi
009000030519      * scrivo FIFSA00F
009100030516     c                   exsr      writefsa
009200030519     c                   endif
009300030516     c                   endsr
009400020521      *------------------------------------------------------
009500030516     c     costi         begsr
009600020521      *------------------------------------------------------
009700030113
009800031023      * legge il file  nel quale sono stati memorizzati i viaggi
009900030516      * i costi
010000031023     c     katt          setll     fiatt01l
010100030113     c                   do        *hival
010200031023     c     katt          reade     fiatt01l
010300031023     c                   if        %eof(fiatt01l)
010400030516     c                   leave
010500030516     c                   end
010600031023      * solo viaggi confermati
010700031023     c                   if        attflg <> 'C'
010800030516     c                   iter
010900030516     c                   endif
011000030516     c* reperisco la competenza per imbucare l'importo
011100030516     c                   exsr      imbcos
011200030113     c                   enddo
011300030516     c*
011400030113     c                   endsr
011500030519      *------------------------------------------------------
011600030519     c     imbcom        begsr
011700030519      *------------------------------------------------------
011800030519     c                   select
011900030519     c                   when      com(x) = '014'
012000030519     c                   add       copimv        fsa014b
012100030519     c                   when      com(x) = '015'
012200030519     c                   add       copimv        fsa015b
012300030519     c                   when      com(x) = '046'
012400030519     c                   add       copimv        fsa046b
012500030519     c                   when      com(x) = '047'
012600030519     c                   add       copimv        fsa047b
012700030519     c                   when      com(x) = '049'
012800030519     c                   add       copimv        fsa049b
012900030519     c                   when      com(x) = '050'
013000030519     c                   add       copimv        fsa050b
013100030519
013200030519     c                   endsl
013300030519
013400030519     c                   endsr
013500020521      *------------------------------------------------------
013600030516     c     imbcos        begsr
013700020521      *------------------------------------------------------
013800020521     c                   select
013900031023     c                   when      attvad   = 'A'
014000031023     c                   add       attimpp       fsa014
014100031023     c                   when      attvad   = 'D'
014200031023     c                   add       attimpp       fsa015
014300020521     c                   endsl
014400020521
014500020521     c                   endsr
014600030516      *------------------------------------------------------
014700030516     c     srnocom       begsr
014800030516      *------------------------------------------------------
014900030516
015000030516     c                   move      datadal       exddc
015100030516      * legge il file delle rettifiche nel quale sono stati memorizzati
015200030516      * i costi
015300031023     c     katt          setll     fiatt01l
015400030519     c                   clear                   exddc
015500031023     c                   do        *hival
015600031023     c     orgfil        reade     fiatt01l
015700031023     c                   if        %eof(fiatt01l)
015800030516     c                   leave
015900030516     c                   end
016000030516      * se data maggiore al limite massimo richiesto esco dal loop
016100031023     c                   if        attddc > dataal
016200030516     c                   leave
016300030516     c                   endif
016400031023      * solo confermati
016500031023     c                   if        attflg <> 'C'
016600030516     c                   iter
016700030516     c                   endif
016800030516     c* controllo se già scritto
016900030519     c     kfsa          setll     fifsa01l                               99
017000030519     c                   if        *in99
017100030516     c                   iter
017200030516     c                   end
017300031023      * se data diversa  scrivo il rek
017400031023     c                   if        attddc <> exddc and exddc <> 0
017500030519     c                   exsr      writefsa
017600030516     c                   endif
017700031023     c                   z-add     attddc        exddc
017800030519     c* imbuca l'importo nella competenza giusta
017900030519     c                   exsr      imbcos
018000030516     c                   enddo
018100030519      * ultimo record
018200030526     c                   if        exddc <> 0
018300030519     c                   exsr      writefsa
018400030516     c                   endif
018500030516     c                   endsr
018600030519      *------------------------------------------------------
018700030519     c     writefsa      begsr
018800030519      *------------------------------------------------------
018900030519     c                   z-add     orgfil        fsafgs
019000030519     c                   z-add     exddc         fsaddc
019100031023     c                   move      *blank        fsatsr
019200030519     c                   move      pdr           fsapdr
019300030519     c                   write     fifsa000
019400030519     c                   clear                   fifsa000
019500030519     c                   endsr
019600020521      *-------------------------------------------------
019700020521     c     *inzsr        begsr
019800020521      *-------------------------------------------------
019900020626     c     *entry        plist
020000020626     c                   parm                    kpjba
020100030113     c                   if        kpjbu = *blank
020200030113     c                   clear                   param
020300030113     c                   movel(p)  param         kpjbu
020400030113     c                   end
020500030113     c                   movel     kpjbu         param
020600030113
020700030113     C***
020800030113     C* GIRO DATA DEL GIORNO: LA PRENDO DA TIME
020900030113     C***
021000030113     C                   TIME                    W0120            14 0
021100030113     C                   MOVE      W0120         WDAT              8 0
021200030113     C*
021300030113     C                   Z-ADD     WDAT          G02DAT
021400030113     C                   MOVEL     *BLANK        G02ERR
021500030113     C                   CALL      'XSRDA8'
021600030113     C                   PARM                    WLBDAT
021700030113     C* UDATE A 8 IN AAAA/MM/GG
021800030113     C                   move      G02INV        dataiso
021900030113      *se non sono stati ricevuti dei parametri significa che il lancio è
022000030113      * stato schedulato per cui imposto i parametri di limite elaborazione
022100030113     c                   if        datadal = 0
022200030205      *reperisco limiti da tabella
022300030205     c                   movel(p)  '1'           tbeke1
022400030205     c                   movel     'CCG'         tbecod
022500030205     c     ktbe          chain     tntbe01l                           90
022600030205     c                   if        not *in90
022700030205     c                   movel     tbeuni        dccg
022800030205      *tolgo giorni di tabella da udate
022900030516     c     dataiso       subdur    §cgadsa:*d    dataiso
023000030205     c                   move      dataiso       dataal
023100030205     c                   move      dataal        dataiso
023200030205      *tolgo giorni di tabella da data calcolata per primo limite
023300030516     c     dataiso       subdur    §cgadsd:*d    dataiso
023400030205     c                   move      dataiso       datadal
023500030205     c                   endif
023600030113     c                   endif
023700030516
023800030519     c     kfsa          klist
023900031023     c                   kfld                    attter
024000031023     c                   kfld                    attddc
024100031023     c                   kfld                    tsr
024200031023     c                   kfld                    pdr
024300030516     c     ksic          klist
024400030516     c                   kfld                    orgfil
024500030516     c                   kfld                    datadal
024600031023     c     ktbe          klist
024700031023     c                   kfld                    tbecod
024800031023     c                   kfld                    tbeke1
024900030205
025000030519     c     ktab          klist
025100030519     c                   kfld                    tblkut
025200030519     c                   kfld                    tblcod
025300030516     c                   z-add     1             tblkut
025400030516     c                   eval      tblcod = '8A'
025500030516
025600031023     c     katt          klist
025700031023     c                   kfld                    orgfil
025800030516     c                   kfld                    exddc
025900030516     c* carico le voci
026000030516     c     ktab          setll     tabel00f
026100030516     c                   do        *hival
026200030516     c     ktab          reade     tabel00f
026300030516     c                   if        %eof(tabel00f)
026400030516     c                   leave
026500030516     c                   end
026600030516     c                   movel     tbluni        ds8a
026700030519     c                   if        §8ausc = 'B'
026800030516     c                   add       1             xx
026900030516     c                   movel     tblkey        voc(xx)
027000030516     c                   movel     §8ace4        com(xx)
027100030516     c                   end
027200030516     c                   enddo
027300030124
027400030113     c                   endsr

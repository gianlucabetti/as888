000100990504     H DEBUG DECEDIT('0,') DATEDIT(*DMY/)
000200030113     fazorg01l  if   e           k disk
000300030113     ffictd01l  if   e           k disk
000400030124     ffifre01l  if   e           k disk
000500030124     ftntbe01l  if   e           k disk
000600030127     ftabel00f  if   e           k disk
000700030113     ffifsc00f  o    e             disk
000800020626     d kpjba         e ds
000900030113
001000030127     d ds8a          e ds
001100030124     d dcre          e ds
001200030205     d dccg          e ds
001300030113     D param           DS
001400030113     D  datadal                       8s 0
001500030113     D  dataal                        8s 0
001600030113
001700030113     d dataiso         s               d   datfmt(*iso)
001800030113     d dataeur         s               d   datfmt(*eur)
001900030113
002000030113     d extsr           s                   like(ctdtsr)
002100030113     d expdr           s                   like(ctdpdr)
002200030113     d exddc           s                   like(ctdddc)
002300030124     d exddck          s                   like(ctdddc)
002400030129     d expog           s                   like(ctdfgs)
002500030113     d keypo           s                   like(ctdfgs)
002600030113     d rekok           s              1
002700030113
002800030113     D WLBDAT          DS
002900030113     D  G02DAT                 1      8  0
003000030113     D  G02INV                 9     16  0
003100030113     D  G02ERR                17     17
003200030113     D  G02TGI                18     22  0
003300030113     d
003400000607      *--------------------------------------------------------------------
003500030113      * eliminazione record esistenti nel periodo da elaborare
003600030113     c                   call      'FICN38R1'
003700030113     c                   parm                    param
003800030113      *loop sui record di azorg per reperire tutti i po
003900030113     c     *loval        setll     azorg01l
004000030113     c                   do        *hival
004100030113     c                   read      azorg01l                               99
004200030113     c   99              leave
004300030113     c                   z-add     orgfil        keypo
004400030113     c                   exsr      elabora
004500030113     c                   enddo
004600000607     c
004700020521     c                   seton                                        lr
004800020521      *------------------------------------------------------
004900020521     c     elabora       begsr
005000020521      *------------------------------------------------------
005100030113
005200030113     c                   clear                   rekok
005300030527     c                   clear                   exddc
005400030527     c                   clear                   exddck
005500030113      * legge i record di dettaglio per totalizzare nel rek di saldo
005600030113     c     kctd          setll     fictd01l
005700030113     c                   do        *hival
005800030113     c     keypo         reade     fictd01l                               98
005900030113     c   98              leave
006000030113     c                   move      'X'           rekok
006100030113      * se data maggiore al limite massimo richiesto esco dal loop
006200030113     c                   if        ctdddc > dataal
006300030211     c                   clear                   rekok
006400030113     c                   leave
006500030113     c                   endif
006600030127      *imposto valori rottura primo giro
006700030127     c                   if        exddc = 0
006800030127     c                   z-add     ctdddc        exddc
006900030127     c                   move      ctdtsr        extsr
007000030127     c                   z-add     ctdpdr        expdr
007100030130     c                   z-add     ctdfgs        expog
007200030127     c                   endif
007300030113
007400030113      * se data diversa o tipo servizio diverso dal precedente scrivo il rek
007500030113     c                   if        ctdtsr <> extsr or ctdddc <> exddc
007600030124      * verifico se devo sommare eventuali rettifiche
007700030124     c                   exsr      trattafre
007800030124
007900030129     c                   z-add     expog         fscfgs
008000030113     c                   z-add     exddc         fscddc
008100030113     c                   move      extsr         fsctsr
008200030113     c                   move      expdr         fscpdr
008300030113     c                   write     fifsc000
008400030113     c                   move      ctdtsr        extsr
008500030113     c                   move      ctdpdr        expdr
008600030130     c                   z-add     ctdfgs        expog
008700030113     c                   z-add     ctdddc        exddc
008800030113     c                   clear                   fifsc000
008900030113     c                   endif
009000030113      * preparo i campi per il rekord di saldo per P.O./data
009100030127     c                   movel(p)  ctdvoc        tblkey
009200030127     c     ktab          chain     tabel00f                           90
009300030127     c                   if        not *in90 and tblflg = *blank
009400030127     c                   movel     tbluni        ds8a
009500030113     c                   exsr      imbuca
009600030127     c                   endif
009700030113     c                   enddo
009800030113      * se esce dal loop verifica se era rimasto qualcosa da scrivere che
009900030113      * non ha avuto rottura all'interno del loop (ultimo rekord)
010000030211     c                   if        rekok  <> *blank
010100030211     c                   z-add     expog         fscfgs
010200030211     c                   z-add     exddc         fscddc
010300030211     c                   move      extsr         fsctsr
010400030211     c                   move      expdr         fscpdr
010500030211     c                   write     fifsc000
010600030527     c                   clear                   fifsc000
010700030211     c                   endif
010800030113     c                   endsr
010900020521      *------------------------------------------------------
011000030113     c     imbuca        begsr
011100020521      *------------------------------------------------------
011200020521     c                   select
011300040205     c                   when      �8ace4 = '002' or �8ace4 = '200'
011400030113     c                   add       ctditc        fsc002
011500030113     c                   add       ctdied        fsc002b
011600020521
011700030127     c                   when      �8ace4 = '005'
011800030113     c                   add       ctditc        fsc005
011900030113     c                   add       ctdied        fsc005b
012000020521
012100040205     c                   when      �8ace4 = '006' or �8ace4 = '201' or
012200040205     c                             �8ace4 = '202'
012300030113     c                   add       ctditc        fsc006
012400030113     c                   add       ctdied        fsc006b
012500020521
012600030127     c                   when      �8ace4 = '009'
012700030113     c                   add       ctditc        fsc009
012800030113     c                   add       ctdied        fsc009b
012900020521
013000030127     c                   when      �8ace4 = '010'
013100030113     c                   add       ctditc        fsc010
013200030113     c                   add       ctdied        fsc010b
013300020521
013400040205     c                   when      �8ace4 = '011' or �8ace4 = '204'
013500030113     c                   add       ctditc        fsc011
013600030113     c                   add       ctdied        fsc011b
013700020521
013800040205     c                   when      �8ace4 = '012' or �8ace4 = '205'
013900030113     c                   add       ctditc        fsc012
014000030113     c                   add       ctdied        fsc012b
014100020521
014200030127     c                   when      �8ace4 = '023'
014300030113     c                   add       ctditc        fsc023
014400030113     c                   add       ctdied        fsc023b
014500020521
014600030127     c                   when      �8ace4 = '039'
014700030113     c                   add       ctditc        fsc039
014800030113     c                   add       ctdied        fsc039b
014900030113
015000030127     c                   when      �8ace4 = '040'
015100030113     c                   add       ctditc        fsc040
015200030113     c                   add       ctdied        fsc040b
015300030113
015400030127     c                   when      �8ace4 = '041'
015500030113     c                   add       ctditc        fsc041
015600030113     c                   add       ctdied        fsc041b
015700030113
015800030127     c                   when      �8ace4 = '060'
015900030113     c                   add       ctditc        fsc060
016000030113     c                   add       ctdied        fsc060b
016100030113
016200030127     c                   when      �8ace4 = '061'
016300030113     c                   add       ctditc        fsc061
016400030113     c                   add       ctdied        fsc061b
016500040623
016600040623     c                   when      �8ace4 = '053' or �8ace4 = '206'
016700040623     c                   add       ctditc        fscxx1
016800040623     c                   add       ctdied        fscxx1b
016900030113
017000040623     c                   when      �8ace4 = '054' or �8ace4 = '207'
017100040623     c                   add       ctditc        fscxx2
017200040623     c                   add       ctdied        fscxx2b
017300040623     c* per ora non si � gestito il transito 5 per non campiare il tracciato
017400040623     c* visto che attualmente il massimo dei transiti � 3
017500030113
017600020521     c                   endsl
017700020521
017800020521     c                   endsr
017900030124      *------------------------------------------------------
018000030124     c     trattafre     begsr
018100030124      *------------------------------------------------------
018200030124
018300030124     c                   if        exddc <> exddck
018400030124     c                   z-add     exddc         exddck
018500030124      * legge i record di dettaglio per totalizzare nel rek di saldo
018600030124     c     kfre          setll     fifre01l
018700030124     c                   do        *hival
018800030124     c     kfre          reade     fifre01l                               97
018900030124     c   97              leave
019000030519      *considero solo le rettifiche inerenti la tariffa COOP TSR ='X'
019100030519     c                   if        fretsr = 'X'
019200030205     c                   move      'CRE'         tbecod
019300030124     c                   movel(p)  frecre        tbeke1
019400030124     c     ktbe          chain     tntbe01l                           90
019500030124     c                   if        not *in90 and tbeatb = *blank
019600030124     c                   movel     tbeuni        dcre
019700030124     c                   exsr      imbucafre
019800030124     c                   endif
019900030416     c                   else
020000030416     c                   iter
020100030416     c                   endif
020200030124     c                   enddo
020300030124     c                   endif
020400030124     c                   endsr
020500030124      *------------------------------------------------------
020600030124     c     imbucafre     begsr
020700030124      *------------------------------------------------------
020800030124     c                   select
020900030124     c                   when      �crvoc = '002'
021000030124     c                   add       fretim        fsc002
021100030124
021200030124     c                   when      �crvoc = '005'
021300030124     c                   add       fretim        fsc005
021400030124
021500030124     c                   when      �crvoc = '006'
021600030124     c                   add       fretim        fsc006
021700030124
021800030124     c                   when      �crvoc = '009'
021900030124     c                   add       fretim        fsc009
022000030124
022100030124     c                   when      �crvoc = '010'
022200030124     c                   add       fretim        fsc010
022300030124
022400030124     c                   when      �crvoc = '011'
022500030124     c                   add       fretim        fsc011
022600030124
022700030124     c                   when      �crvoc = '012'
022800030124     c                   add       fretim        fsc012
022900030124
023000030124     c                   when      �crvoc = '023'
023100030124     c                   add       fretim        fsc023
023200030124
023300030124     c                   when      �crvoc = '039'
023400030124     c                   add       fretim        fsc039
023500030124
023600030124     c                   when      �crvoc = '040'
023700030124     c                   add       fretim        fsc040
023800030124
023900030124     c                   when      �crvoc = '041'
024000030124     c                   add       fretim        fsc041
024100030124
024200030124     c                   when      �crvoc = '060'
024300030124     c                   add       fretim        fsc060
024400030124
024500030124     c                   when      �crvoc = '061'
024600030124     c                   add       fretim        fsc061
024700030124      * voci non ancora assegnate
024800030124     c*                  when      �crvoc = 'XX1'
024900030124     c*                  add       fretim        fscxx1
025000030124
025100030124     c*                  when      �crvoc = 'XX2'
025200030124     c*                  add       fretim        fscxx2
025300030124
025400030124     c                   endsl
025500030124
025600030124     c                   endsr
025700020521      *-------------------------------------------------
025800020521     c     *inzsr        begsr
025900020521      *-------------------------------------------------
026000020626     c     *entry        plist
026100020626     c                   parm                    kpjba
026200030113     c                   if        kpjbu = *blank
026300030113     c                   clear                   param
026400030113     c                   movel(p)  param         kpjbu
026500030113     c                   end
026600030113     c                   movel     kpjbu         param
026700030113
026800030113     C***
026900030113     C* GIRO DATA DEL GIORNO: LA PRENDO DA TIME
027000030113     C***
027100030113     C                   TIME                    W0120            14 0
027200030113     C                   MOVE      W0120         WDAT              8 0
027300030113     C*
027400030113     C                   Z-ADD     WDAT          G02DAT
027500030113     C                   MOVEL     *BLANK        G02ERR
027600030113     C                   CALL      'XSRDA8'
027700030113     C                   PARM                    WLBDAT
027800030113     C* UDATE A 8 IN AAAA/MM/GG
027900030113     C                   move      G02INV        dataiso
028000030113      *se non sono stati ricevuti dei parametri significa che il lancio �
028100030113      * stato schedulato per cui imposto i parametri di limite elaborazione
028200030113     c                   if        datadal = 0
028300030205      *reperisco limiti da tabella
028400030205     c                   movel(p)  '1'           tbeke1
028500030205     c                   movel     'CCG'         tbecod
028600030205     c     ktbe          chain     tntbe01l                           90
028700030205     c                   if        not *in90
028800030205     c                   movel     tbeuni        dccg
028900030205      *tolgo giorni di tabella da udate
029000030205     c     dataiso       subdur    �cggus:*d     dataiso
029100030205     c                   move      dataiso       dataal
029200030205     c                   move      dataal        dataiso
029300030205      *tolgo giorni di tabella da data calcolata per primo limite
029400030205     c     dataiso       subdur    �cggls:*d     dataiso
029500030205     c                   move      dataiso       datadal
029600030205     c                   endif
029700030113     c                   endif
029800030205
029900030113     c     kctd          klist
030000030113     c                   kfld                    keypo
030100030113     c                   kfld                    datadal
030200030113
030300030124     c     kfre          klist
030400030124     c                   kfld                    ctdpdr
030500030124     c                   kfld                    exddck
030600030124
030700030124     c     ktbe          klist
030800030124     c                   kfld                    tbecod
030900030124     c                   kfld                    tbeke1
031000030127
031100030127     c     ktab          klist
031200030127     c                   kfld                    tblkut
031300030127     c                   kfld                    tblcod
031400030127     c                   kfld                    tblkey
031500030127     c                   move      '8A'          tblcod
031600030127     c                   z-add     1             tblkut
031700030124
031800030113     c                   endsr
031900020626

000100031027      /TITLE Fatture Fais Service
000200010601
000300010607     H DFTACTGRP(*NO) ACTGRP('QILE') BNDDIR('PJXBND')
000400990504     H DEBUG DECEDIT('0,') DATEDIT(*DMY/)
000500010514
000600021218     fynfeb01l  if   e           k disk
000700021218     fynfec01l  if   e           k disk
000800021218     fynfed01l  if   e           k disk
000900021218     fanfrn01l  if   e           k disk
001000021218     fanrco98j  if   e           k disk
001100010827     fynfe000f  o    e             disk    commit
001200010827     fynfe100f  o    e             disk    commit
001300031028     fmzfai01l  uf   e           k disk    commit
001400031125     fynfe200f  uf a e             disk    commit
001500031125     f                                     infds(inf2)
001600030113     fynfe301l  uf a e           k disk    commit
001700031125     fynfe302l  uf a e           k disk    commit
001800031125     f                                     rename(ynfe3000:ynfe3002)
001900031028     fYco3070P  o    f  198        printer
002000020205     F                                     OFLIND(*INOA)
002001071024     fqsysprt   o    f  132        printer oflind(*inof)
002100031125     d inf2            ds
002200031125     d  nrrfe2               397    400i 0
002300030114     d*
002400020205     D                SDS
002500020205     D PgmName           *PROC
002600031107     D kpjba         e ds
002700030114     D trmz61ds      e ds                  inz
002701070802     D trmz62ds      e ds                  inz
002702070802     D trmz47ds      e ds                  inz
002800031111     D trmznote1     e ds                  inz
002900030116     D ynfea00f      e ds                  inz
003000030114      *ds per reperimento società
003100030114     D XSoc001DS     E DS                  inz
003200010601     D TipXsc          S              6
003300010601     D SocXsc          S              3
003400010601     D CdsXsc          S              9  0
003500010601     D ModXsc          S              3
003600010601     D RtnXsc          S              1
003700010601     D XnmRic          S              1
003800010601     D XnmSoc          S              3
003900010601     D XnmUni          S              8
004000010601     D XnmCtb          S              2
004100010601     D XnmKey          S              8
004200010601     D XnmSer          S              4
004300010601     D XnmCmt          S              1
004400010601     D XnmDat          S                   LIKE(fe1dtupd)
004500010601     D XnmIva          S              1
004600010601     D XnmReg          S              1
004700010601     D XnmErr          S              1
004800010601     D XnmNum          S              9p 0
004900010601     D XnmAut          S              1
005000010601     D XnmDag          S                   like(fe1dtupd)
005100010601     D numsys          s              3p 0
005200010601     D numass          s              9p 0
005300020205     d times           s               T
005400031107     d dtaiso          s               d   datfmt(*iso)
005401070802     d dtaeur          s               d   datfmt(*eur)
005500031107     d dataiso         s               d   datfmt(*iso)
005600010601     d dataEUR         s               d   datfmt(*EUR)
005700031125     d comodop         s                   like(fedpiva)
005800031125     d comodo          s             13  2
005900031125     d com03           s              3  0
006000021219     d com08           s              8  0
006100021218     d wnrriga         s              5  0
006200020205     D WrkCmtBack      S              1
006300020205     D                                     INZ(Commit)
006400030114     D WrkErrDoc       S              1    inz('0')
006500020206     D Msg             S            198
006501071024     D Msgq            S            132
006600030114     D PrmSif          S                   like(knsif)
006700030116     D fine            S              1
006800030116     D prmesito        S              1
006900031028     D conta           S              1  0 inz
007000031125     D saliva          S                   LIKE(fe3impost)
007100031125     D salimp          S                   LIKE(fe3impnib)
007200031125     D detimpost       S                   LIKE(fe3impost)
007300031125     D detimpnib       S                   LIKE(fe3impnib)
007400031125     D costo2          S                   LIKE(fe0impfat)
007500031125     D costo3          S                   LIKE(fe0impfat)
007600031125     D salnrr          S                   LIKE(nrrfe2)
007700031125     d rig             s              3  0 dim(10)
007800031125     d alq             s              3    dim(10)
007900031125     d tot             s             10  2 dim(10)
008000031125     d iva             s             10  2 dim(10)
008100031125     d imp             s             10  2 dim(10)
008200031125     d cau             s              4    dim(10)
008300031125     d per             s              5  2 dim(10)
008400030114     c* costanti
008500030114     D Commit          C                   'C'
008600030114     D RollBack        C                   'R'
008700030114     D Errore          c                   '1'
008800010531
008900020204     C     *ENTRY        PLIST
009000020205     C                   PARM                    PrmSif
009100020205     C                   PARM                    SocXsc
009200030116     C                   PARM                    fine
009300031107     C                   PARM                    kpjba
009400030116     c                   clear                   fine
009500031028     c*
009600031028     c     ' '           setll     mzfai01l
009700031028     c                   do        *hival
009800031028     c     ' '           reade     mzfai01l
009900031028     c                   if        %eof(mzfai01l)
010000031028     c                   leave
010100031028     c                   end
010200031027     c* operazioni da fare solo il primo record
010300031027     c                   if        conta = 0
010400031027     c                   exsr      newdoc
010500031027     C                   z-add     1             conta
010600031027     c                   end
010700031027     c* dettaglio
010800031027     c                   exsr      srwrite
010900031028     c                   movel     'A'           faiann
011000031028     c                   update    mzfai000
011100031027     C*
011200031027     C                   enddo
011300020807     ** Fine lettura.
011400021219     c                   exsr      srfinedoc
011500021219     c* chiudo il pgm TRMZ61R
011600021219     c                   clear                   trmz61ds
011700021219     c                   eval      d61tpe = 'C'
011800021219     c                   call      'TRMZ61R'
011900021219     c                   parm                    trmz61ds
012000010605      *
012100010601     c                   exsr      uscita
012200021219      *------------------------------------------------
012300021219     c     srwrite       begsr
012400021219      *------------------------------------------------
012500021218     c* inizio dettaglio
012600021218     c                   ADD       1             wnrriga
012700021212     c                   clear                   ynfe2000
012800031028     c* reperisco il centro di costo e la società a valere
012900030117     c                   clear                   trmz61ds
013000031028     c                   if        faitga <> *blanks
013100031028     c                   movel     faitga        d61tga
013200031028     c                   movel     faitgn        d61tgn
013300031107     c                   move      faidtm        dtaiso
013400031107     c                   move      dtaiso        com08
013500030117     c                   move      com08         d61dta
013600030117     c                   call      'TRMZ61R'
013700030117     c                   parm                    trmz61ds
013800031028     c                   else
013900031106     c* se è un canone e non ho trovato la targa
014000070228     c* metto cdc = 129 and soc.val.=società fattura
014100031106     c                   if        faitmv = '37'
014200031106     c                   eval      d61cdc = '129'
014300070228     c                   eval      d61socval = feaSocieta
014400031125     c                   eval      d61cat = 'A1'
014500031106     c                   else
014600031028     c                   eval      d61err = 'E'
014700031106     c                   end
014800031028     c                   end
014900030117     c* ritorno E= errore bloccante A=anomalia
015000030117     c                   select
015100030117     c                   when      d61err = 'E'
015101071024     c* stampa contabilità
015200030117     c                   IF        *INOA
015300030117     c                   EXCEPT    Testata
015400030117     C                   EVAL      *INOA = *OFF
015500030117     c                   ENDIF
015600030117     C                   EVAL      Msg = 'ERRORE Non è stata'
015700030117     C                             + ' trovata l''anagrafica per'
015800030117     C                             + ' l''automezzo '
015900031028     C                             + %TRIM(faitga)
016000031028     C                             + %TRIM(faitgn)
016100030117     C                   EXCEPT    RigMsg
016101071024     c* stampa automezzi
016102071024     c                   IF        *INOf
016103071024     c                   EXCEPT    Testa
016104071024     C                   EVAL      *INOf = *OFF
016105071024     c                   ENDIF
016106071024     C                   EVAL      Msgq= 'ERRORE Non è stata'
016107071024     C                             + ' trovata l''anagrafica per'
016108071024     C                             + ' l''automezzo '
016109071024     C                             + %TRIM(faitga)
016110071024     C                             + %TRIM(faitgn)
016111071024     C                   EXCEPT    RigMsgq
016200030117     c                   EVAL      WrkCmtBack = RollBack
016300030117     c                   exsr      uscita
016400030117     c* anomalia
016500030117     c                   when      d61err = 'A'
016501071024     c* stampa contabilità
016600030117     c                   IF        *INOA
016700030117     c                   EXCEPT    Testata
016800030117     C                   EVAL      *INOA = *OFF
016900030117     c                   ENDIF
017000030122      * importo del costo
017100030117     C                   EVAL      Msg = 'ANAMALIA sono stati impostati'
017200030117     C                             + ' dei valori di default per'
017300030117     C                             + ' l''automezzo '
017400031028     C                             + %TRIM(faitga)
017500031028     C                             + %TRIM(faitgn)
017600030122     C                             + ' pari a EUR '
017700031028     C                             + ' ' + %EDITC(faisco:'K')
017800030117     C                             + ' CONTROLLARE!'
017900030117     C                   EXCEPT    RigMsg
017901071024     c* stampa automezzi
017902071024     c                   IF        *INOf
017903071024     c                   EXCEPT    Testa
017904071024     C                   EVAL      *INOf = *OFF
017905071024     c                   ENDIF
017906071024      * importo del costo
017907071024     C                   EVAL      Msgq= 'ANAMALIA sono stati impostati'
017908071024     C                             + ' dei valori di default per'
017909071024     C                             + ' l''automezzo '
017910071024     C                             + %TRIM(faitga)
017911071024     C                             + %TRIM(faitgn)
017912071024     C                             + ' pari a EUR '
017913071024     C                             + ' ' + %EDITC(faisco:'K')
017914071024     C                             + ' CONTROLLARE!'
017915071024     C                   EXCEPT    RigMsgq
018000030117     C                   ENDsl
018001070802     c* controllo se autista licenziato
018002070802     c                   if        d61cauo <> ' '
018003070802     c                   exsr      srmz47
018004070802     c                   if        d47dlio <> 0 and d61dta >= d47dlio
018005071024     c* stampa automezzi
018006071024     c                   IF        *INOf
018007071024     c                   EXCEPT    Testa
018008071024     C                   EVAL      *INOf = *OFF
018009070906     c                   ENDIF
018010070802     c                   move      d47dlio       dtaiso
018011070802     c                   move      dtaiso        dtaeur
018012070802     c                   move      dtaiso        como10           10
018013071024     C                   EVAL      Msgq= 'ANAMALIA autista '
018014070802     C                             + %TRIM(d61cauo)
018015070802     C                             + ' '
018016070802     C                             + %TRIM(d47cogo)
018017070802     C                             + ' LICENZIATO in data '
018018070802     C                             + como10
018019070802     C                             + ' CONTROLLARE!'
018020071024     C                   EXCEPT    RigMsgq
018021070802     c                   end
018022070802     c                   end
018023070802     c                   if        d61cauo = ' ' or d47erro <> ' '
018024071024     c* stampa automezzi
018025071024     c                   IF        *INOf
018026071024     c                   EXCEPT    Testa
018027071024     C                   EVAL      *INOf = *OFF
018028070906     c                   ENDIF
018029071024     C                   EVAL      Msgq= 'ERRORE Non è stata'
018030070802     C                             + ' trovata l''anagrafica per'
018031070802     C                             + ' l''autista dell''automezzo'
018032070802     C                             + %TRIM(faitga)
018033070802     C                             + %TRIM(faitgn)
018034071024     C                   EXCEPT    RigMsgq
018035070802     c                   end
018100030117     c*
018200010601     c                   z-add     numsys        fe2sys
018300010601     c                   z-add     numass        fe2nrasreg
018400021219     c                   eval      fe2tprig1 = '5'
018500031028     c                   eval      fe2tprig2 = faitmv
018600031125     c                   eval      fe2tpimp = d61cat
018700070228     c* esistono conti diversi se la società in dotazione è la società che riceve la fattura o no
018800070228     c                   if        d61socval = feaSocieta
018900031125     c                   move      'B'           fe2tpimp
019000031125     c                   else
019100031125     c                   move      'G'           fe2tpimp
019200031125     c                   end
019300031027     c* controllo il conto
019400021219     c                   exsr      srconto
019500021218     c                   z-add     wnrriga       fe2nrriga
019600031027     c                   move      'EUR'         fe2divisa
019700031028     c                   move      faidtm        fe2dtperi
019800031028     c                   move      faidtm        fe2dtperf
019900031027     c* reperisco la % IVA
020000031027     c     fe0tpfor      chain     ynfed01l
020100021212     c                   if        %found(ynfed01l)
020200021218     c                   movel     fedpiva       fe2alqiva
020300031125     c* a seconda che sia una % o meno devo riempire o meno il campo esenz.
020400031125     c                   if        fedpiva = 0
020500031125     c                   eval      fe2rifiva = fedrifivae
020600031125     c                   else
020700031125     c                   clear                   fe2rifiva
020800031125     c                   end
020900030114     c                   else
020901071024     c* stampa contabilità
021000021219     c                   IF        *INOA
021100021219     c                   EXCEPT    Testata
021200021219     C                   EVAL      *INOA = *OFF
021300021219     c                   ENDIF
021400021219     C                   EVAL      Msg = 'ERRORE Non è stata'
021500021219     C                             + ' trovata la tavola di raccordo '
021600031028     C                             + 'dell''IVA'
021700021219     C                   EXCEPT    RigMsg
021800021219     c                   EVAL      WrkCmtBack = RollBack
021900021219     c                   exsr      uscita
022000021212     c                   end
022100031125     c                   eval      comodop = 100 + fedpiva
022200031125     c                   eval(h)   fe2impon = (faisco * 100)/comodop
022300031028     c                   eval      fe2imposta = faisco - fe2impon
022400021218     c                   eval      fe2tprif1 = 'CDC'
022500031111     c                   eval      n61tga = faitga
022600031111     c                   eval      n61tgn = faitgn
022700031111     C                   IF        FAIDES <> ' '
022800031111     c                   eval      n61tra = faides
022900031111     C                   else
023000031111     c                   if        faitmv = '37'
023100031111     c                   eval      n61tra = 'CANONE TELEPASS'
023200031111     c                   end
023300031111     c                   end
023400031111     c                   movel     faicod        n61nte
023500031111     c                   eval      n61ora = faioram
023600031111     c                   eval      n61cla = faicla
023700031111     c                   eval      fe2note = trmznote1
023800021218     c                   eval      fe2tprif1 = featprif1
023900021218     c                   eval      fe2entit1 = d61cdc
024000021218     c                   eval      fe2tprif2 = featprif2
024100021218     c                   eval      fe2entit2 = feaentit2
024200021218     c                   eval      fe2tprif3 = featprif3
024300021218     c                   eval      fe2entit3 = feaentit3
024400021218     c                   eval      fe2tprif4 = featprif4
024500021218     c                   eval      fe2entit4 = feaentit4
024600021218     c                   eval      fe2tprif5 = featprif5
024700021218     c                   eval      fe2entit5 = feaentit5
024800021218     c                   eval      fe2tprif6 = featprif6
024900021218     c                   eval      fe2entit6 = feaentit6
025000021218     c                   eval      fe2tprif7 = featprif7
025100021218     c                   eval      fe2entit7 = feaentit7
025200021218     c                   eval      fe2tprif8 = featprif8
025300021218     c                   eval      fe2entit8 = feaentit8
025400021218     c                   eval      fe2socval = d61socval
025500031125     c* memorizzo le causali e le % ind.utilizzate, mi serviranno in seguito
025600031125     c* per l'iva
025700031125     c                   if        feccauin <> *blanks
025800031125     c                   z-add     1             x                 3 0
025900031125     c     feccauin      lookup    cau(x)                                 55
026000031125     c                   if        not *in55
026100031125     c                   add       1             z
026200031125     c                   z-add     fecpind       per(z)
026300031125     c                   movel     feccauin      cau(z)
026400031125     c                   end
026500031125     c                   end
026600031125     c                   z-add     fecpind       fe2pind
026700061018     c*                  z-add     0             salnrr
026800031125     c* calcolo il costo : se c'è la % indetraibilità aggiungo anche la
026900031125     c* quota dell'iva
027000031125     c                   if        feccauin<>*blanks and fe2alqiva<>'000'
027100031125     c                   if        fecpind <> 100
027200031125     c     fe2imposta    mult      fe2pind       comodo
027300031125     c     comodo        div(h)    100           fe2imp
027400031125     c* memorizzo il nrr dell'ultimo fe2 scritto con % indetraibiltà <> 100
027500031125     c* x poter poi quadrare i costi con quelli del fe3
027600031125     c                   z-add     nrrfe2        salnrr
027700031125     c                   else
027800031125     c                   z-add     fe2imposta    fe2imp
027900031125     c                   end
028000031125     c                   end
028100031125     c                   add       fe2impon      fe2imp
028200031125     c                   add       fe2imp        costo2
028300021212     c                   write     ynfe2000
028400021218     c* IVA
028500021218     c                   clear                   ynfe3000
028600031125     c                   if        fe2pind <> 0
028700031125     c                   movel     feccauin      fe3cauin
028800031125     c                   end
028900030114     c                   movel     fe2alqiva     fe3alqiva
029000030114     c                   movel     fe2rifiva     fe3rifiva
029100021212     c     kfe3          chain     ynfe301l
029200031125     c* memorizzo l'importo comprensivo di iva. La scorporto poi alla fine
029300031125     c                   add       faisco        fe3impnib
029400010601     c                   z-add     numsys        fe3sys
029500010601     c                   z-add     numass        fe3nrasreg
029600021212     c                   if        not %found(ynfe301l)
029700021220     c                   add       1             fe3nrriga
029800010531     c                   write     ynfe3000
029900021212     c                   else
030000021212     c                   update    ynfe3000
030100021212     c                   end
030200021218     c*
030300021218     c                   endsr
030301070802     C**------------------------
030302070802     C** decodifica il dipendente
030303070802     C**------------------------
030304070802     C     srmz47        BEGSR
030305070802     c                   clear                   trmz62ds
030306070802     c                   move      d61dta        d62dta
030307070802     c                   move      d61cauo       d62cau
030308070802     c                   call      'TRMZ62R'
030309070802     c                   parm                    trmz62ds
030310070802     C                   clear                   trmz47ds
030311070802     c                   if        d62soco <> ' '
030312070802     c                   eval      d47soc = d62soco
030313070802     c                   eval      d47mat = d62mato
030314070802     c                   eval      kpjbu = trmz47ds
030315070802     c                   call      'TRMZ47C'
030316070802     c                   parm                    kpjba
030317070802     c                   eval      trmz47ds = kpjbu
030318070802     c                   else
030319070802     c                   eval      d47erro = '1'
030320070802     c                   end
030321070802     C                   ENDSR
030400010601      *---------------------------------------------------
030500010601     c     newdoc        begsr
030600010601      *---------------------------------------------------
030700020205     C                   EVAL      WrkErrDoc = *OFF
030800020205
030900010601     C                   MOVE      *DATE         XnmDat
031000010601     C                   MOVE      *DATE         XnmDag                         opzionale
031100010601      *
031200010601     C                   CALLB     'XNMR'
031300020205     C                   PARM                    XnmRic
031400020205     C                   PARM      *blank        XnmSoc
031500020205     C                   PARM      *BLANK        XnmUni
031600020205     C                   PARM      *BLANK        XnmCtb
031700020205     C                   PARM      *BLANK        XnmKey
031800020205     C                   PARM                    XnmSer
031900020205     C                   PARM      *OFF          XnmCmt
032000020205     C                   PARM                    XnmDat
032100020205     C                   PARM      *OFF          XnmIva
032200020205     C                   PARM      *OFF          XnmReg
032300020205     C                   PARM      *BLANK        XnmErr
032400020205     C                   PARM      *ZERO         XnmNum
032500020205     C                   PARM      *BLANK        XnmAut
032600020205     C                   PARM                    XnmDag                         opzionale
032700010601      *
032800010601     C                   IF        XnmErr =  Errore
032900010601     C                   EXSR      Uscita
033000010601     C                   ENDIF
033100010601
033200021219     C                   IF        Xnmser =  'NRAS'
033300021219     c                   clear                   ynfe1000
033400021218     c                   clear                   wnrriga
033500021219     c                   movel     '380'         fe1tpdoc
033600021218     c* aggancio anagrafica fornitori e condizioni di pagamento
033700021218     c                   exsr      srfor
033800021218     c* aggancio anagrafica causali
033900021218     c                   clear                   ynfeb000
034000021218     c     kfeb          chain     ynfeb01l
034100021218     c                   if        not %found(ynfeb01l)
034101071024     c* stampa contabilità
034200021218     c                   IF        *INOA
034300021218     c                   EXCEPT    Testata
034400021218     C                   EVAL      *INOA = *OFF
034500021218     c                   ENDIF
034600021218     C                   EVAL      Msg = 'ERRORE Non trovata'
034700021218     C                             + ' tavola raccordo conti.'
034800021218     C                   EXCEPT    RigMsg
034900021218     c                   EVAL      WrkCmtBack = RollBack
035000021218     c                   exsr      uscita
035100021218     c                   end
035200031027     C*
035300031027     C                   EVAL      numass = XnmNum
035400031028     c                   move      knraz         fe0rifflu
035500031027     c                   eval      fe1tpfor = fe0tpfor
035600031028     c                   clear                   fe1nrdoc
035700031027     c                   eval      fe1rifflu = fe0rifflu
035800031027     c                   movel(p)  feacontra     fe1contra
035900031027     c                   movel     xscrgs        fe1rgscl
036000031027     c                   movel     xscvia        fe1indcl
036100031027     c                   movel     xsccit        fe1loccl
036200031027     c                   movel     xsccap        fe1capcl
036300031027     c                   movel     xsciva        fe1ivacl
036400031027     c                   movel     xsccdf        fe1fiscl
036500031027     c                   movel     'EUR'         fe1divfat
036600031027     c                   movel     'EUR'         fe0divfat
036700031027     c                   movel     'EUR'         fe1divrif
036800031027     c                   add       1             fe0ntfat
036900031027     c                   add       1             fe0ntdoc
037000021218
037100031107     C                   ENDIF
037200010601     c                   endsr
037300020206
037400021219      *------------------------------------------------
037500021219     c     srconto       begsr
037600021219      *------------------------------------------------
037700021219     c     kfec          chain     ynfec01l                           98
037800021219
037900021219     c                   if        *in98
037901071024     c* stampa contabilità
038000021219     c                   IF        *INOA
038100021219     c                   EXCEPT    Testata
038200021219     C                   EVAL      *INOA = *OFF
038300021219     c                   ENDIF
038400021219     C                   EVAL      Msg = 'ERRORE La riga '
038500021219     C                             + %TRIM(Fe2tprig1)
038600021219     C                             + %TRIM(Fe2tprig2)
038700021219     C                             + %TRIM(Fe2tpimp)
038800021219     C                             + ' non è stata trovata'
038900021219     C                             + ' nel relativo archivio.'
039000021219     C                   EXCEPT    RigMsg
039100021219     c                   EVAL      WrkCmtBack = RollBack
039200021219     c                   exsr      uscita
039300021219     c                   end
039400021219     c*
039500021219     c                   endsr
039600031027      *------------------------------------------------
039700031027     c     srfinedoc     begsr
039800031027      *------------------------------------------------
039900031027     c                   if        conta = 1
040000031125     c* calcolo imponibile e IVA del totale fattura
040100031125     c                   clear                   tot
040200031125     c                   clear                   alq
040300031125     c                   clear                   rig
040400031125     c                   clear                   iva
040500031125     c                   clear                   imp
040600031125     c                   clear                   z                 3 0
040700031125     c     kfe32         setll     ynfe302l
040800031125     c                   do        *hival
040900031125     c     kfe32         reade     ynfe302l
041000031125     c                   if        %eof(ynfe302l)
041100031125     c                   leave
041200031125     c                   end
041300031125     c                   if        fe3alqiva <> *all'0'
041400031125     c                   z-add     1             x                 3 0
041500031125     c     fe3alqiva     lookup    alq(x)                                 55
041600031125     c                   if        not *in55
041700031125     c                   add       1             z
041800031125     c                   if        fe3cauin <> *blanks
041900031125     c                   z-add     2             rig(z)
042000031125     c                   else
042100031125     c                   z-add     1             rig(z)
042200031125     c                   end
042300031125     c                   eval      alq(z) = fe3alqiva
042400031125     c                   z-add     fe3impnib     tot(z)
042500031125     c                   else
042600031125     c                   if        fe3cauin <> *blanks
042700031125     c                   add       2             rig(z)
042800031125     c                   else
042900031125     c                   add       1             rig(z)
043000031125     c                   end
043100031125     c                   add       fe3impnib     tot(z)
043200031125     c                   end
043300031125     c                   end
043400031125     c                   enddo
043500031125     c                   do        10            z
043600031125     c                   if        alq(z) <> *blanks
043700031125     c                   move      alq(z)        com03
043800031125     c     tot(z)        mult      com03         comodo
043900031125     c                   add       100           com03
044000031125     c     comodo        div(h)    com03         iva(z)
044100031125     c     tot(z)        sub       iva(z)        imp(z)
044200031125     c                   end
044300031125     c                   enddo
044400031125     c* calcolo imponibile e IVA del file ynfe300f
044500031125     c     kfe32         setll     ynfe302l
044600031125     c                   do        *hival
044700031125     c     kfe32         reade     ynfe302l
044800031125     c                   if        %eof(ynfe302l)
044900031125     c                   leave
045000031125     c                   end
045100031125     c* scorporo l'iva
045200031125     c                   if        fe3alqiva <> *all'0'
045300031125     c                   move      fe3alqiva     com03
045400031125     c     fe3impnib     mult      com03         comodo
045500031125     c                   add       100           com03
045600031125     c     comodo        div(h)    com03         fe3impost
045700031125     c                   sub       fe3impost     fe3impnib
045800031125     c                   update    ynfe3002
045900031125     c                   end
046000031125     c                   enddo
046100031125     c* calcolo la % indetraibilità
046200031125     c     kfe32         setll     ynfe302l
046300031125     c                   do        *hival
046400031125     c     kfe32         reade     ynfe302l
046500031125     c                   if        %eof(ynfe302l)
046600031125     c                   leave
046700031125     c                   end
046800031125     c                   if        fe3cauin <> *blanks and
046900031125     c                             fe3alqiva <> *all'0'
047000031125     c                   z-add     1             x
047100031125     c     fe3cauin      lookup    cau(x)                                 55
047200031125     c                   if        *in55 and per(x) <> 100
047300031125     c                   z-add     fe3impnib     salimp
047400031125     c                   z-add     fe3impost     saliva
047500031125     c     fe3impnib     mult      per(x)        comodo
047600031125     c     comodo        div(h)    100           fe3impnib
047700031125     c     fe3impost     mult      per(x)        comodo
047800031125     c     comodo        div(h)    100           fe3impost
047900031125     c                   update    ynfe3002
048000031125     c* scrivo o aggiorno l' iva detraibile
048100031125     c     saliva        sub       fe3impost     detimpost
048200031125     c     salimp        sub       fe3impnib     detimpnib
048300031125     c                   clear                   fe3cauin
048400031125     c     kfe3          chain     ynfe301l
048500031125     c                   if        not %found(ynfe301l)
048600031125     c                   z-add     detimpost     fe3impost
048700031125     c                   z-add     detimpnib     fe3impnib
048800031125     c                   write     ynfe3000
048900031125     c                   else
049000031125     c                   add       detimpost     fe3impost
049100031125     c                   add       detimpnib     fe3impnib
049200031125     c                   update    ynfe3000
049300031125     c                   end
049400031125     c                   end
049500031125     c                   end
049600031125     c                   enddo
049700031125     c* a questo punto quadro l'iva
049800031125     c                   clear                   costo3
049900031125     c     kfe32         setll     ynfe302l
050000031125     c                   do        *hival
050100031125     c     kfe32         reade     ynfe302l
050200031125     c                   if        %eof(ynfe302l)
050300031125     c                   leave
050400031125     c                   end
050500031125     c                   if        fe3alqiva <> *all'0'
050600031125     c                   z-add     1             x                 3 0
050700031125     c     fe3alqiva     lookup    alq(x)                                 55
050800031125     c                   if        *in55
050900031125     c                   if        rig(x) = 1
051000031125     c                   z-add     imp(x)        fe3impnib
051100031125     c                   z-add     iva(x)        fe3impost
051200031125     c                   update    ynfe3002
051300031125     c                   else
051400031125     c                   sub       fe3impnib     imp(x)
051500031125     c                   sub       fe3impost     iva(x)
051600031125     c                   sub       1             rig(x)
051700031125     c                   end
051800031125     c                   end
051900031125     c                   end
052000031125     c* mentre quador l'iva mi calcolo anche il totale dei costi
052100031125     c                   add       fe3impnib     costo3
052200031125     c                   if        fe3cauin <> *blanks
052300031125     c                   add       fe3impost     costo3
052400031125     c                   end
052500031125     c                   enddo
052600031125     c* a questo punto quadro i costi calcolati nel file ynfe2 e quelli
052700031125     C* calcolati nel ynfe3. La differenza la sommo nell'ultimo record
052800031125     c* del fe2 con % indetraibilità <> 0
052900031125     c                   if        costo2 <> costo3
053000061018     c                             and salnrr <> 0
053100061018     c     salnrr        chain     ynfe200f
053200031125     c                   if        %found(ynfe200f)
053300031125     c                   eval      fe2imp = fe2imp + (costo3 - costo2)
053400031125     c                   update    ynfe2000
053500031125     c                   end
053600031125     c                   end
053700031125     c* SCRIVO YNFE000F
053800031027     c                   move      dataiso       fe0dtwrt
053900031027     c                   write     ynfe0000
054000031027      * a rottura scrivo il record di testata per il documento letto
054100031027     c                   move      dataiso       fe1dtwrt
054200031027     c                   z-add     numsys        fe1sys
054300031027     c                   z-add     numass        fe1nrasreg
054400031027     c                   eval      fe1societa = feasocieta
054500031027     c                   eval      fe1unita = feaunita
054600031027     c                   write     ynfe1000
054700031027     c                   commit
054800031027     ** Non ci sono errori, segnalo la traduzione del documento.
054900031027     C                   IF        WrkErrDoc = *OFF
054901071024     c* stampa contabilità
055000031027     C                   IF        *INOA
055100031027     C                   EXCEPT    Testata
055200031027     C                   EVAL      *INOA = *OFF
055300031027     C                   ENDIF
055400031027     C                   EVAL      Msg =
055500031027     C                             Fe1TpDoc
055600031027     C                             + ' Documento del'
055700031027     C                             + ' ' + %CHAR(Fe1DtDoc)
055800031027     C                             + ' Tradotto con successo'
055900031027     C                             + ' nella società '
056000031027     C                             + FeaSocieta
056100031027     C                             + '.'
056200031027     C                   EXCEPT    RigMsg
056300031027     C                   ENDIF
056400031027     C                   ENDIF
056500031027     c                   endsr
056600010601      *---------------------------------------------------
056700021212     c     srfor         begsr
056800020206      *---------------------------------------------------
056900031027     c                   eval      fe0tpfor = 'FAIS'
057000021216     c* controllo il contratto
057100021216     c                   exsr      contrat
057200021216     c*
057300021212     C     Kfrn          CHAIN     anfrn01l                           20
057400021212     C  N20Krco          CHAIN     anrco98j                           20
057500021212     c                   if        not *in20
057600021212     C                   MOVEL(p)  sogdes        FE0RGSSL
057700021212     C                   eval      FE0INDSL = indindriz
057800021212     c                   if        indprov <> ' '
057900021212     C                   eval      FE0LOCSL=%trimr(indlocalit)+' ('+indprov+')'
058000021212     c                   else
058100021212     C                   eval      FE0LOCSL=indlocalit
058200021212     c                   end
058300021212     C                   eval      FE0CAPSL=indcap
058400021212     C                   eval      FE0IVASL=sogpartiva
058500021212     C                   eval      FE0FISSL=sogcdfisc
058600021216     c                   else
058700021216     c                   EVAL      WrkCmtBack = RollBack
058701071024     c* stampa contabilità
058800021219     C                   eval      msg = 'ERRORE Il codice fornitore non esist-
058900021219     C                             e nel relativo anagrafico.'
059000021216     C                   IF        *INOA
059100021216     C                   EXCEPT    Testata
059200021216     C                   EVAL      *INOA = *OFF
059300021216     C                   ENDIF
059400021216     C                   EXCEPT    RigMsg
059500021219     C                   EVAL      WrkErrDoc = *ON
059600021218     c                   exsr      uscita
059700021216     c                   endif
059800021212     c                   endsr
059900021216     ***********************************************************************
060000021216     ** Controllo contratto:
060100021216     ** - se non esiste è un errore bloccante.
060200021216     ** - se     esiste e il documento è fuori periodo validità,
060300021216     **   è un errore ma non bloccante.
060400021216     ***********************************************************************
060500021216     c     contrat       begsr
060600021216
060700030116     C                   CLEAR                   YnFea00F
060800031027     c                   eval      feacontra = 'FAISERVICE'
060900030116     c                   move      *date         feadtinco
061000030116     C                   CALL      'YCOYNFEAR'
061100030116     C                   PARM                    Fe0TpFor
061200030116     C                   PARM                    FeaContra
061300030116     C                   PARM                    feadtinco
061400030116     C                   PARM      *BLANK        PrmEsito
061500030116     C                   PARM                    YnFea00F
061600030116
061700030116     c                   if        PrmEsito = Errore
061800021216     C                   EVAL      WrkErrDoc = *ON
061900021216     c                   EVAL      WrkCmtBack = RollBack
061901071024     c* stampa contabilità
062000031125     C                   EVAL      Msg = 'ERRORE Il contratto FAISERVICE -
062100031027     C                             non esiste n-
062200021219     C                             el relativo anagrafico.'
062300021216     C                   IF        *INOA
062400021216     C                   EXCEPT    Testata
062500021216     C                   EVAL      *INOA = *OFF
062600021216     C                   ENDIF
062700021216     C                   EXCEPT    RigMsg
062800021218     c                   exsr      uscita
062900021216     c                   endif
063000021216
063100070228      // Mi collego alla società titolare del contratto.
063200070228     C                   IF        feaSocieta <> xscSoc
063300070228     C                   CALLB     'XSOC'
063400070228     C                   PARM      'SOC001'      TipXsc
063500070228     C                   PARM      feaSocieta    SocXsc
063600070228     C                   PARM                    CdsXsc
063700070228     C                   PARM                    ModXsc
063800070228     C                   PARM                    RtnXsc
063900070228     C                   PARM                    XSoc001DS
064000070228     C                   PARM                    Kpjba
064100070228     C                   ENDIF
064200070228
064300021216     c                   ENDSR
064400021212      *---------------------------------------------------
064500021212     c     *inzsr        begsr
064600021212      *---------------------------------------------------
064700020205     C                   EVAL      Knsif = PrmSif
064800021219     c                   eval      rcosnatura = 'F'
064900020204      *
065000020204     C                   CALLB     'XSOC'
065100020205     C                   PARM      'SOC001'      TipXsc
065200020205     C                   PARM                    SocXsc
065300020205     C                   PARM                    CdsXsc
065400020205     C                   PARM                    ModXsc
065500020205     C                   PARM                    RtnXsc
065600020205     C                   PARM                    XSoc001DS
065700020205     C                   PARM                    Kpjba
065800020204      *
065900020204     C                   IF        RtnXsc = Errore
066000020204     C                   EXSR      Uscita
066100020204     C                   ENDIF
066200010601      *---------------------------------------------------
066300020205     c                   TIME                    times
066400020205     c                   TIME                    dataeur
066500020205     c                   TIME                    dataiso
066600010601      * Reperisco sistema della società di immissione.
066700020205     C                   EVAL      XnmRic = '1'
066800020205     C                   EVAL      XnmSer = 'NSYS'
066900010601     C                   EXSR      newdoc
067000020205     C                   EVAL      numSys = XnmNum
067100021219      * Imposto i dati x reperire numero assoluto.
067200021219     C                   EVAL      XnmRic = '2'
067300021219     C                   EVAL      XnmSer = 'NRAS'
067400020205
067500020205     C                   EVAL      *INOA = *ON
067501071025     C                   EVAL      *INOf = *ON
067600021212     C     Kfrn          KLIST
067700021212     C                   KFLD                    socxsc
067800021212     C                   KFLD                    feakscfor
067900021212     C     Krco          KLIST
068000021216     C                   KFLD                    socxsc
068100021212     C                   KFLD                    rcosnatura
068200021219     C                   KFLD                    feakscfor
068300021219     C     Kfeb          KLIST
068400021219     C                   KFLD                    fe0tpfor
068500021219     C                   KFLD                    fe1tpdoc
068600021218     C     Kfe1          KLIST
068700021218     C                   KFLD                    fe0tpfor
068800030116     C                   KFLD                    fe0rifflu
068900021218     C     Kfe3          KLIST
069000021219     C                   KFLD                    numsys
069100021218     C                   KFLD                    numass
069200030114     C                   kfld                    fe3alqiva
069300030114     C                   kfld                    fe3rifiva
069400021218     C                   kfld                    fe3cauin
069500030114     C     Kfe32         KLIST
069600021219     C                   KFLD                    numsys
069700021218     C                   KFLD                    numass
069800021219     C     Kfec          KLIST
069900021219     C                   KFLD                    fe0tpfor
070000021219     C                   KFLD                    fe2tprig1
070100021219     C                   KFLD                    fe2tprig2
070200021219     C                   KFLD                    fe2tpimp
070300021219     C                   endsr
070400010601      *---------------------------------------------------
070500010601     c     uscita        begsr
070600010601      *---------------------------------------------------
070700010601      *
070800020205     c                   if        WrkCmtBack = RollBack
070900020205     c                   rolbk
070901071024     c* stampa contabilità
071000020205     c                   IF        *INOA
071100020205     c                   EXCEPT    Testata
071200020205     C                   EVAL      *INOA = *OFF
071300020205     c                   ENDIF
071400020205     C                   EVAL      Msg = 'ERRORE Il flusso '
071500020205     C                             + %TRIM(Fe0RifFlu)
071600020205     C                             + ' non è stato tradotto.'
071700020205     C                             + ' Correggere gli errori e ripetere'
071800020205     C                             + ' la traduzione.'
071900020205     C                   EXCEPT    RigMsg
072000030116     c                   eval      fine = errore
072100021213     c                   end
072200021213     c                   if        WrkCmtBack = commit
072300020205     c                   commit
072400010827     c                   end
072500010601     c                   seton                                        lr
072600020205     C                   RETURN
072700021213
072800010601     C                   endsr
072900010605     o*------------------------------------------------------*
073000020208     oYco3070P  e            testata        3  1
073100020205     O                       KNSif
073200031028     o                                           70 'Log traduzione FAI SERVICE'
073300020205     o                       PgmName            100
073400020205     o                       dataeur             +1
073500020205     o                       times               +1
073600020205     o                                          127 'Pag.'
073700020205     o                       page               132
073800010605      * dettaglio
073900020205     O          E            RigMsg      1
074000020205     O                       Msg
074001071024     OQSYSPRT   E            TESTA          2 02
074002071024     O                       KNSif
074003100517     O                                         + 20 'LISTA ERRORI DURANTE'
074004071024     O                                         +  1 'IL CARICAMENTO DEL F'
074005100517     O                                         +  0 'ILE FAISSERVICE'
074006071024     o                       PgmName            100
074007071024     o                       dataeur             +1
074008071024     o                       times               +1
074009071024     o                                          127 'Pag.'
074010071024     o                       page               132
074011071024     O          E            RigMsgq     1
074012071024     O                       Msgq
074100010828

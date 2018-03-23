000100021212      /TITLE Fatture Agip traduttore
000200010601
000300010607     H DFTACTGRP(*NO) ACTGRP('QILE') BNDDIR('PJXBND')
000400990504     H DEBUG DECEDIT('0,') DATEDIT(*DMY/)
000500010514
000600021218     fynfeb01l  if   e           k disk
000700021218     fynfec01l  if   e           k disk
000800021218     fynfed01l  if   e           k disk
000900021219     fynfe001l  if   e           k disk    rename(ynfe0000:ynfe0)
001000021218     fanfrn01l  if   e           k disk
001100021218     fanrco98j  if   e           k disk
001200030113     fagip02l   if   e           k disk    rename(agip:agip02)
001300030113     f                                     infds(infagip)
001400030113     fagip      uf   e             disk    rename(agip:agip00)
001500030113     f                                     commit
001600010827     fynfe000f  o    e             disk    commit
001700010827     fynfe100f  o    e             disk    commit
001800030114     fynfe200f  uf a e             disk    commit
001900030114     f                                     infds(inf2)
002000030113     fynfe301l  uf a e           k disk    commit
002100030114     fynfe302l  uf a e           k disk    commit
002200030114     f                                     rename(ynfe3000:ynfe3002)
002300020208     fYco3070P  o    f  198        printer infds(stamp)
002400020205     F                                     OFLIND(*INOA)
002401071024     fqsysprt   o    f  132        printer oflind(*inof)
002500030114     d*
002600020205     D                SDS
002700020205     D PgmName           *PROC
002800020206     D UserName              254    263
002900030114     D kpjba         e ds                  inz
003001070802     D trmz62ds      e ds                  inz
003002070802     D trmz47ds      e ds                  inz
003003070802     D trmz61ds      e ds                  inz
003100070302     D  d61FSSoc     e                     inz(SOCIETA_DOTAZIONE)
003200030114     D trmznote2     e ds                  inz
003300030116     D ynfea00f      e ds                  inz
003400030114     d*ds di comodo
003500030114     ddsagip         e ds                  extname(agip)
003600030114      *ds per reperimento società
003700030114     D XSoc001DS     E DS                  inz
003800010605      *ds controllo printer
003900010605     d stamp           ds
004000010605     d  lin                  367    368b 0
004100030113      *ds file agip x nrr
004200030113     d infagip         ds
004300030113     d  nrragip              397    400i 0
004400030114      *ds file ynfe2 x nrr
004500030114     d inf2            ds
004600030114     d  nrrfe2               397    400i 0
004700030114     d* variabili
004800021212     D Ptr             S               *
004900021216     D                                     INZ(%ADDR(numer00001))
005000021212     D docum           S             17    based(ptr)
005100030122     d docums          s                   like(docum)
005200030122     d impcom          s                   like(importo)
005300021220     d rig             s              3  0 dim(10)
005400021220     d alq             s              3    dim(10)
005500021220     d tot             s             10  2 dim(10)
005600021220     d iva             s             10  2 dim(10)
005700021220     d imp             s             10  2 dim(10)
005800030114     d cau             s              4    dim(10)
005900030114     d per             s              5  2 dim(10)
006000010601     D TipXsc          S              6
006100010601     D SocXsc          S              3
006200010601     D CdsXsc          S              9  0
006300010601     D ModXsc          S              3
006400010601     D RtnXsc          S              1
006500010601     D XnmRic          S              1
006600010601     D XnmSoc          S              3
006700010601     D XnmUni          S              8
006800010601     D XnmCtb          S              2
006900010601     D XnmKey          S              8
007000010601     D XnmSer          S              4
007100010601     D XnmCmt          S              1
007200010601     D XnmDat          S                   LIKE(fe1dtupd)
007300010601     D XnmIva          S              1
007400010601     D XnmReg          S              1
007500010601     D XnmErr          S              1
007600010601     D XnmNum          S              9p 0
007700010601     D XnmAut          S              1
007800010601     D XnmDag          S                   like(fe1dtupd)
007900010601     D numsys          s              3p 0
008000010601     D numass          s              9p 0
008100020205     d times           s               T
008200010601     d dataiso         s               d   datfmt(*iso)
008300010601     d dataEUR         s               d   datfmt(*EUR)
008301070802     d dtaiso          s               d   datfmt(*iso)
008302070802     d dtaEUR          s               d   datfmt(*EUR)
008400021219     D flag1           S                   LIKE(flag00001)
008500021220     D saliva          S                   LIKE(fe3impost)
008600021220     D salimp          S                   LIKE(fe3impnib)
008700030114     D detimpost       S                   LIKE(fe3impost)
008800030114     D detimpnib       S                   LIKE(fe3impnib)
008900030114     D costo2          S                   LIKE(fe0impfat)
009000030114     D costo3          S                   LIKE(fe0impfat)
009100030114     D salnrr          S                   LIKE(nrrfe2)
009200021218     d comodo          s             13  2
009300021218     d com03           s              3  0
009400021219     d com08           s              8  0
009500021218     d wnrriga         s              5  0
009600020205     D WrkCmtBack      S              1
009700020205     D                                     INZ(Commit)
009800030114     D WrkErrDoc       S              1    inz('0')
009900020206     D Msg             S            198
009901071024     D Msgq            S            132
010000030114     D PrmSif          S                   like(knsif)
010100030116     D fine            S              1
010200030116     D prmesito        S              1
010300030114     c* costanti
010400030114     D Commit          C                   'C'
010500030114     D RollBack        C                   'R'
010600030114     D Errore          c                   '1'
010700070302     D SOCIETA_DOTAZIONE...
010800070302     D                 C                   *BLANK
010900070302
011000020204     C     *ENTRY        PLIST
011100020205     C                   PARM                    PrmSif
011200020205     C                   PARM                    SocXsc
011300030116     C                   PARM                    fine
011400030116     c                   clear                   fine
011500021216     c* controllo AGIP00F se ci sono dei record con FLAG00001 = blank
011600021218     c* segnalo l'errore bloccante xchè vuol dire che ci sono dei
011700021216     c* record che non sono ancora stati ricevuti
011800021219     c                   clear                   flag1
011900021219     c     flag1         chain     agip02l
012000021219     c                   if        %found(agip02l)
012001071024     c* stampa contabilità
012100021213     C                   IF        *INOA
012200021213     C                   EXCEPT    Testata
012300021213     C                   EVAL      *INOA = *OFF
012400021213     C                   ENDIF
012500021216     C                   EVAL      Msg = 'Riproporre la contabilizzazione -
012600021216     C                             tra poco perchè è in corso la ricezione -
012700021216     C                             dell''archivio dell''AGIP'
012800021213     C                   EXCEPT    RigMsg
012900021216     c                   exsr      uscita
013000021213     c                   end
013100010601      *lettura file ricevuto e tradotto su AS
013200021219     c                   eval      flag1 = 'A'
013300021219     c     flag1         setll     agip02l
013400010531     c                   do        *hival
013500021219     c     flag1         reade     agip02l                                99
013600010531     c   99              leave
013700021212      * E' iniziato un nuovo documento
013800021212     c                   if        docum <> docums
013900021219     c                   exsr      srfinedoc
014000021218      *operazioni da fare per nuovo documento
014100010601     c                   exsr      newdoc
014200020205     C                   EVAL      numass = XnmNum
014300021219     C                   EVAL      docums = docum
014400021219     C                   ENDIF
014500030113     ** Metto il flag di contabilizzato sul file AGIP
014600030113     c     nrragip       chain     agip
014700030113     c                   movel     'C'           flag00001
014800030113     c                   update    agip00
014900030113     c* Tratto il record letto.
015000021212     c                   exsr      srwrite
015100020205     c                   ENDDO
015200020807
015300020807     ** Fine lettura.
015400021219     c                   exsr      srfinedoc
015500021219     c* chiudo il pgm TRMZ61R
015600021219     c                   clear                   trmz61ds
015700021219     c                   eval      d61tpe = 'C'
015800021219     c                   call      'TRMZ61R'
015900021219     c                   parm                    trmz61ds
016000010605      *
016100010601     c                   exsr      uscita
016200010514      *------------------------------------------------
016300021219     c     srfinedoc     begsr
016400010514      *------------------------------------------------
016500021219     c                   if        fe0impfat <> 0
016600021219     ** Eseguo i controlli per totale flusso.
016700021219     c                   exsr      FineFlusso
016800021219      * a rottura scrivo il record di testata per il documento letto
016900021219     c                   move      dataiso       fe1dtwrt
017000021219     c                   z-add     numsys        fe1sys
017100021219     c                   z-add     numass        fe1nrasreg
017200030113     c                   eval      fe1societa = feasocieta
017300030113     c                   eval      fe1unita = feaunita
017400021219     c                   write     ynfe1000
017500021219     c                   commit
017600021219     ** Non ci sono errori, segnalo la traduzione del documento.
017700021219     C                   IF        WrkErrDoc = *OFF
017701071024     c* stampa contabilità
017800021219     C                   IF        *INOA
017900021219     C                   EXCEPT    Testata
018000021219     C                   EVAL      *INOA = *OFF
018100021219     C                   ENDIF
018200021219     C                   EVAL      Msg =
018300021219     C                             Fe1TpDoc
018400021219     C                             + ' ' + %TRIM(Fe1NrDoc)
018500021219     C                             + ' ' + %CHAR(Fe1DtDoc)
018600021219     C                             + ' ' + %EDITC(Fe1ImpDoc:'K')
018700021219     C                             + ' Tradotto con successo'
018800021219     C                             + ' nella società '
018900021219     C                             + FeaSocieta
019000021219     C                             + '.'
019100021219     C                   EXCEPT    RigMsg
019200021219     C                   ENDIF
019300021219     C                   ENDIF
019400021219     c                   endsr
019500021219      *------------------------------------------------
019600021219     c     srwrite       begsr
019700021219      *------------------------------------------------
019800021219     c* operazioni da fare solo il primo record
019900021219     c                   if        primo = 0
020000021219     c                   move      data_00001    com08
020100021219     c                   move      com08         fe0dtflu
020200021219     c                   move      com08         fe0dtrif
020300021218     c                   eval      fe0rifflu = 'AGIP' + docum
020400021219     c                   exsr      controllo
020500021219     c                   eval      fe1tpfor = fe0tpfor
020600021219     c                   eval      fe1nrdoc = numer00001
020700021219     c                   move      com08         fe1dtdoc
020800021219     c                   movel     com08         fe1prdctb
020900021219     c                   move      com08         fe1dtfati
021000021219     c                   move      com08         fe1dtfatf
021100021219     c                   eval      fe1rifflu = fe0rifflu
021200021219     c                   movel(p)  feacontra     fe1contra
021300021219     c                   movel     xscrgs        fe1rgscl
021400021219     c                   movel     xscvia        fe1indcl
021500021219     c                   movel     xsccit        fe1loccl
021600021219     c                   movel     xsccap        fe1capcl
021700021219     c                   movel     xsciva        fe1ivacl
021800021219     c                   movel     xsccdf        fe1fiscl
021900021219     c                   movel     valuta        fe1divfat
022000021220     c                   movel     valuta        fe0divfat
022100021219     c                   movel     valuta        fe1divrif
022200021219     c                   z-add     1             primo             2 0
022300021219     c                   add       1             fe0ntfat
022400021219     c                   add       1             fe0ntdoc
022500021219     c                   end
022600021219     c* operazioni da fare per tutti i records
022700021218     c                   add       importo       fe0impfat
022800021218     c                   sub       sconto        fe0impfat
022900021218     c                   add       importo       fe0impdoc
023000021218     c                   sub       sconto        fe0impdoc
023100010601      *fatture
023200021218      * importo da pagare
023300021218     c                   add       importo       fe1imppag
023400021218     c                   sub       sconto        fe1imppag
023500021218      * importo totale documento
023600021218     c                   add       importo       fe1impdoc
023700021218     c                   sub       sconto        fe1impdoc
023800021218     c* inizio dettaglio
023900021218     c                   ADD       1             wnrriga
024000021212     c                   clear                   ynfe2000
024100030117     c* reperisco il centro di costo, la società a valere e la categoria
024200030117     c* dall'anagrafico automezzi
024300070129     c                   reset                   trmz61ds
024400030117     c                   movel     nominativo    d61tga
024500060530     c                   if        %subst(nominativo: 8: 1) <> ' '
024600060530     c                   eval      d61tgn = %subst(nominativo: 3: 6)
024700060530     c                   else
024800030117     c                   eval      d61tgn = %subst(nominativo: 3: 3)+' ' +
024900030117     c                             %subst(nominativo: 6: 2)
025000060530     c                   end
025100030117     c                   move      data_00003    com08
025200030117     c                   move      com08         d61dta
025300030117     c                   call      'TRMZ61R'
025400030117     c                   parm                    trmz61ds
025500030117     c* ritorno E= errore bloccante A=anomalia
025600030117     c                   select
025700030117     c                   when      d61err = 'E'
025701071024     c* stampa contabilità
025800030117     c                   IF        *INOA
025900030117     c                   EXCEPT    Testata
026000030117     C                   EVAL      *INOA = *OFF
026100030117     c                   ENDIF
026200030117     C                   EVAL      Msg = 'ERRORE Non è stata'
026300030117     C                             + ' trovata l''anagrafica per'
026400030117     C                             + ' l''automezzo '
026500070906     C                             + ' ' + %TRIM(nominativo)
026600030117     C                   EXCEPT    RigMsg
026601071024     c* stampa automezzi
026602071024     c                   IF        *INOf
026603071024     c                   EXCEPT    Testa
026604071024     C                   EVAL      *INOf = *OFF
026605071024     c                   ENDIF
026606071024     C                   EVAL      Msgq= 'ERRORE Non è stata'
026607071024     C                             + ' trovata l''anagrafica per'
026608071024     C                             + ' l''automezzo '
026609071024     C                             + ' ' + %TRIM(nominativo)
026610071024     C                   EXCEPT    RigMsgq
026700030117     c                   EVAL      WrkCmtBack = RollBack
026800030117     c                   exsr      uscita
026900030117     c* anomalia
027000030117     c                   when      d61err = 'A'
027001071024     c* stampa contabilità
027100030117     c                   IF        *INOA
027200030117     c                   EXCEPT    Testata
027300030117     C                   EVAL      *INOA = *OFF
027400030117     c                   ENDIF
027500030122      * importo del costo
027600030122     c                   z-add     importo       impcom
027700030122     c                   sub       sconto        impcom
027800030117     C                   EVAL      Msg = 'ANAMALIA sono stati impostati'
027900030117     C                             + ' dei valori di default per'
028000030117     C                             + ' l''automezzo '
028100070906     C                             + ' ' + %TRIM(nominativo)
028200030122     C                             + ' pari a EUR '
028300030122     C                             + ' ' + %EDITC(Impcom:'K')
028400030117     C                             + ' CONTROLLARE!'
028500030117     C                   EXCEPT    RigMsg
028501071024     c* stampa automezzi
028502071024     c                   IF        *INOf
028503071024     c                   EXCEPT    Testa
028504071024     C                   EVAL      *INOf = *OFF
028505071024     c                   ENDIF
028506071024      * importo del costo
028507071024     c                   z-add     importo       impcom
028508071024     c                   sub       sconto        impcom
028509071024     C                   EVAL      Msgq= 'ANAMALIA sono stati impostati'
028510071024     C                             + ' dei valori di default per'
028511071024     C                             + ' l''automezzo '
028512071024     C                             + ' ' + %TRIM(nominativo)
028513071024     C                             + ' pari a EUR '
028514071024     C                             + ' ' + %EDITC(Impcom:'K')
028515071024     C                             + ' CONTROLLARE!'
028516071024     C                   EXCEPT    RigMsgq
028600030117     C                   ENDsl
028601070802     c* controllo se autista licenziato
028602070802     c                   if        d61cauo <> ' '
028603070802     c                   exsr      srmz47
028604070802     c                   if        d47dlio <> 0 and d61dta >= d47dlio
028605071024     c* stampa automezzi
028606071024     c                   IF        *INOf
028607071024     c                   EXCEPT    Testa
028608071024     C                   EVAL      *INOf = *OFF
028609070906     c                   ENDIF
028610070802     c                   move      d47dlio       dtaiso
028611070802     c                   move      dtaiso        dtaeur
028612070802     c                   move      dtaiso        como10           10
028613071024     C                   EVAL      Msgq= 'ANAMALIA autista '
028614070906     C                             + ' '
028615070802     C                             + %TRIM(d61cauo)
028616070802     C                             + ' '
028617070802     C                             + %TRIM(d47cogo)
028618070802     C                             + ' LICENZIATO in data '
028619070802     C                             + como10
028620070802     C                             + ' CONTROLLARE!'
028621071024     C                   EXCEPT    RigMsgq
028622070802     c                   end
028623070802     c                   end
028624071024     c                   if        d61cauo = ' ' or d47erro <> ' '
028625071024     c* stampa automezzi
028626071024     c                   IF        *INOf
028627071024     c                   EXCEPT    Testa
028628071024     C                   EVAL      *INOf = *OFF
028629070906     c                   ENDIF
028630071024     C                   EVAL      Msgq= 'ERRORE Non è stata'
028631070802     C                             + ' trovata l''anagrafica per'
028632070802     C                             + ' l''autista dell''automezzo'
028633070802     C                             + %TRIM(nominativo)
028634071024     C                   EXCEPT    RigMsgq
028635070802     c                   end
028700030117     c*
028800010601     c                   z-add     numsys        fe2sys
028900010601     c                   z-add     numass        fe2nrasreg
029000021219     c                   eval      fe2tprig1 = '5'
029100021219     c                   eval      fe2tprig2 = COD_P00001
029200030117     c                   eval      fe2tpimp = d61cat
029300030129     c* esistono conti diversi se la società in dotazione è Bartolini o no
029400070302     c                   if        d61socval = feaSocieta
029500030129     c                   move      'B'           fe2tpimp
029600030129     c                   else
029700030129     c                   move      'G'           fe2tpimp
029800030129     c                   end
029900030117     c* reperisce % indetraibilità e causale indetraibilità
030000021219     c                   exsr      srconto
030100021218     c                   z-add     wnrriga       fe2nrriga
030200021216     c                   move      valuta        fe2divisa
030300021219     c                   move      DATA_00003    com08
030400021219     c                   move      com08         fe2dtperi
030500021219     c                   move      com08         fe2dtperf
030600021218     c* reperisco la % o l'esenzione IVA
030700021218     c                   eval      fedrifivae = aliqu00001
030800021218     c     kali          chain     ynfed01l
030900021212     c                   if        %found(ynfed01l)
031000021218     c                   movel     fedpiva       fe2alqiva
031100030114     c* a seconda che sia una % o meno devo riempire o meno il campo esenz.
031200030114     c                   if        fedpiva = 0
031300030114     c                   eval      fe2rifiva = aliqu00001
031400021219     c                   else
031500030114     c                   clear                   fe2rifiva
031600030114     c                   end
031700030114     c                   else
031701071024     c* stampa contabilità
031800021219     c                   IF        *INOA
031900021219     c                   EXCEPT    Testata
032000021219     C                   EVAL      *INOA = *OFF
032100021219     c                   ENDIF
032200021219     C                   EVAL      Msg = 'ERRORE Non è stata'
032300021219     C                             + ' trovata la tavola di raccordo '
032400021219     C                             + 'dell''IVA per il codice '
032500021219     C                             + %TRIM(aliqu00001)
032600021219     C                   EXCEPT    RigMsg
032700021219     c                   EVAL      WrkCmtBack = RollBack
032800021219     c                   exsr      uscita
032900021212     c                   end
033000021216     c                   movel     un_mi00001    fe2um
033100021216     c                   z-add     quantita      fe2qta
033200021216     c                   z-add     prezzo        fe2przun
033300021218     c                   z-add     sconto        fe2sconto
033400021218     c                   z-add     imponibile    fe2impon
033500021218     c                   z-add     impor00001    fe2imposta
033600021218     c                   eval      fe2tprif1 = 'CDC'
033700021220     c                   eval      n60tga  = d61tga
033800021220     c                   eval      n60tgn  = d61tgn
033900021220     c                   eval      n60nvo  = numer00003
034000021220     c                   eval      n60km  = chilometri
034100021220     c                   eval      n60ind = indir00001
034200021220     c                   eval      n60loc = local00001
034300021220     c                   eval      n60pro = prov_pv
034400021220     c                   eval      fe2note = trmznote2
034500021218     c                   eval      fe2tprif1 = featprif1
034600021218     c                   eval      fe2entit1 = d61cdc
034700021218     c                   eval      fe2tprif2 = featprif2
034800021218     c                   eval      fe2entit2 = feaentit2
034900021218     c                   eval      fe2tprif3 = featprif3
035000021218     c                   eval      fe2entit3 = feaentit3
035100021218     c                   eval      fe2tprif4 = featprif4
035200021218     c                   eval      fe2entit4 = feaentit4
035300021218     c                   eval      fe2tprif5 = featprif5
035400021218     c                   eval      fe2entit5 = feaentit5
035500021218     c                   eval      fe2tprif6 = featprif6
035600021218     c                   eval      fe2entit6 = feaentit6
035700021218     c                   eval      fe2tprif7 = featprif7
035800021218     c                   eval      fe2entit7 = feaentit7
035900021218     c                   eval      fe2tprif8 = featprif8
036000021218     c                   eval      fe2entit8 = feaentit8
036100021218     c                   eval      fe2socval = d61socval
036200030117     c* memorizzo le causali e le % ind.utilizzate, mi serviranno in seguito
036300030114     c* per l'iva
036400030114     c                   if        feccauin <> *blanks
036500030114     c                   z-add     1             x                 3 0
036600030114     c     feccauin      lookup    cau(x)                                 55
036700030114     c                   if        not *in55
036800030114     c                   add       1             z
036900030114     c                   z-add     fecpind       per(z)
037000030114     c                   movel     feccauin      cau(z)
037100030114     c                   end
037200030114     c                   end
037300030114     c                   z-add     fecpind       fe2pind
037400061018     c*                  z-add     0             salnrr
037500030117     c* calcolo il costo : se c'è la % indetraibilità aggiungo anche la
037600030117     c* quota dell'iva
037700030117     c                   if        feccauin<>*blanks and fe2alqiva<>'000'
037800030117     c                   if        fecpind <> 100
037900030114     c     fe2imposta    mult      fe2pind       comodo
038000030114     c     comodo        div(h)    100           fe2imp
038100030117     c* memorizzo il nrr dell'ultimo fe2 scritto con % indetraibiltà <> 100
038200030117     c* x poter poi quadrare i costi con quelli del fe3
038300030114     c                   z-add     nrrfe2        salnrr
038400030114     c                   else
038500030114     c                   z-add     fe2imposta    fe2imp
038600030114     c                   end
038700030117     c                   end
038800030114     c                   add       fe2impon      fe2imp
038900030114     c                   add       fe2imp        costo2
039000021212     c                   write     ynfe2000
039100021218     c* IVA
039200021218     c                   clear                   ynfe3000
039300021218     c                   if        fe2pind <> 0
039400030114     c                   movel     feccauin      fe3cauin
039500021218     c                   end
039600030114     c                   movel     fe2alqiva     fe3alqiva
039700030114     c                   movel     fe2rifiva     fe3rifiva
039800021212     c     kfe3          chain     ynfe301l
039900021218     c* memorizzo l'importo comprensivo di iva. La scorporto poi alla fine
040000021216     c                   add       importo       fe3impnib
040100021216     c                   sub       sconto        fe3impnib
040200010601     c                   z-add     numsys        fe3sys
040300010601     c                   z-add     numass        fe3nrasreg
040400021212     c                   if        not %found(ynfe301l)
040500021220     c                   add       1             fe3nrriga
040600010531     c                   write     ynfe3000
040700021212     c                   else
040800021212     c                   update    ynfe3000
040900021212     c                   end
041000021218     c*
041100021218     c                   endsr
041101070802     C**------------------------
041102070802     C** decodifica il dipendente
041103070802     C**------------------------
041104070802     C     srmz47        BEGSR
041105070802     c                   clear                   trmz62ds
041106070802     c                   move      d61dta        d62dta
041107070802     c                   move      d61cauo       d62cau
041108070802     c                   call      'TRMZ62R'
041109070802     c                   parm                    trmz62ds
041110070802     C                   clear                   trmz47ds
041111070802     c                   if        d62soco <> ' '
041112070802     c                   eval      d47soc = d62soco
041113070802     c                   eval      d47mat = d62mato
041114070802     c                   eval      kpjbu = trmz47ds
041115070802     c                   call      'TRMZ47C'
041116070802     c                   parm                    kpjba
041117070802     c                   eval      trmz47ds = kpjbu
041118070802     c                   else
041119070802     c                   eval      d47erro = '1'
041120070802     c                   end
041123070802     C                   ENDSR
041200010601      *---------------------------------------------------
041300010601     c     newdoc        begsr
041400010601      *---------------------------------------------------
041500020205     C                   EVAL      WrkErrDoc = *OFF
041600020205
041700010601     C                   MOVE      *DATE         XnmDat
041800010601     C                   MOVE      *DATE         XnmDag                         opzionale
041900010601      *
042000010601     C                   CALLB     'XNMR'
042100020205     C                   PARM                    XnmRic
042200020205     C                   PARM      *blank        XnmSoc
042300020205     C                   PARM      *BLANK        XnmUni
042400020205     C                   PARM      *BLANK        XnmCtb
042500020205     C                   PARM      *BLANK        XnmKey
042600020205     C                   PARM                    XnmSer
042700020205     C                   PARM      *OFF          XnmCmt
042800020205     C                   PARM                    XnmDat
042900020205     C                   PARM      *OFF          XnmIva
043000020205     C                   PARM      *OFF          XnmReg
043100020205     C                   PARM      *BLANK        XnmErr
043200020205     C                   PARM      *ZERO         XnmNum
043300020205     C                   PARM      *BLANK        XnmAut
043400020205     C                   PARM                    XnmDag                         opzionale
043500010601      *
043600010601     C                   IF        XnmErr =  Errore
043700010601     C                   EXSR      Uscita
043800010601     C                   ENDIF
043900010601
044000021219     C                   IF        Xnmser =  'NRAS'
044100021219     c                   clear                   ynfe1000
044200021218     c                   clear                   wnrriga
044300021219     c                   clear                   primo
044400021219     c                   movel     '380'         fe1tpdoc
044500021218     c* aggancio anagrafica fornitori e condizioni di pagamento
044600021218     c                   exsr      srfor
044700021218     c* aggancio anagrafica causali
044800021218     c                   clear                   ynfeb000
044900021218     c     kfeb          chain     ynfeb01l
045000021218     c                   if        not %found(ynfeb01l)
045001071024     c* stampa contabilità
045100021218     c                   IF        *INOA
045200021218     c                   EXCEPT    Testata
045300021218     C                   EVAL      *INOA = *OFF
045400021218     c                   ENDIF
045500021218     C                   EVAL      Msg = 'ERRORE Non trovata'
045600021218     C                             + ' tavola raccordo conti.'
045700021218     C                   EXCEPT    RigMsg
045800021218     c                   EVAL      WrkCmtBack = RollBack
045900021218     c                   exsr      uscita
046000021218     c                   end
046100021219     C                   ENDIF
046200021218
046300010601     c                   endsr
046400020206
046500010827      *------------------------------------------------
046600010827     c     controllo     begsr
046700010827      *------------------------------------------------
046800021219     c     kfe1          chain     ynfe001l                           98
046900021213
047000010829     c                   if        not *in98
047001071024     c* stampa contabilità
047100020205     c                   IF        *INOA
047200020205     c                   EXCEPT    Testata
047300020205     C                   EVAL      *INOA = *OFF
047400020205     c                   ENDIF
047500020205     C                   EVAL      Msg = 'ERRORE Il flusso '
047600021218     C                             + %TRIM(Fe1RifFlu)
047700020205     C                             + ' è già stato tradotto.'
047800020205     C                             + ' Traduzione non eseguita.'
047900020205     C                   EXCEPT    RigMsg
048000021218     c                   EVAL      WrkCmtBack = RollBack
048100021218     c                   exsr      uscita
048200010827     c                   end
048300021213     c*
048400010827     c                   endsr
048500021219      *------------------------------------------------
048600021219     c     srconto       begsr
048700021219      *------------------------------------------------
048800021219     c     kfec          chain     ynfec01l                           98
048900021219
049000021219     c                   if        *in98
049001071024     c* stampa contabilità
049100021219     c                   IF        *INOA
049200021219     c                   EXCEPT    Testata
049300021219     C                   EVAL      *INOA = *OFF
049400021219     c                   ENDIF
049500021219     C                   EVAL      Msg = 'ERRORE La riga '
049600021219     C                             + %TRIM(Fe2tprig1)
049700021219     C                             + %TRIM(Fe2tprig2)
049800021219     C                             + %TRIM(Fe2tpimp)
049900021219     C                             + ' non è stata trovata'
050000021219     C                             + ' nel relativo archivio.'
050100021219     C                   EXCEPT    RigMsg
050200021219     c                   EVAL      WrkCmtBack = RollBack
050300021219     c                   exsr      uscita
050400021219     c                   end
050500021219     c*
050600021219     c                   endsr
050700010605      *---------------------------------------------------
050800020807     c     FineFlusso    begsr
050900010605      *---------------------------------------------------
051000021220     c* calcolo imponibile e IVA del totale fattura
051100021220     c                   clear                   tot
051200021220     c                   clear                   alq
051300021220     c                   clear                   rig
051400021220     c                   clear                   iva
051500021220     c                   clear                   imp
051600021220     c                   clear                   z                 3 0
051700030114     c     kfe32         setll     ynfe302l
051800021220     c                   do        *hival
051900030114     c     kfe32         reade     ynfe302l
052000030114     c                   if        %eof(ynfe302l)
052100021220     c                   leave
052200021220     c                   end
052300021220     c                   if        fe3alqiva <> *all'0'
052400021220     c                   z-add     1             x                 3 0
052500021220     c     fe3alqiva     lookup    alq(x)                                 55
052600021220     c                   if        not *in55
052700021220     c                   add       1             z
052800021220     c                   if        fe3cauin <> *blanks
052900021220     c                   z-add     2             rig(z)
053000021220     c                   else
053100021220     c                   z-add     1             rig(z)
053200021220     c                   end
053300021220     c                   eval      alq(z) = fe3alqiva
053400021220     c                   z-add     fe3impnib     tot(z)
053500021220     c                   else
053600021220     c                   if        fe3cauin <> *blanks
053700021220     c                   add       2             rig(z)
053800021220     c                   else
053900021220     c                   add       1             rig(z)
054000021220     c                   end
054100021220     c                   add       fe3impnib     tot(z)
054200021220     c                   end
054300021220     c                   end
054400021220     c                   enddo
054500021220     c                   do        10            z
054600021220     c                   if        alq(z) <> *blanks
054700021220     c                   move      alq(z)        com03
054800021220     c     tot(z)        mult      com03         comodo
054900021220     c                   add       100           com03
055000030113     c     comodo        div(h)    com03         iva(z)
055100030113     c     tot(z)        sub       iva(z)        imp(z)
055200021220     c                   end
055300021220     c                   enddo
055400021220     c* calcolo imponibile e IVA del file ynfe300f
055500030114     c     kfe32         setll     ynfe302l
055600021212     c                   do        *hival
055700030114     c     kfe32         reade     ynfe302l
055800030114     c                   if        %eof(ynfe302l)
055900021212     c                   leave
056000021212     c                   end
056100021220     c* scorporo l'iva
056200021218     c                   if        fe3alqiva <> *all'0'
056300021212     c                   move      fe3alqiva     com03
056400021219     c     fe3impnib     mult      com03         comodo
056500021212     c                   add       100           com03
056600021219     c     comodo        div(h)    com03         fe3impost
056700021212     c                   sub       fe3impost     fe3impnib
056800030114     c                   update    ynfe3002
056900030114     c                   end
057000030114     c                   enddo
057100021220     c* calcolo la % indetraibilità
057200030114     c     kfe32         setll     ynfe302l
057300030114     c                   do        *hival
057400030114     c     kfe32         reade     ynfe302l
057500030114     c                   if        %eof(ynfe302l)
057600030114     c                   leave
057700030114     c                   end
057800021220     c                   if        fe3cauin <> *blanks and
057900030114     c                             fe3alqiva <> *all'0'
058000030114     c                   z-add     1             x
058100030114     c     fe3cauin      lookup    cau(x)                                 55
058200030114     c                   if        *in55 and per(x) <> 100
058300021220     c                   z-add     fe3impnib     salimp
058400021220     c                   z-add     fe3impost     saliva
058500030114     c     fe3impnib     mult      per(x)        comodo
058600021220     c     comodo        div(h)    100           fe3impnib
058700030114     c     fe3impost     mult      per(x)        comodo
058800021220     c     comodo        div(h)    100           fe3impost
058900030114     c                   update    ynfe3002
059000030114     c* scrivo o aggiorno l' iva detraibile
059100030114     c     saliva        sub       fe3impost     detimpost
059200030114     c     salimp        sub       fe3impnib     detimpnib
059300030114     c                   clear                   fe3cauin
059400030114     c     kfe3          chain     ynfe301l
059500030114     c                   if        not %found(ynfe301l)
059600030114     c                   z-add     detimpost     fe3impost
059700030114     c                   z-add     detimpnib     fe3impnib
059800021220     c                   write     ynfe3000
059900030114     c                   else
060000030114     c                   add       detimpost     fe3impost
060100030114     c                   add       detimpnib     fe3impnib
060200030114     c                   update    ynfe3000
060300021220     c                   end
060400030114     c                   end
060500030114     c                   end
060600021212     c                   enddo
060700030114     c* a questo punto quadro l'iva
060800030114     c                   clear                   costo3
060900030114     c     kfe32         setll     ynfe302l
061000021220     c                   do        *hival
061100030114     c     kfe32         reade     ynfe302l
061200030114     c                   if        %eof(ynfe302l)
061300021220     c                   leave
061400021220     c                   end
061500021220     c                   if        fe3alqiva <> *all'0'
061600021220     c                   z-add     1             x                 3 0
061700021220     c     fe3alqiva     lookup    alq(x)                                 55
061800021220     c                   if        *in55
061900021220     c                   if        rig(x) = 1
062000021220     c                   z-add     imp(x)        fe3impnib
062100021220     c                   z-add     iva(x)        fe3impost
062200030114     c                   update    ynfe3002
062300021220     c                   else
062400030113     c                   sub       fe3impnib     imp(x)
062500030113     c                   sub       fe3impost     iva(x)
062600021220     c                   sub       1             rig(x)
062700021220     c                   end
062800021220     c                   end
062900021220     c                   end
063000030114     c* mentre quador l'iva mi calcolo anche il totale dei costi
063100030114     c                   add       fe3impnib     costo3
063200030114     c                   if        fe3cauin <> *blanks
063300030114     c                   add       fe3impost     costo3
063400030114     c                   end
063500021220     c                   enddo
063600030114     c* a questo punto quadro i costi calcolati nel file ynfe2 e quelli
063700030114     C* calcolati nel ynfe3. La differenza la sommo nell'ultimo record
063800030114     c* del fe2 con % indetraibilità <> 0
063900030114     c                   if        costo2 <> costo3
064000061018     c                             and salnrr <> 0
064100061018     c     salnrr        chain     ynfe200f
064200030114     c                   if        %found(ynfe200f)
064300030114     c                   eval      fe2imp = fe2imp + (costo3 - costo2)
064400030114     c                   update    ynfe2000
064500030114     c                   end
064600030114     c                   end
064700030114     c*
064800020807     c                   move      dataiso       fe0dtwrt
064900020807     c                   write     ynfe0000
065000020807
065100020807     c                   clear                   ynfe0000
065200030114     c                   clear                   costo2
065300061018     c                   clear                   salnrr
065400030114     c                   clear                   cau
065500030114     c                   clear                   per
065600030114     c                   clear                   z                 3 0
065700020206
065800010605     c                   endsr
065900020206
066000010601      *---------------------------------------------------
066100021212     c     srfor         begsr
066200020206      *---------------------------------------------------
066300021218     c                   eval      fe0tpfor = 'AGIP'
066400021216     c* controllo il contratto
066500021216     c                   exsr      contrat
066600021216     c*
066700021212     C     Kfrn          CHAIN     anfrn01l                           20
066800021212     C  N20Krco          CHAIN     anrco98j                           20
066900021212     c                   if        not *in20
067000021212     C                   MOVEL(p)  sogdes        FE0RGSSL
067100021212     C                   eval      FE0INDSL = indindriz
067200021212     c                   if        indprov <> ' '
067300021212     C                   eval      FE0LOCSL=%trimr(indlocalit)+' ('+indprov+')'
067400021212     c                   else
067500021212     C                   eval      FE0LOCSL=indlocalit
067600021212     c                   end
067700021212     C                   eval      FE0CAPSL=indcap
067800021212     C                   eval      FE0IVASL=sogpartiva
067900021212     C                   eval      FE0FISSL=sogcdfisc
068000021216     c                   else
068100021216     c                   EVAL      WrkCmtBack = RollBack
068101071024     c* stampa contabilità
068200021219     C                   eval      msg = 'ERRORE Il codice fornitore non esist-
068300021219     C                             e nel relativo anagrafico.'
068400021216     C                   IF        *INOA
068500021216     C                   EXCEPT    Testata
068600021216     C                   EVAL      *INOA = *OFF
068700021216     C                   ENDIF
068800021216     C                   EXCEPT    RigMsg
068900021219     C                   EVAL      WrkErrDoc = *ON
069000021218     c                   exsr      uscita
069100021216     c                   endif
069200021212     c                   endsr
069300021216     ***********************************************************************
069400021216     ** Controllo contratto:
069500021216     ** - se non esiste è un errore bloccante.
069600021216     ** - se     esiste e il documento è fuori periodo validità,
069700021216     **   è un errore ma non bloccante.
069800021216     ***********************************************************************
069900021216     c     contrat       begsr
070000021216
070100030116     C                   CLEAR                   YnFea00F
070200030116     c                   eval      feacontra = 'AGIP'
070300030116     c                   move      *date         feadtinco
070400030116     C                   CALL      'YCOYNFEAR'
070500030116     C                   PARM                    Fe0TpFor
070600030116     C                   PARM                    FeaContra
070700030116     C                   PARM                    feadtinco
070800030116     C                   PARM      *BLANK        PrmEsito
070900030116     C                   PARM                    YnFea00F
071000030116
071100030116     c                   if        PrmEsito = Errore
071200021216     C                   EVAL      WrkErrDoc = *ON
071300021216     c                   EVAL      WrkCmtBack = RollBack
071301071024     c* stampa contabilità
071400021219     C                   EVAL      Msg = 'ERRORE Il contratto AGIP non esiste n-
071500021219     C                             el relativo anagrafico.'
071600021216     C                   IF        *INOA
071700021216     C                   EXCEPT    Testata
071800021216     C                   EVAL      *INOA = *OFF
071900021216     C                   ENDIF
072000021216     C                   EXCEPT    RigMsg
072100021218     c                   exsr      uscita
072200021216     c                   endif
072300021216
072400070302      // Mi collego alla società titolare del contratto.
072500070302     C                   CALLB     'XSOC'
072600070302     C                   PARM      'SOC001'      TipXsc
072700070302     C                   PARM      feaSocieta    SocXsc
072800070302     C                   PARM                    CdsXsc
072900070302     C                   PARM                    ModXsc
073000070302     C                   PARM                    RtnXsc
073100070302     C                   PARM                    XSoc001DS
073200070302     C                   PARM                    Kpjba
073300070302
073400021216     c                   ENDSR
073500021212      *---------------------------------------------------
073600021212     c     *inzsr        begsr
073700021212      *---------------------------------------------------
073800020205     C                   EVAL      Knsif = PrmSif
073900021219     c                   eval      rcosnatura = 'F'
074000020204      *
074100020204     C                   CALLB     'XSOC'
074200020205     C                   PARM      'SOC001'      TipXsc
074300020205     C                   PARM                    SocXsc
074400020205     C                   PARM                    CdsXsc
074500020205     C                   PARM                    ModXsc
074600020205     C                   PARM                    RtnXsc
074700020205     C                   PARM                    XSoc001DS
074800020205     C                   PARM                    Kpjba
074900020204      *
075000020204     C                   IF        RtnXsc = Errore
075100020204     C                   EXSR      Uscita
075200020204     C                   ENDIF
075300010601      *---------------------------------------------------
075400020205     c                   TIME                    times
075500020205     c                   TIME                    dataeur
075600020205     c                   TIME                    dataiso
075700010601      * Reperisco sistema della società di immissione.
075800020205     C                   EVAL      XnmRic = '1'
075900020205     C                   EVAL      XnmSer = 'NSYS'
076000010601     C                   EXSR      newdoc
076100020205     C                   EVAL      numSys = XnmNum
076200021219      * Imposto i dati x reperire numero assoluto.
076300021219     C                   EVAL      XnmRic = '2'
076400021219     C                   EVAL      XnmSer = 'NRAS'
076500020205
076600020205     C                   EVAL      *INOA = *ON
076601071025     C                   EVAL      *INOf = *ON
076700021212     C     Kfrn          KLIST
076800021212     C                   KFLD                    socxsc
076900021212     C                   KFLD                    feakscfor
077000021212     C     Krco          KLIST
077100021216     C                   KFLD                    socxsc
077200021212     C                   KFLD                    rcosnatura
077300021219     C                   KFLD                    feakscfor
077400021218     C     Kali          KLIST
077500021218     C                   KFLD                    fe0tpfor
077600021218     C                   KFLD                    fedrifivae
077700021219     C     Kfeb          KLIST
077800021219     C                   KFLD                    fe0tpfor
077900021219     C                   KFLD                    fe1tpdoc
078000021218     C     Kfe1          KLIST
078100021218     C                   KFLD                    fe0tpfor
078200030116     C                   KFLD                    fe0rifflu
078300021218     C     Kfe3          KLIST
078400021219     C                   KFLD                    numsys
078500021218     C                   KFLD                    numass
078600030114     C                   kfld                    fe3alqiva
078700030114     C                   kfld                    fe3rifiva
078800021218     C                   kfld                    fe3cauin
078900030114     C     Kfe32         KLIST
079000021219     C                   KFLD                    numsys
079100021218     C                   KFLD                    numass
079200021219     C     Kfec          KLIST
079300021219     C                   KFLD                    fe0tpfor
079400021219     C                   KFLD                    fe2tprig1
079500021219     C                   KFLD                    fe2tprig2
079600021219     C                   KFLD                    fe2tpimp
079700021219     C                   endsr
079800010601      *---------------------------------------------------
079900010601     c     uscita        begsr
080000010601      *---------------------------------------------------
080100010601      *
080200020205     c                   if        WrkCmtBack = RollBack
080300020205     c                   rolbk
080301071024     c* stampa contabilità
080400020205     c                   IF        *INOA
080500020205     c                   EXCEPT    Testata
080600020205     C                   EVAL      *INOA = *OFF
080700020205     c                   ENDIF
080800020205     C                   EVAL      Msg = 'ERRORE Il flusso '
080900020205     C                             + %TRIM(Fe0RifFlu)
081000020205     C                             + ' non è stato tradotto.'
081100020205     C                             + ' Correggere gli errori e ripetere'
081200020205     C                             + ' la traduzione.'
081300020205     C                   EXCEPT    RigMsg
081400030116     c                   eval      fine = errore
081500021213     c                   end
081600021213     c                   if        WrkCmtBack = commit
081700020205     c                   commit
081800010827     c                   end
081900010601     c                   seton                                        lr
082000020205     C                   RETURN
082100021213
082200010601     C                   endsr
082300010605     o*------------------------------------------------------*
082400020208     oYco3070P  e            testata        3  1
082500020205     O                       KNSif
082600021219     o                                           70 'Log traduzione AGIP'
082700020205     o                       PgmName            100
082800020205     o                       dataeur             +1
082900020205     o                       times               +1
083000020205     o                                          127 'Pag.'
083100020205     o                       page               132
083200010605      * dettaglio
083300010605     o          e            DetFatture  2
083400010606     o                                           30 'Importo Fatture rilevate  '
083500021219     o                       fe0impfat     2     50
083600021219     o          e            DetFATTURE  1  1
083700020807     o                                              '--------------------------'
083800020807     o                                              '--------------------------'
083900020807     o                                              '--------------------------'
084000020807     o                                              '--------------------------'
084100020807     o                                              '--------------------------'
084200020807     o                                              '--------------------------'
084300020205
084400020205     O          E            RigMsg      1
084500020205     O                       Msg
084600010828
084700071024     OQSYSPRT   E            TESTA          2 02
084701071024     O                       KNSif
084900071024     O                                         + 25 'LISTA ERRORI DURANTE'
085000071024     O                                         +  1 'IL CARICAMENTO DEL F'
085100071024     O                                         +  0 'ILE AGIP'
085401071024     o                       PgmName            100
085402071024     o                       dataeur             +1
085403071024     o                       times               +1
085404071024     o                                          127 'Pag.'
085405071024     o                       page               132
085500071024     O          E            RigMsgq     1
085600071024     O                       Msgq

000100040917     H DECEDIT('0,') DATEDIT(*yMd.)
000200130802      * TISIO1R *----------------------------------------------------*
000300130830      *         - Reperimento servizi orari  Cappario
000400940915      *--------------------------------------------------------------*
000500130830     Fazcos01l  iF   E           K DISK
000600130830     Fazorg01l  iF   E           K DISK
000700060315     Ftabel00f  IF   E           K DISK
000800020805      *
000900020805      * DEFINIZIONE SCHIERE
001000960302     D*
001100130830     D MSG             S             78    DIM(06) CTDATA PERRCD(1)
001200130924     d
001300130924     D FIL_AB          S              3    DIM(85)
001400060315     d
001500040917     D KPJBA         E DS
001600130830     D ds15          E DS
001700060922     d
001800130830     d Datasys         s               d   datfmt(*iso) inz(*sys)
001900130830     d ktbkut          s                   like(tblkut)
002000130830     d ktbcod          s                   like(tblcod)
002100130830     d ktbkey          s                   like(tblkey)
002200130830     d kloc            s                   like(cosloc)
002300130830     d klna            s                   like(coslna)
002400130830     d lna999          s                   like(coslna) inz(999)
002500130830     d kcap            s                   like(coscap)
002600130830     d knar            s                   like(cosnar)
002700130830     d ktser           s                   like(costser)
002800130924     d Indx            s              3  0
002900130924     d xx              s              3  0
003000130924     d yy              s              3  0
003100130802     d
003200130802     d tiSIO0ds      e ds
003300130830     d
003400130830     d fnlv13ds      e ds
003500130830     d tisi95ds      e ds
003600130924     d tibs02ds      e ds
003700060926     d
003800130924      //---------------------------------------------------------------
003900130924      /copy gaitrasrc/srcprotopr,tibs02r
004000130924
004100050103     C**********************************************************************
004200040917     C     *ENTRY        PLIST
004300040917     C                   PARM                    KPJBA
004400130802     C                   PARM                    tiSIO0ds
004500130830     c
004600130830     c                   clear                   do0msg
004700130830     c                   clear                   do0err
004800131010     c                   clear                   do0rif
004900131010     c                   clear                   do0flo
005000131010     c                   clear                   do0ttc
005100131010     c                   clear                   do0serv
005200131010     c                   clear                   DO0OSTIS
005300131010     c                   clear                   DO0OSTFS
005400131010     c                   clear                   DO0OMIIS
005500131010     c                   clear                   DO0OMxfS
005600131010     c                   clear                   DO0OLRSC
005700131010     c                   clear                   DO0OLRNC
005800131010     c                   clear                   DO0OLRNK
005900130830     c*
006000130830     c* Controllo dati di input passati
006100060315     c                   exsr      CARINIZ
006200131011     c
006300131011     c* se non ho il tipo servizio giusto impostando non posso nemmeno reperire i dati STD
006400131011     c                   if        DO0err<>' ' and  do0tser<>'C'
006500131011     c                             and do0tser<>'R'
006600131011     c                   goto      fine
006700131011     c                   endif
006800131010     c
006900131011     c* il tempo di organizzazione lo prendo sempre dal record 999 nazione Italia
007000131010     c* Imposto la key
007100131011     c                   eval      knar=*blanks
007200131010     c                   eval      klna=999
007300131010     c                   eval      ktser=do0tser
007400131010     c     kcos          chain     azcos01l
007500131010     c                   if        %found(azcos01l)
007600131010     c                   eval      do0trzs =costrzs
007700131010     c                   endif
007800131011
007900131011     c*  In tutti gli altri casi di errore --Z èrendo i dati dal record  STD
008000131011     c*
008100131011     c                   if        DO0err<>' '
008200131011     c                   exsr      carOUT
008300131011     c                   eval      do0rif='9'
008400131011     c                   goto      fine
008500131011     c                   endif
008600130830     c*
008700130830     c* Imposto la key
008800131011     c                   eval      knar=*blanks
008900130830     c                   eval      kcap=do0cap
009000130830     c                   eval      klna=do0lna
009100130830     c                   eval      ktser=do0tser
009200130830     c                   eval      kloc=do0loc
009300130830     c
009400130830     c* se passata la località la normalizzo
009500130830     c                   if        kloc<>*blanks
009600130830     c                   exsr      norm_loc
009700130830     c                   endif
009800130830     c
009900130830     c* Reperisco gli orari servizio
010000130830     c
010100130830     c     kcos          chain     azcos01l
010200130830
010300130830    1c                   dow       klna<>lna999  and not%found(azcos01l)
010400130830     c                   exsr      Impokey
010500130830
010600130830     c     kcos          chain     azcos01l
010700130830    1c                   enddo
010800130830
010900130830     c                   if        not%found(azcos01l)
011000130830     c                   eval      do0msg=msg(05)
011100130830     c                   eval      do0err='1'
011200130830     c                   goto      fine
011300130830     c                   endif
011400130830     c*
011500131010     c* Passo i dati di output
011600131011     c                   exsr      CarOUT
011700131011     c* compreso il Livello dati trovato
011800130830     c                   select
011900130830     c                   when      kloc<>*blanks
012000130830     c                   eval      do0rif='L'
012100130830     c                   when      kcap<>*blanks
012200130830     c                   eval      do0rif='C'
012300130830     c                   when      klna<>lna999
012400130830     c                   eval      do0rif='F'
012500130830     c                   other
012600130830     c                   eval      do0rif='9'
012700130830     c                   endsl
012800130830     c
012900130830     c     fine          tag
013000060922     c
013100000000     C                   SETON                                        LR
013200130830     C**************************************************************************
013300130830     c* impostazine della chiave per la ricerca su AZCOS a scalare
013400130830     C**************************************************************************
013500130830     c     Impokey       BEGSR
013600130830     c                   select
013700130830     c                   when      kloc<>*blanks
013800130830     c                   clear                   kloc
013900130830     c                   when      kcap<>*blanks
014000130830     c                   clear                   kcap
014100130830     c                   other
014200130830     c                   eval      klna=lna999
014300130830     c                   endsl
014400130830     c                   ENDSR
014500131011     C**************************************************************************
014600131011     c* Impostazione DS campi di output
014700131011     C**************************************************************************
014800131011     c     carOUT        BEGSR
014900131011     c                   eval      do0ostis=cosostis
015000131011     c                   eval      do0ostfs=cosostfs
015100131011     c                   eval      do0omiis=cosomiis
015200131011     c                   eval      do0omxfs=cosomxfs
015300131011     c                   eval      do0olrsc=cosolrsc
015400131011     c                   eval      do0olrnc=cosolrnc
015500131011     c                   eval      do0olrnk=cosolrnk
015600131011     c                   eval      do0ttc  =costtc
015700131011     c                   eval      do0serv =cosserv
015800131011     c                   eval      do0flo  =cosflo
015900131011     c                   ENDSR
016000050214     C**************************************************************************
016100060315     C     CARiniz       BEGSR
016200130830     c
016300130830     C     KEYTAB        KLIST
016400130830     C                   KFLD                    KTBKUT                         *CODICE UTENTE
016500130830     C                   KFLD                    KTBCOD                         *TABELLA
016600130830     C                   KFLD                    KTBKEY                         *CHIAVE TABELLA
016700130830     C     KCOS          KLIST
016800130830     C                   KFLD                    Do0VER
016900130830     C                   KFLD                    knar
017000130830     C                   KFLD                    kcap
017100130830     C                   KFLD                    kloc
017200130830     C                   KFLD                    klna
017300130830     C                   KFLD                    ktser
017400060331     c
017500131011     c* Se la versione non è stata passata uso la data di riferimento per calcolarla
017600130830    1c                   if        do0ver=0
017700130830     c
017800130830    2c                   if        do0dri=0
017900130830     c                   movel     datasys       do0dri
018000130830    2c                   endif
018100130830     c
018200130830     C                   Z-ADD     Do0DRI        P96DRI            8 0
018300130830     C                   Z-ADD     *ZEROS        P96VER            5 0
018400130830     C                   CALL      'TISI96R'
018500130830     C                   PARM                    P96DRI
018600130830     C                   PARM                    P96VER
018700130830     c
018800130830     c                   eval      do0ver=p96ver
018900130830    1c                   endif
019000131011
019100131011     c* Se non passato tipo servizio --> errore
019200131011     c                   if        do0tser<>'R' and do0tser<>'C'
019300131011     c                   eval      do0msg=msg(02)
019400131011     c                   eval      do0err='E'
019500131011     c                   leavesr
019600131011     c                   endif
019700130830     c
019800130830     c                   if        do0lna>0  and do0lna<>999
019900130830     c
020000130830     c     do0lna        chain     azorg01l
020100130830     c
020200130830     c                   if        not %found(azorg01l)
020300130830     c                             or orgfag<>'F' and orgfag<>'A'
020400130830     c                   eval      do0msg=msg(01)
020500131011     c                   eval      do0err='E'
020600130830     c                   leavesr
020700130830     c                   endif
020800130924     c
020900130924     c* Se la linea passata non è partita con la nuova procedura emetto errore
021000130924     c                   movel     do0lna        w003a             3
021100130924     c                   eval      Indx=%lookup(w003a:fil_ab)
021200130924     c
021300130924     c                   if        indx=0 and fil_ab(1)<>'999'
021400130924     c                   eval      do0msg=msg(05)
021500131011     c                   eval      do0err='A'
021600130924     c                   endif
021700130924     c
021800130924     c                   endif
021900130830     c
022000130830     c* se non ho passato la linea di arrivo imposto 999 se cap e loc=*blanks
022100130830     c*  altrimenti errore
022200130830     c                   if        do0lna=0 and do0cap=*blanks and do0loc=*blanks
022300130830     c                   eval      do0lna=999
022400130830     c                   endif
022500130830     c* Se non passata linea di arrivo --> errore
022600130830     c                   if        do0lna=0
022700130830     c                   eval      do0msg=msg(01)
022800131011     c                   eval      do0err='E'
022900130830     c                   leavesr
023000130830     c                   endif
023100130830     c
023200131011     c* Se passata località e non CAP --> errore
023300130830     c                   if        do0loc<>*blanks
023400130830     c                             and do0cap=*blanks
023500130830     c                   eval      do0msg=msg(04)
023600131011     c                   eval      do0err='E'
023700130830     c                   endif
023800130830     c
023900130830     c
024000130830     c* Ammessa soltanto nazione ITALIA
024100130830     C                   MOVEL     1             KTBKUT
024200130830     C                   MOVEL     '15'          KTBCOD
024300130830     C                   MOVEL(P)  do0nar        KTBKEY
024400130830     C     KEYTAB        CHAIN     TABEL00F                           99
024500130830     c                   clear                   ds15
024600130830IF  1C     *IN99         IFEQ      *OFF
024700130830     C     TBLFLG        ANDEQ     *BLANKS
024800130830     C                   MOVEL     TBLUNI        DS15
024900130830     c                   endif
025000130830     c                   if        §15ita<>'I'
025100130830     c                   eval      do0msg=msg(03)
025200131011     c                   eval      do0err='A'
025300130830     c                   endif
025400130830     c
025500130830     C                   ENDSR
025600130830     C**************************************************************************
025700130830     c* Normalizzo la località
025800130830     C**************************************************************************
025900130830     C     Norm_loc      BEGSR
026000130830     c                   clear                   fnlv13ds
026100130830     c                   clear                   tisi95ds
026200130830     C                   MOVEL     '3'           I95TCN
026300130830     C                   MOVEL     'S'           I13AF0
026400130830     C                   MOVEL     'S'           I13CNV
026500130830     C                   MOVEL     'S'           I13LA3
026600130830     c                   movel     'S'           flgbac
026700130830     c                   eval      i95loc=do0loc
026800130830     c                   eval      i95cap=do0cap
026900130830     c                   eval      i95nar=do0nar
027000130830     c                   eval      i95dat=do0dri
027100130830     c
027200130830     C                   CALL      'FNLV13R'
027300130830     C                   PARM                    KPJBA
027400130830     C                   PARM                    FNLV13DS
027500130830     C                   PARM                    TISI95DS
027600130830     C                   PARM                    flgbac            1
027700130830     c
027800130830     C                   IF        O95ERR= ' ' AND o95loc<>*blanks
027900130830     c                   eval      kloc=o95loc
028000130830     c                   else
028100130830     c                   eval      kloc=do0loc
028200130830     c                   endif
028300130830     c
028400130830     C                   ENDSR
028500130924
028600130924     C**************************************************************************
028700130924     C     *inzsr        BEGSR
028800130924      /free
028900130924        // Carico filiali di test abilitate
029000130924         clear TIBS02ds;
029100130924         T02Mod = 'C';
029200130924         T02sif = knsif;
029300130924         T02cod = 'VPO';
029400130924         t02tla = 'L'   ;
029500130924         T02ke1 ='DECOFI816' ;
029600130924         TNTBE_RicercaControllo  (kpjba : tibs02ds);
029700130924
029800130924         if  T02err  = *blank;
029900130924         xx= 1 ;
030000130924         yy= 1 ;
030100130924
030200130924         dow xx< 256   ;
030300130924           fil_ab(yy)= %subst(t02uni:xx:3)  ;
030400130924           yy=yy+ 1   ;
030500130924           xx=xx+ 3   ;
030600130924         enddo  ;
030700130924
030800130924         endif;
030900130924
031000130924         ENDSR;
031100130924      /end-free
031200130924
031300020805      **************************************************************************
031400960302** SCHIERA MSG - MESSAGGI DI ERRORE
031500130830Linea passata errata                                                           01
031600130920Tipo servizio passato errato: richiedere R-ritiro o C-consegna                 02
031700130830Orari servizi presenti soltanto per nazione ITALIA                             03
031800130830Insieme alla località passare anche il CAP                                     04
031900130920Non sono presenti gli orari servizi                                            05

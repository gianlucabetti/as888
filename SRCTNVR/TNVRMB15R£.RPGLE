000100121218      //---------------------------------------------------------------
000200121218      //
000300121218      //?              Invio immagini LDV ai clienti
000400121218      //
000500121218      //---------------------------------------------------------------
000600121218
000700121218     h Decedit('0,') Datedit(*ymd.) Option(*nodebugio)
000800040119
000900040119      *---------------------------------------------------------------*
001000121218
001100121219     fFIAR531C  if   e           k disk
001200130103     fFNLBL01L  if   e           k disk
001300121218     fTABEL00F  if   e           k disk
001400121218     fTILAC00F  o    e           k disk
001500121218     fTITAS38C  if   e           k disk    rename(TITAS000:TITAS38)
001600121218     f                                     rename(TITAS010:TITAS381)
001700121218     f                                     rename(TITASP00:TITAS38P)
001800121218     fTITAS30C  uf   e           k disk
001900121218     fTNTBE01L  if   e           k disk
002000040119
002100040119      *---------------------------------------------------------------*
002200050330
002300121219     d dAR5doc       e ds                  inz
002400121218     d dLAC          e ds                  inz
002500121218     d dsTB          e ds                  inz
002600121218     d dTASflo       e ds                  inz
002700130103
002800130103      * - Ds per controlli bolla
002900130103     d titasds       e ds                  extname(TITAS00F)
003000130103     d titasdsChk    e ds                  extname(TITAS00F) prefix(Chk_)
003100090212
003200090212       // - Parametri x TRUL55R
003300090212     d TRUL55ds      e ds                  inz
003400090212     d   I55tla      e                     inz('C')
003500121218
003600121218      // Parametri
003700121218     d kpjba         e ds
003800121218
003900121218     d param           ds
004000121218     d  AAAAdal                1      4  0
004100121218     d  MMGGdal                5      8  0
004200121218     d  AAAAal                 9     12  0
004300121218     d  MMGGal                13     16  0
004400121218     d  dataal                 9     16  0
004500121218
004600121218     d skKSC           s              7  0 dim(999) inz
004700121218     d skLDV           s              4  0 dim(999) inz
004800121218
004900121218       // - Campi di comodo
005000121219     d kAR5trd         s                   like(AR5trd) inz('DOC')
005100121218     d kcod            s                   like(tblcod) inz('TB')
005200121218     d kkey            s                   like(tblkey)
005300121218     d waddams         s              6    inz
005400121218     d wDataISO        s               d   datfmt(*ISO) inz
005500121218     d nrweek          s              2  0 inz
005600121218     d savtbl          s                   like(TAStbl) inz('  ')
005700121218     d woggi           s              8  0
005800121218     d wyear           s              4  0 inz
005900121218     d xx              s              4  0 inz
006000130103
006100130103     d k_TASaas        s                   like(TASaas)
006200130103     d k_TASlnp        s                   like(TASlnp)
006300130103     d k_TASnrs        s                   like(TASnrs)
006400130103     d k_TASnsp        s                   like(TASnsp)
006500130103     d k_TAStbl        s                   like(TAStbl)
006600121218
006700121218       // - flag booleani
006800121218     d wEoF            s               n
006900090212
007000090212       //--------------------------------------------------------------
007100090212       //?Definizione procedure usate.                                 ?
007200090212       //--------------------------------------------------------------
007300090212
007400090212       // - Determinazione nome dell'immagine per TILAC
007500090212     d trul55r         pr                  extpgm('TRUL55R')
007600090212     d   trul55ds...
007700090212     d                                     likeds(trul55ds)
007800040119
007900040119      *---------------------------------------------------------------*
008000040119
008100040119      * Predisposizione indicatori per riconoscere i tipi record
008200040119      *   reperiti dal file TITAS30C
008300121219     iTITAS000      31
008400121219     iTITAS010      32
008500121219     iTITASP00      33
008600040119
008700040119      *---------------------------------------------------------------*
008800121218
008900121218     c     *entry        plist
009000121218     c                   parm                    kpjba
009100121218
009200121218     c                   eval      param = kpjbu
009300121218
009400121218     c     Ktab          klist
009500121218     c                   KFLD                    CODUT
009600121218     c                   KFLD                    kcod
009700121218     c                   kfld                    kkey
009800121218     c                   z-add     1             codut             1 0
009900040119
010000121218      /free
010100121218
010200121218        wEoF = *off;
010300121218
010400121218       // Leggo TITAS
010500121218        setll (AAAAdal:MMGGdal) TITAS38C;
010600121218
010700121218        DOW not wEoF;
010800121218          reade (AAAAdal) TITAS38C;
010900121218
011000121218       // fine file
011100121218          IF  %eof(TITAS38C);
011200121218            leave;
011300121218          ENDIF;
011400121218
011500121218       // fino al
011600121218          IF (TASaas * 10000 + TASmgs) > (AAAAal * 10000 + MMGGal);
011700121218            leave;
011800121218          ENDIF;
011900121218
012000121218       // solo le bolle con flag immagine = 'S'
012100121218          dTASflo = TASflo;
012200121218          IF  §FLOiml <> 'S';
012300121218            iter;
012400121218          ENDIF;
012500121218
012600121218       // solo le bolle di clienti abilitati a FTP su tab. LAC
012700121219          clear dLAC;
012800121219          TBEcod = 'LAC';
012900121218          TBEke1 = %editc(TASksc:'X');
013000121219          chain (TBEcod:TBEke1) TNTBE01L;
013100121218          IF  not %found(TNTBE01L) or TBEatb <> *blanks;
013200121218            iter;
013300121218          ENDIF;
013400121218          dLAC = TBEuni;
013500121218          IF  §LACaut <> 'A';
013600121218            iter;
013700121218          ENDIF;
013800121219
013900121219       // se Merge non elaboro
014000121219          IF  §LACmerge = 'S' and §LACla2 = *blanks;
014100121219            iter;
014200121219          ENDIF;
014300121219
014400121219       // se LA2 devo agganciare la LA2
014500121219          IF  §LACla2 = 'S';
014600121219            clear dLAC;
014700121219            TBEcod = 'LA2';
014800121219            chain (TBEcod:TBEke1) TNTBE01L;
014900121219            IF  not %found(TNTBE01L) or TBEatb <> *blanks;
015000121219              iter;
015100121219            ENDIF;
015200121219            dLAC = TBEuni;
015300121219          ENDIF;
015400121219
015500121218          exsr elabora;
015600121218        ENDDO;
015700121218
015800121218        *inLR = *ON;
015900121218
016000121218       //-------------------------------------------------------------**
016100121218       // Elabora
016200121218       //-------------------------------------------------------------**
016300121218        BEGSR elabora;
016400121218
016500121218          woggi = %dec(%date());
016600130103
016700130103          k_TASaas = TASaas;
016800130103          k_TASlnp = TASlnp;
016900130103          k_TASnrs = TAsnrs;
017000130103          k_TASnsp = TASnsp;
017100130103          k_TAStbl = TAStbl;
017200130103          titasdsCHK = titasds;
017300130103
017400130103        // cerco la bolla originale
017500130103          chain (TASaas:TASlnp:TASnrs:TASnsp) FNLBL01L;
017600130103          IF  %found(FNLBL01L);
017700130103            *in31 = *off;
017800130103            *in32 = *off;
017900130103            *in33 = *off;
018000130103            chain(n) (LBLaao:LBLlpo:LBLnro:LBLnso) TITAS30C;
018100130103            IF  %found(TITAS30C);
018200130103              titasdsCHK = titasds;
018300130103            ENDIF;
018400130103          // imposto la chiave della bolla figlia
018500130103            k_TASaas = LBLaan;
018600130103            k_TASlnp = LBLlpn;
018700130103            k_TASnrs = LBLnrn;
018800130103            k_TASnsp = LBLnsn;
018900130103          ENDIF;
019000130103
019100130103        // Aggancio la bolla
019200121219          *in31 = *off;
019300121219          *in32 = *off;
019400121219          *in33 = *off;
019500121219
019600130103          chain (k_TASaas:k_TASlnp:k_TASnrs:k_TASnsp:k_TAStbl) TITAS30C;
019700121219          IF  %found(TITAS30C);
019800130103
019900130103      /end-free
020000130103     c                   if        Chk_tastbl <> savtbl
020100130103     c                   movel(p)  Chk_tastbl    kkey
020200121219     c                   clear                   dstb
020300121219     c     ktab          chain     tabel00f
020400121219     c                   if        %found(tabel00f)
020500121219     c                   movel     TBLuni        dstb
020600121219     c                   endif
020700130103     c                   eval      savtbl = Chk_tastbl
020800130103     c                   endif
020900121218
021000121218     c* controllo porto (rispetto alla bolla originale)
021100121218     c                   if        §LACtpt <> 'E' and §TBtpo <> §LACtpt
021200121218     c                   leavesr
021300121218     c                   endif
021400130103
021500121218     c* reso (rispetto alla bolla corrente (figlia))
021600121218     c                   if        §LACres <> 'S' and TASfbr = 'S'
021700121218     c                   leavesr
021800121218     c                   endif
021900121218     c* recupero (rispetto alla bolla originale)
022000121218     c                   if        §LACrec <> 'S' and §TBrbl = 'R'
022100121218     c                   leavesr
022200121218     c                   endif
022300121218     c* c/servizio (rispetto alla bolla originale)
022400121218     c                   if        §LACcsr <> 'S' and §TBrbl = 'C'
022500121218     c                   leavesr
022600121218     c                   endif
022700121218     c* serie (rispetto alla bolla originale)
022800130103     c                   if        §LACssr = 'S' and chk_TASnrs = *zeros
022900121218     c                   leavesr
023000121218     c                   endif
023100121218     c* linea di partenza (rispetto alla bolla originale)
023200121218     c                   if        §LAClnp <> *zeros and
023300130103     c                             chk_TASlnp <> §LAClnp
023400121218     c                   leavesr
023500121218     c                   endif
023600121218
023700121218      /free
023800121218          clear TILAC000;
023900130103          LACtim  = '20130103100000';
024000121218          LACaas  = %subst(%editc(TASaas:'X'):3:2);
024100121218          LAClnp  = %editc(TASlnp:'X');
024200121218          LACnrs  = %editc(TASnrs:'X');
024300121218          LACnsp  = %editc(TASnsp:'X');
024400121218          LACcop  = 'CLI';
024500121219          LACdir  = §LACdir;
024600121218          LACtpi  = §LACtpi;
024700121218
024800121218       // flag nome immagine:
024900121218       // se = "S "=significa che deve essere impostato col n.sped
025000121218       //           si pensa Laguna) --> quindi lascio = blanks
025100121218if  1     IF  §LACfmi <> 'S';
025200121218            clear  TRUL55ds;
025300121218            I55fmi = §LACfmi;
025400121218            I55lnp = TASlnp;
025500121218            I55nrs = TASnrs;
025600121218            I55nsp = TASnsp;
025700121218            I55aas = TASaas;
025800121218            I55mgs = TASmgs;
025900121218            I55rmn = TASrmn;
026000121218            trul55r ( TRUL55ds );
026100121218            if  O55err = *off;
026200121218              LACnim = O55nim;
026300121218            endif;
026400121218e   1     endif;
026500121218
026600121218          IF  §LACksc <> *zeros;
026700121218            LACidl  = %editc(§LACksc:'X') + §LACctr;
026800121218          ELSE;
026900121218            LACidl  = %editc(TASksc:'X') + %editc(TASctr:'X');
027000121218          ENDIF;
027100121219
027200121219          LACela  = '00';
027300121218          LACtela = 'A';
027400121218          LACksc  = TASksc;
027500121218          LACksu  = §LACksu;
027600121218          LACtad  = §LACtad;
027700121218          LACtadu = §LACtadu;
027800121218          LACfimp = §LACfimp;
027900121218          LACimp  = §LACimp;
028000121218
028100121218         //?AA/MM addebito + IDjob in base alla frequenza addebito
028200121218          SELECT;
028300121218            WHEN  LACtadu = 'I';
028400121218              LACidjob = LACaas + LAClnp + LACnrs + LACnsp;
028500121218            WHEN  LACtadu = 'J';
028600121218              LACidjob = %editc(LACksu:'X') + %editc(woggi:'X');
028700121218            WHEN  LACtadu = 'S';
028800121218      /end-free
028900121218     c     *iso          movel     woggi         wDataISO
029000121218     c                   clear                   nrweek
029100121218     c                   call      'XSRWEEK'
029200121218     c                   parm                    wDataiso
029300121218     c                   parm                    nrweek
029400121218      /free
029500121218              wyear = %subdt(wdataiso:*years);
029600121218              waddams = %editc(wyear:'X') + %editc(nrweek:'X');
029700121218              LACaddms = %dec(waddams:6:0);
029800121218              LACidjob = %editc(LACksu:'X') + 'S' + waddams;
029900121218            WHEN  LACtadu = 'M';
030000121218              LACaddms = %dec(%subst(%editc(woggi:'X'):1:6):6:0);
030100121218              LACidjob = %editc(LACksu:'X') + 'M' + %editc(LACaddms:'X');
030200121218          ENDSL;
030300121218
030400121218          WRITE  TILAC000;
030500050330
030600121218      /end-free
030700040119      *
030800040119     c                   movel     TASflo        dTASflo
030900040331     c                   eval      §FLOiml = 'I'
031000040119     c                   movel     dTASflo       TASflo
031100040119      *
031200040119     c   31              UPDATE    TITAS000
031300040119     c   32              UPDATE    TITAS010
031400040119     c   33              UPDATE    TITASP00
031500040119      *
031600121218     c                   endif
031700040119      *
031800040119     c                   ENDSR

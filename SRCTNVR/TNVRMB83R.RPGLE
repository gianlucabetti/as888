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
001200121218     fTABEL00F  if   e           k disk
001300121218     fTILAC00F  o    e           k disk
001400121218     fTITAS38C  if   e           k disk    rename(TITAS000:TITAS38)
001500121218     f                                     rename(TITAS010:TITAS381)
001600121218     f                                     rename(TITASP00:TITAS38P)
001700121218     fTITAS30C  uf   e           k disk
001800121218     fTNTBE01L  if   e           k disk
001900040119
002000040119      *---------------------------------------------------------------*
002100050330
002200121219     d dAR5doc       e ds                  inz
002300121218     d dLAC          e ds                  inz
002400121218     d dsTB          e ds                  inz
002500121218     d dTASflo       e ds                  inz
002600090212
002700090212       // - Parametri x TRUL55R
002800090212     d TRUL55ds      e ds                  inz
002900090212     d   I55tla      e                     inz('C')
003000121218
003100121218      // Parametri
003200121218     d kpjba         e ds
003300121218
003400121218     d param           ds
003500121218     d  AAAAdal                1      4  0
003600121218     d  MMGGdal                5      8  0
003700121218     d  AAAAal                 9     12  0
003800121218     d  MMGGal                13     16  0
003900121218     d  dataal                 9     16  0
004000121218
004100121218     d skKSC           s              7  0 dim(999) inz
004200121218     d skLDV           s              4  0 dim(999) inz
004300121218
004400121218       // - Campi di comodo
004500121219     d kAR5trd         s                   like(AR5trd) inz('DOC')
004600121218     d kcod            s                   like(tblcod) inz('TB')
004700121218     d kkey            s                   like(tblkey)
004800121218     d waddams         s              6    inz
004900121218     d wDataISO        s               d   datfmt(*ISO) inz
005000121218     d nrweek          s              2  0 inz
005100121218     d savtbl          s                   like(TAStbl) inz('  ')
005200121218     d woggi           s              8  0
005300121218     d wyear           s              4  0 inz
005400121218     d xx              s              4  0 inz
005500121218
005600121218       // - flag booleani
005700121218     d wEoF            s               n
005800090212
005900090212       //--------------------------------------------------------------
006000090212       //?Definizione procedure usate.                                 ?
006100090212       //--------------------------------------------------------------
006200090212
006300090212       // - Determinazione nome dell'immagine per TILAC
006400090212     d trul55r         pr                  extpgm('TRUL55R')
006500090212     d   trul55ds...
006600090212     d                                     likeds(trul55ds)
006700040119
006800040119      *---------------------------------------------------------------*
006900040119
007000040119      * Predisposizione indicatori per riconoscere i tipi record
007100040119      *   reperiti dal file TITAS30C
007200121219     iTITAS000      31
007300121219     iTITAS010      32
007400121219     iTITASP00      33
007500040119
007600040119      *---------------------------------------------------------------*
007700121218
007800121218     c     *entry        plist
007900121218     c                   parm                    kpjba
008000121218
008100121218     c                   eval      param = kpjbu
008200121218
008300121219     c     KTAS30        klist
008400121218     c                   kfld                    TASaas
008500121218     c                   kfld                    TASlnp
008600121218     c                   kfld                    TASnrs
008700121218     c                   kfld                    TASnsp
008800121219     c                   kfld                    TAStbl
008900121218
009000121218     c     Ktab          klist
009100121218     c                   KFLD                    CODUT
009200121218     c                   KFLD                    kcod
009300121218     c                   kfld                    kkey
009400121218     c                   z-add     1             codut             1 0
009500040119
009600121218      /free
009700121218
009800121218        wEoF = *off;
009900121218
010000121218       // Leggo TITAS
010100121218        setll (AAAAdal:MMGGdal) TITAS38C;
010200121218
010300121218        DOW not wEoF;
010400121218          reade (AAAAdal) TITAS38C;
010500121218
010600121218       // fine file
010700121218          IF  %eof(TITAS38C);
010800121218            leave;
010900121218          ENDIF;
011000121218
011100121218       // fino al
011200121218          IF (TASaas * 10000 + TASmgs) > (AAAAal * 10000 + MMGGal);
011300121218            leave;
011400121218          ENDIF;
011500121218
011600121218       // solo le bolle con flag immagine = 'S'
011700121218          dTASflo = TASflo;
011800121218          IF  §FLOiml <> 'S';
011900121218            iter;
012000121218          ENDIF;
012100121218
012200121218       // solo le bolle di clienti abilitati a FTP su tab. LAC
012300121219          clear dLAC;
012400121219          TBEcod = 'LAC';
012500121218          TBEke1 = %editc(TASksc:'X');
012600121219          chain (TBEcod:TBEke1) TNTBE01L;
012700121218          IF  not %found(TNTBE01L) or TBEatb <> *blanks;
012800121218            iter;
012900121218          ENDIF;
013000121218          dLAC = TBEuni;
013100121218          IF  §LACaut <> 'A';
013200121218            iter;
013300121218          ENDIF;
013400121219
013500121219       // se Merge non elaboro
013600121219          IF  §LACmerge = 'S' and §LACla2 = *blanks;
013700121219            iter;
013800121219          ENDIF;
013900121219
014000121219       // se LA2 devo agganciare la LA2
014100121219          IF  §LACla2 = 'S';
014200121219            clear dLAC;
014300121219            TBEcod = 'LA2';
014400121219            chain (TBEcod:TBEke1) TNTBE01L;
014500121219            IF  not %found(TNTBE01L) or TBEatb <> *blanks;
014600121219              iter;
014700121219            ENDIF;
014800121219            dLAC = TBEuni;
014900121219          ENDIF;
015000121219
015100121218          exsr elabora;
015200121218        ENDDO;
015300121218
015400121218        *inLR = *ON;
015500121218
015600121218       //-------------------------------------------------------------**
015700121218       // Elabora
015800121218       //-------------------------------------------------------------**
015900121218        BEGSR elabora;
016000121218
016100121218          woggi = %dec(%date());
016200121218
016300121219          *in31 = *off;
016400121219          *in32 = *off;
016500121219          *in33 = *off;
016600121219
016700121219          chain (TASaas:TASlnp:TASnrs:TASnsp:TAStbl) TITAS30C;
016800121219          IF  %found(TITAS30C);
016900121219
017000121218      /end-free
017100121219     c                   movel(p)  TAStbl        kkey
017200121219     c                   clear                   dstb
017300121219     c     ktab          chain     tabel00f
017400121219     c                   if        %found(tabel00f)
017500121219     c                   movel     TBLuni        dstb
017600121219     c                   endif
017700121218
017800121218     c* controllo porto (rispetto alla bolla originale)
017900121218     c                   if        §LACtpt <> 'E' and §TBtpo <> §LACtpt
018000121218     c                   leavesr
018100121218     c                   endif
018200121218     c* reso (rispetto alla bolla corrente (figlia))
018300121218     c                   if        §LACres <> 'S' and TASfbr = 'S'
018400121218     c                   leavesr
018500121218     c                   endif
018600121218     c* recupero (rispetto alla bolla originale)
018700121218     c                   if        §LACrec <> 'S' and §TBrbl = 'R'
018800121218     c                   leavesr
018900121218     c                   endif
019000121218     c* c/servizio (rispetto alla bolla originale)
019100121218     c                   if        §LACcsr <> 'S' and §TBrbl = 'C'
019200121218     c                   leavesr
019300121218     c                   endif
019400121218     c* serie (rispetto alla bolla originale)
019500121219     c                   if        §LACssr = 'S' and TASnrs = *zeros
019600121218     c                   leavesr
019700121218     c                   endif
019800121218     c* linea di partenza (rispetto alla bolla originale)
019900121218     c                   if        §LAClnp <> *zeros and
020000121219     c                             TASlnp <> §LAClnp
020100121218     c                   leavesr
020200121218     c                   endif
020300121218
020400121218      /free
020500121218          clear TILAC000;
020600121221          LACtim  = '20121221100000';
020700121218          LACaas  = %subst(%editc(TASaas:'X'):3:2);
020800121218          LAClnp  = %editc(TASlnp:'X');
020900121218          LACnrs  = %editc(TASnrs:'X');
021000121218          LACnsp  = %editc(TASnsp:'X');
021100121218          LACcop  = 'CLI';
021200121219          LACdir  = §LACdir;
021300121218          LACtpi  = §LACtpi;
021400121218
021500121218       // flag nome immagine:
021600121218       // se = "S "=significa che deve essere impostato col n.sped
021700121218       //           si pensa Laguna) --> quindi lascio = blanks
021800121218if  1     IF  §LACfmi <> 'S';
021900121218            clear  TRUL55ds;
022000121218            I55fmi = §LACfmi;
022100121218            I55lnp = TASlnp;
022200121218            I55nrs = TASnrs;
022300121218            I55nsp = TASnsp;
022400121218            I55aas = TASaas;
022500121218            I55mgs = TASmgs;
022600121218            I55rmn = TASrmn;
022700121218            trul55r ( TRUL55ds );
022800121218            if  O55err = *off;
022900121218              LACnim = O55nim;
023000121218            endif;
023100121218e   1     endif;
023200121218
023300121218          IF  §LACksc <> *zeros;
023400121218            LACidl  = %editc(§LACksc:'X') + §LACctr;
023500121218          ELSE;
023600121218            LACidl  = %editc(TASksc:'X') + %editc(TASctr:'X');
023700121218          ENDIF;
023800121219
023900121219          LACela  = '00';
024000121218          LACtela = 'A';
024100121218          LACksc  = TASksc;
024200121218          LACksu  = §LACksu;
024300121218          LACtad  = §LACtad;
024400121218          LACtadu = §LACtadu;
024500121218          LACfimp = §LACfimp;
024600121218          LACimp  = §LACimp;
024700121218
024800121218         //?AA/MM addebito + IDjob in base alla frequenza addebito
024900121218          SELECT;
025000121218            WHEN  LACtadu = 'I';
025100121218              LACidjob = LACaas + LAClnp + LACnrs + LACnsp;
025200121218            WHEN  LACtadu = 'J';
025300121218              LACidjob = %editc(LACksu:'X') + %editc(woggi:'X');
025400121218            WHEN  LACtadu = 'S';
025500121218      /end-free
025600121218     c     *iso          movel     woggi         wDataISO
025700121218     c                   clear                   nrweek
025800121218     c                   call      'XSRWEEK'
025900121218     c                   parm                    wDataiso
026000121218     c                   parm                    nrweek
026100121218      /free
026200121218              wyear = %subdt(wdataiso:*years);
026300121218              waddams = %editc(wyear:'X') + %editc(nrweek:'X');
026400121218              LACaddms = %dec(waddams:6:0);
026500121218              LACidjob = %editc(LACksu:'X') + 'S' + waddams;
026600121218            WHEN  LACtadu = 'M';
026700121218              LACaddms = %dec(%subst(%editc(woggi:'X'):1:6):6:0);
026800121218              LACidjob = %editc(LACksu:'X') + 'M' + %editc(LACaddms:'X');
026900121218          ENDSL;
027000121218
027100121218          WRITE  TILAC000;
027200050330
027300121218      /end-free
027400040119      *
027500040119     c                   movel     TASflo        dTASflo
027600040331     c                   eval      §FLOiml = 'I'
027700040119     c                   movel     dTASflo       TASflo
027800040119      *
027900040119     c   31              UPDATE    TITAS000
028000040119     c   32              UPDATE    TITAS010
028100040119     c   33              UPDATE    TITASP00
028200040119      *
028300121218     c                   endif
028400040119      *
028500040119     c                   ENDSR

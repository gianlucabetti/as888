000100160310     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000400060707      *-------------------------------------------------------------------------*
000500060707      ***************************************************************************
000600160309      *
000700971216      ***************************************************************************
000800160824     FFNARB01L  UF   E           K DISK
000802160824     Ftitas30c  UF   E           K DISK
001000090831      *
001001160826     FFiar601L  iF   E           K DISK
001002160831     FFiar401L  iF   E           K DISK
001003160831     FFiar501L  iF   E           K DISK
001100090831     Fazorg01L  IF   E           K DISK
001300160310     Ftabel00f  IF   E           K DISK
001301170308     Ffiar531c  iF   E           K DISK
001302170308     F                                     RENAME(FIAR5000:FIAR50S)
001303170308     ffnarbd0e  o  a e             disk    prefix('E_')
001400060719      *
001500060615      **?------------------------------------------------------------------ */
001600160830     C*?Aggiornamento peso vdl/Fuori Misura/Isola
001601160830     C*?su archivio bolle come ricevuto da DPD (dagli SCAN o dal clearing)
001602160920     c*
001603160920     c* Questo pgm girerà in ambiente di sede dopo che un pgm chiamante avrà
001604160920     c* aggiunto in lista librerie le librerie di filiale.
001700060719      **?------------------------------------------------------------------ */
001701160831     D FIDNA6R         PR
001702160831     D                                     extpgm('FIDNA6R')
001703160831     D fidna6ds                            likeds(fidna6ds)
001800160824     D fieu18r1ds    E DS
001801161005     D tibs34ds      E DS
001802161005     d AZUTEds       e ds                  extname(AZUTE00F)
001803161005     d DDatiUte      e ds
001804160824     D FNLV39DS      E DS
001805160826     d fnlr48ds      e ds
001806160826     d fidna6ds      e ds
001807160826     d trul90ds      e ds                  inz
001808160825     d fnlvv1ds      e ds
001809161005     d fnlv55ds      e ds
001810160826
001811160826     D og143         E DS
001812160831     D dar5gen       E DS
001813160826     D KPJBA         E DS
001814160826     D ds3a          E DS
001815160826     D ds3idp        E DS
001816160831     d* Ds per passaggio parametri a FNLR48R
001817160831     d dsarbd        e ds                  inz
001818160831     d dsarbk        e ds                  inz
001819160826     d darbt         e ds                  inz
001820160826     d dsarbg        e ds                  inz
001821160826     d dsarbn        e ds                  inz
001822160831     d* Ds per passaggio parametri al pgm di Recupero post-fatturazione (FNLVV1R)
001823170306     d dsarbd_       e ds                  extname(fnarbd2t) qualified
001824170306     d dsarbg_       e ds                  extname(fnarbg2t) qualified
001825160831      * Definizione del seguente campo in base alla dsarbd_ che è la più lunga
001826160825      * di tutte.
001827160825     dparvar           s            224
001900060719      **?------------------------------------------------------------------ */
002000160310     d cbf             S              2    DIM(100) inz
002100160310     d y               s              3  0 inz
002200000131      *----------------------------------------------------*
002300010219      *
002400000204     D W0140           S             14  0
002500991129     D WORA            S              6  0
002600991129     D WDTGIO          S              8  0
002700991129     D DATEU           S              8  0
002800000131     D Wdataoggi       S               D   DATFMT(*iso)
002900000203     D Wdata           S               D   DATFMT(*iso)
003101160824     D wcbo            S                   LIKE(ARbcbo)
003102160824     D wlna            S                   LIKE(ARblna)
003103160824     D wpkc            S                   LIKE(ARbpkc)
003104170323     D wpkf            S                   LIKE(ARbpkc)
003105170323     D I18WEIGHT1      S                   LIKE(ARbpkc)
003106160824     D wbollasede      S              1
003107160825     D ktrc            S                   like(ar6trc) inz('1')
003108170314     D s_knmus         S                   like(knmus)
003200020716      *
003201160920     D                SDS
003202160920     D  NOMPGM                 1     10
004400060720      **
004900060720
005000160310      /copy gaitrasrc/srcprotopr,fnlr48r
005001161005      /copy gaitrasrc/srcprotopr,fnlv55r
005100911213      *----------------------------------------------------*
005101160825     ITITAS000      02
005102160825     ITITAS010      03
005103160825     ITITASP00      04
005104160825
005200060615     c     *ENTRY        PLIST
005301160908     c                   parm                    kpjba
005302160908     c                   parm                    fieu18r1ds
005303160908
005305160824
005306160824     c                   eval      o18esito='0'
005307170323     c                   eval(h)   I18WEIGHT1=I18WEIGHT
005308170324
005309170324     c* Luciano ha deciso che:
005310170324     c* i pesi inferiori a 1kg li scartiamo
005311170324     c* tutti gli altri li carichiamo anche se inferiori ai nostri
005312170324     c*
005313170324     c* Attenzione: il peso (i18weight1) potrebbe essere = 0 (es se dpd ci passa 0,04 in
005314170324     c* i18weight1 viene messo 0) IN QUESTO CASO BISOGNA SEMPRE SCARTARE IL RECORD
005315170324
005316170410     c                   if        i18fclr = *blanks and I18WEIGHT1<1
005317170323     c                   return
005318170323     c                   endif
007000000201      *
007001160920     c* Chain spedizione
007002160920     c                   exsr      SR_spe
007003160920
007004160824     c                   select
007005160824     c* Ricevuto Peso
007006170323     c                   when      i18weight1>0
007007170323     c                   if        i18weight1<=§3IPKD
007100160824     C                   exsr      elaboraPeso
007101160824     c                   else
007102160824     C                   exsr      elaboraFuoriM
007103160824     c                   endif
007104160824     c* Ricevuto flag Fuori Misura
007105160830     c                   when      i18fclr='F'
007106160824     C                   exsr      elaboraFuoriM
007107160830     c* Ricevuto flag Isola
007108160830     c                   when      i18fclr='I'
007109160830     C                   exsr      elaboraIsola
007110160824     c                   endsl
007200010219      *
007300060615     C                   eval      *inrt = *on
007400000203      *----------------------------------------------------------------
007500160824      * Aggiornamento peso ricevuto nel peso VDL su ARB
007600000203      *----------------------------------------------------------------
007700160824     C     elaboraPeso   BEGSR
007800000131      *
007801160824     c                   clear                   fnlv39ds
007802170323     c                   z-add     i18weight1    d39plu
007803160824     c                   movel     'A'           d39tbo
007804160824     c                   movel     'C'           d39tpp
007805160824     C                   MOVEL     arbPKF        D39PLF
007806160824     C                   MOVEL     ARBPKB        D39PLB
007807160824     C                   MOVEL     ARBAAS        D39AAS
007808160824     C                   MOVEL     ARBLNP        D39LNP
007809160824     C                   MOVEL     ARBNRS        D39NRS
007810160824     C                   MOVEL     ARBNSP        D39NSP
007811160824     C                   MOVEL     ARBCBO        D39CBO
007812160824     C                   MOVEL     ARBNCL        D39NCL
007813160825     C                   z-add     1             D39NCp
007814160824     C                   MOVEL     'S'           D39FTR
007816160824     C* CHAIN SU FIAR6 PER PRENDERE DATA FATTURA
007817160825     C     kar6          CHAIN     FIAR6000                           30
007818160824     C  N30              MOVEL     AR6DFT        D39DFT
007819160824     C                   MOVEL     ARBLNA        D39LNA
007820160824     C*
007822160824     C                   MOVE      ARBDBR        D39DTE
007827160824     C                   MOVEL     fnlv39ds      KPJBU
007828160824     C                   CALL      'FNLV39R'
007829160824     C                   PARM                    KPJBA
007830160824     C*
007831160824     C                   MOVEL     KPJBU         fnlv39ds
007832160824     C* SE NON C'E' ERRORE : AGGIORNO LA BOLLA
007833160824    3C     D39ERR        IFNE      '1'
007834160824     C                   MOVEL     D39NCP        arbNCP
007835160824     C                   MOVEL     D39PLU        arbPKC
007836160824     C                   MOVEL     D39PLF        arbPKF
007837160824     C                   UPDATE    FNARB000
007838160825     c                   exsr      sr_insRA
007839160825     c                   else
007840170410     c                   eval      o18esito='1'
007841170308     c                   exsr      sr_log
007842160825     c                   unlock    fnarb01l
007843160824E   3C                   ENDIF
010000000203     C                   ENDSR
010001160824      *----------------------------------------------------------------
010002160824      * Aggiornamento peso ricevuto nel peso VDL su ARB
010003160824      *----------------------------------------------------------------
010004160824     C     elaboraFuoriM BEGSR
010007160824     c                   select
010008160824     c* Presente bolla su FNARB
010009160824     c                   when      wbollasede=*blanks
010011160825     c                   unlock    fnarb01l
010012160824     c                   if        arbtc1<>'F' and arbtc2<>'F' and
010013160824     c                             (arbtc1=*blanks or arbtc2=*blanks)
010014160824     c                   exsr      sr_lr48
010015160824     c                   endif
010016160824     c* Non presente bolla su FNARB --> Aggiorno direttamente la consegna
010017160824     c*                                 particolare F e se necessario
010018160824     c*                                 richiamo pgm per il recupero
010019160824     c*                                 post-fatturazione
010020160824     c                   when      wbollasede='S'
010022160824     c                   if        tasftc<>'F' and tastc2<>'F'  and
010023160824     c                             (tasftc=*blanks or tastc2=*blanks)
010024160824     c                   if        tasftc=*blanks
010025160824     c                   movel     'F'           tasftc
010026160824     c                   else
010027160824     c                   movel     'F'           tastc2
010028160824     c                   endif
010029160825     c* se bolla contabilizzata richiamo pgm per recupero post-fatturazione
010030160825     c                   if        tasfic<>*blanks
010031160825     c                   exsr      ctr_vrb
010032160825     c                   endif
010033160825     c   02              update    titas000
010034160831     c   03              update    titas010
010035160831     c   04              update    titasp00
010036160825     c                   exsr      sr_insRA
010037160824     c                   else
010038160824     c                   unlock    titas30c
010039160824     c                   endif
010040160824     c                   endsl
010041160824     c                   endsr
010042160830      *----------------------------------------------------------------
010043160830      * Aggiornamento Inoltro Isola come da Clearing
010044160830      *----------------------------------------------------------------
010045160830     C     elaboraIsola  BEGSR
010046160908     c* Fino a che l'aggiornamento del flag inoltro sulla bolla contabilizzata
010047160908     c* non verrà fatto anche dal trtr39r questo pgm lavorerà sempre solo con TITAS
010048160920     c* (no arb)
010049160908     c* Quindi forzo che wbollasede='S' per aggiornare solo TITAS
010050160908     c                   if        wbollasede=*blanks
010051160908     c                   eval      wbollasede='S'
010052160908      /free
010053160920             *in02=*off;
010054160920             *in03=*off;
010055160920             *in04=*off;
010056160908             chain(E) (i18aas:i18lnp:i18nrs:i18nsp) titas30c;
010057160908             if %error;
010058170410                eval o18esito='1';
010059170308                exsr sr_log;
010060160908                return;
010061160908             endif;
010062160908             if not %found(titas30c);
010063160908                eval o18esito='1';
010064170308                exsr sr_log;
010065160908                return;
010066160908             endif;
010067160908      /end-free
010068160908     c                   endif
010069160830     c                   select
010070160830     c* Presente bolla su FNARB
010071160908     c                   when      wbollasede=*blanks
010072160830     c                   unlock    fnarb01l
010073160920     c* LA "I" deve essere messa anche se c'è già una "D"
010074160830     c                   if        arbfin<>'I'
010075160830     c                   exsr      sr_lr48
010076160830     c                   endif
010077160831     c* Non presente bolla su FNARB -
010078160830     c                   when      wbollasede='S'
010079160830     c                   if        tasfin<>'I'
010080160908     c* se bolla contabilizzata richiamo and il pgm per il recupero post-fatturazione
010081160908     c                   eval      tasfin='I'
010082160830     c                   if        tasfic<>*blanks
010083160830     c                   exsr      ctr_vrb
010084160908     c                   endif
010085160830     c   02              update    titas000
010086160908     c   03              update    titas010
010087160908     c   04              update    titasp00
010088160830     c                   exsr      sr_insRA
010089160830     c                   else
010090160830     c                   unlock    titas30c
010091160830     c                   endif
010092160830     c                   endsl
010093160830     c                   endsr
010094160825      ****************************************************
010095160825     C     ctr_vrb       BEGSR
010096160825     c* Richiamo pgm memorizzazione variazioni post-fatturazione
010097160825     c                   clear                   fnlvv1ds
010098160830     c                   select
010099160830     c                   when      i18fclr='F'
010100160825     c                   eval      iv1cvb='CR'
010101160831     c                   clear                   dsarbg_
010102160831     c* anche se fnlvv1r sta utilizzando solo tc1 e tc2 imposto lo stesso tutti
010103160831     c* i campi della ds
010104160831     c                   eval      dsarbg_.ARBAAS=tasaas
010105160831     c                   eval      dsarbg_.ARBLNP=taslnp
010106160831     c                   eval      dsarbg_.ARBNRS=tasnrs
010107160831     c                   eval      dsarbg_.ARBNSP=tasnsp
010108160831     c                   eval      dsarbg_.ARBCBO=tascbo
010109160831     c                   eval      dsarbg_.ARBDTV=dateu
010110160831     c                   time                    dsarbg_.ARBORV
010111160831     c                   eval      dsarbg_.ARBCVB=iv1cvb
010112160831     c                   eval      dsarbg_.ARBPRU='DPD'
010113160831     c                   eval      dsarbg_.ARBTCR=tastcr
010114160831     c                   eval      dsarbg_.ARBDCR=tasdcr
010115160831     c                   eval      dsarbg_.ARBHCR=tashcr
010116160831     c                   eval      dsarbg_.ARBGC1=tasgc1
010117160831     c                   eval      dsarbg_.ARBGC2=tasgc2
010118160831     c                   eval      dsarbg_.arbtc1=tasftc
010119160831     c                   eval      dsarbg_.arbtc2=tastc2
010120160831     c                   eval      parvar =dsarbg_
010121160830     c                   when      i18fclr='I'
010122160830     c                   eval      iv1cvb='I0'
010123160831     c                   clear                   dsarbd_
010124160831     c                   eval      dsarbd_.ARBAAS=tasaas
010125160831     c                   eval      dsarbd_.ARBLNP=taslnp
010126160831     c                   eval      dsarbd_.ARBNRS=tasnrs
010127160831     c                   eval      dsarbd_.ARBNSP=tasnsp
010128160831     c                   eval      dsarbd_.ARBCBO=tascbo
010129160831     c                   eval      dsarbd_.ARBDTV=dateu
010130160831     c                   time                    dsarbd_.ARBORV
010131160831     c                   eval      dsarbd_.ARBCVB=iv1cvb
010132160831     c                   eval      dsarbd_.ARBPRU='DPD'
010133160831     c                   eval      dsarbd_.ARBRSD=tasrsd
010134160831     c*******            eval      dsarbd_.ARBRD2=
010135160831     c                   eval      dsarbd_.ARBIND=tasind
010136160831     c                   eval      dsarbd_.ARBCAD=tascad
010137160831     c                   eval      dsarbd_.ARBLOD=taslod
010138160831     c                   eval      dsarbd_.ARBPRD=tasprd
010139160831     c                   eval      dsarbd_.ARBNZD=tasnzd
010140160831     c                   eval      dsarbd_.ARBFIN='I'
010141160831     c*******            eval      dsarbd_.ARBCPI=
010142160831     c                   eval      dsarbd_.ARBPKB=taspkb
010143160831     c                   eval      dsarbd_.ARBPKF=taspkf
010144160831     c                   eval      dsarbd_.ARBVLB=tasvlb
010145160831     c                   eval      dsarbd_.ARBFVB=tasfvb
010146160831     c                   eval      dsarbd_.ARBVLF=tasvlf
010147160831     c                   eval      dsarbd_.ARBFVF=tasfvf
010148160831     c                   eval      dsarbd_.ARBFFD=tasffd
010151160831     c                   eval      parvar =dsarbd_
010152160830     c                   endsl
010153160825     c                   eval      iv1pgm=nompgm
010154160825     c                   eval      iv1aas=tasaas
010155160825     c                   eval      iv1lnp=taslnp
010156160825     c                   eval      iv1nrs=tasnrs
010157160825     c                   eval      iv1nsp=tasnsp
010158160825     c                   eval      iv1tbl=tastbl
010159160825     c                   eval      kpjbu=fnlvv1ds
010160160825     c                   call      'FNLVV1R'
010161160825     c                   parm                    kpjba
010162160825     c                   parm                    parvar
010163160825     c                   endsr
010164160825      /free
013301160824       //-------------------------------------------------------------*
013302160824       // Chain spedizione                                            *
013303160824       //-------------------------------------------------------------*
013304160824        begsr  SR_spe;
013305160824         clear wbollasede;
013306160824         if i18nsp=0;
013307160824              eval o18esito='1';
013308170308              exsr sr_log;
013309160824              return;
013310160824         endif;
013326160826         chain(E) (I18aas:i18lnp:i18nrs:i18nsp)  fnarb01l;
013327160826         if %error;
013328160826               eval o18esito='2';
013329170308              exsr sr_log;
013330160826               return;
013331160831         endif;
013332160824         if not %found(fnarb01l);
013333160831           if i18fclr<>' ';
013334160920             *in02=*off;
013335160920             *in03=*off;
013336160920             *in04=*off;
013337160831             chain(E) (i18aas:i18lnp:i18nrs:i18nsp) titas30c;
013338160831             if %error;
013339170410              eval o18esito='1';
013340170308              exsr sr_log;
013341160831                return;
013342160831             endif;
013343160831             if not %found(titas30c);
013344160831                eval o18esito='1';
013345170308              exsr sr_log;
013346160831                return;
013347160831             endif;
013348160831                wbollasede='S';
013349160831                wlna=taslna;
013350160831                wcbo=tascbo;
013351160831                wpkc=taspkc;
013352170323                wpkf=taspkf;
013353160831                exsr sr_chkspe;
013354160831           else;
013355160831       // Se pgm non richiamato dal clearing e non trovata spedizione
013356160824       // su archivi di filiale non faccio nulla
013357160831             eval o18esito='2';
013358170308              exsr sr_log;
013359160831             return;
013360160831           endif;
013361160831         else;
013362160824               wlna=arblna;
013363160824               wcbo=arbcbo;
013364160824               wpkc=arbpkc;
013365170323               wpkf=arbpkf;
013366160824               exsr sr_chkspe;
013367160824       //
013368160824         endif;
013369161005       // Reperisco il ter-par della lna per l'impostazione di simfel
013370161005         clear fnlv55ds ;
013371161005         d55tpt='P';
013372161005         d55lin=wlna;
013373161005         d55drf=dateu;
013374161005         fnlv55r(fnlv55ds);
013375161005IF  1    if d55err<>*blanks;
013376161005            d55tfp=wlna;
013377161005E   1    endif;
013378161005      /end-free
013379161005      ** imposta il SIMFEL come terminal di partenza
013380161005      *   Aggiorno area dati Dati Utente
013381161005     C     *lock         in        DDatiUte
013382161005     c                   movel     d55tfp        simfel
013383161005     C                   out       DDatiUte
013384161005     C                   UnLock(e) DDatiUte
013385161005      /free
013386160824         endsr;
013387160824       //-------------------------------------------------------------*
013388160824       // Controlla che la spedizione abbia i requisiti per l'aggior. *
013389160824       //-------------------------------------------------------------*
013390160824        begsr  SR_chkspe;
013391160831       // per aggiornamento per peso/fuori misura: non elaboro se già presente un peso VDL
013392170323       // oppure se il peso ricevuto da dpd è uguale al pkf
013393161003       // Se fuori misura dal clearing elaboro sempre
013394161003       // if (i18weight>0 or i18fclr='F') and wpkc>0;
013395170323          if i18weight1>0  and (wpkc>0 or i18weight1=wpkf);
013396160825           if wbollasede=' ';
013397160825              unlock fnarb01l ;
013398160825           else;
013399160825              unlock titas30c ;
013400160825           endif;
013401160824             return;
013402160824          endif;
013403160824       // Non elaboro se non è un franco
013404160824             if %lookup(wcbo:cbf)<=0;
013405160825           if wbollasede=' ';
013406160825              unlock fnarb01l ;
013407160825           else;
013408160825              unlock titas30c ;
013409160825           endif;
013410160824                return;
013411160824             endif;
013412160824       // Non elaboro se non  si tratta di bolla export
013413160824             chain wlna azorg01l   ;
013414160824             clear og143;
013415160824             if %found(azorg01l);
013416160824                og143=orgde3;
013417160824             endif;
013418160824             if §ogntw<>'DPD' ;
013419160825           if wbollasede=' ';
013420160825              unlock fnarb01l ;
013421160825           else;
013422160825              unlock titas30c ;
013423160825           endif;
013424160824                return;
013425160824             endif;
013426170308       // Non elaboro se spedizione con instradamento forzato da eex a dpd
013427170308       // ad eccezione dei pesi che ricevo minori o uguali al max stabilito
013428170308       // per dpd
013429170323           if i18fclr<>*blanks or i18weight1>§3ipkd;
013430170308             clear dar5gen;
013431170308             chain (i18aas:i18lnp:i18nrs:i18nsp:'GEN') fiar531c;
013432170308             if %found(fiar531c);
013433170308                dar5gen=ar5uni;
013434170308                if §AR5FDPD='S' ;
013435170308                  if wbollasede=' ';
013436170308                     unlock fnarb01l ;
013437170308                  else;
013438170308                     unlock titas30c ;
013439170308                  endif;
013440170410                  eval o18esito='9';
013441170308                  return;
013442170308                endif;
013443170308             endif;
013444170308           endif;
013445160824         endsr;
013446160824      **?------------------------------------------------------------------ */
013447160929      ***  Richiamo LR48 per l'aggiunta del fuori misura o inoltro ISOLA
013448160824      **?------------------------------------------------------------------ */
013449160824     C     sr_lr48       begSR
013450160824        clear fnlr48ds;
013451160824        clear dsarbg;
013452160824        clear dsarbd;
013453160824        clear dsarbk;
013454160824        clear darbt;
013455160824        d48aas=arbaas;
013456160824        d48lnp=arblnp;
013457160824        d48nrs=arbnrs;
013458160824        d48nsp=arbnsp;
013459160824        d48fgs=arbtfa;
013460160824        d48cbo=arbcbo;
013461160824        d48tbo='A'   ;
013462160824        d48ffr='E'   ;
013463160831        select;
013464170323        when i18weight1>0 or i18fclr='F' ;
013465160831           d48cvb='CR';
013466160831           d48trc='G';
013467160831           §BGTCR=arbtcr;
013468160831           §BGHCR=arbhcr;
013469160831           §BGDCR=arbdcr;
013470160831           §BGGC1=arbgc1;
013471160831           §BGGC2=arbgc2;
013472160831           §BGTC1=arbtc1;
013473160831           §BGTC2=arbtc2;
013474160831           if arbtc1=*blanks;
013475160831              §BGTC1='F';
013476160831           else;
013477160831              §BGTC2='F';
013478160831           endif;
013479160831        when i18fclr='I';
013480160831           d48cvb='I0';
013481160831           d48trc='D';
013482160831           §BDRSD=arbrsd      ;
013483160831           // -?Reperimento ragione sociale estesa del destinatario?
013484160831           chain  ( D48aas : D48lnp : D48nrs : D48nsp : 'D' )  FIAR4000;
013485160831           if  %found(FIAR401L);
013486160831             §BDrd2 = AR4not;
013487160831           endif;
013488160831           §BDIND=arbind      ;
013489160831           §BDCAD=arbcad      ;
013490160831           §BDLOD=arblod      ;
013491160831           §BDPRD=arbprd      ;
013492160831           §BDNZD=arbnzd      ;
013493160831           §BDFIN='I'         ;
013494160831           §BDCPI=arbcpi      ;
013495160831           §BDPKF=arbpkf      ;
013496160831           §BDVLF=arbvlf      ;
013497160831           §BDFVF=arbfvf      ;
013498160831           §BDFFD=arbffd      ;
013499160831           // -?Reperimento referente e telefono del destinatario?
013500160831           clear  dAR5gen;
013501160831           chain  ( D48aas : D48lnp : D48nrs : D48nsp : 'GEN' )  FIAR5000;
013502160831           if  %found(FIAR501L)  and  AR5atb = *blank;
013503160831             dAR5gen = AR5uni;
013504160831             §BDref = §AR5ref;
013505160831             §BDtel = §AR5teld;
013506160831           endif;
013507160831        endsl;
013508160824        kpjbu=fnlr48ds;
013509170314        s_knmus=knmus;
013510170327        knmus='EDPCEDXXX';
013511160824        fnlr48r(kpjba:dsarbd:dsarbk:darbt:dsarbg:trul90ds);
013512160824        fnlr48ds=kpjbu;
013513170314        knmus=s_knmus;
013514160824        if d48err <> *blanks and d48err<>'Z' ;
013515160825              eval o18esito='1';
013516170308              exsr sr_log;
013517160825        else;
013518160825        exsr  sr_insRA;
013519160824        endif;
013520160824
013521160825        endsr;
013522160825       //-------------------------------------------------------------*
013523160825       //inserimento Richiesta Assistenza                             *
013524160825       //-------------------------------------------------------------*
013525160825        Begsr sr_InsRA;
013526160825
013527160825           clear fidna6ds;
013528160825
013529160907           iDA06MAD = ' 30';
013530160825
013531160825           select;
013532160825       // Da scan
013533170323           when I18weight1>0   ;
013534160901              ida06no1= 'Peso kg';
013535170323              ida06no1=%trim(ida06no1) + ' ' + %trim(%editc(i18weight1:'4'));
013536160901              ida06no1=%trim(ida06no1) + ' ' + 'ricevuto da DPD';
013537160830       // Da Clearing - Fuori Misura
013538160830           when I18fclr  = 'F' ;
013539160901              ida06no1 = 'Aggiunto Fuori Misura da Clearing';
013540160830       // Da Clearing - Isola
013541160830           when I18fclr  = 'I' ;
013542170314              ida06no1 = 'Aggiunto Inoltro Isola da Clearing';
013543160825           endsl;
013580160825
013581160825           iDA06TOR = 'S';
013582160825           iDA06OGG = %trim(%editc(i18LNP:'X')) +
013583160825                      %trim(%editc(i18NRS:'X')) +
013584160825                      %trim(%editc(i18NSP:'X')) +
013585160825                      %trim(%editc(i18AAS:'X'));
013587160826              iDA06UTE = 'DPD';
013588160826           if wbollasede='S';
013589160826              iDA06FIL = taslna;
013590160826           else;
013592160825              iDA06FIL = arblna;
013593160826           endif;
013616160825
013625160825
013626160826           callP(e) FIDNA6R (fidna6ds);
013627160825        endsr;
013628160824      /end-free
013629170308       //-------------------------------------------------------------*
013630170308       //scrittura record su fnarbd0e in caso di errore               *
013631170308       //-------------------------------------------------------------*
013632170308        Begsr sr_log  ;
013633170308           eval E_ARBAAS=i18aas     ;
013634170308           eval E_ARBLNP=i18lnp     ;
013635170308           eval E_ARBNRS=i18nrs     ;
013636170308           eval E_ARBNSP=i18nsp     ;
013638170308           eval E_ARBDTV=dateu      ;
013639170308           eval E_ARBPRU='FIEU18R1' ;
013640170308           eval E_ARBDER=o18esito   ;
013641170308      /end-free
013642170308     c                   time                    E_ARBORV
013643170308      /free
013644170308
013645170308           write fnarbd00 ;
013646170308        endsr;
013647060719      **?------------------------------------------------------------------ */
013648060719      *   DEFINIZIONE CHIAVI
013649060719      **?------------------------------------------------------------------ */
013700940321     C     *INZSR        BEGSR
013800940321      *
014300991129      *
014400160825     C     Kar6          KLIST
014500160825     C                   KFLD                    i18aas
014600160825     C                   KFLD                    i18lnp
014700160825     C                   KFLD                    i18nrs
014800160825     C                   KFLD                    i18nsp
014801160825     C                   KFLD                    ktrc
016000000908      *
016100971216      * Recupero data e ora
016200971216     C                   TIME                    WORA
016300991124     C                   TIME                    W0140
016400160826      * UDATE IN GGMMAAAA
016500991124     C                   MOVE      W0140         WDTGIO
016600991124      * UDATE IN AAAAMMGG
016700000131     C     *eur          MOVEL     WDTGIO        Wdataoggi
016800000131     C     *iso          MOVEL     Wdataoggi     dateu
016801161005
016802161005     c     *dtaara       define    §azute        azuteds
016803161005     c     *dtaara       define    §datiute      ddatiute
016804161005      *
016805161005     c                   in(E)     *dtaara
016806161005     c                   IF        %ERROR or RSUT = *blanks
016807161005     c                   clear                   Tibs34Ds
016808161005     c                   call      'TIBS34R'
016809161005     c                   parm                    Tibs34Ds
016810161005     c                   in        *dtaara
016811161005     c                   ENDIF
016900160310      /free
017000160310       // Leggo tabella "3A" per caricare schiera dei codici bolla con c/a
017100160310       // e schiera codici bolla franco
017200160310       y=0;
017300160310       clear cbf;
017400160310       setll (1:'3A') tabel00f;
017500160310       reade (1:'3A') tabel00f;
017600160310
017700160310       dow    not %eof(tabel00f);
017800160310          ds3a=tbluni;
017900160310          if %subst(§3atb1:1:1)='F';
018000160310             y=y+1;
018100160310             cbf(y)=tblkey;
018200160310          endif;
018300160310          reade (1:'3A') tabel00f;
018400160310       enddo;
018401160824       // Recupero Max peso DPD da tabella "3I" "DPD"
018402160824       clear tbluni;
018403160824       chain (1:'3I':'DPD     ') tabel00f;
018404160824       ds3idp=tbluni;
018500160310      /end-free
018600010219      *
018700060616     C                   ENDSR
018800060719      **?------------------------------------------------------------------ */

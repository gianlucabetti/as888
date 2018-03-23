000100040721      *===============================================================*
000200120605      * TNSB73R    * Rigenerazione Documenti Mitt. per clienti        *
000300040721      *===============================================================*
000400040721
000500040721     h Decedit('0,') Datedit(*dmy/) Option(*nodebugio)
000600040721
000700040721      *===============================================================*
000800040721
000900120605     fTITAS31C  if   e           k disk
001000120606     fFIAR531C  if   e           k disk
001100121022     fTIJDC00F  o    e             disk
001200050324     fTABEL00F  if   e           k disk
001300040721
001400040721      *===============================================================*
001500040721
001600040721      * Strutture Dati - - - - - - - - - - - - - - - - - - - - - - - -*
001700040721      *
001800040721     d KPJBA         e ds
001900040721      * - Parametri x Controllo profilo utenti
002000040721     d TIBS34DS      e ds                  inz
002100040721      * - Ds di riferimento al file esterno AZUTE00F
002200040721     d AZUTEDS       e ds                  extname(AZUTE00F)
002300040721      * - Ds per dati organigramma
002400040721     d DDatiUte      e ds
002500050317      *
002600120606     d dAR5doc       e ds                  inz
002700120606
002800120605     d djdc          e ds                  inz
002900080314      * DS PER TIBS02R - GESTIONE TNTBE00F
003000080314     d TIBS02ds      e ds                  inz
003100090209
003200130411       // - Parametri x TRUL55R
003300130411     d TRUL55ds      e ds                  inz
003400130411     d   I55tla      e                     inz('C')
003500040721      *
003600040721     d Status         sds           333
003700040721     d   SDSpgm          *proc
003800040721     d   SDSjob              244    253                                         Job name
003900040721      *
004000120605     d TNSB73ds        ds                  inz
004100120605     d   d73dcd                       8  0 inz
004200120605     d   d73dca                       8  0 inz
004300120605     d   d73ksc                       7  0 inz
004400120605     d   d73imm                       1    inz
004500120605     d   d73dsd                       8  0 inz
004600120605     d   d73dsa                       8  0 inz
004700120605     d   d73dir                      30    inz
004800120605     d   d73fmi                       2    inz
004900121022     d   d73tpi                       3    inz
005000120605     d   d73ksu                       7  0 inz
005100120606     d   d73tad                       1    inz
005200120606     d   d73tadu                      1    inz
005300120606     d   d73fimp                      1    inz
005400120606     d   d73imp                      10  3 inz
005500120606     d   d73kscf                      7  0 inz
005600120606     d   d73ctr                       3    inz
005700120606     d   d73job                      16    inz
005800120606     d   d73tpt                       1    inz
005900120606     d   d73res                       1    inz
006000120606     d   d73rec                       1    inz
006100120606     d   d73csr                       1    inz
006200120606     d   d73ssr                       1    inz
006300120606     d   d73lnp                       3  0 inz
006400120607     d   d73cop                       3    inz
006500040721      *
006600040721     d DS_Time1        ds            14    inz
006700040721     d   dsT1_hms                     6  0 inz
006800040721     d   dsT1_dmy                     8  0 inz
006900040721     d DS_Time2        ds            14    inz
007000040721     d   dsT2_ymd                     8  0 inz
007100040721     d   dsT2_hms                     6  0 inz
007200040721      *
007300040721      * Variabili  - - - - - - - - - - - - - - - - - - - - - - - - - -*
007400040721      *
007500040721      * - Campi di comodo
007600040721     d WdateISO        s               d   datfmt(*ISO) inz
007700040721     d Wtime           s             14  0              inz
007800040721     d WtasDCM         s              8                 inz(*zeros)
007900050330     d WtasAAS         s              4    inz(*zeros)
008000050617     d WtasDSP         s              8    inz(*zeros)
008100120606     d kAR5trd         s                   like(AR5trd) inz('DOC')
008200090209
008300090209       //--------------------------------------------------------------
008400090209       //?Definizione procedure usate.                                 ?
008500090209       //--------------------------------------------------------------
008600090209
008700120605       // - Determinazione nome dell'immagine per TIJDC
008800130411     d trul55r         pr                  extpgm('TRUL55R')
008900130411     d   trul55ds...
009000130411     d                                     likeds(trul55ds)
009100040721
009200040721      *===============================================================*
009300040721      *  RIEPILOGO INDICATORI                                         *
009400040721      *---------------------------------------------------------------*
009500040721      * 10    - Comodo                                                *
009600040721      *===============================================================*
009700040721
009800040721     c     *Entry        plist
009900040721     c                   parm                    KPJBA
010000120605     c                   movel     KPJBU         TNSB73ds
010100040721      *
010200040721      * Operazioni iniziali
010300040721     c                   exsr      OperazIniz
010400040721      *
010500050318      * Ciclo di lettura del file TITAS31C
010600120606     c     d73ksc        setll     TITAS31C
010700120606     c     d73ksc        reade     TITAS31C
010800040721      *
010900050318do  1c                   dow       NOT %eof(TITAS31C)
011000050324     c                   select
011100120606     c                   when      d73dcd > 0
011200120605     c                   if        tasdcm < d73dcd or tasdcm > d73dca
011300050324     c                   goto      leggi
011400050324     c                   endif
011500120605     c                   when      d73dsd > 0
011600120605     c                   if        (tasaas*10000+tasmgs) < d73dsd or
011700120605     c                             (tasaas*10000+tasmgs) > d73dsa
011800050324     c                   goto      leggi
011900050324     c                   endif
012000050324     c                   endsl
012100120606
012200120605      * elaborazione delle sole bolle con documenti
012300120605     c     kAR501        chain     FIAR531C
012400120605     c                   IF        not %found(FIAR531C)
012500120605     c                   goto      leggi
012600120605     c                   ENDIF
012700120605
012800120605     c                   eval      dar5doc = ar5uni
012900050317     c                   select
013000120605if  2c                   when      §AR5jfid = 'I' and (d73imm = 'G' or
013100120605     c                             d73imm = 'E')
013200120606     c                   exsr      Wrt_TIJDC
013300050317     c*
013400120605if  2c                   when      §AR5jfid = 'S' and (d73imm = 'M' or
013500120605     c                             d73imm = 'E')
013600120606     c                   exsr      Wrt_TIJDC
013700050317e   2c                   endsl
013800120606
013900050324     c     leggi         tag
014000040721      *
014100050318      * - Lettura del record successivo nel file TITAS31C
014200120605     c     d73ksc        reade     TITAS31C
014300040721      *
014400040721e   1c                   enddo
014500040721      *
014600040721      * Fine
014700040721     c                   eval      *inLR = *on
014800090209      /free
014900130411           reset  TRUL55ds;
015000130411           trul55r ( TRUL55ds );
015100090209      /end-free
015200040721      *
015300040721      *---------------------------------------------------------------*
015400040721      * Operazioni iniziali                                           *
015500040721      *---------------------------------------------------------------*
015600040721     c     OperazIniz    BEGSR
015700040721      *
015800040721      * Definizione Key-List
015900120606      * - FIAR531C
016000120606     c     KAR501        klist
016100120606     c                   kfld                    TASaas
016200120606     c                   kfld                    TASlnp
016300120606     c                   kfld                    TASnrs
016400120606     c                   kfld                    TASnsp
016500120606     c                   kfld                    kAR5trd
016600040721      *
016700040721      * Reperisco dati job
016800040721     c                   exsr      DatiJob
016900040721      *
017000050317      * Impostazione del campo LAcTIM = aaaa/mm/gg+hh:mm:ss
017100040721     c                   time                    Wtime
017200040721     c                   movel     Wtime         DS_Time1
017300040721     c                   eval      dsT2_hms   = dsT1_hms
017400040721     c     *eur          movel     dsT1_dmy      WdateISO
017500040721     c                   move      WdateISO      dsT2_ymd
017600050317     c*
017700120605     c* Reperisco dati cliente da tabella "JDC"
017800050325     C                   clear                   tibs02ds
017900120606     c                   clear                   djdc
018000050317     C                   MOVEL     'C'           T02MOD
018100050317     C                   MOVEL     KNSIF         T02SIF
018200120605     C                   MOVEL     'JDC'         T02COD
018300120605     c                   movel(P)  d73ksc        t02ke1
018400050317     C                   CALL      'TIBS02R'
018500050317     C                   PARM                    KPJBA
018600050325     C                   PARM                    tibs02ds
018700120605     c                   eval      djdc = t02uni
018800040721      *
018900040721     c                   ENDSR
019000040721      *
019100040721      *---------------------------------------------------------------*
019200040721      * Reperimento Dati del job (Utente/Operativi)                   *
019300040721      *---------------------------------------------------------------*
019400040721     c     DatiJob       BEGSR
019500040721      *
019600040721     c     *dtaara       define    §azute        azuteds
019700040721     c     *dtaara       define    §datiute      ddatiute
019800040721      *
019900040721     c                   in(E)     *dtaara
020000040721     c                   IF        %ERROR or RSUT = *blanks
020100040721     c                   clear                   Tibs34Ds
020200040721     c                   call      'TIBS34R'
020300040721     c                   parm                    Tibs34Ds
020400040721     c                   in        *dtaara
020500040721     c                   ENDIF
020600040721      *
020700040721     c                   ENDSR
020800040721      *
020900040721      *---------------------------------------------------------------*
021000120606      * Registrazione dati nel file TIJDC00F x cliente                *
021100040721      *---------------------------------------------------------------*
021200120606     c     Wrt_TIJDC     BEGSR
021300040721      *
021400040721      * Impostazione dei campi numerici in campi alfanumerici di comodo
021500040721     c                   movel     TASdcm        WtasDCM
021600050330     c                   move      TASaas        WtasAAS
021700050617     c                   movel     TASaas        WtasDSP
021800050617     c                   move      TASMGS        WtasDSP
021900040721      *
022000120606      * Aggancio record corrispondente nel file TIJDC00F
022100120606     c                   clear                   TIJDC000
022200050317     c                   move      TASaas        LACaas
022300050317     c                   move      TASlnp        LAClnp
022400050317     c                   move      TASnrs        LACnrs
022500050317     c                   move      TASnsp        LACnsp
022600120607     c                   movel     d73cop        LACcop
022700040721      *
022800040721      * Impostazione nuovi dati
022900050317     c                   eval      LACtim  = DS_Time2
023000120606     c                   if        d73kscf <> *zeros
023100120606     c                   eval      lacidl = %editc(d73kscf:'X') + d73ctr
023200120606     c                   else
023300120606     c                   eval      lacidl = %editc(tasksc:'X') +
023400120606     c                                      %editc(tasctr:'X')
023500120606     c                   endif
023600050317     c                   eval      LACela  = *zeros
023700120606     c                   eval      LACdir  = d73dir
023800120606     c                   eval      LACtpi  = d73tpi
023900050324     c* flag nome immagine:
024000050324     c* se = "S "=significa che deve essere impostato col n.sped (ci pe
024100050324     c*           sa Laguna) --> quindi lascio = blanks
024200090209      /free
024300090209         clear  LACnim;
024400120606         if  D73fmi <> 'S ';
024500130411           clear  TRUL55ds;
024600130411           I55fmi = D73fmi;
024700130411           I55lnp = TASlnp;
024800130411           I55nrs = TASnrs;
024900130411           I55nsp = TASnsp;
025000130411           I55aas = TASaas;
025100130411           I55mgs = TASmgs;
025200130411           I55rmn = TASrmn;
025300130411           I55fld = 'D';
025400130411           trul55r ( TRUL55ds );
025500130411           if  O55err = *off;
025600130411             LACnim = O55nimd;
025700090209           endif;
025800090209         endif;
025900090209      /end-free
026000120606     c                   eval      lactad = d73tad
026100120606      * imposto i nuovi campi di TIJDC
026200090310     c                   eval      lactela = 'S'
026300120606     c                   eval      lacksc = d73ksc
026400120606     c                   eval      lacksu = d73ksu
026500120606     c                   eval      lactadu = d73tadu
026600120606     c                   eval      lacfimp = d73fimp
026700120606     c                   eval      lacimp = d73imp
026800120606     c                   eval      lacidjob = d73job
026900040721      *
027000120606     c                   WRITE     TIJDC000
027100121022
027200040721     c                   ENDSR

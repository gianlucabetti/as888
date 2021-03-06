000100080714     h decedit('0,') datedit(*ymd/) option(*nodebugio)
000200061227
000300080714      //------------------------------------------------------------------------
000400080714      //         QUADRATURA DATI PER PDA AUT
000500080714      //------------------------------------------------------------------------
000600061227
000700080714      //------------------------------------------------------------------------
000800080714      // RIEPILOGO INDICATORI
000900080714      //------------------------------------------------------------------------
001000071205
001100061227
001200080714      //------------------------------------------------------------------------
001300080714      //  V A R I A B I L I
001400080714      //------------------------------------------------------------------------
001500080714     d $eof            s              1n   inz(*off)
001600061227
001700080714      //------------------------------------------------------------------------
001800080714      //  S C H I E R E
001900080714      //------------------------------------------------------------------------
002000061227
002100080714      //------------------------------------------------------------------------
002200080714      //  D S   I N T E R N E / E S T E R N E
002300080714      //------------------------------------------------------------------------
002400080714     d fipdods       e ds                  extname(fipdo00t)
002500070115     d fior56ds      e ds
002600080714     d fior57ds      e ds
002700080714     d fnorgds       e ds                  extname(fnorg00f)
002800061227     d kpjba         e ds
002900080714
003000080714      //------------------------------------------------------------------------
003100080714      //  P R O G R A M M I   R I C H I A M A T I
003200080714      //------------------------------------------------------------------------
003300080714     d fior56r         pr                  extpgm('FIOR56R')
003400080714     d  kpjba                              likeds(kpjba)
003500080714     d  fior56ds                           likeds(fior56ds)
003600061227
003700080714      //------------------------------------------------------------------------
003800080714
003900080714     c     *entry        plist
004000080714     c                   parm                    kpjba
004100080714     c                   parm                    fior57ds
004200080714
004300080714      /free
004400080714
004500080714       exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
004600080714
004700080915       // verifica ritiri presenti su PDO (non annullati) ma non presenti su ORG
004800080714
004900080714       exsr sr_elabpdo;
005000061227
005100080714       *inlr = *on;
005200080714
005300080714       //--------------------------------------------------------------
005400080714       //?Elabora FIPDO
005500080714       //--------------------------------------------------------------
005600080714       begsr sr_elabpdo;
005700080714
005800080714         reset $eof;
005900080714
006000080714         exsr opencursor;
006100080714         dou $eof;
006200080714          exsr readcursor;
006300080714         enddo;
006400080714
006500080714         exsr closecursor;
006600080714
006700080714       endsr;
006800080714
006900080714       //--------------------------------------------------------------
007000080714       //?Apertura cursore PDO
007100080714       //--------------------------------------------------------------
007200080714       begsr opencursor;
007300080714
007400080714       exec sql
007500080714        declare pdo cursor for select fipdo00t.*
007600080714        from fipdo00t
007700080714        where pdondc = :or57ndc and pdofgs =:or57fgs
007800080915        and pdoatb = ''
007900080714        order by pdopoe, pdonsr, pdonor, pdonrv;
008000080714
008100080714        exec sql open pdo;
008200080714
008300080714       endsr;
008400080714
008500080714       //--------------------------------------------------------------
008600080714       //?Lettura cursore PDO
008700080714       //--------------------------------------------------------------
008800080714       begsr readcursor;
008900080714
009000080714         exec sql fetch next from pdo into :fipdods;
009100080714
009200080714         select;
009300080714          when sqlcode = 100;
009400080714           $eof = *on;
009500080714          when sqlcode < 0;
009600080714           $eof = *on;
009700080714x   1     other;
009800080714           exsr sr_elaborm;
009900080714         endsl;
010000080714
010100080714       endsr;
010200080714
010300080714       //--------------------------------------------------------------
010400080714       //?Chiusura cursore PDO
010500080714       //--------------------------------------------------------------
010600080714       begsr closecursor;
010700080714
010800080714         exec sql close pdo;
010900080714
011000080714       endsr;
011100080714
011200080714       //--------------------------------------------------------------
011300080714       //?Elaborazione singolo record
011400080714       //--------------------------------------------------------------
011500080714       begsr sr_elaborm;
011600080714
011700080714        exec sql select fnorg00f.* into :fnorgds from fnorg00f
011800080714             where orgfgs = :pdofgs and orgndc = :pdondc and
011900080714             orgpoe = :pdopoe and orgnsr = :pdonsr and orgnor = :pdonor
012000080714             and orgnsr = :pdonsr;
012100080714
012200080714        if sqlcod <> 0 ;
012300080714         clear fior56ds;
012400080714         or56tla = 'A';
012500080714         or56fgs = pdofgs;
012600080714         or56ndcd = pdondc;
012700080714         or56ndca = pdondc;
012800080714         or56poe = pdopoe;
012900080714         or56nsr = pdonsr;
013000080714         or56nor = pdonor;
013100080714         or56nrv = pdonrv;
013200080714         fior56r(kpjba:fior56ds);
013300080714        endif;
013400080714
013500080714       endsr;
013600080714

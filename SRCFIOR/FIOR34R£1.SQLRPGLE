000100110624     /*PRM dbgview(*source)
000200110624     /*PRM commit(*NONE)
000300110624     /*END
000400110624
000500080811     h decedit('0,') datedit(*ymd.) option(*nodebugio)
000600110624     h alwnull(*inputonly)
000700110624
000800080811      // ----------------------------------------------------------------------
000900080811      //
001000080811      //         PULIZIA FILE PDA  ?
001100080811      //
001200080811      // ----------------------------------------------------------------------
001300110624
001400080811      // ? V A R I A B I L I ?
001500110714     d wdata           s              8  0 inz
001600080811     d dataiso         s               d   datfmt(*iso)
001700110714     d wdataiso        s               d   datfmt(*iso)
001800080811     d dtapda          s              8  0 inz
001900080811     d dtapdaa         s              8    inz
002000080811     d dtaprosp        s              8  0 inz
002100080811     d dtalog          s              8  0 inz
002200080811     d dtaloga         s              8    inz
002300110624     d wDtaILV         s             14    inz(*zero)
002400080811     d $rpt            s               n
002500080811
002600080811      // ? D S   I N T E R N E / E S T E R N E ?
002700110624     d ds5A2         e ds
002800080811     d firptds       e ds                  extname(firpt00f)
002900080811     d kpjba         e ds
003000080811     d tabelds       e ds                  extname(tabel00f)
003100110624
003200110624       //--------------------------------------------------------------
003300110624       //?Specifiche di calcolo.                                       ?
003400110624       //--------------------------------------------------------------
003500080811
003600080811     c     *entry        plist
003700080811     c                   parm                    kpjba
003800080811
003900080811      /free
004000080811
004100080811       //?operazioni iniziali
004200080811         exsr routinz;
004300080811
004400080811       //?elaborazione per pilizia prospetto ORM
004500080811         exsr sr_prospetto;
004600080811
004700080811       //?elaborazione per pulizia file log
004800080811         exsr sr_log;
004900080811
005000080811       //?elaborazione per pulizia file PDA
005100080811         exsr sr_pda;
005200110624
005300110624       // -?elaborazione per pulizia file PDA?
005400110624       exsr  sr_ILV;
005500080811
005600110624       // -?fine?
005700080811         *inlr = *on;
005800080811
005900080811       // ----------------------------------------------------------------------
006000080811       //?Operazioni iniziali.
006100080811       // ----------------------------------------------------------------------
006200080811       begsr routinz;
006300080811
006400080811         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
006500080811
006600080811       //?cerco la tabella 5a key 2 per recuperare i giorni di pulizia
006700080811         clear ds5a2;
006800080811         exec sql select tabel00f.* into:tabelds from tabel00f
006900080811                  where tblkut = 1 and tblcod = '5A' and
007000080811                        substr(tblkey, 1, 1) = '2' and
007100080811                        tblflg = '';
007200080811         if sqlcod = 0;
007300080811          ds5a2 = tbluni;
007400080811         endif;
007500080811
007600080811       //?dalla data odierna calcolo le date di pulizia
007700080811         wdata = %dec(%date());
007800080811         dataiso = %date(wdata:*iso);
007900080811       //?data prospetto ORM
008000080811         wdataiso = dataiso - %days(§5aporm);
008100080811         dtaprosp = %dec(wdataiso);
008200080811
008300080811       //?data file log
008400080811         wdataiso = dataiso - %days(§5apdal);
008500080811         dtalog = %dec(wdataiso);
008600080811         dtaloga = %editc(%dec(dtalog):'X');
008700080811
008800080811       //?data file pda
008900080811         wdataiso = dataiso - %days(§5apdao);
009000080811         dtapda = %dec(wdataiso);
009100080811         dtapdaa = %editc(%dec(dtapda):'X');
009200080811
009300080811       endsr;
009400080811
009500080811       // ----------------------------------------------------------------------
009600080811       //?Pulisco file prospetto ORM
009700080811       // ----------------------------------------------------------------------
009800080811       begsr sr_prospetto;
009900080811
010000080811         $rpt = *off;
010100080811
010200080811       //?cerco le distinte chiuse fino alla data pulizia
010300080811         exec sql
010400080811          declare firpt cursor for select firpt01l.*
010500080811          from firpt01l where rptdch <= :dtaprosp
010600080811          order by rptfgs, rptnfv, rptpdr;
010700080811
010800080811         exec sql open firpt;
010900080811
011000080811         dow not $rpt;
011100080811
011200080811          exec sql fetch next from firpt into: firptds;
011300080811
011400080811          select;
011500080811       //?fine file esco
011600080811          when sqlcod = 100 or sqlcod < 0;
011700080811           $rpt = *on;
011800080811
011900080811          other;
012000080811       //?cancello firpd
012100080811          exec sql
012200080811           delete from firpd00f
012300080811           where rpdfgs = :rptfgs and rpdnfv = :rptnfv and rpdpdr = :rptpdr;
012400080811       //?cancello firpt
012500080811          exec sql
012600080811           delete from firpt00f
012700080811           where rptfgs = :rptfgs and rptnfv = :rptnfv and rptpdr = :rptpdr;
012800080811          endsl;
012900080811         enddo;
013000080811
013100080811         exec sql close firpt;
013200080811
013300080811       endsr;
013400080811
013500080811       // ----------------------------------------------------------------------
013600080811       //?Pulisco file di log
013700080811       // ----------------------------------------------------------------------
013800080811       begsr sr_log;
013900080811
014000080811       //?cancello log fino alla data pulizia
014100080811         exec sql
014200080811          delete from fiplg00f
014300080811          where substr(plgdatora, 1, 8) <= :dtaloga;
014400080811
014500080811       endsr;
014600080811
014700080811       // ----------------------------------------------------------------------
014800080811       //?Pulisco file dati PDA
014900080811       // ----------------------------------------------------------------------
015000080811       begsr sr_pda;
015100080811
015200080811       //?cancello dati a PDA fino alla data pulizia
015300080811         exec sql
015400080811          delete from fipdo00t
015500080811          where substr(pdodatora, 1, 8) <= :dtapdaa;
015600080811
015700080811       //?cancello OK da PDA fino alla data pulizia
015800080811         exec sql
015900080811          delete from fipro00f
016000080811          where substr(prodatora, 1, 8) <= :dtapdaa;
016100080811
016200080811       endsr;
016300110624
016400110624       //--------------------------------------------------------------
016500110624       //?Pulizia file Immagini LDV.                                   ?
016600110624       //--------------------------------------------------------------
016700110624       BEGSR  sr_ILV;
016800110624
016900110624         // -?Impostazione data limite per la cancellazione dei vecchi?
017000110624         //  ?record dal file FIILV00F?
017100110624         wDtaILV = %subst( %char( %dec( %timestamp() - %days(§5Ailv) ) )
017200110929                           : 1 : 8 ) + '999999';
017300110624
017400110624         // -?Cancellazione vecchi record dal file FIILV00F?
017500110624         exec SQL   delete from FIILV00F
017600110624                     where ILVtimA between 1 and :wDtaILV
017700110624                        or ( ILVtimA = 0 and
017800110624                             ILVtimD between 1 and :wDtaILV );
017900110714
018000110714         // -?Cancellazione record relativi a spedizioni con consegna?
018100110714         //  ?anomala (dirottamento, reso, ...) dal file FIILV00F?
018200110714         exec SQL   delete from FIILV00F
018300110714                     where (digits(ILVaas) concat digits(ILVlnp) concat
018400110714                            digits(ILVnrs) concat digits(ILVnsp)) in
018500110714                   ( select digits(ILVaas) concat digits(ILVlnp) concat
018600110714                            digits(ILVnrs) concat digits(ILVnsp)
018700110714                       from FIILV00F  inner join  TITAS00F
018800110714                         on ILVaas = TASaas and
018900110714                            ILVlnp = TASlnp and
019000110714                            ILVnrs = TASnrs and
019100110714                            ILVnsp = TASnsp and
019200110714                            TAScca <> ' '
019300110714                      UNION
019400110714                     select digits(ILVaas) concat digits(ILVlnp) concat
019500110714                            digits(ILVnrs) concat digits(ILVnsp)
019600110714                       from FIILV00F  inner join  TITAS10F
019700110714                         on ILVaas = TASaas and
019800110714                            ILVlnp = TASlnp and
019900110714                            ILVnrs = TASnrs and
020000110714                            ILVnsp = TASnsp and
020100110714                            TAScca <> ' ' );
020200110624
020300110624       ENDSR;
020400110624
020500110624      /end-free

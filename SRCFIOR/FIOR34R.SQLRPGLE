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
002400130528     d wDtaIRD         s              8  0 inz
002500080811     d $rpt            s               n
002600080811
002700080811      // ? D S   I N T E R N E / E S T E R N E ?
002800110624     d ds5A2         e ds
002900080811     d firptds       e ds                  extname(firpt00f)
003000080811     d kpjba         e ds
003100080811     d tabelds       e ds                  extname(tabel00f)
003200110624
003300110624       //--------------------------------------------------------------
003400110624       //?Specifiche di calcolo.                                       ?
003500110624       //--------------------------------------------------------------
003600080811
003700080811     c     *entry        plist
003800080811     c                   parm                    kpjba
003900080811
004000080811      /free
004100080811
004200080811       //?operazioni iniziali
004300080811         exsr routinz;
004400080811
004500080811       //?elaborazione per pilizia prospetto ORM
004600080811         exsr sr_prospetto;
004700080811
004800080811       //?elaborazione per pulizia file log
004900080811         exsr sr_log;
005000080811
005100080811       //?elaborazione per pulizia file PDA
005200080811         exsr sr_pda;
005300110624
005400110624       // -?elaborazione per pulizia file PDA?
005500110624       exsr  sr_ILV;
005600130528       exsr  sr_IRD;
005700080811
005800110624       // -?fine?
005900080811         *inlr = *on;
006000080811
006100080811       // ----------------------------------------------------------------------
006200080811       //?Operazioni iniziali.
006300080811       // ----------------------------------------------------------------------
006400080811       begsr routinz;
006500080811
006600080811         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
006700080811
006800080811       //?cerco la tabella 5a key 2 per recuperare i giorni di pulizia
006900080811         clear ds5a2;
007000080811         exec sql select tabel00f.* into:tabelds from tabel00f
007100080811                  where tblkut = 1 and tblcod = '5A' and
007200080811                        substr(tblkey, 1, 1) = '2' and
007300080811                        tblflg = '';
007400080811         if sqlcod = 0;
007500080811          ds5a2 = tbluni;
007600080811         endif;
007700080811
007800080811       //?dalla data odierna calcolo le date di pulizia
007900080811         wdata = %dec(%date());
008000080811         dataiso = %date(wdata:*iso);
008100080811       //?data prospetto ORM
008200080811         wdataiso = dataiso - %days(§5aporm);
008300080811         dtaprosp = %dec(wdataiso);
008400080811
008500080811       //?data file log
008600080811         wdataiso = dataiso - %days(§5apdal);
008700080811         dtalog = %dec(wdataiso);
008800080811         dtaloga = %editc(%dec(dtalog):'X');
008900080811
009000080811       //?data file pda
009100080811         wdataiso = dataiso - %days(§5apdao);
009200080811         dtapda = %dec(wdataiso);
009300080811         dtapdaa = %editc(%dec(dtapda):'X');
009400080811
009500080811       endsr;
009600080811
009700080811       // ----------------------------------------------------------------------
009800080811       //?Pulisco file prospetto ORM
009900080811       // ----------------------------------------------------------------------
010000080811       begsr sr_prospetto;
010100080811
010200080811         $rpt = *off;
010300080811
010400080811       //?cerco le distinte chiuse fino alla data pulizia
010500080811         exec sql
010600080811          declare firpt cursor for select firpt01l.*
010700080811          from firpt01l where rptdch <= :dtaprosp
010800080811          order by rptfgs, rptnfv, rptpdr;
010900080811
011000080811         exec sql open firpt;
011100080811
011200080811         dow not $rpt;
011300080811
011400080811          exec sql fetch next from firpt into: firptds;
011500080811
011600080811          select;
011700080811       //?fine file esco
011800080811          when sqlcod = 100 or sqlcod < 0;
011900080811           $rpt = *on;
012000080811
012100080811          other;
012200080811       //?cancello firpd
012300080811          exec sql
012400080811           delete from firpd00f
012500080811           where rpdfgs = :rptfgs and rpdnfv = :rptnfv and rpdpdr = :rptpdr;
012600080811       //?cancello firpt
012700080811          exec sql
012800080811           delete from firpt00f
012900080811           where rptfgs = :rptfgs and rptnfv = :rptnfv and rptpdr = :rptpdr;
013000080811          endsl;
013100080811         enddo;
013200080811
013300080811         exec sql close firpt;
013400080811
013500080811       endsr;
013600080811
013700080811       // ----------------------------------------------------------------------
013800080811       //?Pulisco file di log
013900080811       // ----------------------------------------------------------------------
014000080811       begsr sr_log;
014100080811
014200080811       //?cancello log fino alla data pulizia
014300080811         exec sql
014400080811          delete from fiplg00f
014500080811          where substr(plgdatora, 1, 8) <= :dtaloga;
014600080811
014700080811       endsr;
014800080811
014900080811       // ----------------------------------------------------------------------
015000080811       //?Pulisco file dati PDA
015100080811       // ----------------------------------------------------------------------
015200080811       begsr sr_pda;
015300080811
015400080811       //?cancello dati a PDA fino alla data pulizia
015500080811         exec sql
015600080811          delete from fipdo00t
015700080811          where substr(pdodatora, 1, 8) <= :dtapdaa;
015800080811
015900080811       //?cancello OK da PDA fino alla data pulizia
016000080811         exec sql
016100140211          delete from fipro10f
016200080811          where substr(prodatora, 1, 8) <= :dtapdaa;
016300080811
016400080811       endsr;
016500110624
016600110624       //--------------------------------------------------------------
016700110624       //?Pulizia file Immagini LDV.                                   ?
016800110624       //--------------------------------------------------------------
016900110624       BEGSR  sr_ILV;
017000110624
017100110624         // -?Impostazione data limite per la cancellazione dei vecchi?
017200110624         //  ?record dal file FIILV00F?
017300110624         wDtaILV = %subst( %char( %dec( %timestamp() - %days(§5Ailv) ) )
017400110929                           : 1 : 8 ) + '999999';
017500110624
017600110624         // -?Cancellazione vecchi record dal file FIILV00F?
017700110624         exec SQL   delete from FIILV00F
017800110624                     where ILVtimA between 1 and :wDtaILV
017900110624                        or ( ILVtimA = 0 and
018000110624                             ILVtimD between 1 and :wDtaILV );
018100110714
018200110714         // -?Cancellazione record relativi a spedizioni con consegna?
018300110714         //  ?anomala (dirottamento, reso, ...) dal file FIILV00F?
018400110714         exec SQL   delete from FIILV00F
018500110714                     where (digits(ILVaas) concat digits(ILVlnp) concat
018600110714                            digits(ILVnrs) concat digits(ILVnsp)) in
018700110714                   ( select digits(ILVaas) concat digits(ILVlnp) concat
018800110714                            digits(ILVnrs) concat digits(ILVnsp)
018900110714                       from FIILV00F  inner join  TITAS00F
019000110714                         on ILVaas = TASaas and
019100110714                            ILVlnp = TASlnp and
019200110714                            ILVnrs = TASnrs and
019300110714                            ILVnsp = TASnsp and
019400110714                            TAScca <> ' '
019500110714                      UNION
019600110714                     select digits(ILVaas) concat digits(ILVlnp) concat
019700110714                            digits(ILVnrs) concat digits(ILVnsp)
019800110714                       from FIILV00F  inner join  TITAS10F
019900110714                         on ILVaas = TASaas and
020000110714                            ILVlnp = TASlnp and
020100110714                            ILVnrs = TASnrs and
020200110714                            ILVnsp = TASnsp and
020300110714                            TAScca <> ' ' );
020400110624
020500110624       ENDSR;
020600130528
020700130528       //--------------------------------------------------------------
020800130528       //?Pulizia file Rigenerazione Immagini LDV.                     ?
020900130528       //--------------------------------------------------------------
021000130528       BEGSR  sr_IRD;
021100130528
021200130528         // -?Impostazione data limite per la cancellazione dei vecchi?
021300130528         //  ?record dal file FIIRD10F?
021400130528         wDtaIRD = %dec( %date() - %days(8) );
021500130528
021600130528         // -?Cancellazione vecchi record dal file FIIRD10F?
021700130528         exec SQL   delete from FIIRD10F
021800130528                     where IRDdatIns <= :wDtaIRD;
021900130528
022000130528       ENDSR;
022100110624
022200110624      /end-free

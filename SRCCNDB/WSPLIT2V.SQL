000100000000Sessione salvata è stata riavviata.
000200000000Il collegamento corrente è al database relazionale AS888.
000300170721--------------------------------------------------------------------------------
000400000000CREATE VIEW UNITRAGRP/WSPLIT2V (
000500000000  CAPOCONTO,
000600000000  CLIENTE,
000700000000  INTES_FATT,
000800000000  COD_FISC,
000900000000  PART_IVA,
001000000000  COND_PAG,
001100000000  D_COND_PAG,
001200000000  TIPO_PAG,
001300000000  TRATT_IVA )
001400000000AS (
001500000000  with IND_CLP as ( select INDkcc, INDksc, CLPscf,
001600000000                           INDcdf, INDiva, INDcdp
001700000000                      from CNIND00F inner join CNCLP00F
001800000000                        on INDkut = CLPkut
001900000000                       and INDkcc = CLPkcc
002000000000                       and INDksc = CLPksc
002100000000                     where INDksc = CLPscf ),
002200000000       PagRIBA as ( select TBLcod, TBLkey,
002300000000                           substr(TBLuni,  1, 30) as §FAdes,
002400000000                           substr(TBLuni, 31,  1) as §FAtpg,
002500000000                           substr(TBLuni, 33,  1) as §FAtti
002600000000                      from TABEL00F
002700000000                     where TBLkut = 1
002800000000                       and TBLcod = 'FA'
002900000000                       and substr(TBLuni, 31,  1) = '1' )
003000000000  select INDkcc as CAPOCONTO,
003100000000         INDksc as CLIENTE,
003200000000         CLPscf as INTES_FATT,
003300000000         INDcdf as COD_FISC,
003400000000         INDiva as PART_IVA,
003500000000         substr(INDcdp, 4, 3) as COND_PAG,
003600000000         §FAdes as D_COND_PAG,
003700000000         §FAtpg as TIPO_PAG,
003800000000         §FAtti as TRATT_IVA
003900000000    from IND_CLP exception join ANSPI00F
004000000000      on ( INDcdf = SPIiva or INDiva = SPIiva )
004100000000                 inner join PagRIBA
004200000000      on ( substr(INDcdp, 4, 3) concat '1' = TBLkey )
004300170901   where §FAtti = '2'
004400170901   UNION
004500170901  select INDkcc as CAPOCONTO,
004600170901         INDksc as CLIENTE,
004700170901         CLPscf as INTES_FATT,
004800170901         INDcdf as COD_FISC,
004900170901         INDiva as PART_IVA,
005000170901         substr(INDcdp, 4, 3) as COND_PAG,
005100170901         §FAdes as D_COND_PAG,
005200170901         §FAtpg as TIPO_PAG,
005300170901         §FAtti as TRATT_IVA
005400170901    from IND_CLP inner join ANSPI00F
005500170901      on ( INDcdf = SPIiva or INDiva = SPIiva )
005600170901                 inner join PagRIBA
005700170901      on ( substr(INDcdp, 4, 3) concat '1' = TBLkey )
005800170901   where §FAtti = '2'
005900180123     and ( SPIdde > int( current_date )
006000180123      or   SPIdsc < int( current_date ) )
006100180123   )
006200000000 RCDFMT WSPLIT2V
006300000000Vista WSPLIT2V creata in UNITRAGRP.
006400170721--------------------------------------------------------------------------------
006500000000LABEL ON TABLE UNITRAGRP/WSPLIT2V
006600000000  IS 'Clienti NO SplitPayment, RiBa, T.Iva=2'
006700000000LABEL ON per WSPLIT2V in UNITRAGRP completato.
006800170721--------------------------------------------------------------------------------
006900000000LABEL ON COLUMN UNITRAGRP/WSPLIT2V
007000000000( CAPOCONTO  text is 'CapoConto' ,
007100000000  CLIENTE    text is 'Cod. Cliente' ,
007200000000  INTES_FATT text is 'Cod. Intestazione Fatt.',
007300000000  COD_FISC   text is 'Codice Fiscale' ,
007400000000  PART_IVA   text is 'Partiva Iva' ,
007500000000  COND_PAG   text is 'Condizione Pagamento' ,
007600000000  D_COND_PAG text is 'Descriz. Condiz. Pagam.' ,
007700000000  TIPO_PAG   text is 'Tipo Pagam.: 0=RD,1=RB,...' ,
007800000000  TRATT_IVA  text is 'Tipo Tratt. Iva: 0=NO,1=SI' )
007900000000LABEL ON per WSPLIT2V in UNITRAGRP completato.
008000170721--------------------------------------------------------------------------------
008100000000LABEL ON COLUMN UNITRAGRP/WSPLIT2V
008200000000( CAPOCONTO  is 'CapoConto' ,
008300000000  CLIENTE    is 'Cod. Cliente' ,
008400000000  INTES_FATT is 'Intestaz. Fatt.' ,
008500000000  COD_FISC   is 'Codice Fiscale' ,
008600000000  PART_IVA   is 'Partiva Iva' ,
008700000000  COND_PAG   is 'Condiz. Pagam.' ,
008800000000  D_COND_PAG is 'Descr. Condiz.Pagam.' ,
008900000000  TIPO_PAG   is 'Tipo Pagam.' ,
009000000000  TRATT_IVA  is 'Tipo Tratt. Iva' )
009100000000LABEL ON per WSPLIT2V in UNITRAGRP completato.
009200170721--------------------------------------------------------------------------------
009300000000GRANT SELECT
009400000000ON UNITRAGRP/WSPLIT2V TO PUBLIC
009500000000GRANT di autorizzazione a WSPLIT2V in UNITRAGRP completato.
009600170721--------------------------------------------------------------------------------
009700000000GRANT ALTER , REFERENCES , SELECT
009800000000ON UNITRAGRP/WSPLIT2V TO QPGMR WITH GRANT OPTION
009900000000GRANT di autorizzazione a WSPLIT2V in UNITRAGRP completato.
010000170901---?CANCELLAZIONE?--------------------------------------------------------------
010100171219DROP VIEW UNITRAGRP/WSPLIT2V CASCADE
010200171219Rilascio di WSPLIT2V in UNITRAGRP completo.
010300000000--------------------------------------------------------------------------------

000100000000Sessione salvata è stata riavviata.
000200000000Il collegamento corrente è al database relazionale AS888.
000300170721--------------------------------------------------------------------------------
000400000000CREATE VIEW UNITRAGRP/WSPLIT1V (
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
003900000000    from IND_CLP inner join ANSPI00F
004000000000      on ( INDcdf = SPIiva or INDiva = SPIiva )
004100000000                 inner join PagRIBA
004200000000      on ( substr(INDcdp, 4, 3) concat '1' = TBLkey )
004300170901   where §FAtti <> '2'
004400170901     and SPIdde <= int( current_date )
004500170901     and SPIdsc >= int( current_date ) )
004600000000 RCDFMT WSPLIT1V
004700000000Vista WSPLIT1V creata in UNITRAGRP.
004800170721--------------------------------------------------------------------------------
004900000000LABEL ON TABLE UNITRAGRP/WSPLIT1V
005000000000  IS 'Clienti SplitPayment, RiBa, T.Iva<>2'
005100000000LABEL ON per WSPLIT1V in UNITRAGRP completato.
005200170721--------------------------------------------------------------------------------
005300000000LABEL ON COLUMN UNITRAGRP/WSPLIT1V
005400000000( CAPOCONTO  text is 'CapoConto' ,
005500000000  CLIENTE    text is 'Cod. Cliente' ,
005600000000  INTES_FATT text is 'Cod. Intestazione Fatt.',
005700000000  COD_FISC   text is 'Codice Fiscale' ,
005800000000  PART_IVA   text is 'Partiva Iva' ,
005900000000  COND_PAG   text is 'Condizione Pagamento' ,
006000000000  D_COND_PAG text is 'Descriz. Condiz. Pagam.' ,
006100000000  TIPO_PAG   text is 'Tipo Pagam.: 0=RD,1=RB,...' ,
006200000000  TRATT_IVA  text is 'Tipo Tratt. Iva: 0=NO,1=SI' )
006300000000LABEL ON per WSPLIT1V in UNITRAGRP completato.
006400170721--------------------------------------------------------------------------------
006500000000LABEL ON COLUMN UNITRAGRP/WSPLIT1V
006600000000( CAPOCONTO  is 'CapoConto' ,
006700000000  CLIENTE    is 'Cod. Cliente' ,
006800000000  INTES_FATT is 'Intestaz. Fatt.',
006900000000  COD_FISC   is 'Codice Fiscale' ,
007000000000  PART_IVA   is 'Partiva Iva' ,
007100000000  COND_PAG   is 'Condiz. Pagam.' ,
007200000000  D_COND_PAG is 'Descr. Condiz.Pagam.' ,
007300000000  TIPO_PAG   is 'Tipo Pagam.' ,
007400000000  TRATT_IVA  is 'Tipo Tratt. Iva' )
007500000000LABEL ON per WSPLIT1V in UNITRAGRP completato.
007600170721--------------------------------------------------------------------------------
007700000000GRANT SELECT
007800000000ON UNITRAGRP/WSPLIT1V TO PUBLIC
007900000000GRANT di autorizzazione a WSPLIT1V in UNITRAGRP completato.
008000170721--------------------------------------------------------------------------------
008100000000GRANT ALTER , REFERENCES , SELECT
008200000000ON UNITRAGRP/WSPLIT1V TO QPGMR WITH GRANT OPTION
008300000000GRANT di autorizzazione a WSPLIT1V in UNITRAGRP completato.
008400000000---?CANCELLAZIONE?--------------------------------------------------------------
008500000000DROP VIEW UNITRAGRP/WSPLIT1V CASCADE
008600000000Rilascio di WSPLIT1V in UNITRAGRP completo.
008700170721--------------------------------------------------------------------------------

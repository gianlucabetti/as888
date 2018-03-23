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
001500000000with IND_CLP as ( select INDkcc, INDksc, CLPscf,
001600000000                         INDcdf, INDiva, INDcdp
001700000000                    from CNIND00F inner join CNCLP00F
001800000000                      on INDkut = CLPkut
001900000000                     and INDkcc = CLPkcc
002000000000                     and INDksc = CLPksc
002100000000                   where INDksc = CLPscf ),
002200000000     PagRIBA as ( select TBLcod, TBLkey,
002300000000                         substr(TBLuni,  1, 30) as §FAdes,
002400000000                         substr(TBLuni, 31,  1) as §FAtpg,
002500000000                         substr(TBLuni, 33,  1) as §FAtti
002600000000                    from TABEL00F
002700000000                   where TBLkut = 1
002800000000                     and TBLcod = 'FA'
002900000000                     and substr(TBLuni, 31,  1) = '1' )
003000000000select INDkcc as CAPOCONTO,
003100000000       INDksc as CLIENTE,
003200000000       CLPscf as INTES_FATT,
003300000000       INDcdf as COD_FISC,
003400000000       INDiva as PART_IVA,
003500000000       substr(INDcdp, 4, 3) as COND_PAG,
003600000000       §FAdes as D_COND_PAG,
003700000000       §FAtpg as TIPO_PAG,
003800000000       §FAtti as TRATT_IVA
003900000000  from IND_CLP inner join ANSPI00F
004000000000    on ( INDcdf = SPIiva or INDiva = SPIiva )
004100000000               inner join PagRIBA
004200000000    on ( substr(INDcdp, 4, 3) concat '1' = TBLkey )
004300000000 where §FAtti <> '2' )
004400000000 RCDFMT WSPLIT1V
004500000000Vista WSPLIT1V creata in UNITRAGRP.
004600170721--------------------------------------------------------------------------------
004700000000LABEL ON TABLE UNITRAGRP/WSPLIT1V
004800000000  IS 'Clienti SplitPayment, RiBa, T.Iva<>2'
004900000000LABEL ON per WSPLIT1V in UNITRAGRP completato.
005000170721--------------------------------------------------------------------------------
005100000000LABEL ON COLUMN UNITRAGRP/WSPLIT1V
005200000000( CAPOCONTO  text is 'CapoConto' ,
005300000000  CLIENTE    text is 'Cod. Cliente' ,
005400000000  INTES_FATT text is 'Cod. Intestazione Fatt.',
005500000000  COD_FISC   text is 'Codice Fiscale' ,
005600000000  PART_IVA   text is 'Partiva Iva' ,
005700000000  COND_PAG   text is 'Condizione Pagamento' ,
005800000000  D_COND_PAG text is 'Descriz. Condiz. Pagam.' ,
005900000000  TIPO_PAG   text is 'Tipo Pagam.: 0=RD,1=RB,...' ,
006000000000  TRATT_IVA  text is 'Tipo Tratt. Iva: 0=NO,1=SI' )
006100000000LABEL ON per WSPLIT1V in UNITRAGRP completato.
006200170721--------------------------------------------------------------------------------
006300000000LABEL ON COLUMN UNITRAGRP/WSPLIT1V
006400000000( CAPOCONTO  is 'CapoConto' ,
006500000000  CLIENTE    is 'Cod. Cliente' ,
006600000000  INTES_FATT is 'Intestaz. Fatt.',
006700000000  COD_FISC   is 'Codice Fiscale' ,
006800000000  PART_IVA   is 'Partiva Iva' ,
006900000000  COND_PAG   is 'Condiz. Pagam.' ,
007000000000  D_COND_PAG is 'Descr. Condiz.Pagam.' ,
007100000000  TIPO_PAG   is 'Tipo Pagam.' ,
007200000000  TRATT_IVA  is 'Tipo Tratt. Iva' )
007300000000LABEL ON per WSPLIT1V in UNITRAGRP completato.
007400170721--------------------------------------------------------------------------------
007500000000GRANT SELECT
007600000000ON UNITRAGRP/WSPLIT1V TO PUBLIC
007700000000GRANT di autorizzazione a WSPLIT1V in UNITRAGRP completato.
007800170721--------------------------------------------------------------------------------
007900000000GRANT ALTER , REFERENCES , SELECT
008000000000ON UNITRAGRP/WSPLIT1V TO QPGMR WITH GRANT OPTION
008100000000GRANT di autorizzazione a WSPLIT1V in UNITRAGRP completato.
008200170721--------------------------------------------------------------------------------

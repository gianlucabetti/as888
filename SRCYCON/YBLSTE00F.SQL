000100161122--  Generazione SQL
000200161122--  Versione:                  V7R1M0 100423
000300161122--  Generata su:               22/11/16 10:18:14
000400161122--  Database relazionale:      AS888
000500161122--  Opzioni standard:          DB2 for i
000600161122
000700161122CREATE TABLE PJBARGRU/YBLS_TESTATA_PROPOSTE_BLOCCO FOR SYSTEM NAME YBLSTE00F (
000800161122  DATA_PROPOSTA FOR COLUMN DTPROP     DATE NOT NULL ,
000900161122  DATA_CREAZIONE_PROPOSTA FOR COLUMN DTCREPROP  DATE NOT NULL ,
001000161122  DATA_INVIO_MORA FOR COLUMN DTINVMORA  DATE NOT NULL ,
001100161122  DATA_BLOCCO_SERVIZIO FOR COLUMN DTBLOSERV  DATE NOT NULL ,
001200161122  DATA_LIMITE_DEROGA1 FOR COLUMN LMDEROGA1  DATE NOT NULL ,
001300161122  DATA_LIMITE_DEROGA2 FOR COLUMN LMDEROGA2  DATE NOT NULL ,
001400161122  TIME_INS_TESTATA FOR COLUMN TMINSTES   TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP ,
001500161122  TIME_UPD_TESTATA FOR COLUMN TMUPDTES   TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP )
001600161122
001700161122  RCDFMT YBLS_00001 ;
001800161122
001900161122GRANT DELETE , INSERT , SELECT , UPDATE
002000161122ON PJBARGRU/YBLS_TESTATA_PROPOSTE_BLOCCO TO PUBLIC ;
002100161122
002200161122GRANT ALTER , DELETE , INDEX , INSERT , REFERENCES , SELECT , UPDATE
002300161122ON PJBARGRU/YBLS_TESTATA_PROPOSTE_BLOCCO TO QPGMR WITH GRANT OPTION ;
002400161122

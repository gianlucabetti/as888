000100110201     D*--------------------------------------------------
000200110201     D* Procedure name: COMP_Init
000300110201     D* Purpose:        Inizializza il modulo.
000400110201     D* Returns:        Esito.
000500110201     D* Parameter:      priKpjba
000600110201     D*--------------------------------------------------
000700110201     D COMP_Init       PR            10I 0
000800110201     D  priKpjba                           LIKE(kpjba)
000900110201
001000110201     D*--------------------------------------------------
001100110201     D* Procedure name: COMP_Finalize
001200110201     D* Purpose:        Chiusura del modulo.
001300110201     D* Returns:
001400110201     D*--------------------------------------------------
001500110201     D COMP_Finalize   PR
001600110201
001700110201     D*--------------------------------------------------
001800110201     D* Procedure name: COMP_NewContrassegniByCliente
001900110201     D* Purpose:        Prepara un elenco di contrassegni per cliente.
002000110201     D* Returns:
002100110201     D* Parameter:      priCdi => Codice cliente intestazione assegno.
002200110204     D* Parameter:      priDta => Data incasso contrassegno.
002201110204     D* Parameter:      priOrderBy => '1' = importo discendente, '2' = data bolla.
002300110201     D*--------------------------------------------------
002400110201     D COMP_NewContrassegniByCliente...
002500110201     D                 PR            10I 0
002600110201     D  priCdi                             LIKE(cndizion.$ksc) CONST
002700110204     D  priDta                             LIKE(cndizion.$dta) VALUE
002701110204     D  priOrderBy                    1A   CONST
002800110201
002900110201     D*--------------------------------------------------
003000110201     D* Procedure name: COMP_EndContrassegniByCliente
003100110201     D* Purpose:        Chiude elenco contrassegni.
003200110201     D* Returns:        Esito.
003300110201     D*--------------------------------------------------
003400110201     D COMP_EndContrassegniByCliente...
003500110201     D                 PR            10I 0
003600110201
003700110201     D*--------------------------------------------------
003800110201     D* Procedure name: COMP_GetNextContrassegno
003900110201     D* Purpose:        Restituisce il prossimo contrassegno.
004000110201     D* Returns:        Esito.
004100110201     D* Parameter:      priAas => Anno bolla.
004200110201     D* Parameter:      priLnp => Filiale bolla.
004300110201     D* Parameter:      priNrs => Serie bolla.
004400110201     D* Parameter:      priNsp => Numero bolla.
004500110201     D* Parameter:      priCas => Importo contrassegno.
004600110201     D*--------------------------------------------------
004700110201     D COMP_GetNextContrassegno...
004800110201     D                 PR            10I 0
004900110201     D  priAas                             LIKE(cndizion.$aas)
005000110201     D  priLnp                             LIKE(cndizion.$lnp)
005100110201     D  priNrs                             LIKE(cndizion.$nrs)
005200110201     D  priNsp                             LIKE(cndizion.$nsp)
005300110201     D  priCas                             LIKE(cndizion.$cas)
005301110204     D  priMgs                             LIKE(cndizion.$mmg)
005400110201
005500110202     D*--------------------------------------------------
005600110202     D* Procedure name: COMP_ChkCliente
005700110202     D* Purpose:        Controllo cliente.
005800110202     D* Returns:        Esito.
005900110202     D* Parameter:      PriCdi => Codice cliente.
006000110202     D*--------------------------------------------------
006100110202     DCOMP_ChkCliente  PR            10I 0
006200110202     D  priCdi                             LIKE(cndizion.$ksc) CONST
006300110202
006400110202     D*--------------------------------------------------
006500110202     D* Procedure name: COMP_ChkClienteFatturazione
006600110202     D* Purpose:        Controllo cliente di fatturazione.
006700110202     D* Returns:        Esito.
006800110202     D* Parameter:      PriKscCliFatt => Codice cliente.
006900110202     D*--------------------------------------------------
007000110202     DCOMP_ChkClienteFatturazione...
007100110202     D                 PR            10I 0
007200110202     D  priKscCliFatt                      LIKE(andiz00f.ksc) CONST
007300110202
007400110202     D*--------------------------------------------------
007500110202     D* Procedure name: COMP_GetInfoCliente
007600110202     D* Purpose:        Restituisce informazioni sul cliente.
007700110202     D* Returns:        Esito.
007800110202     D* Parameter:      priRagioneSociale => Ragione sociale.
007900110202     D* Parameter:      priKscClienteFatturazione => Codice cliente di fatt...
008000110202     D*                          urazione.
008100110202     D*--------------------------------------------------
008200110202     D COMP_GetInfoCliente...
008300110202     D                 PR            10I 0
008400110202     D  priRagioneSociale...
008500110202     D                                     LIKE(andiz00f.descr)
008600110202     D                                     OPTIONS(*OMIT:*VARSIZE)
008700110202     D  priRagioneSocialeSize...
008800110202     D                               10I 0 OPTIONS(*OMIT) CONST
008900110202     D  priKscClienteFatturazione...
009000110202     D                                     LIKE(andiz00f.ksc)
009100110202     D                                     OPTIONS(*OMIT)
009200110202
009300110202     D*--------------------------------------------------
009400110202     D* Procedure name: COMP_GetInfoClienteFatturazione
009500110202     D* Purpose:        Restituisce informazioni sul cliente.
009600110202     D* Returns:        Esito.
009700110202     D* Parameter:      priRagioneSociale => Ragione sociale.
009800110202     D* Parameter:      priKscClienteFatturazione => Codice cliente di fatt...
009900110202     D*                          urazione.
010000110202     D*--------------------------------------------------
010100110202     D COMP_GetInfoClienteFatturazione...
010200110202     D                 PR            10I 0
010300110202     D  priRagioneSociale...
010400110202     D                                     LIKE(andiz00f.descr)
010500110202     D                                     OPTIONS(*OMIT:*VARSIZE)
010600110202     D  priRagioneSocialeSize...
010700110202     D                               10I 0 OPTIONS(*OMIT) CONST
010800110202     D  priKcc...
010900110202     D                                     LIKE(andiz00f.kcc)
011000110202     D                                     OPTIONS(*OMIT)
011100110202
011200110202     D*--------------------------------------------------
011300110202     D* Procedure name: COMP_ChkPartitaContabile
011400110202     D* Purpose:        Controlla l'esistenza della partita contabile.
011500110202     D* Returns:        Esito.
011600110202     D* Parameter:      priSocieta => Società.
011700110202     D* Parameter:      priContabilita => Contabilità.
011800110202     D* Parameter:      priCapoConto => Capo conto.
011900110202     D* Parameter:      priSottoConto => Sotto conto.
012000110202     D* Parameter:      priDataPartita => Data partita.
012100110202     D* Parameter:      priNumeroPartita => Numero partita.
012200110202     D* Parameter:      priSeriePartita => Serie partita.
012300110202     D*--------------------------------------------------
012400110202     D COMP_ChkPartitaContabile...
012500110202     D                 PR            10I 0
012600110202     D  priSocieta                         LIKE(andiz00f.societa)
012700110202     D                                     VALUE
012800110202     D priContabilita                      LIKE(andiz00f.ctb)
012900110202     D                                     VALUE
013000110202     D  priCapoConto                       LIKE(andiz00f.kcc)
013100110202     D                                     CONST
013200110202     D  priSottoConto                      LIKE(andiz00f.ksc)
013300110202     D                                     CONST
013400110202     D priDataPartita                      LIKE(andiz00f.data)
013500110202     D                                     CONST
013600110202     D  priNumeroPartita...
013700110202     D                                     LIKE(andiz00f.nr)
013800110202     D                                     CONST
013900110202     D  priSeriePartita...
014000110202     D                                     LIKE(andiz00f.serieNm)
014100110202     D                                     CONST
014200110202
014300110202     D*--------------------------------------------------
014400110202     D* Procedure name: COMP_GetInfoPartitaContabile
014500110202     D* Purpose:        Restituisce informazioni sulla partita contabile.
014600110202     D* Returns:        Esito.
014700110202     D* Parameter:      priSaldoEuro => Saldo partita in euro.
014800110202     D*--------------------------------------------------
014900110202     D COMP_GetInfoPartitaContabile...
015000110202     D                 PR            10I 0
015100110202     D  priSaldoPartitaEuro...
015200110202     D                               15P 2
015300110202
015301110209     D*--------------------------------------------------
015302110209     D* Procedure name: COMP_NewStampaContrassegniCompensati
015303110209     D* Purpose:        Inizia una nuova stampa di contrassegni compensati.
015304110209     D* Returns:        Esito.
015305110209     D*--------------------------------------------------
015306110209     D COMP_NewStampaContrassegniCompensati...
015307110209     D                 PR            10I 0
015308110209
015309110209     D*--------------------------------------------------
015310110209     D* Procedure name: COMP_EndStampaContrassegniCompensati
015311110209     D* Purpose:        Chiude la stampa dei contrassegni compensati.
015312110209     D* Returns:        Esito.
015313110209     D*--------------------------------------------------
015314110209     D COMP_EndStampaContrassegniCompensati...
015315110209     D                 PR            10I 0
015316110209
015317110208     D*--------------------------------------------------
015318110209     D* Procedure name: COMP_WrtStampaContrassegniCompensati_Riga
015319110209     D* Purpose:        Stampa una riga di contrassegni compensati.
015320110208     D* Returns:        Esito.
015324110208     D*--------------------------------------------------
015325110209     D COMP_WrtStampaContrassegniCompensati_Riga...
015326110209     D                 PR            10I 0
015327110209     D  priAas                             LIKE(cndizion.$aas) CONST
015328110210     D  priMgs                             LIKE(cndizion.$mmg) CONST
015329110210     D  priLnp                             LIKE(cndizion.$lnp) CONST
015330110209     D  priNrs                             LIKE(cndizion.$nrs) CONST
015331110209     D  priNsp                             LIKE(cndizion.$nsp) CONST
015332110209

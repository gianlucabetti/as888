000100090827     ***************************************************************************
000200090827     **
000300090827     **
000400090827     ***************************************************************************
000500090914     H DFTACTGRP(*NO) ACTGRP(*CALLER) BNDDIR('UBBNDDIR')
000600090831
000700090831     ***************************************************************************
000800090831     **
000900090831     ** File.
001000090831     **
001100090831     ***************************************************************************
001200090831
001300090827     ***************************************************************************
001400090827     **
001500090827     ** Costanti.
001600090827     **
001700090827     ***************************************************************************
001800090827     D ESITO_ERROR...
001900090827     D                 C                   -1
002000090827     D ESITO_ERROR_RQS_OPCODE...
002100090827     D                 C                   -2
002200090827     D ESITO_ERROR_IDBOLLA...
002300090827     D                 C                   -11
002400090827     D ESITO_ERROR_KSU...
002500090827     D                 C                   -12
002600090827     D ESITO_ERROR_SUN...
002700090827     D                 C                   -13
002800090914     D ESITO_ERROR_FLGKSCCCM...
002900090914     D                 C                   -14
003000090914     D ESITO_ERROR_GEST_BOLLA...
003100090914     D                 C                   -15
003200090827     D ESITO_OK...
003300090827     D                 C                   0
003400090827     D ESITO_WARNING...
003500090827     D                 C                   1
003600090827
003700090827     ***************************************************************************
003800090827     **
003900090827     ** Prototipi.
004000090827     **
004100090827     ***************************************************************************
004200090914     D/COPY GAITRASRC/SRCPROTOPR,TIS7702R
004300090914     D/COPY GAITRASRC/SRCPROTOPR,TIS7700R
004400090914     D/COPY GAITRASRC/SRCPROTOPR,UBRTVTPO
004500090914     D/COPY GAITRASRC/SRCPROTOPI,UBRTVTPO
004600090914     D/COPY GAITRASRC/SRCPROTOPR,UBAR5GEN
004700090914     D/COPY GAITRASRC/SRCPROTOPI,UBAR5GEN
004800090827
004900090827     ***************************************************************************
005000090827     **
005100090827     ** Strutture dati.
005200090827     **
005300090827     ***************************************************************************
005400090914     D cndizion      E DS                  QUALIFIED
005500090914     D                                     BASED(nullPtr)
005600090914     D tis7700i0     E DS                  QUALIFIED
005700090914     D                                     INZ(*EXTDFT)
005800090914     D tis7700o0     E DS                  QUALIFIED
005900090914     D thisParms       DS                  QUALIFIED
006000090831     D                                     INZ
006100090831     D  aas                                LIKE(cndizion.$aas)
006200090831     D  lnp                                LIKE(cndizion.$lnp)
006300090831     D  nrs                                LIKE(cndizion.$nrs)
006400090831     D  nsp                                LIKE(cndizion.$nsp)
006500090914     D  tbl                                LIKE(cndizion.$tbl)
006600090914     D  ksu                                LIKE(cndizion.$ksu)
006700090914     D  sun                                LIKE(cndizion.$sun)
006800090914     D  flgKscCcm                     1A
006900090901     D*
007000090827
007100090827     ***************************************************************************
007200090827     **
007300090827     ** Campi.
007400090827     **
007500090827     ***************************************************************************
007600090914     D wEsitoSvPgm     S             10I 0 INZ(*zeros)
007700090914     D flg_GestGiac    S              1A   INZ(*blanks)
007800090915     D wCallrpyEsito   S             10I 0 INZ(*zeros)
007900090827
008000090827     ***************************************************************************
008100090827     **
008200090827     ** Parametri.
008300090828     **
008400090827     **
008500090827     ***************************************************************************
008600090914     D Verifica_gestione_giacenza...
008700090827     D                 PI
008800090827     D  rqsOpCode                    10A   CONST
008900090914     D  rqsAas                             CONST LIKE(cndizion.$aas)
009000090914     D                                     OPTIONS(*NOPASS)
009100090914     D  rqsLnp                             CONST LIKE(cndizion.$lnp)
009200090914     D                                     OPTIONS(*NOPASS)
009300090914     D  rqsNrs                             CONST LIKE(cndizion.$nrs)
009400090914     D                                     OPTIONS(*NOPASS)
009500090914     D  rqsNsp                             CONST LIKE(cndizion.$nsp)
009600090914     D                                     OPTIONS(*NOPASS)
009700090914     D  rqsTbl                             CONST LIKE(cndizion.$tbl)
009800090914     D                                     OPTIONS(*NOPASS)
009900090914     D  rqsKsu                             CONST LIKE(cndizion.$ksu)
010000090914     D                                     OPTIONS(*NOPASS)
010100090914     D  rqsSun                             CONST LIKE(cndizion.$sun)
010200090914     D                                     OPTIONS(*NOPASS)
010300090914     D  rqsFlgKscCcm                  1A   CONST
010400090914     D                                     OPTIONS(*NOPASS)
010500090914     D  rpyFlgGiac                    1A   OPTIONS(*NOPASS)
010600090914     D  rpyEsito                     10I 0 OPTIONS(*NOPASS)
010700090831
010800090831     D*--------------------------------------------------
010900090914     D* Procedure name: GetFlgGiac
011000090914     D* Purpose:        Restituisce se x la bolla/ksu corrente è consentito gestire la giacenza
011100090831     D* Returns:
011200090831     D*--------------------------------------------------
011300090914     D GetFlgGiac      PR            10I 0
011400090827
011500090827     ***************************************************************************
011600090827     **
011700090827     ** Main.
011800090827     **
011900090827     ***************************************************************************
012000090827
012100090827      /FREE
012200090827
012300090827       SELECT;
012400090914         WHEN rqsOpCode = 'GETFLGGIAC';
012500090914           GetFlgGiac();
012600090827         WHEN rqsOpCode = 'SETONLR';
012700090914           UBAR5GEN_Close();
012800090914           *INLR = *ON;
012900090827         OTHER;
013000090827           rpyEsito = ESITO_ERROR_RQS_OPCODE;
013100090827       ENDSL;
013200090827
013300090827       RETURN;
013400090827
013500090827      /END-FREE
013600090827
013700090827     P*--------------------------------------------------
013800090914     P* Procedure name: GetFlgGiac
013900090914     P* Purpose:        Restituisce se x la bolla/ksu corrente è consentito gestire la giacenza
014000090827     P* Returns:
014100090827     P*--------------------------------------------------
014200090914     P GetFlgGiac      B
014300090914     D GetFlgGiac      PI            10I 0
014400090831
014500090827
014600090827      /FREE
014700090827
014800090827       CLEAR rpyEsito;
014900090914
015000090914
015100090914       // Ricevo e controllo i parametri di input
015200090914       thisParms.aas       = rqsAas;
015300090914       thisParms.lnp       = rqsLnp;
015400090914       thisParms.nrs       = rqsNrs;
015500090914       thisParms.nsp       = rqsNsp;
015600090914       thisParms.tbl       = rqsTbl;
015700090914       thisParms.ksu       = rqsKsu;
015800090914       thisParms.sun       = rqsSun;
015900090914       thisParms.flgKscCcm = rqsFlgKscCcm;
016000090914
016100090914
016200090914       // ID bolla
016300090914       IF thisParms.aas < 1 OR thisParms.lnp < 1 OR
016400090914          thisParms.nrs < 0 OR thisParms.nsp < 1 OR
016500090914          thisParms.tbl = *BLANKS;
016600090914         rpyEsito = ESITO_ERROR_IDBOLLA;
016700090914         RETURN rpyEsito;
016800090914       ENDIF;
016900090914
017000090914
017100090914       // Cliente unificante obbligatorio
017200090914       IF thisParms.ksu = *BLANK OR thisParms.ksu = *ZERO;
017300090914         rpyEsito = ESITO_ERROR_KSU;
017400090914         RETURN rpyEsito;
017500090914       ENDIF;
017600090914
017700090914
017800090914       // Strategi user number obbligatorio
017900090914       IF thisParms.sun = *BLANK OR thisParms.sun = *ZERO;
018000090914         rpyEsito = ESITO_ERROR_SUN;
018100090914         RETURN rpyEsito;
018200090914       ENDIF;
018300090914
018400090914
018500090914       // Flag "lavoro" x KSC o CCM
018600090914       // Se *blanks lo calcolo
018700090914       IF thisParms.flgKscCcm = *BLANK;
018800090914          RESET tis7700i0;
018900090914          tis7700i0.aas = thisParms.aas;
019000090914          tis7700i0.lnp = thisParms.lnp;
019100090914          tis7700i0.nrs = thisParms.nrs;
019200090914          tis7700i0.nsp = thisParms.nsp;
019300090914          tis7700i0.tbl = thisParms.tbl;
019400090914          tis7700i0.ksu = thisParms.ksu;
019500090914          tis7700i0.sun = thisParms.sun;
019600090914          // Se bolla nn in gestione => esco con errore
019700090914          Selettore_bolla_subUnificante('CHKBOLLA'
019800090915                    : wCallrpyEsito
019900090914                    : 'TIS7700I0' : tis7700i0 : %SIZE(tis7700i0)
020000090914                    : 'TIS7700O0' : tis7700o0 : %SIZE(tis7700o0));
020100090914
020200090915          IF wCallrpyEsito >= *zeros AND tis7700o0.bollValida = *ON;
020300090914             // Se bolla in gestione => utilizzo il flag KSC/CCM reperito
020400090914             thisParms.flgKscCcm = tis7700o0.flg_kscccm;
020500090914
020600090914          ELSE;
020700090914
020800090914             rpyEsito = ESITO_ERROR_GEST_BOLLA;
020900090914             RETURN rpyEsito;
021000090914          ENDIF;
021100090914
021200090914       ELSE;
021300090914
021400090914          IF thisParms.flgKscCcm <> 'C' AND
021500090914             thisParms.flgKscCcm <> 'K' AND
021600090914             thisParms.flgKscCcm <> 'E';
021700090914             rpyEsito = ESITO_ERROR_FLGKSCCCM;
021800090914             RETURN rpyEsito;
021900090914          ENDIF;
022000090914       ENDIF;
022100090827
022200090827
022300090827       // Pulisco i parametri di output.
022400090914       rpyFlgGiac = *BLANKS;
022500090831
022600090901
022700090914       // Eseguo il controllo
022800090914       flg_GestGiac = 'N';
022900090914
023000090914       // Se sto "lavorando" x CCM => allora sempre ok la gestione giacenza
023100090914       IF thisParms.flgKscCcm = 'C' or thisParms.flgKscCcm = 'E';
023200090914          flg_GestGiac = 'S';
023300090914
023400090914       ELSE;
023500090914
023600090914          // Verifico se la bolla è un porto franco o assegnato
023700090914          IF UBRTVTPO_RtvPorto (thisParms.tbl : UBRTVTPO.pOutTpPorto);
023800090914             rpyEsito = ESITO_ERROR;
023900090914             RETURN rpyEsito;
024000090914
024100090914          ELSE;
024200090914
024300090914             // Se franco => allora sempre ok la gestione giacenza
024400090914             IF UBRTVTPO.pOutTpPorto = 'F';
024500090914                flg_GestGiac = 'S';
024600090914
024700090914             ELSE;
024800090914
024900090914                // Se assegnato => verifico se trattasi d "triangolazione"
025000090914                IF UBAR5GEN_Rtv ( thisParms.aas :
025100090914                                  thisParms.lnp :
025200090914                                  thisParms.nrs :
025300090914                                  thisParms.nsp :
025400090915                                  UBAR5GEN.pOutAr5Gen ) < *zeros;
025500090914                   rpyEsito = ESITO_ERROR;
025600090914                   RETURN rpyEsito;
025700090914
025800090914                ELSE;
025900090914
026000090914                   dar5gen = UBAR5GEN.pOutAr5Gen;
026100090914                   // Se trattasi di triangolazione => allora ok la gestione giacenza
026200090914                   IF dar5gen.§AR5ASS = 'T';
026300090914                      flg_GestGiac = 'S';
026400090914                   ENDIF;
026500090914                ENDIF;
026600090914             ENDIF;
026700090914          ENDIF;
026800090914       ENDIF;
026900090914
027000090914
027100090914       // Valorizzo il buffer d output
027200090914       rpyFlgGiac = flg_GestGiac;
027300090914
027400090827
027500090831       RETURN ESITO_OK;
027600090831
027700090827      /END-FREE
027800090914     P GetFlgGiac      E
027900090831

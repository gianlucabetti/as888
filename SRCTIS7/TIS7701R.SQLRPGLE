000100090827     ***************************************************************************
000200090827     **
000300090827     **
000400090827     ***************************************************************************
000500090827     H DFTACTGRP(*NO) ACTGRP(*CALLER)
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
002000090902     D ESITO_ERROR_RQS_OPCODE...
002100090827     D                 C                   -2
002200090902     D ESITO_ERROR_ISV...
002300090827     D                 C                   -11
002400090827     D ESITO_ERROR_KSU...
002500090827     D                 C                   -12
002600090827     D ESITO_ERROR_SUN...
002700090827     D                 C                   -13
002800090827     D ESITO_OK...
002900090827     D                 C                   0
003000090827     D ESITO_WARNING...
003100090827     D                 C                   1
003200090827
003300090827     ***************************************************************************
003400090827     **
003500090827     ** Prototipi.
003600090827     **
003700090827     ***************************************************************************
003800090902     D/COPY GAITRASRC/SRCPROTOPR,TIS7701R
003900090827
004000090827     ***************************************************************************
004100090827     **
004200090827     ** Strutture dati.
004300090827     **
004400090827     ***************************************************************************
004500090902     D tivssds       E DS                  EXTNAME(tivss00f) QUALIFIED
004600090831     D thisParms       DS                  QUALIFIED
004700090831     D                                     INZ
004800090902     D  ksu                                LIKE(tivssds.vssksu)
004900090902     D  isv                                LIKE(tivssds.vssisv)
005000090902     D  sun                                LIKE(tivssds.vsssun)
005100090902     D  data                               LIKE(tivssds.vssdde)
005200090831     D prevParms       DS                  LIKEDS(thisParms)
005300090831     D                                     INZ
005400091123     D prevEsito       S                   LIKE(rpyEsito) INZ
005500090827
005600090827     ***************************************************************************
005700090827     **
005800090827     ** Campi.
005900090827     **
006000090827     ***************************************************************************
006100090827
006200090827     ***************************************************************************
006300090827     **
006400090827     ** Parametri.
006500090828     **
006600090827     **
006700090827     ***************************************************************************
006800090902     D Selettore_record_TIVSS...
006900090827     D                 PI
007000090827     D  rqsOpCode                    10A   CONST
007100090902     D                                     OPTIONS(*NOPASS)
007200090902     D  rqsDeterminis                 1A   CONST
007300090902     D                                     OPTIONS(*NOPASS)
007400090902     D  rqsKsu                             CONST LIKE(tivssds.vssksu)
007500090902     D                                     OPTIONS(*NOPASS)
007600090902     D  rqsIsv                             CONST LIKE(tivssds.vssisv)
007700090902     D                                     OPTIONS(*NOPASS)
007800090902     D  rqsSun                             CONST LIKE(tivssds.vsssun)
007900090902     D                                     OPTIONS(*NOPASS)
008000090902     D  rqsData                            CONST LIKE(tivssds.vssdde)
008100090902     D                                     OPTIONS(*NOPASS)
008200090902     D  rpyTIVSS                           LIKEDS(tivssds)
008300090902     D                                     OPTIONS(*NOPASS)
008400090902     D  rpyEsito                     10I 0 OPTIONS(*NOPASS)
008500090827
008600090827     D*--------------------------------------------------
008700090902     D* Procedure name: GetRecordTIVSS
008800090902     D* Purpose:        Reperisce il record d TIVSS in base ai parametri ricevuti
008900090827     D* Returns:
009000090827     D*--------------------------------------------------
009100090902     D GetRecordTIVSS  PR            10I 0
009200090831
009300090827
009400090827     ***************************************************************************
009500090827     **
009600090827     ** Main.
009700090827     **
009800090827     ***************************************************************************
009900090827
010000090827      /FREE
010100090827
010200090827       SELECT;
010300090902         WHEN rqsOpCode = 'GETRCDVSS';
010400090902           GetRecordTIVSS();
010500090827         WHEN rqsOpCode = 'SETONLR';
010600090827           *INLR = *ON;
010700090827         OTHER;
010800090827           rpyEsito = ESITO_ERROR_RQS_OPCODE;
010900090827       ENDSL;
011000090827
011100090827       RETURN;
011200090827
011300090827      /END-FREE
011400090827
011500090902
011600090902
011700090827     P*--------------------------------------------------
011800090902     P* Procedure name: GetRecordTIVSS
011900090902     P* Purpose:        Reperisce il record d TIVSS in base ai parametri ricevuti
012000090827     P* Returns:
012100090827     P*--------------------------------------------------
012200090902     P GetRecordTIVSS  B
012300090902     D GetRecordTIVSS  PI            10I 0
012400090831
012500090827
012600090827      /FREE
012700090827
012800090902       // Pulisco l'esito della chiamata
012900090827       CLEAR rpyEsito;
013000090827
013100090827
013200090902       // Ricevo e controllo i parametri di input
013300090902       thisParms.ksu  = rqsKsu;
013400090902       thisParms.isv  = rqsIsv;
013500090902       thisParms.sun  = rqsSun;
013600090902       thisParms.data = rqsData;
013700090902
013800090827
013900090902       // Cliente unificante obbligatorio
014000090831       IF thisParms.ksu = *BLANK OR thisParms.ksu = *ZERO;
014100090827         rpyEsito = ESITO_ERROR_KSU;
014200090831         RETURN rpyEsito;
014300090827       ENDIF;
014400090827
014500090902
014600090902       // Strategi user number obbligatorio
014700090902       //IF thisParms.sun = *BLANK OR thisParms.sun = *ZERO;
014800090902       //  rpyEsito = ESITO_ERROR_SUN;
014900090902       //  RETURN rpyEsito;
015000090902       //ENDIF;
015100090902
015200090902
015300090902       // Tipo servizio obbligatorio
015400090902       IF thisParms.isv = *BLANK OR thisParms.isv = *ZERO;
015500090902         rpyEsito = ESITO_ERROR_ISV;
015600090902         RETURN rpyEsito;
015700090902       ENDIF;
015800090902
015900090902
016000090902       // Data di riferimento => se nn passata assumo data corrente
016100090902       IF thisParms.data = *ZERO;
016200090902         thisParms.data = %DEC(%DATE());
016300090902       ENDIF;
016400090831
016500090901
016600090902       // Eseguo il reperimento se sono cambiati i parametri di input
016700090902       IF rqsDeterminis = *ON AND thisParms = prevParms;
016800091123          rpyEsito = prevEsito;
016900091021          EVAL-CORR rpyTIVSS = tivssds;
017000090902       ELSE;
017100090902
017200090902
017300090902         // Pulisco i parametri di output
017400090902         CLEAR tivssds;
017500090902         CLEAR rpyTIVSS;
017600090902
017700090902
017800090902         // Effettuo la ricerca "precisa" con anche SUN => se passato
017900090902         IF thisParms.sun <> *BLANK AND thisParms.sun <> *ZERO;
018000090902           EXEC SQL
018100090902             SELECT TIVSS00F.*
018200090902               INTO :tivssds
018300090902               FROM TIVSS00F
018400090902               WHERE TIVSS00F.VSSKSU = :thisParms.ksu
018500090902                 AND TIVSS00F.VSSISV = :thisParms.isv
018600090902                 AND TIVSS00F.VSSSUN = :thisParms.sun
018700090902                 AND :thisParms.data BETWEEN TIVSS00F.VSSDDE AND TIVSS00F.VSSDSC
018800090902                 AND TIVSS00F.VSSVAT = ''
018900090902               FETCH FIRST 1 ROW ONLY
019000090902           ;
019100090902
019200090902         ELSE;
019300090902
019400090902           // Effettuo la ricerca "nn precisa" senza SUN => se nn passato
019500090902           EXEC SQL
019600090902             SELECT TIVSS00F.*
019700090902               INTO :tivssds
019800090902               FROM TIVSS00F
019900090902               WHERE TIVSS00F.VSSKSU = :thisParms.ksu
020000090902                 AND TIVSS00F.VSSISV = :thisParms.isv
020100090902                 AND :thisParms.data BETWEEN TIVSS00F.VSSDDE AND TIVSS00F.VSSDSC
020200090902                 AND TIVSS00F.VSSVAT = ''
020300090902               FETCH FIRST 1 ROW ONLY
020400090902           ;
020500090902         ENDIF;
020600091123
020700091123
020800091123         prevEsito = rpyEsito;
020900091123         EVAL-CORR prevParms = thisParms;
021000091123
021100090902
021200090902         IF sqlCode < *ZERO;
021300090902           DUMP(A);
021400090902           rpyEsito = ESITO_ERROR;
021500091123           prevEsito = rpyEsito;
021600090902           RETURN sqlCode;
021700090902         ENDIF;
021800090902
021900090902         IF sqlCode = 100;
022000090902           rpyEsito = ESITO_WARNING;
022100091123           prevEsito = rpyEsito;
022200090902           RETURN rpyEsito;
022300090902         ENDIF;
022400090902
022500090902         EVAL-CORR rpyTIVSS = tivssds;
022600090902
022700090901       ENDIF;
022800090831
022900090827
023000090831       RETURN ESITO_OK;
023100090831
023200090902
023300090827      /END-FREE
023400090902     P GetRecordTIVSS  E
023500090831

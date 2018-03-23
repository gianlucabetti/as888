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
001200090831     Ftitas30c  IF   E           K DISK    USROPN
001300090831
001400090827     ***************************************************************************
001500090827     **
001600090827     ** Costanti.
001700090827     **
001800090827     ***************************************************************************
001900090827     D ESITO_ERROR...
002000090827     D                 C                   -1
002100090827     D ESITO_ERROR_RQS_OPCODE...
002200090827     D                 C                   -2
002300090827     D ESITO_ERROR_RQS_FORMAT_NAME...
002400090827     D                 C                   -3
002500090827     D ESITO_ERROR_RPY_FORMAT_NAME...
002600090827     D                 C                   -4
002700090827     D ESITO_ERROR_RQS_DATA_SIZE...
002800090827     D                 C                   -5
002900090827     D ESITO_ERROR_RPY_DATA_SIZE...
003000090827     D                 C                   -6
003100090827     D ESITO_ERROR_IDBOLLA...
003200090827     D                 C                   -11
003300090827     D ESITO_ERROR_KSU...
003400090827     D                 C                   -12
003500090827     D ESITO_ERROR_SUN...
003600090827     D                 C                   -13
003700090827     D ESITO_OK...
003800090827     D                 C                   0
003900090827     D ESITO_WARNING...
004000090827     D                 C                   1
004100090903     D ESITO_WARNING_NO_SUBK...
004200090903     D                 C                   2
004300090827
004400090827     ***************************************************************************
004500090827     **
004600090827     ** Prototipi.
004700090827     **
004800090827     ***************************************************************************
004900090827     D/COPY GAITRASRC/SRCPROTOPR,TIS7700R
005000090902     D/COPY GAITRASRC/SRCPROTOPR,TIS7701R
005100090831     D/COPY GAITRASRC/SRCPROTOPR,TIBS10R1
005200090901     D/COPY GAITRASRC/SRCPROTOPI,TIBS10R1
005300090827
005400090827     ***************************************************************************
005500090827     **
005600090827     ** Strutture dati.
005700090827     **
005800090827     ***************************************************************************
005900090827     D cndizion      E DS                  QUALIFIED
006000090827     D                                     BASED(nullPtr)
006100090827     D dVssDta       E DS                  QUALIFIED
006200090902     D tivssds       E DS                  EXTNAME(tivss00f) QUALIFIED
006300090827     D tis7700i0     E DS                  QUALIFIED
006400090831     D                                     INZ(*EXTDFT)
006500090827     D tis7700o0     E DS                  QUALIFIED
006600090831     D                                     INZ(*EXTDFT)
006700090831     D tis7700i1     E DS                  QUALIFIED
006800090831     D                                     INZ(*EXTDFT)
006900090831     D tis7700o1     E DS                  QUALIFIED
007000090831     D                                     INZ(*EXTDFT)
007100090831     D thisParms       DS                  QUALIFIED
007200090831     D                                     INZ
007300090831     D  aas                                LIKE(cndizion.$aas)
007400090831     D  lnp                                LIKE(cndizion.$lnp)
007500090831     D  nrs                                LIKE(cndizion.$nrs)
007600090831     D  nsp                                LIKE(cndizion.$nsp)
007700090831     D  tbl                                LIKE(cndizion.$tbl)
007800090831     D  ksu                           8A
007900090831     D  isv                           2A
008000090831     D  sun                           9A
008100090831     D prevParms       DS                  LIKEDS(thisParms)
008200090831     D                                     INZ
008300090901     D*
008400090901     D p               S             10I 0
008500090901     D n11             S             11  0
008600090827
008700090827     ***************************************************************************
008800090827     **
008900090827     ** Campi.
009000090827     **
009100090827     ***************************************************************************
009200090827     D bollaValida     S               N   INZ(*ON)
009300090903     D subkbolla       S                   LIKE(tis7700o0.subkbolla)
009400090903     D subksun         S                   LIKE(tis7700o0.subksun)
009500090903     D flg_kscccm      S                   LIKE(tis7700o0.flg_kscccm)
009600090827
009700090827     ***************************************************************************
009800090827     **
009900090827     ** Parametri.
010000090828     **
010100090828     ** Il parametro RQSDATA deve essere lungo quanto la lunghezza massima delle
010200090828     ** strutture dati TIS7700I*.
010300090828     **
010400090828     ** Il parametro RPYDATA deve essere lungo quanto la lunghezza massima delle
010500090828     ** strutture dati TIS7700O*.
010600090827     **
010700090827     ***************************************************************************
010800090827     D Selettore_bolla_subUnificante...
010900090827     D                 PI
011000090827     D  rqsOpCode                    10A   CONST
011100090827     D  rpyEsito                     10I 0 OPTIONS(*NOPASS)
011200090827     D  rqsFormatName                10A   CONST
011300090827     D                                     OPTIONS(*NOPASS)
011400090828     D  rqsData                      38A   OPTIONS(*NOPASS:*VARSIZE)
011500090827     D  rqsDataSize                  10I 0 CONST
011600090827     D                                     OPTIONS(*NOPASS)
011700090827     D  rpyFormatName                10A   CONST
011800090827     D                                     OPTIONS(*NOPASS)
011900090901     D  rpyData                      72A   OPTIONS(*NOPASS:*VARSIZE)
012000090827     D  rpyDataSize                  10I 0 CONST
012100090827     D                                     OPTIONS(*NOPASS)
012200090827
012300090827     D*--------------------------------------------------
012400090831     D* Procedure name: ChkBolla
012500090827     D* Purpose:        Controlla se la bolla appartiene al subunificante.
012600090827     D* Returns:
012700090827     D*--------------------------------------------------
012800090831     D ChkBolla        PR            10I 0
012900090831
013000090831     D*--------------------------------------------------
013100090831     D* Procedure name: GetSubKSun
013200090831     D* Purpose:        Restituisce il subunificante legato al SUN.
013300090831     D* Returns:
013400090831     D*--------------------------------------------------
013500090831     D GetSubKSun      PR            10I 0
013600090827
013700090827     ***************************************************************************
013800090827     **
013900090827     ** Main.
014000090827     **
014100090827     ***************************************************************************
014200090827
014300090827      /FREE
014400090827
014500090827       SELECT;
014600090827         WHEN rqsOpCode = 'CHKBOLLA';
014700090831           ChkBolla();
014800090831         WHEN rqsOpCode = 'GETSUBKSUN';
014900090831           GetSubKSun();
015000090827         WHEN rqsOpCode = 'SETONLR';
015100090827           *INLR = *ON;
015200090827         OTHER;
015300090827           rpyEsito = ESITO_ERROR_RQS_OPCODE;
015400090827       ENDSL;
015500090827
015600090827       RETURN;
015700090827
015800090827      /END-FREE
015900090827
016000090827     P*--------------------------------------------------
016100090831     P* Procedure name: ChkBolla
016200090827     P* Purpose:        Controlla se la bolla appartiene al subunificante.
016300090827     P* Returns:
016400090827     P*--------------------------------------------------
016500090831     P ChkBolla        B
016600090831     D ChkBolla        PI            10I 0
016700090831
016800090827
016900090827      /FREE
017000090827
017100090827       CLEAR rpyEsito;
017200090827
017300090827       IF rqsDataSize < 1;
017400090827         rpyEsito = ESITO_ERROR_RQS_DATA_SIZE;
017500090831         RETURN rpyEsito;
017600090827       ENDIF;
017700090827
017800090827       IF rpyDataSize < 1;
017900090827         rpyEsito = ESITO_ERROR_RPY_DATA_SIZE;
018000090831         RETURN rpyEsito;
018100090827       ENDIF;
018200090827
018300090827       // Pulisco i parametri di output.
018400090827
018500090827       %SUBST(rpyData : 1 : rpyDataSize) = *BLANK;
018600090827
018700090827       SELECT;
018800090828         WHEN rpyFormatName = 'TIS7700O0';
018900090827           RESET tis7700o0;
019000090827         OTHER;
019100090827           rpyEsito = ESITO_ERROR_RPY_FORMAT_NAME;
019200090831           RETURN rpyEsito;
019300090827       ENDSL;
019400090827
019500090827       // Ricevo e controllo i parametri di input.
019600090827
019700090827       SELECT;
019800090827         WHEN rqsFormatName = 'TIS7700I0';
019900090827           tis7700i0 = %SUBST(rqsData : 1 : rqsDataSize);
020000090831           IF tis7700i0.isv = *BLANK;
020100090831             RESET tis7700i0.isv;
020200090831           ENDIF;
020300090831           thisParms.aas = tis7700i0.aas;
020400090831           thisParms.lnp = tis7700i0.lnp;
020500090831           thisParms.nrs = tis7700i0.nrs;
020600090831           thisParms.nsp = tis7700i0.nsp;
020700090831           thisParms.tbl = tis7700i0.tbl;
020800090831           thisParms.ksu = tis7700i0.ksu;
020900090831           thisParms.isv = tis7700i0.isv;
021000090831           thisParms.sun = tis7700i0.sun;
021100090827         OTHER;
021200090827           rpyEsito = ESITO_ERROR_RQS_FORMAT_NAME;
021300090831           RETURN rpyEsito;
021400090827       ENDSL;
021500090827
021600090827       // ID bolla obbligatorio.
021700090827
021800090831       IF thisParms.aas < 1 OR thisParms.lnp < 1
021900090831       OR thisParms.nrs < 0 OR thisParms.nsp < 1;
022000090827         rpyEsito = ESITO_ERROR_IDBOLLA;
022100090831         RETURN rpyEsito;
022200090827       ENDIF;
022300090827
022400090827       // Codice cliente unificante obbligatorio.
022500090901
022600090831       IF thisParms.ksu = *BLANK OR thisParms.ksu = *ZERO;
022700090827         rpyEsito = ESITO_ERROR_KSU;
022800090831         RETURN rpyEsito;
022900090827       ENDIF;
023000090831
023100090901
023200090831       // Eseguo il controllo solo se sono cambiati i parametri di input.
023300090901
023400090901       IF tis7700i0.determinis <> *ON OR thisParms.ksu <> prevParms.ksu;
023500090903
023600090901          // Sviluppo la "familia" del unificante ricevuto in input
023700091020          CLEAR tibs10r1ds;
023800090901          tibs10r1ds.tibs10ds.d10paf = 'F';  //Reperisco tutta la familia
023900090901          tibs10r1ds.tibs10ds.d10cod = %dec(tis7700i0.ksu : 11 : 0);
024000091021          tibs10r1ds.d10isun = tis7700i0.sun;
024100091202          tibs10r1ds.d10igrptle = 'W';
024200091021          tibs10r1(tibs10r1ds);
024300090901
024400090901          IF tibs10r1ds.tibs10ds.d10err = *blanks;
024500091020             p = %lookup (*zeros : tibs10r1ds.d10skcof);
024600090901             IF p = 0;
024700091020                p = %elem(tibs10r1ds.d10skcof);
024800090901             ELSE;
024900090901                p = p - 1;
025000090901             ENDIF;
025100090901          ELSE;
025200091020             tibs10r1ds.d10skcof(1) = tibs10r1ds.tibs10ds.d10cod;
025300090901             p = 1;
025400090901          ENDIF;
025500090903
025600090901       ENDIF;
025700090901
025800090903
025900090901
026000090831       IF tis7700i0.determinis <> *ON OR thisParms <> prevParms;
026100090903
026200090903         // Imposto a ok il flag "bolla valida"
026300090903         RESET bollaValida;
026400090827
026500090831         IF NOT %OPEN(titas30c);
026600090831           OPEN titas30c;
026700090831         ENDIF;
026800090831
026900090831         IF thisParms.tbl <> *BLANK;
027000090831           CHAIN (thisParms.aas : thisParms.lnp : thisParms.nrs : thisParms.nsp
027100090831                 : thisParms.tbl) titas30c;
027200090831         ELSE;
027300090831           CHAIN (thisParms.aas : thisParms.lnp : thisParms.nrs : thisParms.nsp
027400090831                 ) titas30c;
027500090831         ENDIF;
027600090831
027700090831
027800090831         IF %found(titas30c);
027900090901
028000090903            flg_kscccm = *blanks;
028100090901            // Verifico appartenza bolla x KSC
028200090901            IF bollaValida;
028300090901               n11 = tasKSC;
028400091020               IF %lookup (n11 : tibs10r1ds.d10skcof : 1 : p) > 0;
028500090903                  flg_kscccm = 'K';
028600090903               ENDIF;
028700090903
028800090903               // Verifico appartenza bolla x CCM
028900090903               n11 = tasCCM;
029000091020               IF %lookup (n11 : tibs10r1ds.d10skcof : 1 : p) > 0;
029100090903                  IF flg_kscccm <> *blanks;
029200090903                     flg_kscccm = 'E';
029300090903                  ELSE;
029400090903                     flg_kscccm = 'C';
029500090903                  ENDIF;
029600090903               ENDIF;
029700090903               IF flg_kscccm = *blanks;
029800090903                  bollaValida = *off;
029900090903               ENDIF;
030000090901            ENDIF;
030100090903
030200090903
030300090903            // Strategi user number obbligatorio solo x reperimento SUBK
030400090903
030500090903            IF thisParms.sun = *BLANK OR thisParms.sun = *ZERO;
030600090903              rpyEsito = ESITO_WARNING_NO_SUBK;
030700090903            ELSE;
030800090903              IF GetSubKSun() < ESITO_OK;
030900090903                 RETURN rpyEsito;
031000090903              ELSE;
031100090903                 // TEMPORANEO:
031200091201 xxx     //         subkbolla = %editc(tasNRS:'X');
031300091201  "      //         IF subksun <> *blanks;
031400091201  "      //            // Reperimento SUBK bolla
031500091201  "      //            IF subksun <> subkbolla;
031600091201  "      //               bollaValida = *off;
031700091201  "      //            ENDIF;
031800091201 xxx     //         ENDIF;
031900090903              ENDIF;
032000090903            ENDIF;
032100090831
032200090831         ENDIF;
032300090903
032400090827
032500090902         // Salvo i parametri correnti
032600090902         prevParms.aas = thisParms.aas;
032700090902         prevParms.lnp = thisParms.lnp;
032800090902         prevParms.nrs = thisParms.nrs;
032900090902         prevParms.nsp = thisParms.nsp;
033000090902         prevParms.tbl = thisParms.tbl;
033100090902
033200090831
033300090831       ENDIF;
033400090831
033500090827       SELECT;
033600090828         WHEN rpyFormatName = 'TIS7700O0';
033700090827           tis7700o0.bollValida = bollaValida;
033800090903           tis7700o0.subkbolla  = subkbolla;
033900090903           tis7700o0.subksun    = subksun;
034000090903           tis7700o0.flg_kscccm = flg_kscccm;
034100090827           %SUBST(rpyData : 1 : rpyDataSize) = tis7700o0;
034200090827       ENDSL;
034300090827
034400090831       RETURN ESITO_OK;
034500090831
034600090827      /END-FREE
034700090831     P ChkBolla        E
034800090827
034900090831
035000090831     P*--------------------------------------------------
035100090831     P* Procedure name: GetSubKSun
035200090831     P* Purpose:        Restituisce il subunificante legato al SUN.
035300090831     P* Returns:
035400090831     P*--------------------------------------------------
035500090831     P GetSubKSun      B
035600090831     D GetSubKSun      PI            10I 0
035700090831
035800090831     D deterministic   S               N
035900090831
036000090831      /FREE
036100090831
036200090831       CLEAR rpyEsito;
036300090831
036400090831       IF rqsDataSize < 1;
036500090831         rpyEsito = ESITO_ERROR_RQS_DATA_SIZE;
036600090831         RETURN rpyEsito;
036700090831       ENDIF;
036800090831
036900090831       IF rpyDataSize < 1;
037000090831         rpyEsito = ESITO_ERROR_RPY_DATA_SIZE;
037100090831         RETURN rpyEsito;
037200090831       ENDIF;
037300090831
037400090831       // Pulisco i parametri di output.
037500090831
037600090831       %SUBST(rpyData : 1 : rpyDataSize) = *BLANK;
037700090831
037800090831       SELECT;
037900090831         WHEN rpyFormatName = 'TIS7700O0';
038000090831         WHEN rpyFormatName = 'TIS7700O1';
038100090831           RESET tis7700o1;
038200090831         OTHER;
038300090831           rpyEsito = ESITO_ERROR_RPY_FORMAT_NAME;
038400090831           RETURN rpyEsito;
038500090831       ENDSL;
038600090831
038700090831       // Ricevo i parametri di input.
038800090831
038900090831       SELECT;
039000090831         WHEN rqsFormatName = 'TIS7700I0';
039100090831           tis7700i0 = %SUBST(rqsData : 1 : rqsDataSize);
039200090831           deterministic = tis7700i0.determinis = *on;
039300090831           IF tis7700i0.isv = *BLANK;
039400090831             RESET tis7700i0.isv;
039500090831           ENDIF;
039600090831           thisParms.ksu = tis7700i0.ksu;
039700090831           thisParms.isv = tis7700i0.isv;
039800090831           thisParms.sun = tis7700i0.sun;
039900090831
040000090831         WHEN rqsFormatName = 'TIS7700I1';
040100090831           tis7700i1 = %SUBST(rqsData : 1 : rqsDataSize);
040200090831           deterministic = tis7700i1.determinis = *on;
040300090831           IF tis7700i1.isv = *BLANK;
040400090831             RESET tis7700i1.isv;
040500090831           ENDIF;
040600090831           thisParms.ksu = tis7700i1.ksu;
040700090831           thisParms.isv = tis7700i1.isv;
040800090831           thisParms.sun = tis7700i1.sun;
040900090831
041000090831         OTHER;
041100090831           rpyEsito = ESITO_ERROR_RQS_FORMAT_NAME;
041200090831           RETURN rpyEsito;
041300090831       ENDSL;
041400090831
041500090831       // Codice cliente unificante obbligatorio.
041600090831
041700090831       IF thisParms.ksu = *BLANK OR thisParms.ksu = *ZERO;
041800090831         rpyEsito = ESITO_ERROR_KSU;
041900090831         RETURN rpyEsito;
042000090831       ENDIF;
042100090831
042200090831       // Strategi user number obbligatorio.
042300090831
042400090831       IF thisParms.sun = *BLANK OR thisParms.sun = *ZERO;
042500090831         rpyEsito = ESITO_ERROR_SUN;
042600090831         RETURN rpyEsito;
042700090831       ENDIF;
042800090831
042900090831       IF deterministic <> *ON OR thisParms.sun <> prevParms.sun
043000090831       OR thisParms.ksu <> prevParms.ksu OR thisParms.isv <> prevParms.isv;
043100090831
043200090831         RESET dVssDta;
043300090902         RESET tivssds;
043400090902         Selettore_record_TIVSS('GETRCDVSS' : '1' : thisParms.ksu :
043500090902               thisParms.isv : thisParms.sun : 0 : tivssds : rpyEsito);
043600090902
043700090902         IF rpyEsito = *ZERO;
043800090902            dVssDta = tivssds.vssdta;
043900090902         ELSE;
044000090902            RETURN rpyEsito;
044100090902         ENDIF;
044200090831
044300090902         // Salvo i parametri correnti
044400090902         prevParms.ksu = thisParms.ksu;
044500090902         prevParms.isv = thisParms.isv;
044600090902         prevParms.sun = thisParms.sun;
044700090831
044800090831       ENDIF;
044900090831
045000090903
045100090903       subksun = dVssDta.§VssSubK;
045200090903
045300090831       SELECT;
045400090831         WHEN rpyFormatName = 'TIS7700O1';
045500090903           tis7700o1.subksun = subksun;
045600090831           %SUBST(rpyData : 1 : rpyDataSize) = tis7700o1;
045700090831       ENDSL;
045800090831
045900090831       RETURN ESITO_OK;
046000090831
046100090831      /END-FREE
046200090831     P GetSubKSun      E
046300090831

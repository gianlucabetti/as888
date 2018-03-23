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
001200170822     Ffnblp01l  IF   E           K DISK    USROPN EXTDESC('FILTRAPRD/FNBLP01L')
001300170822     Ftitas30c  IF   E           K DISK    USROPN
001400090831
001500090827     ***************************************************************************
001600090827     **
001700090827     ** Costanti.
001800090827     **
001900090827     ***************************************************************************
002000090827     D ESITO_ERROR...
002100090827     D                 C                   -1
002200090827     D ESITO_ERROR_RQS_OPCODE...
002300090827     D                 C                   -2
002400090827     D ESITO_ERROR_RQS_FORMAT_NAME...
002500090827     D                 C                   -3
002600090827     D ESITO_ERROR_RPY_FORMAT_NAME...
002700090827     D                 C                   -4
002800090827     D ESITO_ERROR_RQS_DATA_SIZE...
002900090827     D                 C                   -5
003000090827     D ESITO_ERROR_RPY_DATA_SIZE...
003100090827     D                 C                   -6
003200090827     D ESITO_ERROR_IDBOLLA...
003300090827     D                 C                   -11
003400090827     D ESITO_ERROR_KSU...
003500090827     D                 C                   -12
003600090827     D ESITO_ERROR_SUN...
003700090827     D                 C                   -13
003800090827     D ESITO_OK...
003900090827     D                 C                   0
004000090827     D ESITO_WARNING...
004100090827     D                 C                   1
004200090903     D ESITO_WARNING_NO_SUBK...
004300090903     D                 C                   2
004400090827
004500090827     ***************************************************************************
004600090827     **
004700090827     ** Prototipi.
004800090827     **
004900090827     ***************************************************************************
005000090827     D/COPY GAITRASRC/SRCPROTOPR,TIS7700R
005100090902     D/COPY GAITRASRC/SRCPROTOPR,TIS7701R
005200090831     D/COPY GAITRASRC/SRCPROTOPR,TIBS10R1
005300090901     D/COPY GAITRASRC/SRCPROTOPI,TIBS10R1
005400090827
005500090827     ***************************************************************************
005600090827     **
005700090827     ** Strutture dati.
005800090827     **
005900090827     ***************************************************************************
006000090827     D cndizion      E DS                  QUALIFIED
006100090827     D                                     BASED(nullPtr)
006200090827     D dVssDta       E DS                  QUALIFIED
006300090902     D tivssds       E DS                  EXTNAME(tivss00f) QUALIFIED
006400090827     D tis7700i0     E DS                  QUALIFIED
006500090831     D                                     INZ(*EXTDFT)
006600090827     D tis7700o0     E DS                  QUALIFIED
006700090831     D                                     INZ(*EXTDFT)
006800090831     D tis7700i1     E DS                  QUALIFIED
006900090831     D                                     INZ(*EXTDFT)
007000090831     D tis7700o1     E DS                  QUALIFIED
007100090831     D                                     INZ(*EXTDFT)
007200090831     D thisParms       DS                  QUALIFIED
007300090831     D                                     INZ
007400090831     D  aas                                LIKE(cndizion.$aas)
007500090831     D  lnp                                LIKE(cndizion.$lnp)
007600090831     D  nrs                                LIKE(cndizion.$nrs)
007700090831     D  nsp                                LIKE(cndizion.$nsp)
007800090831     D  tbl                                LIKE(cndizion.$tbl)
007900090831     D  ksu                           8A
008000090831     D  isv                           2A
008100090831     D  sun                           9A
008200090831     D prevParms       DS                  LIKEDS(thisParms)
008300090831     D                                     INZ
008400090901     D*
008500090901     D p               S             10I 0
008600090901     D n11             S             11  0
008700090827
008800090827     ***************************************************************************
008900090827     **
009000090827     ** Campi.
009100090827     **
009200090827     ***************************************************************************
009300090827     D bollaValida     S               N   INZ(*ON)
009400170822     D foundBLP        S               N   INZ(*OFF)
009500170822     D foundTAS        S               N   INZ(*OFF)
009600090903     D subkbolla       S                   LIKE(tis7700o0.subkbolla)
009700090903     D subksun         S                   LIKE(tis7700o0.subksun)
009800090903     D flg_kscccm      S                   LIKE(tis7700o0.flg_kscccm)
009900090827
010000090827     ***************************************************************************
010100090827     **
010200090827     ** Parametri.
010300090828     **
010400090828     ** Il parametro RQSDATA deve essere lungo quanto la lunghezza massima delle
010500090828     ** strutture dati TIS7700I*.
010600090828     **
010700090828     ** Il parametro RPYDATA deve essere lungo quanto la lunghezza massima delle
010800090828     ** strutture dati TIS7700O*.
010900090827     **
011000090827     ***************************************************************************
011100090827     D Selettore_bolla_subUnificante...
011200090827     D                 PI
011300090827     D  rqsOpCode                    10A   CONST
011400090827     D  rpyEsito                     10I 0 OPTIONS(*NOPASS)
011500090827     D  rqsFormatName                10A   CONST
011600090827     D                                     OPTIONS(*NOPASS)
011700090828     D  rqsData                      38A   OPTIONS(*NOPASS:*VARSIZE)
011800090827     D  rqsDataSize                  10I 0 CONST
011900090827     D                                     OPTIONS(*NOPASS)
012000090827     D  rpyFormatName                10A   CONST
012100090827     D                                     OPTIONS(*NOPASS)
012200090901     D  rpyData                      72A   OPTIONS(*NOPASS:*VARSIZE)
012300090827     D  rpyDataSize                  10I 0 CONST
012400090827     D                                     OPTIONS(*NOPASS)
012500090827
012600090827     D*--------------------------------------------------
012700090831     D* Procedure name: ChkBolla
012800090827     D* Purpose:        Controlla se la bolla appartiene al subunificante.
012900090827     D* Returns:
013000090827     D*--------------------------------------------------
013100090831     D ChkBolla        PR            10I 0
013200090831
013300090831     D*--------------------------------------------------
013400090831     D* Procedure name: GetSubKSun
013500090831     D* Purpose:        Restituisce il subunificante legato al SUN.
013600090831     D* Returns:
013700090831     D*--------------------------------------------------
013800090831     D GetSubKSun      PR            10I 0
013900090827
014000090827     ***************************************************************************
014100090827     **
014200090827     ** Main.
014300090827     **
014400090827     ***************************************************************************
014500090827
014600090827      /FREE
014700090827
014800090827       SELECT;
014900090827         WHEN rqsOpCode = 'CHKBOLLA';
015000090831           ChkBolla();
015100090831         WHEN rqsOpCode = 'GETSUBKSUN';
015200090831           GetSubKSun();
015300090827         WHEN rqsOpCode = 'SETONLR';
015400090827           *INLR = *ON;
015500090827         OTHER;
015600090827           rpyEsito = ESITO_ERROR_RQS_OPCODE;
015700090827       ENDSL;
015800090827
015900090827       RETURN;
016000090827
016100090827      /END-FREE
016200090827
016300090827     P*--------------------------------------------------
016400090831     P* Procedure name: ChkBolla
016500090827     P* Purpose:        Controlla se la bolla appartiene al subunificante.
016600090827     P* Returns:
016700090827     P*--------------------------------------------------
016800090831     P ChkBolla        B
016900090831     D ChkBolla        PI            10I 0
017000090831
017100090827
017200090827      /FREE
017300090827
017400090827       CLEAR rpyEsito;
017500090827
017600090827       IF rqsDataSize < 1;
017700090827         rpyEsito = ESITO_ERROR_RQS_DATA_SIZE;
017800090831         RETURN rpyEsito;
017900090827       ENDIF;
018000090827
018100090827       IF rpyDataSize < 1;
018200090827         rpyEsito = ESITO_ERROR_RPY_DATA_SIZE;
018300090831         RETURN rpyEsito;
018400090827       ENDIF;
018500090827
018600090827       // Pulisco i parametri di output.
018700090827
018800090827       %SUBST(rpyData : 1 : rpyDataSize) = *BLANK;
018900090827
019000090827       SELECT;
019100090828         WHEN rpyFormatName = 'TIS7700O0';
019200090827           RESET tis7700o0;
019300090827         OTHER;
019400090827           rpyEsito = ESITO_ERROR_RPY_FORMAT_NAME;
019500090831           RETURN rpyEsito;
019600090827       ENDSL;
019700090827
019800090827       // Ricevo e controllo i parametri di input.
019900090827
020000090827       SELECT;
020100090827         WHEN rqsFormatName = 'TIS7700I0';
020200090827           tis7700i0 = %SUBST(rqsData : 1 : rqsDataSize);
020300090831           IF tis7700i0.isv = *BLANK;
020400090831             RESET tis7700i0.isv;
020500090831           ENDIF;
020600090831           thisParms.aas = tis7700i0.aas;
020700090831           thisParms.lnp = tis7700i0.lnp;
020800090831           thisParms.nrs = tis7700i0.nrs;
020900090831           thisParms.nsp = tis7700i0.nsp;
021000090831           thisParms.tbl = tis7700i0.tbl;
021100090831           thisParms.ksu = tis7700i0.ksu;
021200090831           thisParms.isv = tis7700i0.isv;
021300090831           thisParms.sun = tis7700i0.sun;
021400090827         OTHER;
021500090827           rpyEsito = ESITO_ERROR_RQS_FORMAT_NAME;
021600090831           RETURN rpyEsito;
021700090827       ENDSL;
021800090827
021900090827       // ID bolla obbligatorio.
022000090827
022100090831       IF thisParms.aas < 1 OR thisParms.lnp < 1
022200090831       OR thisParms.nrs < 0 OR thisParms.nsp < 1;
022300090827         rpyEsito = ESITO_ERROR_IDBOLLA;
022400090831         RETURN rpyEsito;
022500090827       ENDIF;
022600090827
022700090827       // Codice cliente unificante obbligatorio.
022800090901
022900090831       IF thisParms.ksu = *BLANK OR thisParms.ksu = *ZERO;
023000090827         rpyEsito = ESITO_ERROR_KSU;
023100090831         RETURN rpyEsito;
023200090827       ENDIF;
023300090831
023400090901
023500090831       // Eseguo il controllo solo se sono cambiati i parametri di input.
023600090901
023700090901       IF tis7700i0.determinis <> *ON OR thisParms.ksu <> prevParms.ksu;
023800090903
023900090901          // Sviluppo la "familia" del unificante ricevuto in input
024000091020          CLEAR tibs10r1ds;
024100090901          tibs10r1ds.tibs10ds.d10paf = 'F';  //Reperisco tutta la familia
024200090901          tibs10r1ds.tibs10ds.d10cod = %dec(tis7700i0.ksu : 11 : 0);
024300091021          tibs10r1ds.d10isun = tis7700i0.sun;
024400091202          tibs10r1ds.d10igrptle = 'W';
024500091021          tibs10r1(tibs10r1ds);
024600090901
024700090901          IF tibs10r1ds.tibs10ds.d10err = *blanks;
024800091020             p = %lookup (*zeros : tibs10r1ds.d10skcof);
024900090901             IF p = 0;
025000091020                p = %elem(tibs10r1ds.d10skcof);
025100090901             ELSE;
025200090901                p = p - 1;
025300090901             ENDIF;
025400090901          ELSE;
025500091020             tibs10r1ds.d10skcof(1) = tibs10r1ds.tibs10ds.d10cod;
025600090901             p = 1;
025700090901          ENDIF;
025800090903
025900090901       ENDIF;
026000090901
026100090903
026200090901
026300090831       IF tis7700i0.determinis <> *ON OR thisParms <> prevParms;
026400090903
026500090903         // Imposto a ok il flag "bolla valida"
026600090903         RESET bollaValida;
026700170822         RESET foundBLP;
026800170822         RESET foundTAS;
026900090827
027000170822         IF NOT %OPEN(fnblp01l);
027100170822           OPEN fnblp01l;
027200170822         ENDIF;
027300090831
027400170822         CHAIN (thisParms.aas : thisParms.lnp : thisParms.nrs : thisParms.nsp
027500170822               ) fnblp01l;
027600170822
027700170822         IF %found(fnblp01l);
027800170822
027900170822            foundBLP = *ON;
028000170822
028100170822            // Se trovata bolla in partenza "overrido" i campi di TAS con quelli da BLP
028200170822            tasKSC = blpKSC;
028300170822            tasCCM = blpCCM;
028400170822            tasNRS = blpNRS;
028500170822
028600170822         ELSE;
028700170822
028800170822           IF NOT %OPEN(titas30c);
028900170822             OPEN titas30c;
029000170822           ENDIF;
029100170822
029200170822           IF thisParms.tbl <> *BLANK;
029300170822            CHAIN (thisParms.aas : thisParms.lnp : thisParms.nrs : thisParms.nsp
029400170822                   : thisParms.tbl) titas30c;
029500170822           ELSE;
029600170822            CHAIN (thisParms.aas : thisParms.lnp : thisParms.nrs : thisParms.nsp
029700170822                   ) titas30c;
029800170822           ENDIF;
029900170822
030000170822           IF %found(titas30c);
030100170822              foundTAS = *ON;
030200170822           ENDIF;
030300170822
030400170822         ENDIF;
030500170822
030600090831
030700170822         IF foundTAS or foundBLP;
030800090901
030900090903            flg_kscccm = *blanks;
031000090901            // Verifico appartenza bolla x KSC
031100090901            IF bollaValida;
031200090901               n11 = tasKSC;
031300091020               IF %lookup (n11 : tibs10r1ds.d10skcof : 1 : p) > 0;
031400090903                  flg_kscccm = 'K';
031500090903               ENDIF;
031600090903
031700090903               // Verifico appartenza bolla x CCM
031800090903               n11 = tasCCM;
031900091020               IF %lookup (n11 : tibs10r1ds.d10skcof : 1 : p) > 0;
032000090903                  IF flg_kscccm <> *blanks;
032100090903                     flg_kscccm = 'E';
032200090903                  ELSE;
032300090903                     flg_kscccm = 'C';
032400090903                  ENDIF;
032500090903               ENDIF;
032600090903               IF flg_kscccm = *blanks;
032700090903                  bollaValida = *off;
032800090903               ENDIF;
032900090901            ENDIF;
033000090903
033100090903
033200090903            // Strategi user number obbligatorio solo x reperimento SUBK
033300090903
033400090903            IF thisParms.sun = *BLANK OR thisParms.sun = *ZERO;
033500090903              rpyEsito = ESITO_WARNING_NO_SUBK;
033600090903            ELSE;
033700090903              IF GetSubKSun() < ESITO_OK;
033800090903                 RETURN rpyEsito;
033900090903              ELSE;
034000090903                 // TEMPORANEO:
034100091201 xxx     //         subkbolla = %editc(tasNRS:'X');
034200091201  "      //         IF subksun <> *blanks;
034300091201  "      //            // Reperimento SUBK bolla
034400091201  "      //            IF subksun <> subkbolla;
034500091201  "      //               bollaValida = *off;
034600091201  "      //            ENDIF;
034700091201 xxx     //         ENDIF;
034800090903              ENDIF;
034900090903            ENDIF;
035000090831
035100090831         ENDIF;
035200090903
035300090827
035400090902         // Salvo i parametri correnti
035500090902         prevParms.aas = thisParms.aas;
035600090902         prevParms.lnp = thisParms.lnp;
035700090902         prevParms.nrs = thisParms.nrs;
035800090902         prevParms.nsp = thisParms.nsp;
035900090902         prevParms.tbl = thisParms.tbl;
036000090902
036100090831
036200090831       ENDIF;
036300090831
036400090827       SELECT;
036500090828         WHEN rpyFormatName = 'TIS7700O0';
036600090827           tis7700o0.bollValida = bollaValida;
036700090903           tis7700o0.subkbolla  = subkbolla;
036800090903           tis7700o0.subksun    = subksun;
036900090903           tis7700o0.flg_kscccm = flg_kscccm;
037000090827           %SUBST(rpyData : 1 : rpyDataSize) = tis7700o0;
037100090827       ENDSL;
037200090827
037300090831       RETURN ESITO_OK;
037400090831
037500090827      /END-FREE
037600090831     P ChkBolla        E
037700090827
037800090831
037900090831     P*--------------------------------------------------
038000090831     P* Procedure name: GetSubKSun
038100090831     P* Purpose:        Restituisce il subunificante legato al SUN.
038200090831     P* Returns:
038300090831     P*--------------------------------------------------
038400090831     P GetSubKSun      B
038500090831     D GetSubKSun      PI            10I 0
038600090831
038700090831     D deterministic   S               N
038800090831
038900090831      /FREE
039000090831
039100090831       CLEAR rpyEsito;
039200090831
039300090831       IF rqsDataSize < 1;
039400090831         rpyEsito = ESITO_ERROR_RQS_DATA_SIZE;
039500090831         RETURN rpyEsito;
039600090831       ENDIF;
039700090831
039800090831       IF rpyDataSize < 1;
039900090831         rpyEsito = ESITO_ERROR_RPY_DATA_SIZE;
040000090831         RETURN rpyEsito;
040100090831       ENDIF;
040200090831
040300090831       // Pulisco i parametri di output.
040400090831
040500090831       %SUBST(rpyData : 1 : rpyDataSize) = *BLANK;
040600090831
040700090831       SELECT;
040800090831         WHEN rpyFormatName = 'TIS7700O0';
040900090831         WHEN rpyFormatName = 'TIS7700O1';
041000090831           RESET tis7700o1;
041100090831         OTHER;
041200090831           rpyEsito = ESITO_ERROR_RPY_FORMAT_NAME;
041300090831           RETURN rpyEsito;
041400090831       ENDSL;
041500090831
041600090831       // Ricevo i parametri di input.
041700090831
041800090831       SELECT;
041900090831         WHEN rqsFormatName = 'TIS7700I0';
042000090831           tis7700i0 = %SUBST(rqsData : 1 : rqsDataSize);
042100090831           deterministic = tis7700i0.determinis = *on;
042200090831           IF tis7700i0.isv = *BLANK;
042300090831             RESET tis7700i0.isv;
042400090831           ENDIF;
042500090831           thisParms.ksu = tis7700i0.ksu;
042600090831           thisParms.isv = tis7700i0.isv;
042700090831           thisParms.sun = tis7700i0.sun;
042800090831
042900090831         WHEN rqsFormatName = 'TIS7700I1';
043000090831           tis7700i1 = %SUBST(rqsData : 1 : rqsDataSize);
043100090831           deterministic = tis7700i1.determinis = *on;
043200090831           IF tis7700i1.isv = *BLANK;
043300090831             RESET tis7700i1.isv;
043400090831           ENDIF;
043500090831           thisParms.ksu = tis7700i1.ksu;
043600090831           thisParms.isv = tis7700i1.isv;
043700090831           thisParms.sun = tis7700i1.sun;
043800090831
043900090831         OTHER;
044000090831           rpyEsito = ESITO_ERROR_RQS_FORMAT_NAME;
044100090831           RETURN rpyEsito;
044200090831       ENDSL;
044300090831
044400090831       // Codice cliente unificante obbligatorio.
044500090831
044600090831       IF thisParms.ksu = *BLANK OR thisParms.ksu = *ZERO;
044700090831         rpyEsito = ESITO_ERROR_KSU;
044800090831         RETURN rpyEsito;
044900090831       ENDIF;
045000090831
045100090831       // Strategi user number obbligatorio.
045200090831
045300090831       IF thisParms.sun = *BLANK OR thisParms.sun = *ZERO;
045400090831         rpyEsito = ESITO_ERROR_SUN;
045500090831         RETURN rpyEsito;
045600090831       ENDIF;
045700090831
045800090831       IF deterministic <> *ON OR thisParms.sun <> prevParms.sun
045900090831       OR thisParms.ksu <> prevParms.ksu OR thisParms.isv <> prevParms.isv;
046000090831
046100090831         RESET dVssDta;
046200090902         RESET tivssds;
046300090902         Selettore_record_TIVSS('GETRCDVSS' : '1' : thisParms.ksu :
046400090902               thisParms.isv : thisParms.sun : 0 : tivssds : rpyEsito);
046500090902
046600090902         IF rpyEsito = *ZERO;
046700090902            dVssDta = tivssds.vssdta;
046800090902         ELSE;
046900090902            RETURN rpyEsito;
047000090902         ENDIF;
047100090831
047200090902         // Salvo i parametri correnti
047300090902         prevParms.ksu = thisParms.ksu;
047400090902         prevParms.isv = thisParms.isv;
047500090902         prevParms.sun = thisParms.sun;
047600090831
047700090831       ENDIF;
047800090831
047900090903
048000090903       subksun = dVssDta.§VssSubK;
048100090903
048200090831       SELECT;
048300090831         WHEN rpyFormatName = 'TIS7700O1';
048400090903           tis7700o1.subksun = subksun;
048500090831           %SUBST(rpyData : 1 : rpyDataSize) = tis7700o1;
048600090831       ENDSL;
048700090831
048800090831       RETURN ESITO_OK;
048900090831
049000090831      /END-FREE
049100090831     P GetSubKSun      E
049200090831

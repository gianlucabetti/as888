000100080116     H**** DFTACTGRP(*NO) BNDDIR('QC2LE')
000200080111     H DECEDIT('0,') DATEDIT(*DMY.)
000300080111
000400060621     FTITA430C  IF   E           K DISK
000500080111     FTITAS30C  IF   E           K DISK    prefix(MAM_)
000600080111     FFNLBL01L  if   e           k disk
000700080121     FtaBEL00f  if   e           k disk
000800080111
000900080121     FTNVRDPDiF o  a E           k DISK    rename(DPDTOTALIi:tnvrDPDi)
001000080121     f                                     usropn
001100080121     FTNVRDPDeF o  a E           k DISK    rename(DPDTOTALIe:tnvrDPDe)
001200080121     f                                     usropn
001300080121     FTNVRDPD1F uf a E           k DISK    rename(DPDTOTALI:tnvrDPD1)
001400080121     f                                     usropn
001500080116     FTNVRDPD2F o  a E             DISK    rename(DPDRESI:tnvrDPD2)
001600080111
001700060621     D DS_TAS        E DS                  EXTNAME(TITAS00F) INZ
001800080116     D DSBL4I        E DS                  INZ
001900080121     D DSTB          E DS                  INZ
002000080111
002100080111     d RPYESITO        s             10S 0                                      Esito.
002200080111     d RPYOPCODE       s             10A                                        Cod Oper.risposta
002300080111     d RPYSTATUS       s             10S 0                                      Stato risposta.
002400080116
002500080116     d TITA4_Trovato   s              1
002600080116     d TiTAS_Trovato   s              1
002700080116     d FnLBL_Trovato   s              1
002800080116
002900080116     d Anno_di4        s              4s 0
003000080116     d Mese_di2        s              2s 0
003100080116
003200080116     d Bolla_Reso      s              1
003300080116     d Filiale_Depot   s              1
003400080121     d Bolla_di        s              3                                         Bolla tipo
003500080116     d ParcelNr        s             14
003600080117     d Parcel_di       s              1
003700080121     d Lunghezza_Parcel...
003800080121     d                 s              2s 0
003900080116
004000080115     d Depot_DPD       s              4
004100080116     d Filiale_Bartolini...
004200080116     d                 s              4
004300080111
004400080111     d acceso          c                   '1'
004500080111     d spento          c                   '0'
004600080111     D ESITO_OK        C                   0
004700080111     D ESITO_WARNING   C                   1
004800080111     D ESITO_ERROR     C                   -1
004900080117     d Import          c                   'Imp'
005000080117     d Export          c                   'Exp'
005100080115     d Bartolini       c                   'B'
005200080115     d DPD             c                   'D'
005300080115     d Bolla           c                   'B'
005400080115     d Reso            c                   'R'
005500080116     d Depot           c                   'D'
005600080116     d Filiale         c                   'F'
005700080111
005800080111      *------------------------------------------------------------------------*
005900080111      * Creazione RecordSET bolle DPD  Import/Export
006000080111      *   interessate alla verifica nei 6 mesi precedenti
006100080111      *------------------------------------------------------------------------*
006200080111     C/EXEC SQL
006300080111     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
006400080111     C/END-EXEC
006500080116      *------------------------------------------------------------------------*
006600080121     c                   open      tnvrDPD1F
006700080111
006800080111      *  In ordine legge prima Bolle Import e poi Export solo DPD
006900080111
007000080111     C/EXEC SQL
007100080111     C+ DECLARE C1 CURSOR FOR
007200080111     C+ SELECT * FROM TITAS10F
007300080111     C+    WHERE TASLNP between 190 and 197 or
007400080111     C+          TASLNA between 190 and 197
007500080111     C/END-EXEC
007600080111
007700080111     C/EXEC SQL
007800080111     C+ OPEN C1
007900080111     C/END-EXEC
008000080111      /FREE
008100080111
008200080111         IF sqlCode < 0;
008300080111           rpyEsito = ESITO_ERROR;
008400080111           rpyOpCode = 'SQLCODE';
008500080111           rpyStatus = sqlCode;
008600080111           EXSR uscita;
008700080111         ENDIF;
008800080111
008900080111         DOU sqlCod = 100;
009000080111
009100080111      /END-FREE
009200080111     C/EXEC SQL
009300080111     C+ FETCH NEXT FROM C1 INTO :DS_TAS
009400080111     C/END-EXEC
009500080111      /FREE
009600080111
009700080111           SELECT;
009800080111             WHEN sqlCode = 100;
009900080111               LEAVE;
010000080111
010100080111             WHEN sqlCode < 0;
010200080111               rpyEsito = ESITO_ERROR;
010300080111               rpyOpCode = 'SQLCODE';
010400080111               rpyStatus = sqlCode;
010500080111               LEAVE;
010600080111
010700080111             OTHER;
010800080121
010900080121?            //  Tipi Bolle : Normali o Storno_Normali
011000080121               clear TBLKEY;
011100080121               TBLKEY = tasTBL;
011200080121               CHAIN (1:'TB':tblKEY) taBEL00f;
011300080121
011400080121               if %Found(tabel00f);
011500080121               dsTB = tblUNI;
011600080121
011700080121?            //  Raggruppamento Bolle
011800080121                 if §TbrBL='S' or §TbrBL='N';
011900080121                   EXSR run_Row;
012000080121                   EXSR Conta_Parcels;
012100080121                 endIF;
012200080121
012300080121               endIF;
012400080111
012500080111           ENDSL;
012600080111
012700080111         ENDDO;
012800080111
012900080111      /END-FREE
013000080111     C/EXEC SQL
013100080111     C+ CLOSE C1
013200080111     C/END-EXEC
013300080121     c                   close     tnvrDPD1F
013400080111      /FREE
013500080111
013600080111       EXSR uscita;
013700080111
013800080111       //*******************************************************************************************
013900080111       // Uscita.
014000080111       //*******************************************************************************************
014100080121       BEGSR uscita;
014200080121
014300080121?            //  Divide i dati su 2 files x separare Import e Export
014400080121         open  TnvrDPD1F;
014500080121         open  TnvrDPDiF;
014600080121         open  TnvrDPDeF;
014700080121
014800080121         read  TnvrDPD1F;
014900080121         dow   not %Eof(TnvrDPD1F);
015000080121
015100080121         if    IMP_O_EXP = Import;
015200080121           write TnvrDPDi;
015300080121         endif;
015400080121
015500080121         if    IMP_O_EXP = Export;
015600080121           write TnvrDPDe;
015700080121         endif;
015800080121
015900080121         read  TnvrDPD1F;
016000080121         enddo;
016100080111
016200080121
016300080111         IF rpyOpCode = *BLANK;
016400080111           rpyOpCode = 'DONE';
016500080111           rpyStatus = ESITO_OK;
016600080111         ENDIF;
016700080111
016800080111         *INLR = *ON;
016900080111         RETURN;
017000080111
017100080111       ENDSR;
017200080111
017300080111       //*******************************************************************************************
017400080111       // Dettaglio
017500080111       //*******************************************************************************************
017600080111       BEGSR Run_Row;
017700080114
017800080121?      //  Anno e Mese
017900080117         Anno_di4 = tasAAS;
018000080116         Mese_di2 = tasMGS/100;
018100080116
018200080117?      //  Import/Export in base alla linea
018300080117         IF  190 <= tasLNP and tasLNP <= 197;             // Linea Partenza DPD -> Import
018400080121               Filiale_Bartolini  = ' '+ %editc(tasLNA:'X');
018500080121
018600080117           Bolla_di = Import;
018700080121
018800080117         elSE;                                            // Linea Arrivo   DPD -> Export
018900080121               Filiale_Bartolini  = ' '+ %editc(tasLNP:'X');
019000080121
019100080121           Bolla_di = Export;
019200080121
019300080115         endIF;
019400080115
019500080115?      //  trova il Parcel e il Depot   ?
019600080116         CHAIN (tasAAS:tasLNP:tasNRS:tasNSP:'I') tiTA430c;
019700080115
019800080115?        // trovato il nome del parcel e il depot .  ?
019900080115         if %Found(Tita430c);
020000080115             DsBL4i = ta4not;
020100080116             Tita4_trovato = 'S';
020200080121
020300080121             Lunghezza_Parcel = 14;
020400080121             if   %subst(§B4IPN:12:1) = ' ';
020500080121             Lunghezza_Parcel = 11;
020600080121             ParcelNr = %subst(§B4IPN:1:11);
020700080121             else;
020800080121             ParcelNr = §B4IPN;
020900080121             ParcelNr = %subst(§B4IPN:1:14);
021000080121             endIF;
021100080115
021200080115?        // con il nome Parcel si sa di chi è la spedizione
021300080121             if   %subst(§B4IPN:1:4) = '0844' and Lunghezza_Parcel = 14 or
021400080121                  %subst(§B4IPN:1:4) = '0845' and Lunghezza_Parcel = 14 or
021500080121                  %subst(§B4IPN:1:3) = '844'  and Lunghezza_Parcel = 11 or
021600080121                  %subst(§B4IPN:1:3) = '845'  and Lunghezza_Parcel = 11;
021700080117                Parcel_di = Bartolini;
021800080121
021900080121?           //  se si tratta di un reso ...occorre prendere il Depot DPD sulla Mamma
022000080121?           //  quindi occorre sostituirlo
022100080121                Depot_DPD = §B4IDA;                          // Depot di Arrivo   DPD
022200080121
022300080115             else;
022400080121
022500080117                Parcel_di = DPD;
022600080121                If Lunghezza_Parcel = 14;
022700080121                   Depot_DPD = %subst(§B4IPN:1:4);           // Depot di Partenza DPD
022800080121                else;
022900080121                   Depot_DPD = '0' + %subst(§B4IPN:1:3);     // Depot di Partenza DPD
023000080121                endIF;
023100080115             endIF;
023200080115
023300080115         elSE;
023400080116             Tita4_trovato = 'N';
023500080117             Parcel_di = *blank;
023600080117             Depot_DPD = *blank;
023700080117             ParcelNr = *blank;
023800080115         endIF;
023900080115
024000080116         //?  controlla se è un Reso o è una Semplice Bolla
024100080115         Bolla_Reso = Bolla;
024200080117         iF  Bolla_di = Import and Parcel_di = Bartolini  or
024300080117             Bolla_di = Export and Parcel_di = DPD;
024400080121
024500080117             Bolla_Reso = Reso;
024600080117             exsr Controlla_Bolla_Di_Reso;
024700080121
024800080116         endIF;
024900080115
025000080116
025100080115       ENDSR;
025200080121       //*******************************************************************************************
025300080121       // x Bolla di Reso verifica i dati della Mamma
025400080121       //*******************************************************************************************
025500080121       BEGSR Controlla_Bolla_Di_Reso;
025600080121
025700080121         clear tnvrDPD2;
025800080121         tiTAS_Trovato = 'N';
025900080121         fnLBL_Trovato = 'N';
026000080121
026100080121?      //  inverte il verso come descrizione sul RESO
026200080121         If Bolla_di = Import;
026300080121                 Imp_Exp  = Export;
026400080121         else;
026500080121                 Imp_Exp  = Import;
026600080121         endif;
026700080121
026800080121
026900080121         Anno_Mese= %char(Anno_di4) + '/' + %editc(Mese_di2:'X');
027000080121         Parcel   = ParcelNr;
027100080121         reso_Ann = tasAAS;
027200080121         reso_LNP = tasLNP;
027300080121         reso_NRS = tasNRS;
027400080121         reso_NSP = tasNSP;
027500080121         reso_MGS = tasMGS;
027600080121         reso_LNA = tasLNA;
027700080121         reso_FBR = tasFBR;
027800080121         reso_CCA = tasCCA;
027900080121         reso_TBL = tasTBL;
028000080121         reso_CBO = tasCBO;
028100080121
028200080121        //?  trova i legami con la Mamma
028300080121         CHAIN (tasAAS:tasLNP:tasNRS:tasNSP) fnLBL01L;
028400080121
028500080121         IF %FOUND(fnLBL01L);
028600080121            fnLBL_Trovato = 'S';
028700080121            CHAIN (lblAAP:lblLPP:lblNRP:lblNSP) tiTAS30c;
028800080121
028900080121            IF %FOUND(tiTAS30c);
029000080121              TiTAS_Trovato = 'S';
029100080121              mamma_Ann = MAM_tasAAS;
029200080121              mamma_LNP = MAM_tasLNP;
029300080121              mamma_NRS = MAM_tasNRS;
029400080121              mamma_NSP = MAM_tasNSP;
029500080121              mamma_MGS = MAM_tasMGS;
029600080121              mamma_LNA = MAM_tasLNA;
029700080121              mamma_CCA = MAM_tasCCA;
029800080121              mamma_TBL = MAM_tasTBL;
029900080121              mamma_CBO = MAM_tasCBO;
030000080121
030100080121?      //  Ora x un Reso di Parcel Bartolini in IMPORT si prende
030200080121?      //  il TITA4 della mamma x reperire il Depot di arrivo della
030300080121?      //  Bolla Madre di Export.
030400080121              If Bolla_di = Import;
030500080121?                //  trova il Depot di Arrivo?sulla mamma
030600080121                   CHAIN (MAM_tasAAS:MAM_tasLNP:MAM_tasNRS:
030700080121                          MAM_tasNSP:'I') tiTA430c;
030800080121?                // trovato il nome del DEPOT di ARRIVO?DPD
030900080121                   if %Found(Tita430c);
031000080121                       DsBL4i = ta4not;
031100080121                       Depot_DPD = §B4IDA;       // Depot di Arrivo   DPD
031200080121                   Else;
031300080121                   EndiF;
031400080121
031500080121              EndiF;
031600080121
031700080121            ENDIF;
031800080121
031900080121         ENDIF;
032000080121
032100080121?      //  Attenzione al Depot DPD
032200080121         CodDepot = Depot_DPD;
032300080121
032400080121?      //  Se non è un RESO:
032500080121        //?  Se si tratta di un semplice Dirottamento non deve scriverla fra i Resi
032600080121        //?  Né se è un Nostro export con il Parcel sbagliato manualmente
032700080121         If (Reso_FBR = ' ' and mamma_CCA = '1') or
032800080121            (Bolla_di = Export and TiTAS_Trovato = 'N');
032900080121           Bolla_Reso = Bolla;
033000080121         EndIF;
033100080121
033200080121?      //  Solo se è un RESO:
033300080121         If Bolla_Reso = Reso;
033400080121           write tnvrDPD2;
033500080121         EndIF;
033600080121
033700080121
033800080121       ENDSR;
033900080115       //*******************************************************************************************
034000080116       // scrive il file di totali Parcels movimentati sul Depot o sulla filiale
034100080115       //*******************************************************************************************
034200080116       BEGSR Conta_Parcels;
034300080116
034400080117        //?  Conta i parcel x Depot
034500080116       clear totParcels;
034600080116       clear totResi;
034700080122
034800080122         if Bolla_Reso = Reso;
034900080122            If Bolla_di = Import;
035000080122               Bolla_di = Export;
035100080122            else;
035200080122               Bolla_di = Import;
035300080122            endif;
035400080122         endIF;
035500080121
035600080121        //?  Prima i dati li ha su un unico file... poi li separerà
035700080121         CHAIN (Bolla_di:Depot_DPD:Anno_di4:Mese_di2) tnvrDPD1F;
035800080121
035900080121         if Bolla_Reso = Reso;
036000080121               totResi = totResi + 1;
036100080121         else;
036200080121               totParcels = totParcels + 1;
036300080121         endIF;
036400080121
036500080121         if %Found(tnvrDPD1f);
036600080121           update tnvrDPD1;
036700080121         else;
036800080121           Imp_o_Exp = Bolla_di;
036900080121           NomeDepot = Depot_DPD;
037000080121           Anno      = Anno_di4;
037100080121           Mese      = Mese_di2;
037200080121           write  tnvrDPD1;
037300080121         endif;
037400080121
037500080116       ENDSR;
037600080111       //*******************************************************************************************
037700080111      /END-FREE
